#!/usr/bin/python
# -*- coding: utf-8 -*-

# ------------------------------------
# file: features.py
# date: Tue April 22 18:31 2014
# author:
# Maarten Versteegh
# github.com/mwv
# maartenversteegh AT gmail DOT com
#
# Licensed under GPLv3
# ------------------------------------
"""features:

"""

from __future__ import division

import argparse
import json
import os.path as path
import os
import wave
import struct

import numpy as np

import spectral
from oct2py import octave
import logging
from oct2py import Oct2Py, get_log
import npz2h5features
import shutil
import scipy.io
import glob


def resample(sig, ratio):
    try:
        import scikits.samplerate
        return scikits.samplerate.resample(sig, ratio, 'sinc_best')
    except ImportError:
        import scipy.signal
        return scipy.signal.resample(sig, int(round(sig.shape[0] * ratio)))


def parse_args():
    parser = argparse.ArgumentParser(
        prog='features.py',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description='Extract Mel spectral features from audio files.',
        epilog="""Example usage:

$ python features.py -f test/wavs/*.wav -c mel_config.json

extracts features from audio files in current directory.\n

The output format is binary .npz. Each file contains two arrays, one holding
the features, the other the center times (in seconds) of the frames. To load
these files in python:

>>> import numpy as np
>>> data = np.load('/path/to/file.npz')
>>> features = data['features']
>>> center_times = data['time']
""")
    parser.add_argument('files', metavar='WAV',
                        nargs='+',
                        help='input audio files')
    parser.add_argument('-h5',
                        action='store',
                        dest='h5_output',
                        required=False,
                        help='output file in h5 format.\n'
                             ' This is the default output format')
    parser.add_argument('-npz',
                        action='store',
                        dest='npz_output',
                        required=False,
                        help='output directory in npz format.\n'
                             'only precise it if you want to use the numpy '
                             'matrices directly')
    parser.add_argument('-mat',
                        action='store',
                        dest='mat_output',
                        required=False,
                        help='output directory in matlab format.\n'
                             'Only precise it if you want to use the matlab '
                             'matrices directly')
    parser.add_argument('-c', '--config',
                        action='store',
                        dest='config',
                        required=True,
                        help='configuration file.\n'
                             'Contains the type of features to extract and the'
                             ' parameters in json format. See the default '
                             'config files for examples:\n'
                             'mel_config.json, mfcc_config.json, '
                             'rasta_config.json, lyon_config.json, '
                             'drnl_config.json')
    parser.add_argument('-f', '--force',
                        action='store_true',
                        dest='force',
                        default=False,
                        help='force resampling in case of samplerate mismatch.'
                             '\nUse this option with parcimony, only to '
                             'make the samplerate uniform among the files. '
                             'If you want to resample every file, you should '
                             'do it before running the program.')
    return vars(parser.parse_args())


def convert(files, outdir, encoder, force):
    for f in files:
        try:
            fid = wave.open(f, 'r')
            _, _, fs, nframes, _, _ = fid.getparams()
            sig = np.array(struct.unpack_from("%dh" % nframes,
                                              fid.readframes(nframes)))
            fid.close()
        except IOError:
            print 'No such file:', f
            exit()

        if fs != encoder.config['fs']:
            if force:
                sig = resample(sig, encoder.config['fs'] / fs)
            else:
                print ('Samplerate mismatch, expected {0}, got {1}, in {2}.\n'
                       'Use option -f to force resampling of the audio file. '
                       'Note that you should use force with parsimony, it is '
                       'better to adjust the sampling rate to your wav files'
                       .format(encoder.config['fs'], fs, f))
                exit()

        feats = encoder.transform(sig)[0]
        bname = path.splitext(path.basename(f))[0]
        wshift_smp = encoder.config['fs'] / encoder.config['frate']
        wlen_smp = encoder.config['wlen'] * encoder.config['fs']
        nframes = int(sig.shape[0] / wshift_smp + 1)
        if nframes != feats.shape[0]:
            raise ValueError('nframes mismatch. expected {0}, got {1}'
                             .format(feats.shape[0], nframes))
        center_times = np.zeros((nframes,))
        for fr in range(nframes):
            start_smp = round(fr * wshift_smp)
            end_smp = min(sig.shape[0],
                          start_smp + wlen_smp)
            start_ms = start_smp / fs
            end_ms = end_smp / fs
            center_times[fr] = (start_ms + end_ms) / 2
        np.savez(path.join(outdir, bname + '.npz'),
                 features=feats,
                 time=center_times)


def center_times(fs, wshift_smp, wlen_smp, sig_len):
    nframes = int(sig_len / wshift_smp + 1)
    center_times = np.zeros((nframes,))
    for fr in range(nframes):
        start_smp = round(fr * wshift_smp)
        end_smp = min(sig_len,
                      start_smp + wlen_smp)
        start_ms = start_smp / fs
        end_ms = end_smp / fs
        center_times[fr] = (start_ms + end_ms) / 2


def mat2npz(indir, outdir):
    for f in glob.glob(indir + '*.mat'):
        mat = scipy.io.loadmat(f)
        np.savez(path.join(outdir, os.path.basename(f) + '.npz'),
                 features=mat['features'],
                 time=np.ravel(mat['center_times']))

if __name__ == '__main__':
    args = parse_args()
    config_file = args['config']
    try:
        with open(config_file, 'r') as fid:
            config = json.load(fid)
    except IOError:
        print 'No such file: ', config_file
        exit()

    force = args['force']
    feat = config['features']
    files = args['files']
    h5file = args['h5_output']
    npzdir = args['npz_output']
    matdir = args['mat_output']

    if not (h5file or npzdir or matdir):
        h5file = feat + '.features'

    if h5file and os.path.exists(h5file):
        print 'The output file already exists:', h5file
        exit()
    if npzdir and not os.path.exists(npzdir):
        print 'No such directory:', npzdir
        exit()
    if matdir and not os.path.exists(matdir):
        print 'No such directory:', matdir
        exit()

    tmp = False
    python = (feat == 'mel' or feat == 'mfcc')
    octave = (feat == 'rasta' or feat == 'lyon' or feat == 'drnl')
    if (python and not npzdir) or (octave and not matdir):
        outdir = os.getcwd() + '/tmp/'
        if not os.path.exists(outdir):
            os.makedirs(outdir)
            tmp = True
        else:
            print 'tmp folder already exists, please remove it'
            exit()

    if feat == 'mel':
        del config['features']
        encoder = spectral.CubicMel(**config)
        convert(files, outdir, encoder, force)
    elif feat == 'mfcc':
        del config['features']
        encoder = spectral.MFCC(**config)
        convert(files, outdir, encoder, force)
    else:
        oc = Oct2Py(logger=get_log())
        oc.logger = get_log('new_log')
        oc.logger.setLevel(logging.INFO)
        oc.call("features", files, outdir, feat, config_file, force,
                verbose=True)
        if npzdir:
            outdir2 = npzdir
        else:
            outdir2 = outdir
        mat2npz(outdir, outdir2)
    if h5file:
        npz2h5features.convert(outdir, h5file)
    if tmp:
        shutil.rmtree(outdir)
