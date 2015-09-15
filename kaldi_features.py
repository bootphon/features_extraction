"""Extracting features with kaldi
"""

# import distutils.spawn.find_executable as find_executable
import subprocess
import tempfile
import os
import logging
import shlex
import numpy as np
from kaldi_io import *
import argparse
import json
# assert find_executable('compute-mfcc-feats'), "kaldi 'featsbin' not in the PATH"

logging.basicConfig(level=logging.INFO)


import glob
files = glob.glob('/fhgfs/bootphon/scratch/roland/zerospeech2015/english_wavs/*.wav')[:2]
output_path = '/fhgfs/bootphon/scratch/roland/zerospeech2015/english_feats/mfcc39pitch3_npz/'
def run(files, output_path, config_file, save, batch_size=50):
    """Split in the file list into batches. Handle arguments.

    Parameters:
    -----------
    batch_size: int, max batch size in number of files (adjust for RAM usage)
    """
    with open(config_file, 'r') as fid:
        config = json.load(fid)

    batches = [files[i:i + batch_size] for i in range(0, len(files), batch_size)]
    res = {}
    for files_batch in batches:
        res.update(extract_features(files_batch, delta=0))
    if 'np' in save:
        for f in res:
            np.save(output_path + f, res[f])
    if 'h5' in save:
        raise NotImplementedError


def extract_features(files, feature_type='mfcc', normalize=False,
                     delta=2, pitch=False, vtln=False, spk2utt=None,
                     config_file=None):
    """Extract speech features using kaldi

    Parameters:
    files: list
    feature_type: {'mfcc', 'plp'}
    normalize: bool, do mean variance normalization
    delta: int, [0..2], 0 -> no deltas, 1 -> do deltas, 2 -> do delta+deltasdeltas
    pitch: bool, compute pitch
    config_file: str, path to the config file for non default parameters
    """
    try:
        # writing 'file path' in a .scp file for kaldi
        def get_fname(path):
            return os.path.basename(path).split('.')[0]
        (scpid, scpfn) = tempfile.mkstemp()
        with open(scpfn, 'w') as fout:
            fout.write('\n'.join((' '.join([get_fname(f), f]) for f in files)))
        mfccs = extract_mfccs(scpfn, delta)
        if pitch:
            pitches = compute_pitch(scpfn)
            for fname in mfccs:
                try:
                    mfccs[fname] = np.hstack((mfccs[fname], pitches[fname]))
                except ValueError:
                    pitch_len = pitches[fname].shape[0]
                    mfccs_len = mfccs[fname].shape[0]
                    logging.warning(
                        'dimension mismatch for file {}: {}, {}'
                        .format(fname, pitch_len, mfccs_len))
                    length = min(pitch_len, mfccs_len)
                    mfccs[fname] = np.hstack((mfccs[fname][:length], pitches[fname][:length]))
        return mfccs
    finally:
        tryremove(scpfn)
        

def tryremove(path):
    try:
        os.remove(path)
    except:
        pass


def compute_pitch(scpfile):
    logging.info('Extracting pitch')
    try:
        (outid, outfn) = tempfile.mkstemp()
        command_line = """compute-kaldi-pitch-feats scp:{} ark:{}""".format(scpfile, outfn)
        # command_line = """compute-and-process-kaldi-pitch-feats scp:{} ark,t:{}""".format(scpfile, outfn)
        logging.info(command_line)
        subprocess.check_output(command_line, shell=True)
        pitches = ark2dict(outfn)
        logging.debug(pitches)
        return pitches
    finally:
        tryremove(outfn)


def extract_mfccs(scpfile, delta=0):
    logging.info('Extracting MFCCs')
    #TODO write configfile from args
    NCOEFF = 13
    try:
        (outid, outfn) = tempfile.mkstemp()
        prefix_command_line = """compute-mfcc-feats scp:{} """.format(scpfile)
        suffix_command_line = """ark:{}""".format(outfn)
        if delta > 0:
            prefix_command_line += """ark:- | apply-cmvn-sliding ark:- ark:- | add-deltas ark:- """
            #TODO check cmvn, do it on spkr ? all ?
        command_line = prefix_command_line + suffix_command_line
        logging.info(command_line)
        #TODO shell=True is a security hazard. Replace pipes by communicate ?
        subprocess.check_output(command_line, shell=True)
        # mfccs = kalditext2python(outfn)
        mfccs = ark2dict(outfn)
        if delta == 1:
            # remove acceleration
            for fname in mfccs:
                mfccs[fname] = mfccs[fname][:-NCOEFF]
        logging.debug(mfccs)
        return mfccs
    finally:
        tryremove(outfn)


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
                        # required=True,
                        help='configuration file.\n'
                             'Contains the type of features to extract and the'
                             ' parameters in json format. See the default '
                             'config files for examples:\n'
                             'mel_config.json, mfcc_config.json, '
                             'rasta_config.json, lyon_config.json, '
                             'drnl_config.json')
    return vars(parser.parse_args())


if __name__ == '__main__':
    args = parse_args()
    config_file = args['config']
    # force = args['force']
    # feat = config['features']
    files = args['files']
    h5file = args['h5_output']
    npzdir = args['npz_output']
    save = set()
    if h5file:
        save.add('h5')
    if npzdir:
        save.add('np')

    run(files, output_path, config_file, save)
