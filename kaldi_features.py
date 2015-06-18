"""Extracting features with kaldi
"""

# import distutils.spawn.find_executable as find_executable
import subprocess
import tempfile
import os
import logging
import shlex
import numpy as np
import struct
# assert find_executable('compute-mfcc-feats'), "kaldi 'featsbin' not in the PATH"

logging.basicConfig(level=logging.INFO)


# files = ['/home/roland/features_extraction/test/wavs/s_f101_ba.wav']
import glob
files = glob.glob('/fhgfs/bootphon/scratch/roland/zerospeech2015/english_wavs/*.wav')[:2]
def run(files, batch_size=5):
    """Split in the file list into batches. Handle arguments.

    Parameters:
    -----------
    batch_size: int, max batch size in number of files (adjust for RAM usage)
    """
    batches = [files[i:i + batch_size] for i in range(0, len(files), batch_size)]
    res = {}
    for files_batch in batches:
        res.update(extract_features(files_batch, delta=0, pitch=True))
    output_path = '/fhgfs/bootphon/scratch/roland/zerospeech2015/english_feats/mfcc39pitch3_npz/'
    for f in res:
        np.save(output_path + f, res[f])


def extract_features(files, feature_type='mfcc', normalize=False,
                     delta=0, pitch=False, vtln=False, spk2utt=None,
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


def ark2dict(arkfile):  # bufferized version
    res = {}
    with open(arkfile) as fin:
        while True:
            #TODO: break if empty buffer here
            fname = ''
            c = fin.read(1)
            if c == '':  # EOF (EOFError not raised by read(empty))
                break
            while c != ' ':
                fname += c
                c = fin.read(1)
            logging.debug(fname)
            # end of fname
            fin.read(1)
            # data type
            assert fin.read(4) == 'BFM ', 'type not supported'
            # nrows type
            assert struct.unpack('b', fin.read(1))[0] == 4,  'type not supported'
            nrows = struct.unpack('i', fin.read(4))[0]
            # ncols type:
            assert struct.unpack('b', fin.read(1))[0] == 4,  'type not supported'
            ncols = struct.unpack('i', fin.read(4))[0]
            # data
            size = nrows * ncols * 4
            data = np.fromstring(fin.read(size), dtype=np.float32).reshape((nrows, ncols))
            res[fname] = data
    return res


def dict2ark(dictionnary, handler):
    for fname, data in dictionnary.iteritems():
        if data.dtype == np.float32:
            handler.write(fname + ' BFM ')
        elif data.dtype == np.float64:
            handler.write(fname + ' BDM ')
        else:
            logging.warning('Invalid data type for {}: {}'
                            .format(fname, data.dtype))
            logging.warning('converting data type to C float')
            data = data.astype(np.float32)
            handler.write(fname + ' BFM ')
        nrows, ncols = data.shape
        handler.write(struct.pack('bibi', 4, nrows, 4, ncols))
        handler.write(data.tobytes())
        

def tryremove(path):
    try:
        os.remove(path)
    except:
        pass


def kalditext2python(textfile):
    logging.debug('kaldi text file to numpy array: {}'.format(textfile))
    res = {}
    tmparr = []
    arrname = ''
    with open(textfile) as fin:
        for line in fin:
            splitted = line.strip().split()
            if splitted[-1] == '[':
                if arrname:
                    res[arrname] = np.array(tmparr)
                arrname = splitted[0]
            else:
                if splitted[-1] == ']':
                    splitted = splitted[:-1]
                tmparr.append(map(float, splitted))
        res[arrname] = np.array(tmparr)
    return res


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


if __name__ == '__main__':
    run(files)
