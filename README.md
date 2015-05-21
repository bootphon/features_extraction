Installation:

	python setup.py build && python setup.py install

Uses:

- spectral
- h5features
- octave
- ltfat
- amtoolbox
- rastamat
- signal (octave package), require control and general
- AuditoryToolbox


For the octave part:

install octave
run octave
pkg install -forge general
pkg install -forge control
pkg install -forge signal
install AuditoryToolbox (makefile in src)


Note:

for now, octave features dont have many options since they are hard coded.
.mat output for python features not implemented yet
center_times in some octave features are wrong...


Example:

	python features.py test/wavs/*.wav -h5 my_mfccs.h5f -c mfcc_config.json

see config options at config_doc.rst
