Require:
- spectral
- h5features
- octave
- ltfat
- amtoolbox
- rastamat
- signal (octave package), require control and general
- AuditoryToolbox


Installation:

add h5features to pyhtonpath

For the octave part:
install octave
run octave
pkg install -forge general
pkg install -forge control
pkg install -forge signal
install AuditoryToolbox (makefile in src)

For the python part:
git clone https://github.com/mwv/spectral.git
add spectral to pythonpath


Note:
for now, octave features dont have many options since they are hard coded.
not parallelized yet
.mat output for python features not implemented yet
center_times in octave features are wrong...
add the possibility to generate multiple features ? (much more efficient than running the program several times)
separate octave requirement from the package ? (and have them installed with a makefile)


Example:
python features.py test/wavs/*.wav -f -c rasta_config.json
see config options at config_doc.rst
