Installation:
============

In short if all dependences are installed on the system 
you can install **feature_extraction** within the cli:

    python setup.py build && python setup.py install

If you are installing from source using conda  and
dependences are not installed you can do:

Gget **feature_extraction** from github:
```
>> git clone https://github.com/bootphon/features_extraction
```

Create your environment, for example if using conda

```
>> cd features_extraction
>> conda create --name feat --file requirements.txt
>> source activate feat
```

Install other dependences that are not available with conda
```
(feat) >> pip install oct2py   
(feat) >> pip install git+http://github.com/bootphon/spectral
(feat) >> pip install git+https://github.com/bootphon/h5features    
```

If you are working from the source code, you will need to 
include **features_extraction** in your path, in bash/linux:

```
>> export PATH=$PATH:{DIR_FEATURES_EXTRACTION}
>> export PYTHONPATH=$PYTHONPATH:{DIR_FEATURES_EXTRACTION}
>> chmod 755 features.py
```


Dependecies
===========

python dependences:

- spectral
- h5features
- octave

octave dependences:

- ltfat
- amtoolbox
- rastamat
- signal (octave package), require control and general
- AuditoryToolbox

To run rastama the previous packages you will need a 
working octave's distribution, these packages depends of 
the octaves packages: **general**, **control** and **signal**.
To install the packages, from octave's console line you'll need
to run:

```
>> pkg install -forge general
>> pkg install -forge control
>> pkg install -forge signal
```

AuditoryToolbox depends on compiled octave/matlab mex files, to 
create these file you will need to compile the files within the 
AuditoryToolbox directory, in linux command line **on** AuditoryToolbox/src
directory do:

>> make 

Notes
=====

for now, octave features dont have many options since they are hard coded.
.mat output for python features not implemented yet
center_times in some octave features are wrong...


Example:
```
python features.py test/wavs/*.wav -h5 my_mfccs.h5f -c mfcc_config.json
```


see config options at config_doc.rst
