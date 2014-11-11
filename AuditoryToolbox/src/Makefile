MEX = agc.c soscascade.c sosfilters.c dtw.c

MEXFLAGS =

all:    soscascade sosfilters agc invsoscascade dtw inverseagc

soscascade:     soscascade.c
		# mex $(MEXFLAGS) soscascade.c
		mkoctfile --mex $(MEXFLAGS) soscascade.c
		cp soscascade.mex* ..

invsoscascade:  invsoscascade.c
		# mex $(MEXFLAGS) invsoscascade.c
		mkoctfile --mex $(MEXFLAGS) invsoscascade.c
		cp invsoscascade.mex* ..

sosfilters:     sosfilters.c
		# mex $(MEXFLAGS) sosfilters.c
		mkoctfile --mex $(MEXFLAGS) sosfilters.c
		cp sosfilters.mex* ..

agc:            agc.c
		# mex $(MEXFLAGS) agc.c
		mkoctfile --mex $(MEXFLAGS) agc.c
		cp agc.mex* ..

inverseagc:     agc.c
		# mex $(MEXFLAGS) agc.c -DINVERSE -output inverseagc
		mkoctfile --mex $(MEXFLAGS) agc.c -DINVERSE -o inverseagc
		cp inverseagc.mex* ..

dtw:            dtw.c
		# mex $(MEXFLAGS) dtw.c -DMATLAB
		mkoctfile --mex $(MEXFLAGS) dtw.c -DMATLAB
		cp dtw.mex* ..
