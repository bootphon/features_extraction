/* =======================================================================
*		sosfilters.c 	Main code for filtering with second
*				order sections.  Either one input channel
*				and a bunch of filters, or a bunch of
*				input channels and one filter, or a bunch
*				of input data and filters.  This implements
*				a PARALLEL bank of second order sections.
*	
*		Written : 	June 17, 1993
*		by :		Malcolm Slaney
*		Based on:	Code by Dan Naar and Malcolm Slaney
*		Changes: 	November 11, 1997 malcolm@interval.com
*		(c) 1997 Interval Research Corporation
*		[output] = sosfilters(input, Coeffs, output, states)
* ========================================================================*/

/*
 *	Test this command by trying the following... the correct results are 
 *	shown below.  (The two filters are both simple exponentials.)
 *	This first example is one input applied to a bunch of filters.
 *	>> sosfilters([1 0 0 0 0 0], [1 0 0 -.9 0; 1 0 0 -.8 0])
 *
 *	ans =
 *
 *	    1.0000    0.9000    0.8100    0.7290    0.6561    0.5905
 *	    1.0000    0.8000    0.6400    0.5120    0.4096    0.3277
 *
 *	This command can also filter an array of inputs.
 *	>> sosfilters([1 0 0 0 0 0;2 0 0 0 0 0], [1 0 0 -.9 0; 1 0 0 -.8 0])
 *
 *	ans =
 *
 *	    1.0000    0.9000    0.8100    0.7290    0.6561    0.5905
 *	    2.0000    1.6000    1.2800    1.0240    0.8192    0.6554
 *
 *	Or a bunch of inputs, independently applied to just one filter.
 *	>> sosfilters([1 0 0 0 0 0;2 0 0 0 0 0], [1 0 0 -.9 0])
 *
 *	ans =
 *
 *	    1.0000    0.9000    0.8100    0.7290    0.6561    0.5905
 *	    2.0000    1.8000    1.6200    1.4580    1.3122    1.1810
 *
 *	To check that the state variables are handled correctly, compare
 *	>> sosfilters([1 zeros(1,9)], [1 0 0 -.9 0; 1 0 0 -.8 0])
 *	and
 *	>> [output,s] = sosfilters([1 zeros(1,4)], [1 0 0 -.9 0; 1 0 0 -.8 0])
 *	>> sosfilters(zeros(1,5), [1 0 0 -.9 0; 1 0 0 -.8 0], s)
 */

#define	pINPUTM			prhs[0]
#define	pCOEFFSM		prhs[1]
#define pSTATEM			prhs[2]

#define	kNumStateVars		2

#include	<stdio.h>
#include 	<math.h>
#include	"mex.h"

#ifndef	DOUBLE
#define	DOUBLE	double
#endif

#ifndef	INT
#define	INT	int
#endif

#define	mxGetSize(m)	(mxGetN(m) * mxGetM(m))
#define max(a,b)	((a) > (b)? (a) : (b))

					/* OK, this is sleazy.... but
					 * we only want to save this 
					 * information between the CheckArgs()
					 * function and the main program.
					 */
static DOUBLE	*inputData, *outputData, *state1, *state2;
static DOUBLE	*a0, *a1, *a2, *b1, *b2;
static INT	nSamples, nInputChannels, nFilterChannels, nOutputChannels;
static mxArray	*outputMatrix, *stateMatrix;

/* =====================================================================*/
/*	Function to determine if arguments are valid. 			*/
/*	Also fill in some pointers we will need later.			*/

static int CheckArguments(int nlhs, mxArray *plhs[], 
				int nrhs, const mxArray *prhs[]){
	int	n, m;
	DOUBLE	*stateData;

	if (nrhs < 2 || nrhs > 4 || nlhs > 2){
		printf("Incorrect calling syntax:\n [output, state] = ");
		printf("sosfilter(input, Coeffs, state)\n");
		printf("        input is 1 x N or C x N\n");
		printf("        Coeffs is 1 x 5 or C x 5\n");
		printf("        state is max(1,C) x 2\n");
		printf("        output is max(1,C) x N\n");
		return 1;
	}

/*------------ Check input is not empty ------------------------------ */	
	n = mxGetN(pINPUTM);
	m = mxGetM(pINPUTM);
	if(n*m == 0)   {
		printf("Input array is empty\n");
		return 1;
	}
	if (n == 1){
		nSamples = m;
		nInputChannels = 1;
	} else {
		nSamples = n;
		nInputChannels = m;
	}
	inputData = mxGetPr(pINPUTM);
	
/*------------ Check to See if Coeffs Matrix Is the right Size ------- */
	if ( mxGetN(pCOEFFSM) != 5) {
		printf("Coefficient Matrix Must have 5 Columns: ");
		printf("Coeffs=[A0 A1 A2 B1 B2]\n");
		return 1;
	}
	nFilterChannels = mxGetM(pCOEFFSM);
	if (nInputChannels > 1 && nFilterChannels > 1 &&
	    nInputChannels != nFilterChannels){
		printf("Number of input channels and filter channels must be");
		printf(" compatible.\n");
		printf("  One or the other equals one and or both are the ");
		printf("same.\n");
		return 1;
	}
	nOutputChannels = max(nInputChannels, nFilterChannels);
	a0 = &(mxGetPr(pCOEFFSM)[0*nFilterChannels]);
	a1 = &(mxGetPr(pCOEFFSM)[1*nFilterChannels]);
	a2 = &(mxGetPr(pCOEFFSM)[2*nFilterChannels]);
	b1 = &(mxGetPr(pCOEFFSM)[3*nFilterChannels]);
	b2 = &(mxGetPr(pCOEFFSM)[4*nFilterChannels]);
	
/*-------------------- Create the output matrix -----------------------*/
	outputMatrix = mxCreateDoubleMatrix(nOutputChannels, nSamples, mxREAL);
	outputData = mxGetPr(outputMatrix);

/*---- Create the state matrix, fill in the input values if specified --*/
	stateMatrix = mxCreateDoubleMatrix(nOutputChannels, kNumStateVars, 
		mxREAL);
	stateData = mxGetPr(stateMatrix);
	if ( nrhs >= 3 ) {
		DOUBLE	*inputStateArray;
		int	i;

		if ( mxGetM(pSTATEM) != nOutputChannels || 
		     mxGetN(pSTATEM) != kNumStateVars){
			mexPrintf("MEX file sosfilters got a bad state "
				"input. Should be size %dx%d.\n", 
				nOutputChannels, kNumStateVars);
			return 1;
		}
		inputStateArray = mxGetPr(pSTATEM);
		for (i=0; i<mxGetSize(pSTATEM); i++)
			stateData[i] = inputStateArray[i];
	}
	state1 = &stateData[0*nOutputChannels];
	state2 = &stateData[1*nOutputChannels];

	return 0;
}  

/* =======================================================================*/

#define	kCommandSize	20

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]){
	register int	i, n;
	register DOUBLE	in, *output;

	if (nrhs >= 1 && mxIsChar(prhs[0])){
						/* Ignore obsolete cmds */
		return;
	}

	if (CheckArguments(nlhs, plhs, nrhs, prhs) ){
		mexErrMsgTxt("sosfilters argument checking failed.");
		return ;
	}

	if (nInputChannels == 1){
	    for (n=0; n<nSamples; n++){
	        in = inputData[n];
	        output = &outputData[n*nOutputChannels];
	        for (i=0; i<nOutputChannels; i++){
	    	    output[i] = a0[i] * in                     + state1[i];
	    	    state1[i] = a1[i] * in - b1[i] * output[i] + state2[i];
	    	    state2[i] = a2[i] * in - b2[i] * output[i];
	        }
	    }
	} else if (nFilterChannels == 1){
	    for (n=0; n<nSamples; n++){
	        output = &outputData[n*nOutputChannels];
	        for (i=0; i<nOutputChannels; i++){
	            in = inputData[n*nInputChannels + i];
	    	    output[i] = a0[0] * in                     + state1[i];
	    	    state1[i] = a1[0] * in - b1[0] * output[i] + state2[i];
	    	    state2[i] = a2[0] * in - b2[0] * output[i];
	        }
	    }
	} else {
	    for (n=0; n<nSamples; n++){
	        output = &outputData[n*nOutputChannels];
	        for (i=0; i<nOutputChannels; i++){
	            in = inputData[n*nInputChannels + i];
	    	    output[i] = a0[i] * in                     + state1[i];
	    	    state1[i] = a1[i] * in - b1[i] * output[i] + state2[i];
	    	    state2[i] = a2[i] * in - b2[i] * output[i];
	        }
	    }
	}
						/* Assign output pointers */
	plhs[0] = outputMatrix; 
	if (nlhs > 1)
		plhs[1] = stateMatrix;
	else
		mxDestroyArray(stateMatrix);
}
