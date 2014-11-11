/* =======================================================================
*		invsoscascade.c Main code for inverting a cochlear filter 
*				bank.  Assumes that the filter coefficients 
*				have been designed.  This does not run the
*				signal backwards.
*	
*		Written : 	January, 1992
*		by :		Daniel Naar	
*									
*		Changes	:	Cleaned up by Malcolm
*				June 29, 1993-March 8, 1994
*				November 11, 1997 malcolm@interval.com
*		(c) 1997 Interval Research Corporation
*		[output] = invsoscascade(input, Coeffs, output, states)
* ========================================================================*/

/*
 *	Test this command by trying the following... the correct results are 
 *	shown below.  (The first filter is a simple exponential decay.  The
 *	second filter sums the last two outputs from filter one.)
 *
 *	>>invsoscascade([1 0 0 0 0;1 0 0 0 0],[1 0 0 -.9 0;1 1 0 0 0]) 
 *	ans =
 *	    2.0000    2.8000    2.5200    2.2680    2.0412
 *
 *
 */

#define	pINPUTM			prhs[0]
#define	pCOEFFSM		prhs[1]
#define	pGAINM			prhs[2]
#define pSTATEM			prhs[3]

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

					/* OK, this is sleazy.... but
					 * we only want to save this 
					 * information between the CheckArgs()
					 * function and the main program.
					 */
static DOUBLE	*inputData, *outputData, *state1, *state2;
static DOUBLE	*a0, *a1, *a2, *b1, *b2, *gains;
static INT	nSamples, nChannels;
static mxArray	*outputMatrix, *stateMatrix;


/* =====================================================================*/
/*	Function to determine if arguments are valid. 			*/
/*	Also fill in some pointers we will need later.			*/

#ifdef	calloc
#undef	calloc
#endif

static int CheckArguments(int nlhs, mxArray *plhs[], 
			int nrhs, const mxArray *prhs[])
{     
	int	i;
	DOUBLE	*stateData;
	
	if (nrhs < 2 || nrhs > 4 || nlhs > 2){
		mexPrintf("Incorrect calling syntax:\n [output, state] = ");
		mexPrintf("invsoscascade(input, Coeffs, gains, state)\n");
		mexPrintf("   input has C x N samples\n");
		mexPrintf("   Coeffs is C x 5 where C is the number of "
			"channels\n");
		mexPrintf("   gains is a C x 1 array of channel gains\n");
		mexPrintf("   state is C x 2");
		mexPrintf("   output is 1 x N\n");
		return 1;
	}

/*------------ Check input is not empty ------------------------------ */	
	nSamples = mxGetN(pINPUTM);
	if(nSamples == 0) {
		mexPrintf("Input array is empty.\n");
		return 1;
	}
	inputData = mxGetPr(pINPUTM);
	nChannels = mxGetM(pINPUTM);
	
/*------------ Check to See if Coeffs Matrix Is the right Size ------- */
	if ( mxGetN(pCOEFFSM) != 5) {
		mexPrintf("Coefficient Matrix Must have 5 Columns: ");
		mexPrintf("Coeffs=[A0 A1 A2 B1 B2]\n");
		return 1;
	}
	if ( mxGetM(pCOEFFSM) != nChannels) {
		mexPrintf("Number of input channels must be same as number");
		mexPrintf(" of filter channels (rows).\n");
		return 1;
	}
	a0 = &(mxGetPr(pCOEFFSM)[0*nChannels]);
	a1 = &(mxGetPr(pCOEFFSM)[1*nChannels]);
	a2 = &(mxGetPr(pCOEFFSM)[2*nChannels]);
	b1 = &(mxGetPr(pCOEFFSM)[3*nChannels]);
	b2 = &(mxGetPr(pCOEFFSM)[4*nChannels]);
	
	if ( nrhs > 2 ){
		int	i = mxGetM(pGAINM) * mxGetN(pGAINM);

		if (i == nChannels){
			gains = mxGetPr(pGAINM);
		} else {
			mexPrintf("Number of gain terms must be same as "
				"number of filter channels (rows).\n");
			return 1;
		}
	} else {
		gains = 0;
	}
	
/*------------ Allocate the output matrix ------------------------------- */	
	outputMatrix = mxCreateDoubleMatrix((INT)1, nSamples, mxREAL);
	outputData = mxGetPr(outputMatrix);

/*---- Create the state matrix, fill in the input values if specified --*/
	stateMatrix = mxCreateDoubleMatrix(nChannels, kNumStateVars, mxREAL);
	stateData = mxGetPr(stateMatrix);
	if ( nrhs >= 4 ) {
		DOUBLE	*inputStateArray;
		int	i;

		if ( mxGetM(pSTATEM) != nChannels || 
		     mxGetN(pSTATEM) != kNumStateVars){
			mexPrintf("MEX file soscascade got a bad state "
				"input. Should be size %dx%d.\n", 
				nChannels, kNumStateVars);
			return 1;
		}
		inputStateArray = mxGetPr(pSTATEM);
		for (i=0; i<mxGetSize(pSTATEM); i++)
			stateData[i] = inputStateArray[i];
	}
	stateData = mxGetPr(stateMatrix);
	state1 = &stateData[0*nChannels];
	state2 = &stateData[1*nChannels];
	
	return 0;
}  

/* =======================================================================*/

#define	kCommandSize	20

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{ 	
	register int	i, n;
	register DOUBLE	in, output, *input;

	if (nrhs >= 1 && mxIsChar(prhs[0])){
						/* Ignore obsolete text cmds */
		return;
	}
	
	if (CheckArguments(nlhs, plhs, nrhs, prhs) ){
		mexErrMsgTxt("invsoscascade argument checking failed.");
		return ;
	}

	for (n=0; n<nSamples; n++){
		output = 0;
		input = &inputData[n*nChannels];
		for (i=nChannels-1; i>=0; i--){
			if (gains)
				in = output + gains[i]*input[i];
			else
				in = output + input[i];
			output    = a0[i] * in                  + state1[i];
			state1[i] = a1[i] * in - b1[i] * output + state2[i];
			state2[i] = a2[i] * in - b2[i] * output;
		}
		outputData[n] = output;
	}
						/* Assign output pointers */
	plhs[0] = outputMatrix; 
}
