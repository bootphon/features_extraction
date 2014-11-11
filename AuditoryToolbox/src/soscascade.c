/* =======================================================================
*		soscascade.c 	Main code for doing cochlear filter 
*				operations for the second order 
*				section (sos) model.  Assumes that 
*				the filter coefficients have been 
*				designed.  This implements a CASCADE
*				of second order sections.
*	
*		Written : 	January 7, 1992
*		by :		Daniel Naar	
*									
*		Changes	:	Cleaned up by Malcolm
*				June 11, 1993-March 8, 1994
*				November 11, 1997 malcolm@interval.com
*				September 17, 1998 malcolm@interval.com
*		(c) 1997 Interval Research Corporation
*		[output,state] = soscascade(input, Coeffs, states)
* ========================================================================*/

/*
 *	Test this command by trying the following... the correct results are 
 *	shown below.  (The first filter is a simple exponential decay.  The
 *	second filter sums the last two outputs from filter one.)
 *
 *	>>soscascade([1 0 0 0 0],[1 0 0 -.9 0;1 1 0 0 0]) 
 *	ans =
 *
 *	     1.0000    0.9000    0.8100    0.7290    0.6561
 *	     1.0000    1.9000    1.7100    1.5390    1.3851
 *	
 *	To check the state variable usage compare the output of these two
 *	sets of commands
 *	>>soscascade([1 0 0 0 0 0 0 0 0 0],[1 0 0 -.9 0;1 1 0 0 0])
 *	and
 *	>>[y,s] = soscascade([1 0 0 0 0],[1 0 0 -.9 0;1 1 0 0 0])
 *	>>soscascade([0 0 0 0 0],[1 0 0 -.9 0;1 1 0 0 0], s)
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

					/* OK, this is sleazy.... but
					 * we only want to save this 
					 * information between the CheckArgs()
					 * function and the main program.  This
					 * works as long as we aren't multi-
					 * threaded.
					 */
static DOUBLE	*inputData, *outputData, *state1, *state2;
static DOUBLE	*a0, *a1, *a2, *b1, *b2;
static INT	nSamples, nChannels;
static mxArray	*outputMatrix = 0, *stateMatrix =0;

/* =====================================================================*/
/*	Function to determine if arguments are valid. 			*/
/*	Also fill in some pointers we will need later.			*/

static  int CheckArguments(int nlhs, mxArray *plhs[], 
				int nrhs, const mxArray *prhs[])
{     
	DOUBLE	*stateData;

	if (nrhs < 2 || nrhs > 3 || nlhs > 2){
		printf("Incorrect calling syntax:\n [output,state] = ");
		printf("sosfilter(input, Coeffs, state)\n");
		printf("        input has N samples\n");
		printf("        Coeffs is C x 5 where C is the number of "
			"channels\n");
		printf("        state is C x 2\n");
		printf("        output is C x N\n");
		printf("nlhs is %d, nrhs is %d.\n", nlhs, nrhs);
		return 1;
	}

/*------------ Check input is not empty ------------------------------ */	
	nSamples = mxGetSize(pINPUTM);
	if(nSamples == 0)   {
		printf("Input array is empty\n");
		return 1;
	}
	inputData = mxGetPr(pINPUTM);
	
/*------------ Check to See if Coeffs Matrix Is the right Size ------- */
	if ( mxGetN(pCOEFFSM) != 5) {
		printf("Coefficient Matrix Must have 5 Columns: ");
		printf("Coeffs=[A0 A1 A2 B1 B2]\n");
		return 1;
	}
	nChannels = mxGetM(pCOEFFSM);
	a0 = &(mxGetPr(pCOEFFSM)[0*nChannels]);
	a1 = &(mxGetPr(pCOEFFSM)[1*nChannels]);
	a2 = &(mxGetPr(pCOEFFSM)[2*nChannels]);
	b1 = &(mxGetPr(pCOEFFSM)[3*nChannels]);
	b2 = &(mxGetPr(pCOEFFSM)[4*nChannels]);
	
/*-------------------- Create the output matrix ----------------------- */
	outputMatrix = mxCreateDoubleMatrix(nChannels, nSamples, mxREAL);
	outputData = mxGetPr(outputMatrix);

/*---- Create the state matrix, fill in the input values if specified --*/
	stateMatrix = mxCreateDoubleMatrix(nChannels, kNumStateVars, mxREAL);
	stateData = mxGetPr(stateMatrix);
	if ( nrhs >= 3 ) {
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
	state1 = &stateData[0*nChannels];
	state2 = &stateData[1*nChannels];

	return 0;
}  

/* =======================================================================*/


#define	kCommandSize	20

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])  
{ 	
	register int	i, n;
	register double	in, *output;

	if (nrhs >= 1 && mxIsChar(prhs[0])){
						/* Obsolete commands ignored */
		return;
	}

	if (CheckArguments(nlhs, plhs, nrhs, prhs) ){
		mexErrMsgTxt("soscascade argument checking failed.");
		return ;
	}

	for (n=0; n<0*nChannels; n++){
		printf(" Channel %d: %g %g %g, %g %d\n", n, a0[n], a1[n], 
			a2[n], b1[n], b2[n]);
		printf("    state1 is %g, state2 is %g.\n", state1[n], 
			state2[n]);
	}

	for (n=0; n<nSamples; n++){
		in = inputData[n];
		output = &outputData[n*nChannels];
		for (i=0; i<nChannels; i++){
			output[i] = a0[i] * in                     + state1[i];
			state1[i] = a1[i] * in - b1[i] * output[i] + state2[i];
			state2[i] = a2[i] * in - b2[i] * output[i];
			in = output[i];
		}
	}
						/* Assign output pointers */
	plhs[0] = outputMatrix; 
	if (nlhs > 1)
		plhs[1] = stateMatrix;
	else
		mxDestroyArray(stateMatrix);
}
