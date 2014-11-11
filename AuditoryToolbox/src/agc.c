/* =======================================================================
*		agc.c-	Main code for doing cochlear filter 
*			automatic gain control.
*	
*		Written : 	January 7, 1992
*		by :		Daniel Naar	
*									
*		Changes	:	Cleaned up by Malcolm
*				June 11, 1993
*				November 11, 1997 malcolm@interval.com
*				Massive cleanup by malcolm on 9/17/98
*		(c) 1997 Interval Research Corporation
*		[output] = agc(input, Coeffs, output, states)
* ========================================================================*/

/*
 *	Test this command by trying the following... the correct results are 
 *	shown below.  
 *
 *	>> agc(ones(1,20),[.5;.5])
 *
 *	ans =
 *	
 *	Columns 1 through 7
 *	
 *	  1.0000    0.1000    0.4500    0.2750    0.3625    0.3187    0.3406
 *	
 *	Columns 8 through 14
 *	
 *	  0.3297    0.3352    0.3324    0.3338    0.3331    0.3334    0.3333
 *	
 *	Columns 15 through 20
 *	
 *	  0.3334    0.3333    0.3333    0.3333    0.3333    0.3333
 *	
 *	Note, if you change the value of EPS (the time constant), then the 
 *	first few results will change, but the asymptote should be the same.
 *
 *	This new version of the AGC mex function makes the state variables
 *	explicit.  To make sure the state is handled correctly, compare the
 *	output of these two sequences of commands.
 *	>> agc(ones(1,20),[.5;.5])
 *	and
 *	>> [output s] = agc(ones(1,10),[.5;.5])
 *	>> agc(ones(1,10),[.5; .5], s)
 *
 *	To check the inverse AGC function do the following
 *	>> output = agc(ones(1,20),[.5;.5])
 *	>> inverseagc(output, [.5; .5])
 *	The answer should be all ones, identical to the input.
 */

#define	pINPUTM		prhs[0]
#define	pCOEFFSM	prhs[1]
#define pSTATEM		prhs[2]

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
					 * information  between the CheckArgs()
					 * and the main program.
					 */
static DOUBLE	*inputData, *outputData, *state, *agcParms, *stateArray;
static INT	nSamples, nChannels, nStages;
static mxArray	*outputMatrix = 0, *stateMatrix = 0;

#ifdef	INVERSE	
#define	MexFunctionName "inverseagc"
#else
#define	MexFunctionName	"agc"
#endif

   
/* =======================================================================*/
/*	Function to determine if arguments are valid. 			*/

static int CheckArguments(int nlhs, mxArray *plhs[], int nrhs, 
	const mxArray *prhs[])
{     
	if (nrhs < 2 || nrhs > 3 || nlhs > 2){
		mexPrintf("Incorrect calling syntax : [output,state] = %s", 
			MexFunctionName);
		mexPrintf("(input, Coeffs, state)\n");
		mexPrintf("        input is C x N\n");
		mexPrintf("        Coeffs is 2 x S (targets;epsilons)\n");
		mexPrintf("        state is C x S\n");
		mexPrintf("        output is C x N\n");
		return 1;
	}

/*------------ Check that input is not empty ---------------------- */	
	nSamples = mxGetN(pINPUTM);
	nChannels = mxGetM(pINPUTM);
	if(nSamples == 0 || nChannels == 0)   {
		mexPrintf("Input array is empty\n");
		return 1;
	}
	inputData = mxGetPr(pINPUTM);
	
/*------------ Get the number of AGC stages ----------------------- */
	nStages = mxGetN(pCOEFFSM);
	if ( mxGetM(pCOEFFSM) != 2) {
		mexPrintf("Coefficient Matrix must have 2 rows: "
			"Coeffs=[target;epsilon]");
		return 1;
	}
	agcParms = mxGetPr(pCOEFFSM);
	
/*------------ Allocate the Output Matrix ------------------------- */	
	outputMatrix = mxCreateDoubleMatrix(nChannels, nSamples, mxREAL);
	outputData = mxGetPr(outputMatrix);

/*---- Create the state matrix, fill in the input values if specified --*/
	stateMatrix = mxCreateDoubleMatrix(nChannels, nStages, mxREAL);
	stateArray = mxGetPr(stateMatrix);
	if ( nrhs >= 3 ) {
		DOUBLE	*inputStateArray;
		int	i;

		if ( mxGetM(pSTATEM) != nChannels || 
		     mxGetN(pSTATEM) != nStages){
			mexPrintf("MEX file %s got a bad state "
				"input. Should be size %dx%d.\n", 
				mexFunctionName,
				nChannels, nStages);
			mexPrintf(" Got %dx %d.\n", mxGetM(pSTATEM),
				mxGetN(pSTATEM));
			return 1;
		}
		inputStateArray = mxGetPr(pSTATEM);
		for (i=0; i<mxGetSize(pSTATEM); i++)
			stateArray[i] = inputStateArray[i];
	}

	return 0;
}  

/* =======================================================================*/

#define	EPS	(1e-1)

#ifdef	macintosh			/* This gets around float size prob */
#define	fabs(x)	((x)>0?x:0)
#endif

void agc (DOUBLE *input, DOUBLE *output, DOUBLE *state, double epsilon, 
	double target, int n) {
        register INT	i;
        DOUBLE 		f, StateLimit=1.-EPS;
        register DOUBLE OneMinusEpsOverThree = (1.0 - epsilon)/3.0;
        register DOUBLE EpsOverTarget = epsilon/target;
	register DOUBLE	prevState;

	prevState = state[0];
	for (i=0;i<n-1;i++){
#ifdef	INVERSE
		f = input[i] * EpsOverTarget + 
		    OneMinusEpsOverThree* (prevState + state[i] + state[i+1]);
		output[i] = fabs(input[i] / (1.0 - state[i]));
#else
		output[i] = fabs(input[i] * (1.0 - state[i]));
		f = output[i] * EpsOverTarget + 
		    OneMinusEpsOverThree* (prevState + state[i] + state[i+1]);
#endif
		if (f > StateLimit) f = StateLimit;
		prevState = state[i];
		state[i] = f;		
        }
#ifdef	INVERSE
	f = input[i] * EpsOverTarget + 
		OneMinusEpsOverThree*(prevState + state[i] + state[i]);
	output[i] = fabs(input[i] / (1.0 - state[i]));
#else
	output[i] = fabs(input[i] * (1.0 - state[i]));
	f = output[i] * EpsOverTarget + 
		OneMinusEpsOverThree*(prevState + state[i] + state[i]);
#endif
	if (f > StateLimit) f = StateLimit;
	state[i] = f;
}

#define	kCommandSize	20
          
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) { 	
	char 		*ErrMessage ;
	register int	i, j;

	if (nrhs >= 1 && mxIsChar(prhs[0])){
						/* Ignore old text commands */
		return;
	}
	
	if (CheckArguments(nlhs, plhs, nrhs, prhs)){
		mexErrMsgTxt("AGC argument checking failed.");
		return ;
	}

#ifdef	INVERSE
	for (j=0; j<nSamples; j++){
		agc(inputData + j*nChannels, outputData + j*nChannels,
			stateArray + (nStages-1)*nChannels,
			(double)agcParms[1+(nStages-1)*2], (double)agcParms[0+(nStages-1)*2], 
			(int)nChannels);
						/* Target; Epsilon */
		for (i=nStages-2; i>=0; i--){
			agc(outputData + j*nChannels, outputData + j*nChannels,
				stateArray + i*nChannels,
				(double)agcParms[1+i*2], 
				(double)agcParms[0+i*2], 
				(int)nChannels);
		}
	}
#else
	for (j=0; j<nSamples; j++){
		agc(inputData + j*nChannels, outputData + j*nChannels,
			stateArray + 0*nChannels,
			(double)agcParms[1+0*2], (double)agcParms[0+0*2], 
			(int)nChannels);
						/* Target; Epsilon */
		for (i=1; i<nStages; i++){
			agc(outputData + j*nChannels, outputData + j*nChannels,
				stateArray + i*nChannels,
				(double)agcParms[1+i*2], 
				(double)agcParms[0+i*2], 
				(int)nChannels);
		}
	}
#endif
						/* Assign output pointers */
	plhs[0] = outputMatrix; 
	if (nlhs > 1)
		plhs[1] = stateMatrix;
	else
		mxDestroyArray(stateMatrix);
}

