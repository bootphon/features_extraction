/* 
 * This file is part of the gammatone filterbank reference implementation
 * described in V. Hohmann's 2002 `acta acustica' article.
 *
 * It contains the matlab interface function for the Gfb_Analyzer_fprocess
 * function.
 * Gfb_Analyzer_fprocess is a Matlab extension in the C programming
 * language with the same functionality as Gfb_Analyzer_process: It
 * invokes the optimized filterbank analysis as described in
 * `Improved numerical methods for gammatone filterbank analysis and
 * synthesis' by T. Herzke and V. Hohmann, published 2007 in `acta
 * acustica'.
 *
 * filename : Gfb_Analyzer_fprocess.c
 * copyright: Universitaet Oldenburg
 * author   : tp
 * date     : Jan 2002, Nov 2006, Jan Feb 2007
 *
 * update   : 
 */

/*-----------------------------------------------------------------------------
 *   Copyright (C) 2002 2006 2007 AG Medizinische Physik,
 *                        Universitaet Oldenburg, Germany
 *                        http://www.physik.uni-oldenburg.de/docs/medi
 */   

/*
 * file: Gfb_Analyzer_fprocess.c
 * 
 * Authors: 2001 2002 2006 2007 Tobias Herzke
 *
 */

#include <string.h>
#include "mex.h"
/*
 * For easy compilation, we include the source file of the computational
 * function rather than its header file:
 */ 
#include "Gfb_analyze.c"


/**
 * struct Filterbank_Data helps us by bundling the data extracted from
 * the Matlab Gfb_Analyzer object in a single object, with easy to use
 * constructor and destructor functions.
 */
struct Filterbank_Data {
  /**
   * A vector containing the normalization factors of the individual bands.
   */
  double * normalization_factors   ;

  /**
   * two vectors holding the real and the imaginary part, respectively, of
   * each band's filter coefficients.
   */ 
  double * real_filter_coefficients; double * imag_filter_coefficients;

  /**
   * two vectors holding the real and the imaginary part, respectively, of
   * each band's state. The filter state part corresponding to a specific
   * band and filter stage is stored at index
   * [band * filter_order + filter_stage], where filter_order denotes
   * the order of the gammatone filters (usually == 4).
   */   
  double * real_filter_state       ; double * imag_filter_state       ;
};

/**
 * The constructor function for a struct Filterbank_Data. The allocated
 * vectors are large enough to hold the contained arrays for the given number
 * of bands and filter order.
 */
static
struct Filterbank_Data *
Filterbank_Data_new(unsigned bands, unsigned gamma_order)
{
  struct Filterbank_Data * fbd =
    (struct Filterbank_Data *) mxMalloc(1 * sizeof(struct Filterbank_Data));
  fbd->normalization_factors    = (double*) mxMalloc(bands * sizeof(double));
  fbd->real_filter_coefficients = (double*) mxMalloc(bands * sizeof(double));
  fbd->imag_filter_coefficients = (double*) mxMalloc(bands * sizeof(double));
  fbd->real_filter_state        = (double*) mxMalloc(bands * gamma_order *
						     sizeof(double));
  fbd->imag_filter_state        = (double*) mxMalloc(bands * gamma_order *
						     sizeof(double));
  return fbd;
}

/**
 * The destructor of struct Filterbank_Data. It frees all memory
 * occupied by the structure and the contained vectors at once.
 */
static void
Filterbank_Data_delete(struct Filterbank_Data * fbd)
{
  if (fbd == NULL) return;
  mxFree(fbd->normalization_factors);    fbd->normalization_factors    = NULL;
  mxFree(fbd->real_filter_coefficients); fbd->real_filter_coefficients = NULL;
  mxFree(fbd->imag_filter_coefficients); fbd->imag_filter_coefficients = NULL;
  mxFree(fbd->real_filter_state);        fbd->real_filter_state        = NULL;
  mxFree(fbd->imag_filter_state);        fbd->imag_filter_state        = NULL;
  mxFree(fbd);
}



/**
 * The Matlab extension interface function. It checks for proper arguments,
 * extracts the needed data from the input arguments and creates the output
 * arguments.
 */
void
mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  /**
   * The row vector containing the signal to analyze.
   */
  const mxArray * input = NULL;

  /**
   * The Gfb_Analyzer struct containing the filter parameters and the current
   * filter state.
   */
  const mxArray * analyzer_in = NULL;

  /**
   * A complex matrix containing the filterbank output. Rows in this matrix
   * correspond to the individual filter bands, Columns correspond to input
   * signal samples.
   */
  mxArray * output  = NULL;

  /**
   * A non-const copy of analyzer_in. If the calling function expects a second
   * return value, analyzer_in will be copied into this variable, and the
   * filter state in here will be updated.
   */
  mxArray * analyzer_out = NULL;

  /**
   * The number of samples present in the input signal.
   */
  unsigned samples = 0;

  /**
   * The vector containing the center frequencies of the gammatone filters,
   * extracted from analyzer_in.  This is only used to get the number of
   * filter bands (== length of vector).
   */
  const mxArray * center_frequencies_hz = NULL;

  /**
   * The number of gammatone filter bands contained in the filterbank
   */
  unsigned bands = 0;
  
  /**
   * An index variable for iterating over the bands.
   */
  unsigned band;

  /**
   * The filter order of the gammatone filters
   */
  unsigned gamma_order = 0;

  /**
   * The structure array containing the individual Gfb_Filter matlab structs
   */
  const mxArray * analyzer_in_filters = NULL;

  /**
   * The bundled extracted filterbank data needed by the computational
   * function
   */
  struct Filterbank_Data * fbd = NULL;



  /* Clean-up on unload. (Function defined in Gfb_analyze.c) */
  mexAtExit(&release_tables);

  /* Check for proper number of arguments and extract values. */
  if (nrhs != 2) {
    mexErrMsgTxt("Gfb_Analyzer_fprocess: needs 2 input arguments");
    return;
  }
  if (nlhs > 2) {
    mexErrMsgTxt("Gfb_Analyzer_fprocess: returns one or two parameters");
    return;
  }

  /* Check second argument */
  input = prhs[1];
  if (!mxIsNumeric(input)
      || mxIsComplex(input)
      || mxIsSparse(input)
      || !mxIsDouble(input)
      || mxGetM(input) != 1) {
    mexErrMsgTxt("Gfb_Analyzer_fprocess: 2nd argument must be a real vector");
    return;
  }
  samples = mxGetN(input);

  /* Check first argument */
  analyzer_in = prhs[0];
  if (!analyzer_in
      || !mxIsStruct(analyzer_in)) {
    mexErrMsgTxt("Gfb_Analyzer_fprocess: 1st argument must be a "
		 "Gfb_Analyzer structure");
    return;
  }
  if (mxGetNumberOfElements(analyzer_in) != 1) {
    mexErrMsgTxt("Gfb_Analyzer_fprocess: 1st argument must be a single "
		 "Gfb_Analyzer structure");
    return;
  }
  {
    /*
     * Check if analyzer_in contains a field `type' with the value
     * "Gfb_Analyzer"
     */
    mxArray * analyzer_in_type = mxGetField(analyzer_in, 0, "type");
    if (analyzer_in_type == NULL
	|| !mxIsChar(analyzer_in_type)) {
      mexErrMsgTxt("Gfb_Analyzer_fprocess: 1st argument must be a "
		   "Gfb_Analyzer structure");
      return;
    }
    {
      char * analyzer_in_typename = mxArrayToString(analyzer_in_type);
      int analyzer_is_Gfb_Analyzer =
	(strcmp(analyzer_in_typename, "Gfb_Analyzer") == 0);
      mxFree(analyzer_in_typename);
      if (!analyzer_is_Gfb_Analyzer) {
	mexErrMsgTxt("Gfb_Analyzer_fprocess: 1st argument must be a "
		     "Gfb_Analyzer structure");
	return;
      }
    }
  }

  /* Get the number of gammatone filter bands */
  center_frequencies_hz = mxGetField(analyzer_in, 0, "center_frequencies_hz");
  if (center_frequencies_hz == NULL
      || !mxIsNumeric(center_frequencies_hz)
      || mxIsComplex(center_frequencies_hz)
      || mxIsSparse(center_frequencies_hz) 
      || !mxIsDouble(center_frequencies_hz)
      || mxGetM(center_frequencies_hz) != 1) {
    mexErrMsgTxt("Gfb_Analyzer_fprocess: 1st argument must be a Gfb_Analyzer\n"
		 "       structure, and must contain a real column vector\n"
		 "       field named `center_frequencies_hz'.  Use the\n"
		 "       Gfb_Analyzer_new function to construct a valid\n"
		 "       Gfb_Analyzer structure");
    return;
  }
  bands = mxGetN(center_frequencies_hz);

  /* If there are 0 bands, then we have finished the calculation */
  if (bands == 0) {
    plhs[0] = mxCreateDoubleMatrix(0, samples, mxCOMPLEX);
    if (nlhs == 2) {
      analyzer_out = mxDuplicateArray(analyzer_in);
      plhs[1] = analyzer_out;
    }
    return;
  }

  analyzer_in_filters = mxGetField(analyzer_in, 0, "filters");
  if (analyzer_in_filters == NULL
      || !mxIsStruct(analyzer_in_filters)
      || mxGetNumberOfElements(analyzer_in_filters) != bands) {
    mexErrMsgTxt
      ("Gfb_Analyzer_fprocess: 1st argument must be a Gfb_Analyzer\n"
       "       structure, and must contain a structure field named\n"
       "       `filters', containing the filter specifications.  Use\n"
       "       the Gfb_Analyzer_new function to construct a valid\n"
       "       Gfb_Analyzer structure");
    return;
  }
  {
    /* extract the filter order of the gammatone filters */
    mxArray * analyzer_in_gamma_order =
      mxGetField(analyzer_in_filters, 0, "gamma_order");
    if (analyzer_in_gamma_order == NULL
	|| !mxIsNumeric(analyzer_in_gamma_order)
	|| mxIsComplex(analyzer_in_gamma_order)
	|| mxGetNumberOfElements(analyzer_in_gamma_order) != 1) {
      mexErrMsgTxt
	("Gfb_Analyzer_fprocess: 1st argument must be a Gfb_Analyzer\n"
	 "       structure, and must contain a structure field named\n"
	 "       `filters', containing the filter specifications.  Use\n"
	 "       the Gfb_Analyzer_new function to construct a valid\n"
	 "       Gfb_Analyzer structure");
      return;
    }
    gamma_order = (unsigned) mxGetScalar(analyzer_in_gamma_order);
  }


  fbd = Filterbank_Data_new(bands, gamma_order);
  /*
   * extract filter coefficients, initial filter states, and normalization
   * factors from the matlab Gfb_Filter structures
   */
  for (band = 0; band < bands; ++band) {
    const mxArray * filter_type        = mxGetField(analyzer_in_filters,
						    band,
						    "type");
    const mxArray * filter_coefficient = mxGetField(analyzer_in_filters,
						    band,
						    "coefficient");
    const mxArray * filter_state       = mxGetField(analyzer_in_filters,
						    band,
						    "state");
    const mxArray * filter_gamma_order = mxGetField(analyzer_in_filters,
						    band,
						    "gamma_order");
    const mxArray * filter_norm        = mxGetField(analyzer_in_filters,
						    band,
						    "normalization_factor");
    unsigned filter_stage;

    if (filter_type == NULL
	|| !mxIsChar(filter_type)
	|| mxGetM(filter_type) != 1
	|| mxGetN(filter_type) != strlen("Gfb_Filter")

	|| filter_coefficient == NULL
	|| !mxIsNumeric(filter_coefficient)
	|| !mxIsDouble(filter_coefficient)
	|| mxIsSparse(filter_coefficient)
	|| mxGetNumberOfElements(filter_coefficient) != 1

	|| filter_state == NULL
	|| !mxIsNumeric(filter_state)
	|| !mxIsDouble(filter_state)
	|| mxIsSparse(filter_state)
	|| mxGetM(filter_state) != 1
	|| mxGetN(filter_state) != gamma_order
	
	|| filter_gamma_order == NULL
	|| !mxIsNumeric(filter_gamma_order)
	|| mxIsComplex(filter_gamma_order)
	|| mxGetNumberOfElements(filter_gamma_order) != 1
	|| mxGetScalar(filter_gamma_order) != gamma_order
	
	|| filter_norm == NULL
	|| !mxIsNumeric(filter_norm)
	|| mxIsComplex(filter_norm)
	|| mxGetNumberOfElements(filter_norm) != 1) {
      Filterbank_Data_delete(fbd);
      mexErrMsgTxt
	("Gfb_Analyzer_fprocess: 1st argument must be a Gfb_Analyzer\n"
	 "       structure, and must contain a structure field named\n"
	 "       `filters', containing valid filter specifications.\n"
	 "       Use the Gfb_Analyzer_new function to construct a\n"
	 "       valid Gfb_Analyzer structure");
      return;
    }

    fbd->normalization_factors[band] = mxGetScalar(filter_norm);

    fbd->real_filter_coefficients[band] = *mxGetPr(filter_coefficient);
    /* permit real filter coefficients */
    fbd->imag_filter_coefficients[band] =
      mxGetPi(filter_coefficient) ? *mxGetPi(filter_coefficient) : 0.0;

    for (filter_stage = 0; filter_stage < gamma_order; ++filter_stage) {
      fbd->real_filter_state[band * gamma_order + filter_stage] =
	mxGetPr(filter_state)[filter_stage];
      /* permit real initial filter states */
      fbd->imag_filter_state[band * gamma_order + filter_stage] =
	mxGetPi(filter_state) ? mxGetPi(filter_state)[filter_stage] : 0.0;
    }
  }

  /* allocate output matrix */
  output = mxCreateDoubleMatrix(bands, samples, mxCOMPLEX);

  /* compute output and update filter states */
  analyze(bands, gamma_order, samples,
          fbd->real_filter_coefficients, fbd->imag_filter_coefficients,
          fbd->normalization_factors,
          fbd->real_filter_state, fbd->imag_filter_state,
          mxGetPr(input),
          mxGetPr(output), mxGetPi(output));
  plhs[0] = output;

  if (nlhs == 2) {
    /* return Gfb_Analyzer structure with updated filter states */
    mxArray * filters = NULL;
    analyzer_out = mxDuplicateArray(analyzer_in);
    filters = mxGetField(analyzer_out, 0, "filters");
    for (band = 0; band < bands; ++band) {
      mxArray * state_vector = mxCreateDoubleMatrix(1, gamma_order, mxCOMPLEX);
      unsigned filter_stage;
      for (filter_stage = 0; filter_stage < gamma_order; ++filter_stage) {
	mxGetPr(state_vector)[filter_stage] =
	  fbd->real_filter_state[filter_stage + band*gamma_order];
	mxGetPi(state_vector)[filter_stage] =
	  fbd->imag_filter_state[filter_stage + band*gamma_order];
      }
      /* replace old filter state */
      mxDestroyArray(mxGetField(filters, band, "state"));
      mxSetField(filters, band, "state", state_vector);
    }
    plhs[1] = analyzer_out;
  }
}

