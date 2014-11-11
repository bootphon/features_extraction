/* 
 * This file is part of the gammatone filterbank reference implementation
 * described in V. Hohmann's `acta acustica' article.
 *
 * It contains the declaration of a C function that implements filter bank
 * analysis. 
 *
 * filename : Gfb_analyze.h
 * copyright: Universitaet Oldenburg
 * author   : tp
 * date     : Jan 2002, Nov 2006, Feb 2007
 *
 * update   : 
 */

/*-----------------------------------------------------------------------------
 *   Copyright (C) 2002 2006 2007 AG Medizinische Physik,
 *                        Universitaet Oldenburg, Germany
 *                        http://www.physik.uni-oldenburg.de/docs/medi
 */   

#ifndef GFB_ANALYZER_FPROCESS_ANALYZE_H
#define GFB_ANALYZER_FPROCESS_ANALYZE_H

#ifdef __cplusplus
extern "C" {
#endif

  /**
   * gfb_analyze implements the filter bank analysis as a C function.
   * data input and output is done in doubles, and the computation is performed
   * with doubles as well.
   *
   * For efficient computation, this function allocates tables of
   * precomputed values.  These tables will be recomputed when the
   * relevant input parameters change.  It is necessary that the
   * caller calls the release_tables function to free memory allocated
   * for these tables when they are no longer needed.
   *
   * @param bands           number of gammatone filters in this filterbank.
   *                        must be > 0.
   * @param gamma           gammatone filter order (usually == 4)
   * @param samples         number of samples in input signal
   * @param real_filter_coefficients
   *                        a vector holding the real parts of each band's
   *                        filter coefficients
   * @param imag_filter_coefficients
   *                        a vector holding the imaginary parts of each
   *                        band's filter coefficients
   * @param normalization_factors
   *                        a vector containing the normalization factors of
   *                        the individual gammatone filter bands
   * @param real_filter_states
   *                        a vector holding the real parts of each bands
   *                        filter states. The filter state part corresponding
   *                        to a specific band and filter stage is stored at
   *                        index [band * filter_order + filter_stage],
   *                        where filter_order denotes the order of the
   *                        gammatone filters (usually == 4).
   * @param imag_filter_states
   *                        a vector holding the imaginary parts of each
   *                        band's filter states
   * @param real_input      a vector containing the (pure real) input samples
   * @param real_output     a pointer to the array where the filterbank output
   *                        (real part) is stored by this function.
   * @param imag_output     a pointer to the array where the filterbank output
   *                        (imaginary part) is stored by this function.
   */ 
   void gfb_analyze(const unsigned       bands,
                    const unsigned       gamma,
                    const unsigned       samples,
                    const double * const real_filter_coefficients,
                    const double * const imag_filter_coefficients,
                    const double * const normalization_factors,
                    double * const       real_filter_states,
                    double * const       imag_filter_states,
                    const double *       real_input,
                    double * const       real_output,
                    double * const       imag_output);

  /** 
   * Release the tables with precomputed values that were allocated by
   * gfb_analyze.
   */
  void release_tables();

#ifdef __cplusplus
}
#endif
#endif
