/* 
 * This file is part of the gammatone filterbank reference implementation
 * described in V. Hohmann's `acta acustica' article.
 *
 * It implements the filter bank analysis as a C function.
 *
 * filename : Gfb_analyze.c
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

#include <stdlib.h>
#include "Gfb_analyze.h"

/*
 * This C implementation of the gammatone filterbank is optimized for low
 * computational load. It uses 10.x Additions and 6.x multiplications
 * per sample and filter. For each filterbank band, it stores Ca^-1 to Ca^-128
 * and a^1 to a^128 as precomputed values in tables.
 */

/* Macros for complex computations */

/** Check for equality */
#define complex_equal(real_a,imag_a,real_b,imag_b) \
((real_a) == (real_b) && (imag_a) == (imag_b))

/** compute square of absolute value */
#define complex_abs2(real,imag) ((real)*(real) + (imag)*(imag))

/**
 * compute reciprocal of complex value
 * @param real real part of complex value
 * @param imag imaginary part of complex value
 * @param real_result storage for real part of result
 * @param imag_result storage for imaginary part of result
 * @param temp storage for temporary value
 * @return nothing. This is a C-statement.
 */
#define complex_reciprocal_(real, imag, real_result, imag_result, temp) \
do { \
  temp = complex_abs2((real),(imag)); \
  real_result = (real) / (temp); \
  imag_result = -(imag) / (temp); \
} while(0)

/**
 * compute product of 2 complex values. Not for in-place computation.
 * @param real_a real part of first factor
 * @param imag_a imaginary part of first factor
 * @param real_b real part of second factor
 * @param imag_b imaginary part of second factor
 * @param real_result storage for real part of result
 * @param imag_result storage for imaginary part of result
 * @return nothing. This is a C-statement.
 */
#define complex_mult_(real_a,imag_a, real_b,imag_b, real_result,imag_result)\
do { \
  real_result = (real_a) * (real_b) - (imag_a) * (imag_b); \
  imag_result = (real_a) * (imag_b) + (imag_a) * (real_b); \
} while(0)

/**
 * compute product of 2 complex values, store it in the place of the first
 * factor
 * @param real_a storage for real part of first factor and result
 * @param imag_a storage imaginary part of first factor and result
 * @param real_b real part of second factor
 * @param imag_b imaginary part of second factor
 * @param temp storage for temporary value
 * @return nothing. This is a C-statement.
 */
#define complex_mult_in_place_(real_a, imag_a, real_b, imag_b, temp)\
do { \
  temp = (real_a) * (real_b) - (imag_a) * (imag_b); \
  imag_a =  (real_a) * (imag_b) + (imag_a) * (real_b); \
  real_a = temp; \
} while(0)

/** Length of tables */
#define TABLE_SIZE 128

/** Number of bands with precomputed values */
static unsigned precomp_bands = 0;

/* precomputed tables follow. The order of data in these tables is:
 * offset = (exponent-1) * bands + band_index
 */

/** Precomputed powers of the filter coefficients, real part. */
static double * precomp_real_pow = NULL;

/** Precomputed powers of the filter coefficients, imaginary part */
static double * precomp_imag_pow = NULL;

/**
 * Precomputed normalization factor divided by values in the precomp_*_pow
 * tables, real part
 */
static double * precomp_real_norm = NULL;

/**
 * Precomputed normalization factor divided by values in the precomp_*_pow
 * tables, imaginary part
 */
static double * precomp_imag_norm = NULL;

/**
 * Called on DLL unloading to free allocated persistent tables. Also called
 * when parameters have changed and new tables have to be allocated.
 */
void release_tables()
{
    if (precomp_bands) {
        precomp_bands = 0;
        free(precomp_real_pow);  precomp_real_pow = NULL;
        free(precomp_imag_pow);  precomp_imag_pow = NULL;
        free(precomp_real_norm); precomp_real_norm = NULL;
        free(precomp_imag_norm); precomp_imag_norm = NULL;
    }
}

/**
 * Allocate memory for tables.
 * @return 0 on success, -1 on memory allocation failure
 */
static
int allocate_tables(unsigned int bands)
{
    /* Not allowing for zero bands. */
    if (bands == 0)
        return -1;

    precomp_bands = bands;
    precomp_real_pow = calloc(bands * TABLE_SIZE, sizeof(double));
    if (precomp_real_pow != NULL) {
        precomp_imag_pow = calloc(bands * TABLE_SIZE, sizeof(double));
        if (precomp_imag_pow != NULL) {
            precomp_real_norm = calloc(bands * TABLE_SIZE, sizeof(double));
            if (precomp_real_norm != NULL) {
                precomp_imag_norm = calloc(bands * TABLE_SIZE, sizeof(double));
                if (precomp_imag_norm != NULL) {
                    return 0;
                }
                free(precomp_real_norm); precomp_real_norm = NULL;
            }
            free(precomp_imag_pow);  precomp_imag_pow = NULL;
        }
        free(precomp_real_pow);  precomp_real_pow = NULL;
    }
    precomp_bands = 0;
    return -1;
}

/**
 * check if the precomputed tables are out-of-date for these filterbank
 * parameters
 */
static
int check_if_parameters_changed(const unsigned       bands,
                                const double * const real_filter_coefficients,
                                const double * const imag_filter_coefficients,
                                const double * const normalization_factors)
{
    unsigned band;
    double real_reciprocal, imag_reciprocal, temp;
    if (bands != precomp_bands)
        return 1;
    for (band = 0; band < bands; ++band) {
        /*   precomp_*_pow values for exponent 1 contain
         *   *_filter_coefficients                            */
        if (real_filter_coefficients[band] != precomp_real_pow[band])
            return 1;
        if (imag_filter_coefficients[band] != precomp_imag_pow[band])
            return 1;

        /*   precomp_*_norm values for exponent 1 contain normalization
         *   factors divided by filter coefficients.          */
        complex_reciprocal_(real_filter_coefficients[band],
                            imag_filter_coefficients[band],
                            real_reciprocal,
                            imag_reciprocal,
                            temp);
        if (normalization_factors[band] * real_reciprocal
            != precomp_real_norm[band])
            return 1;
        if (normalization_factors[band] * imag_reciprocal
            != precomp_imag_norm[band])
            return 1;
    }
    return 0;
}

/** recompute tables for these filterbank parameters */
static
void recompute_tables(const unsigned       bands,
                      const double * const real_filter_coefficients,
                      const double * const imag_filter_coefficients,
                      const double * const normalization_factors)
{
    unsigned exponent, band;
    double temp;
    release_tables();
    allocate_tables(bands);
    for (exponent = 1; exponent <= TABLE_SIZE; ++exponent) {
        unsigned table_idx = exponent - 1;
        for (band = 0; band < bands; ++band) {
            if (exponent == 1) {
                precomp_real_pow[band] = real_filter_coefficients[band];
                precomp_imag_pow[band] = imag_filter_coefficients[band];
            }
            else {
                complex_mult_(precomp_real_pow[(table_idx-1) * bands + band],
                              precomp_imag_pow[(table_idx-1) * bands + band],
                              real_filter_coefficients[band],
                              imag_filter_coefficients[band],
                              precomp_real_pow[table_idx * bands + band],
                              precomp_imag_pow[table_idx * bands + band]);
            }
            complex_reciprocal_(precomp_real_pow[table_idx * bands + band],
                                precomp_imag_pow[table_idx * bands + band],
                                precomp_real_norm[table_idx * bands + band],
                                precomp_imag_norm[table_idx * bands + band],
                                temp);
            precomp_real_norm[table_idx * bands + band] *=
                normalization_factors[band];
            precomp_imag_norm[table_idx * bands + band] *=
                normalization_factors[band];
        }
    }
}

void analyze(const unsigned       bands,
	     const unsigned       gamma,
	     const unsigned       samples,
	     const double * const real_filter_coefficients,
	     const double * const imag_filter_coefficients,
	     const double * const normalization_factors,
	     double * const       real_filter_states,
	     double * const       imag_filter_states,
	     const double * const real_input,
	     double * const       real_output,
	     double * const       imag_output)
{
    /** indices */
    unsigned sample, band;

    /** This implementation uses cascaded 1st-order filters. filter_stage
     * is the index that identifies a single 1st-order filter in the cascade.
     */
    unsigned filter_stage;

    /** index into the tables of precomputed values. (table_idx + 1)
     * corresponds to the exponent applied to the filter coefficient
     * at this table location. */
    unsigned table_idx;

    /** Some complex computations need this temporary storage. */
    double temp;

    /* Every time matlab calls us, the gammatone filterbank may have changed.
     * Check if the tables of precomputed values match the currently used
     * filterbank, and recompute the tables if they don't. This step would
     * not be necessary if the filterbank is fixed. */
    if (check_if_parameters_changed(bands,
                                    real_filter_coefficients,
                                    imag_filter_coefficients,
                                    normalization_factors)) {
        recompute_tables(bands,
                         real_filter_coefficients,
                         imag_filter_coefficients,
                         normalization_factors);
    }
    
    /* This is the filter implementation, optimized for low computational 
     * load. */
    for (table_idx = sample = 0; sample < samples; ++sample, ++table_idx) {

        /* Detecting roll-over, table index points past the end of the tables*/
        int roll_over = (table_idx == TABLE_SIZE);
        if (roll_over) {
            /* Wrap around the table index */
            table_idx = 0;
        }
        for (band = 0; band < bands; ++band) {
            if (roll_over) {
                /* Adjust filter states of first first-order filter.
                 * The filter states of the remaining filters in the
                 * cascade are adjusted in the loop through the filter
                 * cascade below. */
                complex_mult_in_place_(real_filter_states[band * gamma],
                                       imag_filter_states[band * gamma],
                                       precomp_real_pow[(TABLE_SIZE-1) * bands
                                                        + band],
                                       precomp_imag_pow[(TABLE_SIZE-1) * bands 
                                                        + band],
                                       temp);
            }
            
            /* First filter stage: apply normalization factor and
             * divide by the currently used power of the filter
             * coefficient (combined in a single complex factor in the
             * precomp_*_norm tables). */
            real_filter_states[band * gamma] += 
                real_input[sample]
                * precomp_real_norm[table_idx * bands + band];
            imag_filter_states[band * gamma] += 
                real_input[sample]
                * precomp_imag_norm[table_idx * bands + band];

            /* Remaining filter stages. */
            for (filter_stage = 1; filter_stage < gamma; ++filter_stage) {
                /* Adjust filter states in case of roll-over. */
                if (roll_over) {
                    complex_mult_in_place_(real_filter_states[band * gamma
                                                              + filter_stage],
                                           imag_filter_states[band * gamma
                                                              + filter_stage],
                                           precomp_real_pow[(TABLE_SIZE-1)
                                                            * bands + band],
                                           precomp_imag_pow[(TABLE_SIZE-1)
                                                            * bands + band],
                                           temp);
                }
                /* Filtering in these stages simply by addition. */
                real_filter_states[band * gamma + filter_stage] +=
                    real_filter_states[band * gamma + filter_stage - 1];
                imag_filter_states[band * gamma + filter_stage] +=
                    imag_filter_states[band * gamma + filter_stage - 1];
            }
            
            /** Apply the currently used power of the filter coefficient. */
            complex_mult_(real_filter_states[band * gamma + gamma - 1],
                          imag_filter_states[band * gamma + gamma - 1],
                          precomp_real_pow[table_idx * bands + band],
                          precomp_imag_pow[table_idx * bands + band],
                          real_output[sample * bands + band],
                          imag_output[sample * bands + band]);
        }
    }

    /* Final adjustment of filter states follows.  We do not save the
     * current value of table_idx, and allow mixing of calls to
     * Gfb_Analyzer_fprocess (mex file) and Gfb_Analyzer_process
     * (matlab file).  In a pure C implementation that saves the
     * current value of table_idx, this step would not be necessary
     * here. */
    if (samples == 0) return;  /* No final adjustment if no filtering */
    for (band = 0; band < bands; ++band)
        for (filter_stage = 0; filter_stage < gamma; ++filter_stage)
            complex_mult_in_place_(real_filter_states[band * gamma
                                                      + filter_stage],
                                   imag_filter_states[band * gamma
                                                      + filter_stage],
                                   precomp_real_pow[(table_idx-1)
                                                    * bands + band],
                                   precomp_imag_pow[(table_idx-1)
                                                    * bands + band],
                                   temp);
}

/*
 * Local Variables:
 * c-basic-offset: 4
 * End:
 */
