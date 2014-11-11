This directory contains a Matlab implementation of the gammatone
filterbank described in V. Hohmann's Paper `Frequency analysis and
synthesis using a Gammatone filterbank', published 2002 in `acta
acustica'.  It uses numerical methods described in `Improved numerical
methods for gammatone filterbank analysis and synthesis' by T. Herzke
and V. Hohmann, published 2007 in `acta acustica'.

copyright: Universitaet Oldenburg
author   : tp
date     : Jan Apr 2002, Nov 2006, Jan Feb 2007
filename : README_examples.txt

There are 3 example scripts included in this Package:
Example_Filter.m      demonstrates construction and usage of a gammatone filter
                      as described in section 2.3 [Hohmann 2002]
Example_Filterbank.m  demonstrates the filterbank design as explained in
                      section 3 [Hohmann 2002]
Example_Synthesis.m   creates a filterbank and the apropriate synthesizer as in
                      section 4.1 [Hohmann 2002]

The following sections in this file contain more information about the
necessary actions to create a filter, a filterbank, or a synthesizer.

Filter implementation
=====================

The filter implementation can be found in file "Gfb_Filter_process.m".  This
filter implementation uses matlab's builtin filter function from the signal
processing toolbox. As described in the article, <gamma> complex first-order
filters are applied in a cascade to achieve the effect of an <gamma>-order
filter.

A suitable "Gfb_Filter" structure has to be initialized in order to use this
filter implementation (see next section, "Filter parameter calculation").

An optimized implementation of the Gammatone filter as described in
[Herzke & Hohmann 2007] can be found in the file "Gfb_analyze.c",
where it is part of the C extension that implements filterbank
analysis.


Filter parameter calculation
============================

This implementation includes 2 different filter parameter calculation methods.
Both are invoked via the matlab function Gfb_Filter_new, and are implemented in
file "Gfb_Filter_new.m".  The function stores the filter parameters in a matlab
structure, the "Gfb_Filter" structure.

(A)
Filter parameter calculation as in section 2.3 of [Hohmann 2002] requires five
arguments:
- the used sampling rate in Hz
- the center frequency of the gammatone filter in Hz
- the desired bandwidth of the gammatone filter in Hz
- the desired attenuation, in dB, of the input signal at
  center_frequency +/- (bandwidth / 2)
- the order of the gammatone filter

The Gfb_Filter_new function then returns a structure that contains the filter
parameters in the following fields (among other fields):
  coefficient           -  contains the complex filter coefficient
  normalization_factor  -  contains the normalization factor for this filter

(B)
Filter parameter calculation as in section 3 (equations 13,14) of 
[Hohmann 2002] requires three arguments:
- the used sampling rate in Hz
- the center frequency of the gammatone filter in Hz
- the order of the gammatone filter

The bandwidth of the resulting filter will be one ERB.  The returned
"Gfb_Filter" structure contains the same fields as in case (A).


Filterbank design
=================
The function Gfb_Analyzer new will create a Gammatone filterbank as described
in section 3 of the article.  This function is implemented in file
"Gfb_Analyzer_new.m" and expects 5 arguments:
- the used sampling rate in Hz
- the lower cutoff frequency in Hz.  No filter will have a lower center
  frequency than this
- the base frequency in Hz. One of the created gammatone filters will have the
  base frequency as its center frequency
- the upper cutoff frequency in Hz.  No filter will have a higher center
  frequency than this
- the desired number of gammatone filters per ERB

This function will return a "Gfb_Analyzer" structure with the following fields
(among others):
  center_frequencies_hz  -  the center frequencies of all filters in a row
                            vector
  filters                -  a row vector of "Gfb_Filter" structures,
                            containing the filter parameters and the filter
                            state


Frequency synthesis
===================
The function Gfb_Synthesizer_new will create a synthesis filter as described
in section 4.1 of [Hohmann 2002].

The synthesizer consists of a band-dependent delay line and a mixer, thus
this function will itself call two other functions, namely Gfb_Delay_new
(implemented in file "Gfb_Delay_new.m", creates a Gfb_Delay object) and
Gfb_Mixer_new (implemented in file "Gfb_Mixer_new.m", creates a Gfb_Mixer
object).  These functions use the numerical methods described in
[Herzke & Hohmann 2007] to deduce the synthesis parameters.

Gfb_Synthesizer_new is located in file "Gfb_Synthesizer_new.m" and requires
two arguments:
- the Gfb_Analyzer structrure that produces the output that this
  Gfb_Synthesizer shall resynthesize
- the desired group delay of the analysis-synthesis system in seconds


Filter design example (section 2.4)
===================================
This is a demonstration how to create a filter as described in section 2.4 in
matlab:
>> center_frequency_hz = 1000;
>> bandwidth_hz = 100;
>> attenuation_db = 3;
>> sampling_frequency_hz = 10000;
>> filter_order = 4;
>> filter = Gfb_Filter_new(sampling_frequency_hz, center_frequency_hz, ...
                           bandwidth_hz, attenuation_db, filter_order)

filter = 

                    type: 'Gfb_Filter'
             gamma_order: 4
             coefficient: 0.7526+ 0.5468i
    normalization_factor: 4.7434e-05
                   state: [0 0 0 0]
>> %%% plot the impulse response of this filter: %%%
>> impulse = [1, zeros(1,199)];
>> [impulse_response, filter] = Gfb_Filter_process(filter, impulse);
>> filter.state

ans =

   1.0e-04 *

   0.0000 - 0.0000i   0.0000 - 0.0000i   0.0043 - 0.0031i   0.2907 - 0.2112i

>> plot(0:199, ...
        [real(impulse_response);imag(impulse_response);abs(impulse_response)])


Filterbank design example
=========================
This examples shows how to create the analysis filterbank of section 3.1: 
>> lower_cutoff_frequency_hz = 70;
>> upper_cutoff_frequency_hz = 6700;
>> base_frequency_hz = 1000;
>> sampling_rate_hz = 16276;
>> filters_per_ERB = 1.0;
>> analyzer = Gfb_Analyzer_new(sampling_rate_hz, lower_cutoff_frequency_hz, ...
                               base_frequency_hz, upper_cutoff_frequency_hz,...
                               filters_per_ERB)

analyzer = 

                             type: 'Gfb_Analyzer'
            sampling_frequency_hz: 16276
        lower_cutoff_frequency_hz: 70
    specified_center_frequency_hz: 1000
        upper_cutoff_frequency_hz: 6700
               filters_per_ERBaud: 1
                             fast: 0
            center_frequencies_hz: [1x30 double]
                          filters: [1x30 struct]

>> %%% plot the frequency response of the individual filters: %%%         
>> impulse = [1, zeros(1,8191)];                                          
>> [impulse_response, analyzer] = Gfb_Analyzer_process(analyzer, impulse);
>> frequency_response = fft(real(impulse_response)');                     
>> frequency = [0:8191] * sampling_rate_hz / 8192;                        
>> plot(frequency(1:4100), abs(frequency_response(1:4100,:)))             


Synthesis design example
========================

This examples shows how to create the synthesizer that resynthesizes the output
of the analysis filterbank created in the previous examples, with a group delay
of 4ms.

>> %%% Assuming the variables "analyzer" and "impulse_response" %%%
>> %%% from the previous example is still in memory...          %%%
>> % clear all filters' states:
>> analyzer = Gfb_Analyzer_clear_state(analyzer);
>> desired_delay_in_seconds = 0.004;
>> synthesizer = Gfb_Synthesizer_new(analyzer, desired_delay_in_seconds) 

synthesizer = 

     type: 'Gfb_Synthesizer'
    delay: [1x1 struct]
    mixer: [1x1 struct]

>> %%% plot the resynthesized impulse %%%
>> [processed_impulse, synthesizer] = ...
       Gfb_Synthesizer_process(synthesizer, impulse_response);
>> plot([40:120], processed_impulse(41:121))


