This directory contains a Matlab implementation of the gammatone
filterbank described in V. Hohmann's Paper `Frequency analysis and
synthesis using a Gammatone filterbank', published 2002 in `acta
acustica'.  It uses numerical methods described in `Improved numerical
methods for gammatone filterbank analysis and synthesis' by T. Herzke
and V. Hohmann, published 2007 in `acta acustica'.

copyright: Universitaet Oldenburg
author   : tp
date     : Jan 2002, Nov 2006, Jan Feb 2007
filename : README_implementation.txt

Design principles
=================

An object oriented design is used in this implementation.  Global variables
are mostly avoided, except for global constants that do not change during exe-
cution.

Although object oriented, this implementation does not use Matlab's
native classes and objects.  Instead, objects are implemented as
Matlab structures, and have, among other fields, a field called `type'
that identifies the name of the class of this object.  Methods that
act on this object are normal Matlab functions.  Their names start
with the name of the class, followed by an underscore.  All methods
(except for the constructors, named `<class name>_new') expect, as
their first argument, the object on which they should act.  Because
Matlab lacks a call-by-reference, all methods return, as their last
return argument, a (possibly changed) copy of this object.

All class names and function names of this implementation have a prefix `Gfb_'
(for Gammatone FilterBank), and the names of all global constants have a prefix
`GFB_'.

Implementation details
======================

A list of classes and their methods follows.  For a detailed description of the
methods, please refer to their online help within matlab.  Referenced Equations
are contained in the 2002 `acta acustica' article.


Gfb_Filter
----------
The gammatone filter described in section 2 of Hohmann's paper is implemented
in the class `Gfb_Filter'.

  Constructor: Gfb_Filter_new          three different ways to create a filter:
                                       The filter coefficient is either speci-
                                       fied directly;  or it can be derived
                                       from sampling frequency, center fre-
                                       quency, and filter order `gamma' with
                                       equations (10),(13),(14);  or it can be
                                       derived from sampling frequency, center
                                       frequency, bandwidth, attenuation, and
                                       filter order `gamma' with equations
                                       (10),(11),(12)
  Methods:     Gfb_Filter_process      filters input data, cascading Matlab's
                                       `filter' function.
               Gfb_Filter_clear_state  clears the internal filter state
               Gfb_Filter_zresponse    computes frequency response

Gfb_Analyzer
------------
This class implements a gammatone filterbank as described in section 3.  It is
implemented as an aggregation of Gfb_Filter objects.

  Constructor: Gfb_Analyzer_new        creates a new gammatone filterbank.
                                       Filter bandwidths are each 1 ERBaud,
                                       filter coefficients are computed from
                                       equations (13),(14).  Sampling rate,
                                       lower cutoff frequency, base frequency,
                                       upper cutoff frequency, and density of
                                       the filters on the ERBscale have to be
                                       specified
  Methods:     Gfb_Analyzer_process    filters input data. Output is a matrix
                                       with a row for each gammatone filter.
               Gfb_Analyzer_clear_state
                                       clears the contained filters' states
               Gfb_Analyzer_zresponse  computes frequency response for each 
                                       filter in the filterbank

Gfb_Delay
---------
Gfb_Delay implements step 2 of section 4.

  Constructor: Gfb_Delay_new           constructs a Gfb_Delay object corres-
                                       ponding to the Gfb_Analyzer and desired
                                       analysis-synthesis delay given as para-
                                       meters.  Channel-specific delay and
                                       phase factors are computed for each
                                       band numerically
  Methods:     Gfb_Delay_process       delays each band of Gfb_Analyzer out-
                                       put as appropriate, and applies phase
                                       factors
               Gfb_Delay_clear_state   clears the internal delay lines


Gfb_Mixer
---------
Gfb_Mixer implements step 3 of section 4.

  Constructor: Gfb_Mixer_new           constructs a Gfb_Mixer object corres-
                                       ponding to the Gfb_Analyzer and the
                                       Gfb_Mixer given as parameters. Computes
                                       the gain factors using a simple, itera-
                                       tive numerical optimization
  Methods:     Gfb_Mixer_process       computes a weighted sum from the output
                                       of the corresponding Gfb_Delay object


Gfb_Synthesizer
---------------
This class combines a Gfb_Delay and and a Gfb_Mixer.

  Constructor: Gfb_Synthesizer_new     constructs a Gfb_Synthesizer (containing
                                       a Gfb_Delay and a Gfb_Mixer) correspon-
                                       ding to the Gfb_Analyzer and desired
                                       analysis-synthesis delay given as para-
                                       meters
  Methods:     Gfb_Synthesizer_process calls Gfb_Delay_process and
                                       Gfb_Mixer_process
               Gfb_Delay_clear_state   calls Gfb_Delay_clear_state


Helper functions
----------------
  Gfb_center_frequencies
                    computes ERB-scale equidistant frequencies
  Gfb_erbscale2hz   computes a frequency in Hz from its value on the ERBscale
  Gfb_hz2erbscale   computes an ERBscale value from a frequency in Hz
  Gfb_plot          helper function for creating labeled plots


Helper scripts
--------------
  Gfb_set_constants  initializes global constants


