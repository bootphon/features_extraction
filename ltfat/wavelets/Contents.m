% LTFAT - Wavelets
%
%   Zdenek Prusa, 2013.
%
%   Basic analysis/synthesis
%      FWT               - Fast Wavelet Transform 
%      IFWT              - Inverse Fast Wavelet Transform
%      FWT2              - 2D Fast Wavelet Transform 
%      IFWT2             - 2D Inverse Fast Wavelet Transform
%      UFWT              - Undecimated Fast Wavelet Transform
%      IUFWT             - Inverse Undecimated Fast Wavelet Transform 
%      FWTLENGTH         - Length of Wavelet system to expand a signal
%      FWTCLENGTH        - Lengths of the wavelet coefficients subbands
%
%   Advanced analysis/synthesis
%      WFBT              - Transform using general Wavelet Filterbank Tree 
%      IWFBT             - Inverse transform using general Wavelet Filterbank Tree
%      UWFBT             - Undecimated transform using general Wavelet Filterbank Tree 
%      IUWFBT            - Inverse Undecimated transform using general Wavelet Filterbank Tree
%      WPFBT             - Wavelet Packet Transform using general Wavelet Filterbank Tree 
%      IWPFBT            - Inverse Wavelet Packet Transform using general Wavelet Filterbank Tree
%      UWPFBT            - Undecimated Wavelet Packet Transform using general Wavelet Filterbank Tree 
%      IUWPFBT           - Inverse Undecimated Wavelet Packet Transform using general Wavelet Filterbank Tree
%      WPBEST            - Best Tree selection
%      WFBTLENGTH        - Length of Wavelet filterbank system to expand a signal
%
%   Wavelet Filterbank trees manipulation
%      WFBTINIT          - Wavelet Filterbank tree structure initialization
%      WFBTPUT           - Puts node (basic filterbank) to the specific  tree coordinates
%      WFBTREMOVE        - Removes node (basic filterbank) from the specific tree coordinates
%      WFBT2FILTERBANK   - Creates a non-iterated filterbank using the multirate identity
%      FWT2FILTERBANK    - Creates a non-iterated filterbank using the multirate identity
%      FWTINIT           - Basic Wavelet Filters structure initialization
%  
%   Plots
%      PLOTWAVELETS      - Plot wavelet coefficients
%      WFILTINFO         - Plot wavelet filters impulse and frequency responses and approximation of scaling and wavelet functions
%
%   Auxilary
%      WAVFUN            - Aproximate of the continuous scaling and wavelet functions
%      WAVCELL2PACK      - Changes wavelet coefficient storing format
%      WAVPACK2CELL      - Changes wavelet coefficient storing format back
%
%   Filters defined in the time-domain
%      WFILT_ALGMBAND    - An ALGebraic construction of orthonormal M-BAND wavelets with perfect reconstruction
%      WFILT_APR         - Almost Perfect Reconstruction Filter Bank for Non-redundant, Approximately Shift-Invariant, ComplexWavelet Transforms
%      WFILT_DB          - DauBechies orthogonal filters (ortonormal base)
%      WFILT_DDEN        - Double-DENsity dwt filters (tight frame)
%      WFILT_DGRID       - Dense GRID framelets (tight frame, symmetric)
%      WFILT_DTREE       - Dual-TREE complex wavelet transform filters (two orthonormal bases)
%      WFILT_HDEN        - Higher DENsity dwt filters (tight frame, frame)  
%      WFILT_LEMARIE         - Battle and Lemarie quadrature filters
%      WFILT_MATLABWTWRAPPER - Wrapper of the wfilters function from the Matlab Wavelet Toolbox 
%      WFILT_MAXFLAT         - Maximally flat FIR filters
%      WFILT_MBAND           - M-band filters
%      WFILT_OPTFS           - Optimized orthogonal filters with improved Frequency Selectivity (ortonormal base)
%      WFILT_REMEZ           - Wavelet orthonogal filters based on the Remez Exchange algorithm
%      WFILT_SYMDS           - SYMmetric wavelet Dyadic Siblings (frames)
%      WFILT_SPLINE          - Biorthogonal spline wavelet filters
%      WFILT_SYM             - Least asymmetric Daubechies wavelet filters
%      
%   Wavelet filters defined in the frequency-domain
%      WFREQ_LEMARIE         - Battle and Lemarie filters sampled freq. resp.
%
%  For help, bug reports, suggestions etc. please send an email to
%  ltfat-help@lists.sourceforge.net
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/Contents.php

% Copyright (C) 2005-2013 Peter L. Søndergaard <soender@users.sourceforge.net>.
% This file is part of LTFAT version 1.4.3
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.


