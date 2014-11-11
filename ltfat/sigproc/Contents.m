% LTFAT - Signal processing tools
%
%  Peter L. Søndergaard, 2007 - 2013.
%
%  General
%    RMS            -  Root Mean Square norm of signal.
%    NORMALIZE      -  Normalize signal by specified norm.
%    GAINDB         -  Scale input signal
%    CRESTFACTOR    -  Compute the crest factor of a signal.
%    UQUANT         -  Simulate uniform quantization.
%
%  Ramping
%    RAMPUP         -  Rising ramp.
%    RAMPDOWN       -  Falling ramp.
%    RAMPSIGNAL     -  Ramp a signal.
%
%  Thresholding methods
%    THRESH         -  Coefficient thresholding.
%    LARGESTR       -  Keep largest ratio of coefficients.
%    LARGESTN       -  Keep N largest coefficients.
%    DYNLIMIT       -  Limit the dynamical range.
%    GROUPTHRESH    -  Group thresholding.
%
%  Image processing
%    RGB2JPEG       -  Convert RGB values to the JPEG color model
%    JPEG2RGB       -  Convert values from the JPEG color model to RGB
%
%  Tools for OFDM
%    QAM4           -  Quadrature amplitude modulation, order 4
%    IQAM4          -  Inverse QAM of order 4
%
%  For help, bug reports, suggestions etc. please send email to
%  ltfat-help@lists.sourceforge.net
%
%
%   Url: http://ltfat.sourceforge.net/doc/sigproc/Contents.php

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

