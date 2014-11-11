% AMT - Filter functions
%
%  The AMT team, 2012.
%
%  General routines
%     UFILTERBANKZ     - Apply multiple filters
%     FILTERBANKZ      - Apply multiple filters with non-equidistant downsampling
%     FILTERBANK_INIT  - Create control structure for FILTERBANK_BLOCK
%     FILTERBANK_BLOCK - Filterbank block processing
%
%  Auditory filters
%     GAMMATONE        - Gammatone filter coefficients
%     CQDFT            - FFT-based filter bank with constant relative bandwidth
%
%  Hohmann (2002) filterbank
%     GFB_ANALYZER_NEW            - XXX 
%     GFB_ANALYZER_PROCESS        - XXX 
%     GFB_DELAY_NEW               - XXX 
%     GFB_DELAY_PROCESS           - XXX 
%     GFB_FILTER_NEW              - XXX 
%     GFB_FILTER_PROCESS          - XXX 
%     GFB_MIXER_NEW               - XXX 
%     GFB_SYNTHESIZER_NEW         - XXX
%
%  Averaging
%     WEIGHTEDAVERAGEFILTER       - XXX
%
%  For help, bug reports, suggestions etc. please send email to
%  amtoolbox-help@lists.sourceforge.net
%
%   Url: http://amtoolbox.sourceforge.net/doc//filters/Contents.php

% Copyright (C) 2009-2013 Peter L. Søndergaard and others.
% This file is part of AMToolbox version 0.9.1
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


