function center_frequencies_hz = gfb_center_frequencies(filters_per_ERBaud,flow,basef,fhigh)
%GFB_CENTER_FREQUENCIES  Obsolete, use erbspacebw instead
%   Usage: frequencies_hz = gfb_center_frequencies(frequencies_per_ERBaud,flow,basef,fhigh);
% 
%   The call:
%
%     gfb_center_frequencies(filters_per_ERBaud,flow,basef,fhigh)
%
%   can be replaced by:
%
%     erbspacebw(flow,fhigh,1/filters_per_ERBaud,basef);
%
%
%   Url: http://amtoolbox.sourceforge.net/doc//filters/obsolete/gfb_center_frequencies.php

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
  
if (nargin < 4)
  fhigh = basef;
end

% Calculate the values of the parameter frequencies on the ERBscale:
lower_cutoff_frequency_erb     = freqtoerb(flow);
specified_center_frequency_erb = freqtoerb(basef);
upper_cutoff_frequency_erb     = freqtoerb(fhigh);


% The center frequencies of the individual filters are equally
% distributed on the ERBscale.  Distance between adjacent filters'
% center frequencies is 1/filters_per_ERBaud.
% First, we compute how many filters are to be placed at center
% frequencies below the base frequency:
erbs_below_base_frequency = ...
    specified_center_frequency_erb - lower_cutoff_frequency_erb;
num_of_filters_below_base_freq = ...
    floor(erbs_below_base_frequency * filters_per_ERBaud);

% Knowing this number of filters with center frequencies below the
% base frequency, we can easily compute the center frequency of the
% gammatone filter with the lowest center frequency:
start_frequency_erb = ...
    specified_center_frequency_erb - ...
    num_of_filters_below_base_freq / filters_per_ERBaud;

% Now we create a vector of the equally distributed ERBscale center
% frequency values:
center_frequencies_erb = ...
    [start_frequency_erb:(1/filters_per_ERBaud):upper_cutoff_frequency_erb];
center_frequencies_hz = erbtofreq(center_frequencies_erb);

