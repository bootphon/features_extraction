function phi = itd2azimuth(itd,lookup)
%ITD2AZIMUTH estimates the azimuth for the given itd
%   Usage: phi = itd2azimuth(itd,lookup);
%
%   Input parameters:
%       itd    : interaural time difference, ITD (s)
%       lookup : lookup table to use
%
%   Output parameters:
%       phi    : azimuth angle (rad)
%
%   ITD2AZIMUTH(itd,lookup) estimates the azimuth angle by the given
%   interaural time difference (itd) and the given lookup table.  Note*: only
%   the first 12 auditory channels are considered.
%
%   see also: lookup_table
%
%   Url: http://amtoolbox.sourceforge.net/doc//modelstages/itd2azimuth.php

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

% AUTHOR: Hagen Wierstorf


%% ===== Checking of input parameters ===================================
nargmin = 2;
nargmax = 2;
error(nargchk(nargmin,nargmax,nargin));
isargmatrix(itd);
isargstruct(lookup);


%% ===== Computation ====================================================
% Fit the lookup data
p = zeros(12,13);
for n = 1:12
    p(n,:)=polyfit(lookup.itd(:,n),lookup.azimuth',12);
end
% Use the fit parameter to estimate the azimuth angle phi with the
% calculated ITD
phi = zeros(size(itd,1),12);
for n = 1:12
    phi(:,n) = polyval(p(n,:),itd(:,n));
end
% Check if we got azimuth values, that extend our range
phi(abs(phi)>rad(95)) = NaN;

