function c=crestfactor(insig)
%CRESTFACTOR  Crest factor of input signal in dB
%   Usage:  c=crestfactor(insig);
%
%   CRESTFACTOR(insig) computes the crest factor of the input signal
%   insig. The output is measured in dB.
%
%   See also: rms, gaindb
%
%   Url: http://ltfat.sourceforge.net/doc/sigproc/crestfactor.php

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

c=20*log10(norm(insig,Inf)/rms(insig));


