function L=framelengthcoef(F,Ncoef);
%FRAMELENGTHCOEF  Frame length from coefficients
%   Usage: L=framelengthcoef(F,Ncoef);
%
%   FRAMELENGTHCOEF(F,Ncoef) returns the length of the frame F, such that
%   F is long enough to expand the coefficients of length Ncoef.
%
%   If instead a signal is given, call FRAMELENGTH.
%
%   See also: frame, framelength
%
%   Url: http://ltfat.sourceforge.net/doc/frames/framelengthcoef.php

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
  
if nargin<2
  error('%s: Too few input parameters.',upper(mfilename));
end;

if ~isscalar(Ncoef)
  error('%s: Ncoef must be a scalar.',upper(mfilename));
end;

L = F.lengthcoef(Ncoef);
% sprintf for Octave compatibility
assert(abs(L-round(L))<1e-3,sprintf('%s: There is a bug. L=%d should be an integer.',upper(mfilename),L));
L=round(L);
    
% Verify the computed length
if ~(L==framelength(F,L))
    error(['%s: The coefficient number given does not correspond to a valid ' ...
           'set of coefficients for this type of frame.'],upper(mfilename));
    
end;
