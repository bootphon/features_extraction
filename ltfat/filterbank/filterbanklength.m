function L=filterbanklength(Ls,a);
%FILTERBANKLENGTH  Filterbank length from signal
%   Usage: L=filterbanklength(Ls,a);
%
%   FILTERBANKLENGTH(Ls,a) returns the length of a filterbank with
%   time shifts a, such that it is long enough to expand a signal of
%   length Ls.
%
%   If the filterbank length is longer than the signal length, the signal
%   will be zero-padded by FILTERBANK or UFILTERBANK.
%
%   If instead a set of coefficients are given, call FILTERBANKLENGTHCOEF.
%
%   See also: filterbank, filterbanklengthcoef
%
%   Url: http://ltfat.sourceforge.net/doc/filterbank/filterbanklength.php

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

if ~isnumeric(Ls)
  error('%s: Ls must be numeric.',upper(mfilename));
end;

if ~isscalar(Ls)
  error('%s: Ls must a scalar.',upper(mfilename));
end;

if ~isnumeric(a)
  error('%s: a must be numeric.',upper(mfilename));
end;

%if ~isvector(a)
%    
%end;

if any(a<=0)
      error('%s: "a" must consists of positive numbers only.',upper(mfilename));
end;

lcm_a=a(1);
for m=2:size(a,1)
  lcm_a=lcm(lcm_a,a(m,1));
end;

L=ceil(Ls/lcm_a)*lcm_a;

