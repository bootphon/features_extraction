function L=fwtlength(Ls,w,J,varargin);
%FWTLENGTH  FWT length from signal
%   Usage: L=fwtlength(Ls,w,J);
%          L=fwtlength(Ls,w,J,...);
%
%   FWTLENGTH(Ls,w,J) returns the length of a Wavelet system that is long
%   enough to expand a signal of length Ls. Please see the help on
%   FWT for an explanation of the parameters h and J.
%
%   If the returned length is longer than the signal length, the signal
%   will be zero-padded by FWT to length L.
%
%   See also: fwt
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/fwtlength.php

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

% Initialize the wavelet filters structure
w = fwtinit(w);

definput.import = {'fwtext'};
[flags,kv]=ltfatarghelper({},definput,varargin);

if flags.do_per
   blocksize=w.a(1)^J;
   L=ceil(Ls/blocksize)*blocksize;
elseif flags.do_valid
   m = numel(w.g{1}.h);
   a = w.a(1);
   rred = (a^J-1)/(a-1)*(m-a);
   blocksize=w.a(1)^J;
   L=rred+floor((Ls-rred)/blocksize)*blocksize;
else
   L = Ls;
end

