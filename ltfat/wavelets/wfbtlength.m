function L=wfbtlength(Ls,wt,varargin);
%WFBTLENGTH  WFBT length from signal
%   Usage: L=wfbtlength(Ls,wt);
%          L=wfbtlength(Ls,wt,...);
%
%   WFBTLENGTH(Ls,wt) returns the length of a Wavelet system that is long
%   enough to expand a signal of length Ls. Please see the help on
%   WFBT for an explanation of the parameter wt.
%
%   If the returned length is longer than the signal length, the signal
%   will be zero-padded by WFBT to length L.
%
%   See also: wfbt, fwt
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfbtlength.php

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


definput.import = {'fwt'};
[flags,kv]=ltfatarghelper({},definput,varargin);

% Initialize the wavelet filters structure
wt = wfbtinit(wt);


if(flags.do_per)
   % In all proper cases the following slould be equal to max(treeSub(wt))
   blocksize=max(treeSub(wt));
   L=ceil(Ls/blocksize)*blocksize;
else
   L = Ls;
end

