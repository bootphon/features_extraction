function [AF,BF]=fwtbounds(w,J,L)
%FWTBOUNDS Frame bounds of DWT
%   Usage: fcond=fwtbounds(w,J,L);
%          [A,B]=fwtbounds(w,J,L);
%
%   FWTBOUNDS(w,a,L) calculates the ratio B/A of the frame bounds
%   of the filterbank specified by w and J for a system of length
%   L. The ratio is a measure of the stability of the system. 
%
%   [A,B]=FWTBOUNDS(w,J,L) returns the lower and upper frame bounds
%   explicitly. 
%
%   See FWT for explanation of parameters w and J.
%
%   See also: fwt, filterbankbounds
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/fwtbounds.php

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


if nargin<3
  error('%s: Too few input parameters.',upper(mfilename));
end;

% There seem to be two possibilites:
% 1) Find the frame bounds of the equaivalent uniform filterbank. The
%    equivalent non-uniform filterbank can be created using fwt2filterbank,
%    non-uniform to uniform filterbank transform is described in the book:
%    Beyond Wavelets, chapter 10, Nonuniform filter banks: New results and
%    open problems by Akkarakaran and Vaidyanathan.
% 2) Use the recursive polyphase matrix multiplication algorithm from 
%    Stanhill, D.; Zeevi, Y. Y.: Frame analysis of wavelet-type filter banks 

%ad 1) (shotcut trough wfbtbounds)

if nargout<2
   AF = wfbtbounds({w,J,'dwt'},L);
elseif nargout == 2
   [AF, BF] = wfbtbounds({w,J,'dwt'},L);
end

