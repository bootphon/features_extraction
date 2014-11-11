function h=framemuladj(f,Fa,Fs,s,varargin)
%FRAMEMULADJ  Adjoint operator of frame multiplier
%   Usage: h=framemuladj(f,Fa,Fs,s);
%
%   Input parameters:
%          Fa   : Analysis frame
%          Fs   : Synthesis frame
%          s    : Symbol
%          f    : Input signal
%
%   Output parameters: 
%          h    : Output signal
%
%   FRAMEMULADJ(f,Fa,Fs,s) applies the adjoint of the frame multiplier
%   with symbol s to the signal f. The frame Fa is used for analysis
%   and the frame Fs for synthesis.
%
%   FRAMEMULADJ(f,Fa,Fs,s) does the same using the frames Fa for
%   analysis and Fs for synthesis.
%
%   See also: framemul, iframemul
%
%   Url: http://ltfat.sourceforge.net/doc/operators/framemuladj.php

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
  
% Author: Peter L. Søndergaard

if nargin < 4
    error('%s: Too few input parameters.',upper(mfilename));
end;

% Swap the analysis and synthesis frames and conjugate the symbol.
h=frsyn(Fa,conj(s).*frana(Fs,f));







