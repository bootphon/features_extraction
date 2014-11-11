function h=framemul(f,Fa,Fs,s,varargin)
%FRAMEMUL  Frame multiplier
%   Usage:  h=framemul(f,Fa,Fs,s);
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
%   FRAMEMUL(f,Fa,Fs,s) applies the frame multiplier with symbol s*
%   to the signal f. The frame Fa is used for analysis and the frame
%   Fs for synthesis.
%
%   See also: iframemul, framemuladj
%
%   Url: http://ltfat.sourceforge.net/doc/operators/framemul.php

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

% Check for compatibility
L1=framelength(Fa,size(f,1));
L2=framelengthcoef(Fs,size(s,1));
if L1~=L2
    error(['%s: The symbol and signal lengths are incompatible.'],upper(mfilename));
end;

% This is not *strictly* necessary, but we cannot check that the symbol
% is complex-valued in just the right way.
if Fa.realinput && ~isreal(s)
    error(['%s: For real-valued-input-only frames, the symbol must also ' ...
           'be real.'],upper(mfilename));
end;

h=frsyn(Fs,s.*frana(Fa,f));







