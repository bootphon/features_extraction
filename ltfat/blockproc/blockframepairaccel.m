function [Fao,Fso] = blockframepairaccel(Fa, Fs, Lb, varargin)
%BLOCKFRAMEPAIRACCEL Precompute structures for block processing
%   Usage: F = blockframepairaccel(Fa,Fs,Lb);
%
%   [Fao,Fso]=BLOCKFRAMEPAIRACCEL(Fa,Fs,Lb) works similar to 
%   BLOCKFRAMEACCEL with a pair of frames. The only difference from
%   calling BLOCKFRAMEACCEL separatelly for each frame is correct
%   default choice of the slicing windows.   
%
%      'sliwin',sliwin   : Slicing window. sliwin have to be a window
%                            of length 2Lb. It is used in the slicing
%                            window approach.
%
%   Url: http://ltfat.sourceforge.net/doc/blockproc/blockframepairaccel.php

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

definput.flags.blockalg = {'naive','sliced','segola'};
definput.keyvals.anasliwin = [];
definput.keyvals.synsliwin = [];
[flags,kv]=ltfatarghelper({},definput,varargin);

assert(~(~flags.do_sliced && (~isempty(kv.anasliwin) || ~isempty(kv.synsliwin))),...
   '%s: Definig slicing window without setting the ''silced'' flag.',mfilename);

if flags.do_sliced 
   if isempty(kv.anasliwin)
      kv.anasliwin = 'hann';
   end
   
   if isempty(kv.synsliwin)
      kv.synsliwin = 'rect';
   end

   Fao = blockframeaccel(Fa,Lb,'sliced','sliwin',kv.anasliwin);
   Fso = blockframeaccel(Fs,Lb,'sliced','sliwin',kv.synsliwin);
else
   Fao = blockframeaccel(Fa,Lb,flags.blockalg);
   Fso = blockframeaccel(Fs,Lb,flags.blockalg);
end





