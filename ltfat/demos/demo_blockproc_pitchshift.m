function demo_blockproc_pitchshift(source,varargin)
%DEMO_BLOCKPROC_PITCHSHIFT Pitch shift by Gabor coefficient bands shift
%   Usage: demo_blockproc_pitchshift('gspi.wav')
%
%   For additional help call DEMO_BLOCKPROC_PITCHSHIFT without arguments.
%
%   This script demonstrates a real-time Gabor coefficient manipulation.
%   Frequency bands are shifted up or down according to the slider
%   position.
%
%
%   Url: http://ltfat.sourceforge.net/doc/demos/demo_blockproc_pitchshift.php

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

if demo_blockproc_header(mfilename,nargin)
   return;
end

M = 1000;

fobj = blockfigure();

% Basic Control pannel (Java object)
parg = {
        {'GdB','Gain',-20,20,0,21},...
        {'Shi','Shift',-200,200,0,401}
       };

p = blockpanel(parg);

bufLen = 1024;
% Setup blocktream
fs=block(source,varargin{:},'loadind',p,'L',bufLen);

% Window length in ms
winLenms = 20; 
[F,Fdual] = framepair('dgtreal',{'hann',floor(fs*winLenms/1e3)},'dual',128,M);
[Fa,Fs] = blockframepairaccel(F,Fdual, bufLen,'segola');

flag = 1;
%Loop until end of the stream (flag) and until panel is opened
while flag && p.flag
   gain = blockpanelget(p,'GdB');
   gain = 10.^(gain/20);
   shift = fix(blockpanelget(p,'Shi'));

   % Read block of data
   [f,flag] = blockread();

   % Apply gain
   f=f*gain;
   
   % Obtain DGT coefficients
   c = blockana(Fa, f);
   
   % Do the actual coefficient shift
   cc = Fa.coef2native(c,size(c));
   
   if(strcmpi(source,'playrec'))
      % Hum removal (aka low-pass filter)
      cc(1:2,:,:) = 0;
   end
   
   if shift<0
      cc = [cc(-shift+1:end,:,:); zeros(-shift,size(cc,2),size(cc,3))];
   else
      cc = [zeros(shift,size(cc,2),size(cc,3)); cc(1:end-shift,:,:)];
   end
   c = Fa.native2coef(cc);
   
   % Plot the transposed coefficients
   blockplot(fobj,Fa,c(:,1));
   
   % Reconstruct from the modified coefficients
   fhat = blocksyn(Fs, c, size(f,1));

   % Enqueue to be played
   blockplay(fhat);
end
% Clear and close all
blockdone(p,fobj);

