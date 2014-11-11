function Fd=framedual(F);
%FRAMEDUAL  Construct the canonical dual frame
%   Usage: F=framedual(F);
%          F=framedual(F,L);
%
%   Fd=FRAMEDUAL(F) returns the canonical dual frame of F.
%
%   The canonical dual frame can be used to get perfect reconstruction as in
%   the following example:
%
%     % Create a frame and its canonical dual
%     F=frame('dgt','hamming',32,64);
%     Fd=framedual(F);
%
%     % Compute the frame coefficients and test for perfect
%     % reconstruction
%     f=gspi;
%     c=frana(F,f);
%     r=frsyn(Fd,c);
%     norm(r(1:length(f))-f)
%
%   See also: frame, framepair, frametight
%
%   Url: http://ltfat.sourceforge.net/doc/frames/framedual.php

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
  
if nargin<1
  error('%s: Too few input parameters.',upper(mfilename));
end;

% Default operation, work for a lot of frames
Fd=F;

% Handle the windowed transforms
switch(F.type)
  case {'dgt','dgtreal','dwilt','wmdct','filterbank','ufilterbank',...
        'nsdgt','unsdgt','nsdgtreal','unsdgtreal',...
        'fwt','ufwt','wfbt','uwfbt','wpfbt','uwpfbt'}
    
    Fd=frame(F.type,{'dual',F.g},F.origargs{2:end});
    
  case {'filterbankreal','ufilterbankreal'}
    Fd=frame(F.type,{'realdual',F.g},F.origargs{2:end});
    
  case 'gen'
    Fd=frame('gen',pinv(F.g)');
    
  case 'tensor'
    for ii=1:F.Nframes
        dual_frames{ii}=framedual(F.frames{ii});        
    end;
    Fd=frame('tensor',dual_frames{:});
        
  case 'fusion'
    dual_w=1./(F.Nframes*F.w);
    for ii=1:F.Nframes
        dual_frames{ii}=framedual(F.frames{ii});        
    end;
    Fd=frame('fusion',dual_w,dual_frames{:});
    
      
end;

