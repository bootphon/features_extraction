function Ft=frametight(F);
%FRAMETIGHT  Construct the canonical tight frame
%   Usage: F=frametight(F);
%          F=frametight(F,L);
%
%   Ft=FRAMETIGHT(F) returns the canonical tight frame of F.
%
%   The canonical tight frame can be used to get perfect reconstruction if
%   it is used for both analysis and synthesis. This is demonstrated in the
%   following example:
%
%     % Create a frame and its canonical tight
%     F=frame('dgt','hamming',32,64);
%     Ft=frametight(F);
%
%     % Compute the frame coefficients and test for perfect
%     % reconstruction
%     f=gspi;
%     c=frana(Ft,f);
%     r=frsyn(Ft,c);
%     norm(r(1:length(f))-f)
%
%   See also: frame, framepair, framedual
%
%   Url: http://ltfat.sourceforge.net/doc/frames/frametight.php

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

% Default operation, works for a lot of frames
Ft=F;

% Handle the windowed transforms
switch(F.type)
  case {'dgt','dgtreal','dwilt','wmdct','filterbank','ufilterbank',...
        'nsdgt','unsdgt','nsdgtreal','unsdgtreal'}
    
    Ft=frame(F.type,{'tight',F.g},F.origargs{2:end});
    
  case {'filterbankreal','ufilterbankreal'}
    Ft=frame(F.type,{'realtight',F.g},F.origargs{2:end});
    
  case 'gen'
    [U,sv,V] = svd(F.g,'econ');    
    Ft=frame('gen',U*V');

  case 'tensor'
    for ii=1:F.Nframes
        tight_frames{ii}=frametight(F.frames{ii});
    end;
    F=frame('tensor',tight_frames{:});

  case 'fusion'
    tight_w=1./F.w;
    for ii=1:F.Nframes
        tight_frames{ii}=frametight(F.frames{ii});
    end;
    Ft=frame('fusion',tight_w,tight_frames{:});

end;

