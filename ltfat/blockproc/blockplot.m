function cola=blockplot(p,F,c,cola)
%BLOCKPLOT Plot block coefficients
%   Usage: blockplot(p,F,c);
%
%   Input parameters:
%         p     : JAVA object of the class net.sourceforge.ltfat.SpectFrame.
%         F     : Frame object.
%         c     : Block coefficients.
%         cola  : (Optional) overlap from previous block.
%
%   Output parameters:
%         cola  : Overlap to the next block.
%
%   BLOCKPLOT(p,F,c) appends the block coefficients c to the running 
%   coefficient plot in p.
%
%   cola=BLOCKPLOT(p,F,c,cola) doest the same, but adds cola to the 
%   first respective coefficients in c and returns last coefficients from
%   c. This is only relevant for the sliced window blocking approach.
%
%   Url: http://ltfat.sourceforge.net/doc/blockproc/blockplot.php

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

if size(c,2)>1
   error('%s: Only one channel input is supported.',upper(mfilename));
end

ctf = framecoef2tf(F,c(:,1));

if strcmp(F.blockalg,'sliced')
   % DO the coefficient overlapping or cropping
   %ctf = ctf(:,floor(end*3/8):floor(end*5/8)+1);
   
   if nargin>3 
      olLen = ceil(size(ctf,2)/2);
      if isempty(cola)
         cola = zeros(size(ctf,1),olLen,class(ctf));
      end
         
      ctf(:,1:olLen) = ctf(:,1:olLen) + cola;
      cola = ctf(:,end+1-olLen:end);
      ctf = ctf(:,1:olLen);
   end
end


ctf = abs(ctf);

if isoctave
   % The JAVA 2D-array handling is row-major
   ctf = cast(ctf,'double').';
   javaMethod('append',p,ctf(:),size(ctf,2),size(ctf,1));
else
   % Matlab casts correctly
   ctf = cast(ctf,'single');
   javaMethod('append',p,ctf);
end



