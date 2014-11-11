function coef = comp_cellcoef2tf(coef,maxLen)
%COMP_CELLCOEF2TF Cell to a tf-layout
%   Usage: coef = comp_cellcoef2tf(coef,dim)
%
%
%   Url: http://ltfat.sourceforge.net/doc/comp/comp_cellcoef2tf.php

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

% TO DO: This does not work in octave, because interp1 does not work with
% TO DO: complex data and always returns double.


coefLenMax = max(cellfun(@(cEl)size(cEl,1),coef));
coefTmp = zeros(coefLenMax,numel(coef),size(coef{1},2),class(coef{1}));

if nargin>1
   coefLenMax = min([coefLenMax,maxLen]);
end

for ii=1:numel(coef)
   if size(coef{ii},1) == 1
      coefTmp(:,ii) = coef{ii};
      continue;
   end
   coefTmp(:,ii) = interp1(coef{ii},linspace(1,size(coef{ii},1),coefLenMax),'nearest');
end
coef = coefTmp';
