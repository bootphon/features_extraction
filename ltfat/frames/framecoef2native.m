function coef=framecoef2native(F,coef);
%FRAMECOEF2NATIVE  Convert coefficients to native format
%   Usage: cout=framecoef2native(F,cin);
%
%   FRAMECOEF2NATIVE(F,coef) converts the frame coefficients coef into the
%   native coefficient format of the frame. The frame object F must have been
%   created using FRAME.
%
%   See also: frame, framenative2coef, framecoef2tf
%
%   Url: http://ltfat.sourceforge.net/doc/frames/framecoef2native.php

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
  
if nargin<2
  error('%s: Too few input parameters.',upper(mfilename));
end;

if ~isstruct(F)
  error('%s: First agument must be a frame definition structure.',upper(mfilename));
end;

[MN,W]=size(coef);

if isfield(F,'coef2native')
    coef=F.coef2native(coef,size(coef));
else
    
    switch(F.type)                                
      case {'filterbank','filterbankreal'}
        L=framelengthcoef(F,MN);
        N=L./F.a
        coef=mat2cell(coef,N,W);
  
    end;
end;

