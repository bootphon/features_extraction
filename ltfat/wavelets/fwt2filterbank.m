function [g,a] = fwt2filterbank( w, J)
%FWT2FILTERBANK  FWT equivalent non-iterated filterbank
%   Usage: [g,a] = fwt2filterbank(wtdef)
%
%   Input parameters:
%          w : Wavelet filters definition
%          J : Number of filterbank iterations.
%
%   Output parameters:
%         g   : Cell array containing filters
%         a   : Vector of sub-/upsampling factors
%
%   FWT2FILTERBANK( w, J) calculates the impulse responses g and the 
%   subsampling factors a of non-iterated filterbank, which is equivalent
%   to the wavelet filterbank tree described by w and J. The returned 
%   parameters can be used directly in FILTERBANK, UFILTERBANK.
%   
%   The filters are scaled if a is not returned. 
%
%   The function is wrapper for calling WFBT2FILTERBANK.
%   
%   See also: wfbtinit, wfbt2filterbank, filterbank
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/fwt2filterbank.php

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


if(nargin<2)
    error('%s: Not enough input arguments',upper(mfilename));
end

if nargout<2
  g = wfbt2filterbank({w,J,'dwt'});
elseif nargout == 2
  [g,a] = wfbt2filterbank({w,J,'dwt'});   
end


