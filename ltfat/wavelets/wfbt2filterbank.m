function [g,a] = wfbt2filterbank( wtdef, varargin)
%WFBT2FILTERBANK  WFBT equivalent non-iterated filterbank
%   Usage: [g,a] = wfbt2filterbank(wtdef)
%
%   Input parameters:
%         wtdef : Wavelet filter tree definition
%
%   Output parameters:
%         g   : Cell array containing filters
%         a   : Vector of sub-/upsampling factors
%
%   wfbtmultid(wtdef) calculates the impulse responses g and the 
%   subsampling factors a of non-iterated filterbank, which is equivalent
%   to the wavelet filterbank tree described by wtdef. The returned 
%   parameters can be used directly in FILTERBANK, UFILTERBANK or 
%   FILTERBANK. 
%   
%   The filters are scaled if a is not returned. 
%
%   The function internally calls WFBTINIT and passes wtdef and all 
%   additional parameters to it.   
%   
%   Examples:
%   --------- 
%   
%   The following two examples create a multirate identity filterbank
%   using a tree of depth 3. In the first example, the filterbank is
%   identical to the DWT tree:
%
%     [g,a] = wfbt2filterbank({'db10',3,'dwt'});
%     
%
%   In the second example, the filterbank is identical to the full
%   wavelet tree:
%
%     [g,a] = wfbt2filterbank({'db10',3,'full'});
%
%   See also: wfbtinit
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfbt2filterbank.php

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


if(nargin<1)
    error('%s: Not enough input arguments',upper(mfilename));
end

% build the tree
wt = wfbtinit({'strict',wtdef},varargin{:});

% Pick just nodes with outputs
wtPath = 1:numel(wt.nodes);
wtPath(noOfNodeOutputs(1:numel(wt.nodes),wt)==0)=[];


rangeLoc = rangeInLocalOutputs(wtPath,wt);
rangeOut = rangeInOutputs(wtPath,wt); 

[g,a] = nodesMultid(wtPath,rangeLoc,rangeOut,wt);

if nargout<2
   % Scale filters if a is not returned
   for nn=1:numel(g)
       g{nn}.h = g{nn}.h/sqrt(a(nn));
   end
end

















