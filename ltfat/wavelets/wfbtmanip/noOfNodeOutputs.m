function noOut = noOfNodeOutputs(nodeNo,wt)
%NOOFNODEOUTPUTS Number of node Outputs
%   Usage:  noOut = noOfNodeOutputs(nodeNo,wt);
%
%   Input parameters:
%         nodeNo  : Node index.
%         wt      : Structure containing description of the filter tree.
%
%   Output parameters:
%         noOut      : Number of node outputs. 
%
%   NOOFNODEOUTPUTS(nodeNo,wt) Return number of the terminal 
%   outputs of the node nodeNo. For definition of the structure
%   see wfbinit.
%
%   See also: wfbtinit
%
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfbtmanip/noOfNodeOutputs.php

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

if(any(nodeNo>numel(wt.nodes)))
   error('%s: Invalid node index range. Number of nodes is %d.\n',upper(mfilename),numel(wt.nodes));
end

%This is slow..
noOut = cellfun(@(nEl) numel(nEl.g), wt.nodes(nodeNo)) -...
        cellfun(@(chEl) numel(chEl(chEl~=0)), wt.children(nodeNo));

%This is even slower
% nodesCount = numel(nodeNo);
% noOut = zeros(nodesCount,1);
% for ii = 1:nodesCount
%    noOut(ii) = numel(wt.nodes{ii}.filts) - numel(find(wt.children{ii}~=0));
% end

%  chan = max([length(wt.nodes{nodeNo}.g), length(wt.nodes{nodeNo}.h)]);
%  child = length(find(wt.children{nodeNo}~=0));
%  noOut = chan -child;



