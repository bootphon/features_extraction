function nodesIdxs = nodeSubtreeDF(nodeNo,wt)
%NODESUBTREEBF Node subtree nodes in Depth-First order
%   Usage:  noOut = nodeSubtreeDF(nodeNo,wt);
%
%   Input parameters:
%         nodeNo  : Node index.
%         wt      : Structure containing description of the filter tree.
%
%   Output parameters:
%         noOut   : Nodes in a Depth-First order. 
%
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfbtmanip/nodeSubtreeDF.php

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


% subtreeIdx = [];
% 
% children = treeStruct.children{nodeNo}(find(treeStruct.children{nodeNo}~=0));
% subtreeIdx(end+1:end+length(children)) = children;
% 
% for ii=1:length(children)
%    tmpSbIdx = nodeSubtreeBF(children(ii),treeStruct);
%    subtreeIdx(end+1:end+length(tmpSbIdx)) = tmpSbIdx;
% end

toGoTrough = nodeNo;
nodesIdxs = [];
while ~isempty(toGoTrough)
   chtmp = find(wt.children{toGoTrough(1)}~=0);
   chIdxtmp = wt.children{toGoTrough(1)}(chtmp);
   nodesIdxs = [nodesIdxs,toGoTrough(1)];
   toGoTrough = [chIdxtmp,toGoTrough(2:end)];
end

% remove the nodeNo. Just to be consistent with nodeSubtreeBF
% TO DO: is it wise?
nodesIdxs = nodesIdxs(2:end);
