function nodesIdxs = nodesLevelsBForder(treeStruct)
%NODESBFORDER Nodes in the Breadth-First search order
%  Usage:  nodesIdxs = nodesBForder(treeStruct)
%
%   Input parameters:
%         treeStruct  : Structure containing description of the filter tree.
%
%   Output parameters:
%         nodesIdxs   : Node indexes in the Breadth-First search order.
%
%   nodesBForder(treeStruct) For definition of the structure see
%   wfbinit.
%
%   See also: wfbtinit
%
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfbtmanip/nodesLevelsBForder.php

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


%find root
nodeNo = find(treeStruct.parents==0);
toGoTrough = [nodeNo];
nodesIdxs = {nodeNo};
inLevel = [1];
counter = 0;
level = 2;
chIdxSum = 0;
while ~isempty(toGoTrough)
   chtmp = find(treeStruct.children{toGoTrough(1)}~=0);
   chIdxtmp = treeStruct.children{toGoTrough(1)}(chtmp);
   counter = counter + 1;

   if(length(nodesIdxs)<level&&~isempty(chIdxtmp))
       nodesIdxs = {nodesIdxs{:},[]}; 
   end
   
   chIdxSum = chIdxSum + length(chIdxtmp);
   if(~isempty(chIdxtmp))
       nodesIdxs{level} = [nodesIdxs{level},chIdxtmp];
   end
   
   toGoTrough = [toGoTrough(2:end),chIdxtmp];

   if(counter==inLevel(level-1))
       counter = 0;
       inLevel(level) = chIdxSum;
       level = level + 1;
       chIdxSum = 0;
   end
end


