function wt = deleteNode(nodeNo,wt)
%DELETENODE Removes specified node from the tree
%   Usage:  wt = deleteNode(nodeNo,wt)
%
%   Input parameters:
%         nodeNo   : Node index.
%         wt       : Structure containing description of the filter tree.
%
%   Output parameters:
%         wt       : Modified wt.
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfbtmanip/deleteNode.php

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

if(~isempty(find(wt.children{nodeNo}~=0)))
    error('Deleting non-leaf node!');
end

parId = wt.parents(nodeNo);
toZero = find(wt.children{parId}==nodeNo);
wt.children{parId}(toZero) = 0;

newIdx = 1:length(wt.nodes);
newIdx = newIdx(find(newIdx~=nodeNo));
wt.nodes = wt.nodes(newIdx);
%treeStruct.a = {treeStruct.a{newIdx}};
%treeStruct.origins = {treeStruct.origins{newIdx}};
wt.parents = wt.parents(newIdx); 
wt.children = wt.children(newIdx);

% and all children and parents with higher idx are lessened
 for ii =1:length(wt.children)
     biggerIdx = find(wt.children{ii}>nodeNo);
     wt.children{ii}(biggerIdx) = wt.children{ii}(biggerIdx)-1;
 end
 biggerIdx = find(wt.parents>nodeNo);
 wt.parents(biggerIdx) = wt.parents(biggerIdx)-1;
