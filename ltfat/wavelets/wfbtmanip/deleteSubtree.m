function wt = deleteSubtree(nodeNo,wt)
%DELETESUBTREE Removes subtree with root node
%   Usage:  wt = deleteSubtree(nodeNo,wt)
%
%   Input parameters:
%         nodeNo   : Node index.
%         wt       : Structure containing description of the filter tree.
%
%   Output parameters:
%         wt       : Modified wt.
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfbtmanip/deleteSubtree.php

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

toDelete = nodeSubtreeBF(nodeNo,wt);

for ii = length(toDelete):-1:1
  wt = deleteNode(toDelete(ii),wt); 
  biggerIdx = find(toDelete>toDelete(ii));
  toDelete(biggerIdx) = toDelete(biggerIdx) - 1;
end
wt = deleteNode(nodeNo,wt); 
