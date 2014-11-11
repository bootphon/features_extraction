function [nodeNo,nodeChildIdx] = depthIndex2NodeNo(d,k,treeStruct)

if(d==0)
    nodeNo=0;
    nodeChildIdx=0;
    return;
end

% find ordered nodes at depth d-1
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfbtmanip/depthIndex2NodeNo.php

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
nodesNo = getNodesInDepth(d,treeStruct);
if(isempty(nodesNo))
   error('%s: Depth of the tree is less than given d.',mfilename); 
end
ktemp = k;
% k is index in children of ordered nodes at depth d
for ii=1:length(nodesNo)
    chNo = max([length(treeStruct.nodes{nodesNo(ii)}.g), length(treeStruct.nodes{nodesNo(ii)}.h)]);
    %chNo = length(treeStruct.nodes{nodesNo(ii)});
    if(ktemp<chNo)
        nodeChildIdx = ktemp+1;
        nodeNo = nodesNo(ii);
        return;
    else
        ktemp = ktemp-chNo;
    end
end

error('%s: Index k out of bounds.',mfilename);


function nodd = getNodesInDepth(d,treeStruct)
% find all nodes with d steps to the root ordered
nodd = [];
toGoTrough = {};


   nodeNo = find(treeStruct.parents==0);
   toGoTrough = cell(d+1,1);
   toGoTrough{1} = nodeNo;
   tempd = 1;


while(tempd<d)
    for jj=1:length(toGoTrough{tempd})
       actNod = toGoTrough{tempd}(jj);
       childrenIdx = find(treeStruct.children{actNod}~=0);
       ch = treeStruct.children{actNod}(childrenIdx);
       toGoTrough{tempd+1} = [toGoTrough{tempd+1},ch];
    end

    tempd = tempd+1;
end

nodd=toGoTrough{d};

