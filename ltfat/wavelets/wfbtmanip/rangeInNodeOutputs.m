function outRange = rangeInNodeOutputs(nodeNo,treeStruct)
%RANGEINNODEOUTPUTS Index range of the node outputs
%   Usage:  outRange = rangeInNodeOutputs(nodeNo,treeStruct)
%
%   Input parameters:
%         nodeNo     : Node index.
%         treeStruct : Structure containing description of the filter tree.
%
%   Output parameters:
%         outRange   : Index range. 
%
%   RANGEINNODEOUTPUTS(nodeNo,treeStruct) For definition of the structure
%   see wfbinit.
%
%   See also: wfbtinit
%
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfbtmanip/rangeInNodeOutputs.php

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
chIdx = find(treeStruct.children{nodeNo}~=0);
chan = numel(treeStruct.nodes{nodeNo}.g);
outNodes = zeros(chan,1);
outNodes(chIdx) = treeStruct.children{nodeNo}(chIdx);

outRangeStart = 0;
outRange = [];
for ii=1:chan
   if(outNodes(ii)==0)
       outRange(end+1) = outRangeStart+1;
       outRangeStart = outRangeStart+1;
   else
      outRangeStart=outRangeStart + noOfSubtreeOutputs(outNodes(ii),treeStruct);
   end
end
