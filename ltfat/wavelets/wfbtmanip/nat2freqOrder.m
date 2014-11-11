function wt = nat2freqOrder(wt)
%NAT2FREQORDER Natural To Frequency Ordering
%   Usage:  wt = nat2freqOrder(wt);
%
%   Input parameters:
%         wt    : Structure containing description of the filter tree.
%
%   Output parameters:
%         wt    : Structure containing description of the filter tree.
%
%   NAT2FREQORDER(wt) Creates new wavelet filterbank tree definition
%   with permuted order of some filters for purposes of the correct frequency
%   ordering of the resultant identical filters. For definition of the
%   structure see wfbinit.
%
%   See also: wfbtinit,  wfbtmultid, nodesBForder
%
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfbtmanip/nat2freqOrder.php

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

treePath = nodesBForder(wt);
%skip root
treePath = treePath(2:end);

for ii=1:length(treePath)
    % should not be zero
    nodeId = treePath(ii);
    parentId = wt.parents(nodeId);
    % local index of the parent output connected to the treePath(ii) node,
    % is in range 1:chan
    locIdx = find(wt.children{parentId}==nodeId,1);
    
    % do nothing if the node is connected to the first (hopefully lowpass)
    % output
    if(rem(locIdx,2)~=1)
       % now for the filter reordering
       chan = numel(wt.nodes{nodeId}.g);
       wt.nodes{nodeId}.g = wt.nodes{nodeId}.g(chan:-1:1);
       wt.nodes{nodeId}.h = wt.nodes{nodeId}.h(chan:-1:1);
       wt.nodes{nodeId}.a = wt.nodes{nodeId}.a(chan:-1:1);
    end    
    
end



