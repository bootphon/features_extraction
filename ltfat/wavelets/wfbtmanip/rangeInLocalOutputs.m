function outRange = rangeInLocalOutputs(nodeNo,wt)
%RANGEINLOCALOUTPUTS Node output index range of the terminal outputs
%   Usage:  outRange = rangeInLocalOutputs(nodeNo,wt);
%
%   Input parameters:
%         nodeNo     : Node index.
%         wt : Structure containing description of the filter tree.
%
%   Output parameters:
%         noOut      : Index range. 
%
%   RANGEINLOCALOUTPUTS(nodeNo,wt) Return range of indexes of the
%   terminal outputs of the node nodeNo. For definition of the structure
%   see wfbinit.
%
%   See also: wfbtinit
%
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfbtmanip/rangeInLocalOutputs.php

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


nodesCount = length(nodeNo);
outRange = cell(nodesCount,1); 


nodeChans = cellfun(@(nEl) numel(nEl.g), wt.nodes(nodeNo));
chIdx = cellfun(@(chEl) find(chEl~=0), wt.children(nodeNo),'UniformOutput',0);


for ii = 1:nodesCount
 outRangeTmp = 1:nodeChans(ii);
 outRangeTmp(chIdx{ii}) = [];
 outRange{ii} = outRangeTmp;
% outRange{ii} = setdiff(1:nodeChans(ii),chIdx{ii}); 

%    outNodes = zeros(nodeChans(ii),1);
%    outNodes(chIdx{ii}) = 1;
%    zeroIdx = find(outNodes==0);
%    if(~isempty(zeroIdx))
%       outRange{ii} = zeroIdx;
%    end
end

% if(numel(outRange)==1)
%    outRange = outRange{1};
% end


% for ii = 1:nodesCount
%    chIdx = find(wt.children{nodeNo(ii)}~=0);
%    chan = max([length(wt.nodes{nodeNo(ii)}.g), length(wt.nodes{nodeNo(ii)}.h)]);
%    outNodes = zeros(chan,1);
%    outNodes(chIdx) = wt.children{nodeNo(ii)}(chIdx);
%    if(iscell(outRange))
%       outRange{ii} = find(outNodes==0);
%    else
%       outRange = find(outNodes==0);
%    end
% end


