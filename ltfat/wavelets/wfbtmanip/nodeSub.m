function subNo = nodeSub(nodeNo,wt)


if(any(nodeNo>numel(wt.nodes)))
   error('%s: Invalid node index range. Number of nodes is %d.\n',upper(mfilename),numel(wt.nodes));
end

nodeNoa = cellfun(@(nEl) nEl.a,wt.nodes,'UniformOutput',0);
nodeNoUps = nodeFiltUps(nodeNo,wt);

nodesCount = numel(nodeNo);
subNo = cell(1,nodesCount);
for ii=1:nodesCount
   subNo{ii} = nodeNoUps(ii)*nodeNoa{ii};
end

% if(nodesCount==1)
%    subNo = subNo{1};
% end
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfbtmanip/nodeSub.php

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


%subNo = nodeFiltUps(nodeNo,wt).*wt.nodes{nodeNo}.a;
