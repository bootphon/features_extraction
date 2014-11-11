function [pOutIdxs,chOutIdxs] = rangeInWpOutputs(wt)

treePath = nodesBForder(wt);
trLen = length(treePath);
pOutIdxs = zeros(1,trLen);
chOutIdxs = cell(1,trLen);
pRunIdx = [0];
chRunIdx = 1;
% do trough tree and look for nodeNo and its parrent
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfbtmanip/rangeInWpOutputs.php

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
for ii=1:trLen
    tmpfiltNo = length(wt.nodes{treePath(ii)}.filts);
    chOutIdxs{ii} = chRunIdx:chRunIdx+tmpfiltNo-1;
    chRunIdx = chRunIdx + tmpfiltNo;
    pOutIdxs(ii) = pRunIdx(1);
    pRunIdx = [pRunIdx(2:end),chOutIdxs{ii}];
end


