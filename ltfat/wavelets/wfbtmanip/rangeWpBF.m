function [pOutIdxs,chOutIdxs] = rangeWpBF(wt,varargin)

treePath = nodesBForder(wt);
trLen = numel(treePath);
pOutIdxs = zeros(1,trLen);
chOutIdxs = cell(1,trLen);
pRunIdx = [0];
chRunIdx = 1;
% do trough tree and look for nodeNo and its parent
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfbtmanip/rangeWpBF.php

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
    tmpfiltNo = length(wt.nodes{treePath(ii)}.g);
    locRange = rangeInLocalOutputs(treePath(ii),wt);
    diffRange = 1:tmpfiltNo;
    diffRange(locRange{1})=[];
    chOutIdxs{ii} = chRunIdx:chRunIdx+tmpfiltNo-1;
    chRunIdx = chRunIdx + tmpfiltNo;
    pOutIdxs(ii) = pRunIdx(1);
    pRunIdx = [pRunIdx(2:end),chOutIdxs{ii}(diffRange)];
end


if(~isempty(varargin))
    if(strcmpi(varargin{1},'rev'))
       pOutIdxs = pOutIdxs(end:-1:1); 
       chOutIdxs = chOutIdxs(end:-1:1);
    end
end


