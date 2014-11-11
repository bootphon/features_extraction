function nodesIdxs = nodesDForder(treeStruct,varargin)
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
%   Supported flags:
%
%             'ord','rev'
%
%   See also: wfbtinit
%
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfbtmanip/nodesDForder.php

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
nodesIdxs = [nodeNo,nodeSubtreeDF(nodeNo,treeStruct)];


if(~isempty(varargin))
    if(strcmpi(varargin{1},'rev'))
       nodesIdxs = nodesIdxs(end:-1:1); 
    end
end
