function noOut = noOfOutputs(wt)
%NOOFOUTPUTS Returns number of outputs of the filter tree 
%   Usage:  noOut=noOfOutputs(treeStruct)
%
%   Input parameters:
%         wt  : Structure containing description of the filter tree.
%
%   Output parameters:
%         noOut       : Number of outputs of the whole filter tree.
%
%   NOOFOUTPUTS(wt) For definition of the structure see wfbinit.
%
%   See also: wfbtinit
%
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfbtmanip/noOfOutputs.php

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

noOut = sum(noOfNodeOutputs(1:numel(wt.nodes),wt));

%noOut = sum( cellfun(@(nEl) numel(nEl.filts),wt.nodes) -...
%        cellfun(@(chEl) numel(chEl(chEl~=0)), wt.children) );

% Equivalent:     
% noOut = 0;
% for jj =1:length(wt.nodes)
%     chan = max([length(wt.nodes{jj}.filts), length(wt.nodes{jj}.h)]);
%     children = length(find(wt.children{jj}~=0));
%     noOut = noOut + chan-children;
% end
