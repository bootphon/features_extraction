function wt = wfbtremove(d,kk,wt,varargin)
%WFBTREMOVE Remove node from the filterbank tree
%   Usage:  wt = wbftremove(d,kk,wt);
%           wt = wfbtremove(d,kk,wt,'force');
%
%   Input parameters:
%           d   : Level in the tree (0 - root).
%           kk  : Index of the node at level d (starting at 0) or array 
%                 of indexes. 
%           wt  : Wavelet filterbank tree structure (as returned from
%                 WFBTINIT).
%
%   Output parameters:
%           wt : Modified filterbank structure.
%   
%   WFBTREMOVE(d,kk,wt) removes existing node at level d and index kk*
%   from the filterbank tree structure wt. The function fails if the 
%   node has any children (is not a leaf node).
%
%   WFBTREMOVE(d,k,wt,'force') does the same, but any childern of the
%   node are removed too.
%
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfbtremove.php

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

if(nargin<3)
   error('%s: Too few input parameters.',upper(mfilename)); 
end

definput.flags.force = {'noforce','force'};
[flags,kv]=ltfatarghelper({},definput,varargin);

if(isempty(wt.nodes))
   error('%s: Tree is empty.',mfilename); 
end

for i=1:numel(kk)
   k=kk(i);
   [nodeNo,nodeChildIdx] = depthIndex2NodeNo(d,k,wt);
   if(nodeNo==0)
       % removing root 
       rootNo = find(wt.parents==0);
       % check for any children of the root
       if(isempty(find(wt.children{rootNo}~=0,1)))
           wt = wfbtinit();
       else
           if(flags.do_force)
               wt = wfbtinit();
           else
               error('%s: Deleting root node. To delete the whole tree use FORCE option.',mfilename,d,k); 
           end
       end
   end

   % check if node exists
   childrenIdx = find(wt.children{nodeNo}~=0);
   found = find(childrenIdx==nodeChildIdx,1);

   if(isempty(found))
        error('%s: Such node (depth=%d, idx=%d) does not exist.',mfilename,d,k); 
   end


   nodeToDelete = wt.children{nodeNo}(nodeChildIdx);
   % Check if it is a leaf (terminal node)
   if(~isempty(find(wt.children{nodeToDelete}~=0,1)))
       if(flags.do_force)
           wt = deleteSubtree(nodeToDelete,wt);
           return;
       else
           error('%s: Deleting non-leaf node. To delete whole subtree use FORCE option.',mfilename);
       end
   else
       wt = deleteNode(nodeToDelete,wt); 
   end
end
%wtree = deleteNode(nodeToDelete,wtree);

