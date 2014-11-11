function f=comp_iuwpfbt(c,wtNodes,nodesUps,pOutIdxs,chOutIdxs)
%COMP_IUWPFBT Compute Inverse Undecimated Wavelet Packet Filter-Bank Tree
%   Usage:  f=comp_iuwpfbt(c,wtNodes,nodesUps,pOutIdxs,chOutIdxs)
%
%   Input parameters:
%         c          : Coefficients stored in L*M*W array.
%         wtNodes    : Filterbank tree nodes (elementary filterbans) in
%                      reverse BF order. Cell array of structures of length nodeNo.
%         nodesUps   : Filters upsampling factor of each node. Array of
%                      length nodeNo.
%         pOutIdxs   : Idx of each node's parent. Array of length nodeNo.
%         chOutIdxs  : Idxs of each node children. Cell array of vectors of
%                      length nodeNo.
%
%   Output parameters:
%         f     : Reconstructed data in L*W array.
%
%
%   Url: http://ltfat.sourceforge.net/doc/comp/comp_iuwpfbt.php

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

% For each node in tree in the BF order...
 for jj=1:length(wtNodes)
    % Node filters subs. factors
    a = wtNodes{jj}.a;
    % Node filters to a matrix
    gMat = cell2mat(cellfun(@(gEl) gEl.h(:),wtNodes{jj}.g(:)','UniformOutput',0));
    % Normalize each filter
    gMat = bsxfun(@rdivide,gMat,sqrt(a(:)'));
    % Node filters initial skips
    gOffset = cellfun(@(gEl) gEl.offset,wtNodes{jj}.g);
    
    % Zero index position of the upsampled filters.
    offset = nodesUps(jj).*(gOffset);% + nodesUps(jj);

    % Run filterbank
    ctmp = comp_iatrousfilterbank_td(c(:,chOutIdxs{jj},:),gMat,nodesUps(jj),offset);
    
    if(pOutIdxs(jj))
       % Add to the existing subband
       c(:,pOutIdxs(jj),:) = (1/sqrt(2))*(c(:,pOutIdxs(jj),:)+reshape(ctmp,size(ctmp,1),1,size(ctmp,2)));
    else
       % We are at the root.
       f = ctmp;
    end
 end

