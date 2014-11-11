function f=iuwpfbt(c,par,varargin)
%IUWPFBT Inverse Undecimated Wavelet Packet Filterbank Tree
%   Usage:  f=iuwpfbt(c,info);
%           f=iuwpfbt(c,wt,...);
%
%   Input parameters:
%         c     : Coefficients stored in a cell-array.
%         wt    : Wavelet Filterbank tree
%
%   Output parameters:
%         f     : Reconstructed data.
%
%   f=IUWPFBT(c,wt) reconstructs signal f from coefficients c using the
%   wavelet filterbank tree wt. 
%
%   The following flags are supported:
%
%   'per','zpd','sym','symw','asym','asymw','ppd','sp0'
%          Type of the boundary handling.
%
%   'full','dwt'
%          Type of the tree to be used.
%
%   'freq','nat'
%          Frequency or natural order of the coefficient subbands.
%
%   Please see the help on FWT for a description of the flags.
%
%   Examples:
%   ---------
%   
%   A simple example showing perfect reconstruction using the "full decomposition" wavelet tree:
% 
%     f = greasy;
%     J = 7;
%     wtdef = {'db10',J,'full'};
%     c = uwpfbt(f,wtdef);
%     fhat = iuwpfbt(c,wtdef);
%     % The following should give (almost) zero
%     norm(f-fhat)
%
%   See also: wfbt, wfbtinit
%
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/iuwpfbt.php

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
if nargin<2
   error('%s: Too few input parameters.',upper(mfilename));
end;

if(isstruct(par)&&isfield(par,'fname'))
   if nargin>2
      error('%s: Too many input parameters.',upper(mfilename));
   end
   if ~strcmpi(par.fname,'uwpfbt')
      error('%s: Wrong func name in info struct. The info parameter was created by %s.',upper(mfilename),par.fname);
   end
   wt = wfbtinit({'dual',par.wt},par.fOrder,'syn');
else
   %% PARSE INPUT
   definput.import = {'wfbtcommon'};
   if(~isnumeric(c))
       error('%s: Unrecognized coefficient format.',upper(mfilename));
   end

   [flags,kv]=ltfatarghelper({},definput,varargin);

   % Initialize the wavelet tree structure
   wt = wfbtinit(par,flags.forder);
end

wtPath = nodesBForder(wt,'rev');
[pOutIdxs,chOutIdxs] = rangeWpBF(wt,'rev');
nodesUps = nodeFiltUps(wtPath,wt);
f = comp_iuwpfbt(c,wt.nodes(wtPath),nodesUps,pOutIdxs,chOutIdxs);

