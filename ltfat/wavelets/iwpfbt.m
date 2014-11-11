function f=iwpfbt(c,par,varargin)
%IWPFBT   Inverse Wavelet Packet Filterbank Tree
%   Usage:  f=iwpfbt(c,info);
%           f=iwpfbt(c,wt,Ls);
%
%   Input parameters:
%         c       : Coefficients stored in a cell-array.
%         info,wt : Transform parameters struct/Wavelet Filterbank tree.
%         Ls      : Length of the reconstructed signal.
%
%   Output parameters:
%         f     : Reconstructed data.
%
%   f = IWPFBT(c,info) reconstructs signal f from the coefficients c 
%   using parameters from info struct. both returned by WFBT function.
%
%   f = IWPFBT(c,wt,Ls) reconstructs signal f from the coefficients c*
%   using filter bank tree defined by wt. Plese see WFBT function for
%   possible formats of wt. The Ls parameter is mandatory due to the 
%   ambiguity of reconstruction lengths introduced by the subsampling 
%   operation and by boundary treatment methods. Note that the same flag as
%   in the WFBT function have to be used, otherwise perfect reconstruction
%   cannot be obtained. Please see help for WFBT for description of the
%   flags.
%
%   Examples:
%   ---------
%   
%   A simple example showing perfect reconstruction using the "full decomposition" wavelet tree:
% 
%     f = gspi;
%     J = 7;
%     wtdef = {'db10',J,'full'};
%     c = wpfbt(f,wtdef);
%     fhat = iwpfbt(c,wtdef,length(f));
%     % The following should give (almost) zero
%     norm(f-fhat)
%
%   See also: wpfbt, wfbtinit
%
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/iwpfbt.php

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

if(~iscell(c))
    error('%s: Unrecognized coefficient format.',upper(mfilename));
end



if(isstruct(par)&&isfield(par,'fname'))
   if nargin>2
      error('%s: Too many input parameters.',upper(mfilename));
   end
   wt = wfbtinit({'dual',par.wt},par.fOrder);
   Ls = par.Ls;
   ext = par.ext;
   do_scale = ~par.isNotScaled;
else
   if nargin<3
      error('%s: Too few input parameters.',upper(mfilename));
   end
   %% PARSE INPUT
   definput.keyvals.Ls=[];    
   definput.import = {'fwt','wfbtcommon'};
   definput.flags.scale = {'scale','noscale'};
   [flags,kv,Ls]=ltfatarghelper({'Ls'},definput,varargin);

   ext = flags.ext;
   do_scale = flags.scale;
   % Initialize the wavelet tree structure
   wt = wfbtinit(par,flags.forder);
end

wtPath = nodesBForder(wt,'rev');
[pOutIdxs,chOutIdxs] = rangeWpBF(wt,'rev');
f = comp_iwpfbt(c,wt.nodes(wtPath),pOutIdxs,chOutIdxs,Ls,ext,do_scale);

