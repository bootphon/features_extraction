function f=iuwfbt(c,par,varargin)
%IUWFBT   Inverse Undecimated Wavelet Filterbank Tree
%   Usage: f = iuwfbt(c,info) 
%          f = iuwfbt(c,wt) 
%
%   Input parameters:
%         c      : Coefficients stored in L xM matrix.
%         info,w : Transform parameters struct/Wavelet tree definition.
%
%   Output parameters:
%         f     : Reconstructed data.
%
%   f = IUWFBT(c,info) reconstructs signal f from the coefficients c 
%   using parameters from info struct. both returned by the UWFBT function.
%
%   f = IUWFBT(c,wt) reconstructs signal f from the wavelet coefficients 
%   c using the undecimated wavelet filterbank tree described by wt using
%   the "a-trous" algorithm.
%
%   Examples:
%   ---------
%   
%   A simple example showing perfect reconstruction using the "full decomposition" wavelet tree:
% 
%     f = greasy;
%     J = 6;
%     c = uwfbt(f,{'db8',J,'full'});
%     fhat = iuwfbt(c,{'db8',J,'full'});
%     % The following should give (almost) zero
%     norm(f-fhat)
%
%   See also:  uwfbt, plotwavelets
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/iuwfbt.php

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
   wt = wfbtinit({'dual',par.wt},par.fOrder);
else
   %% PARSE INPUT
   definput.import = {'wfbtcommon'};
   flags=ltfatarghelper({},definput,varargin);

   % Initialize the wavelet tree structure
   wt = wfbtinit(par,flags.forder);
end


%% ----- step 2 : Prepare input parameters
wtPath = nodesBForder(wt,'rev');
nodesUps = nodeFiltUps(wtPath,wt);
rangeLoc = rangeInLocalOutputs(wtPath,wt);
rangeOut = rangeInOutputs(wtPath,wt);
%% ----- step 3 : Run computation
f = comp_iuwfbt(c,wt.nodes(wtPath),nodesUps,rangeLoc,rangeOut);
