function [c,info]=wpfbt(f,wt,varargin)
%WPFBT   Wavelet Packet FilterBank Tree
%   Usage:  c=wpfbt(f,wt);
%           [c,info]=wpfbt(...);
%
%   Input parameters:
%         f   : Input data.
%         wt  : Wavelet Filterbank tree definition.
%
%   Output parameters:
%         c    : Coefficients stored in a cell-array.
%         info : Transform parameters struct.
%
%   c=WPFBT(f,wt) returns wavelet packet coefficients c obtained by
%   applying a wavelet filterbank tree defined by wt to the input data
%   f. In addition, the function returns struct. info containing transform
%   parameters. It can be conviniently used for the inverse transform IWPFBT
%   e.g. fhat = iWPFBT(c,info). It is also required by the PLOTWAVELETS 
%   function.
%
%   In contrast to the WFBT, the c contain every intermediate output
%   of each node in the tree. The c{jj} are ordered in the breadth-first
%   node order manner.
%
%   If f is row/column vector, the coefficient vectors c{jj} are 
%   columns. If f is a matrix, the transformation is applied to each of 
%   column of the matrix.
%
%   By default, the function scales all intermediate outputs in the tree
%   which are not terminal ones in order to preserve energy. Passing a 
%   'noscale' flag overrides this behavior. This is necessary for the
%   WPBEST function to correctly work with the cost measures.
%
%   Please see help for WFBT description of possible formats of wt and
%   of the additional flags.
%
%
%   Examples:
%   ---------
%   
%   A simple example of calling the WPFBT function using the "full
%   decomposition" wavelet tree:
% 
%     f = gspi;
%     J = 6;
%     [c,info] = wpfbt(f,{'sym10',J,'full'});
%     plotwavelets(c,info,44100,'dynrange',90);
%
%   See also: wfbt, iwpfbt, wfbtinit, plotwavelets, wpbest
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wpfbt.php

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


if(nargin<2)
   error('%s: Too few input parameters.',upper(mfilename));  
end

definput.import = {'fwt','wfbtcommon'};
definput.flags.scale = {'scale','noscale'};
[flags,kv]=ltfatarghelper({},definput,varargin);

% Initialize the wavelet tree structure
wt = wfbtinit(wt,flags.forder);
    
%% ----- step 1 : Verify f and determine its length -------
[f,Ls]=comp_sigreshape_pre(f,upper(mfilename),0);
if(Ls<2)
   error('%s: Input signal seems not to be a vector of length > 1.',upper(mfilename));  
end

% Determine next legal input data length.
L = wfbtlength(Ls,wt,flags.ext);

% Pad with zeros if the safe length L differ from the Ls.
if(Ls~=L)
   f=postpad(f,L); 
end

%% ----- step 3 : Run computation
wtPath = nodesBForder(wt);
rangeLoc = rangeInLocalOutputs(wtPath,wt);
c = comp_wpfbt(f,wt.nodes(wtPath),rangeLoc,flags.ext,flags.do_scale);

%% ----- Optional : Fill the info struct. -----
if nargout>1
   info.fname = 'wpfbt';
   info.wt = wt;
   info.ext = flags.ext;
   info.Lc = cellfun(@(cEl) size(cEl,1),c);
   info.Ls = Ls;
   info.fOrder = flags.forder;
   info.isPacked = 0;
   info.isNotScaled = ~flags.do_scale;
end

