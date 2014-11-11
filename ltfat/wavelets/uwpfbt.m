function [c,info]=uwpfbt(f,wt,varargin)
%UWPFBT Undecimated Wavelet Packet FilterBank Tree
%   Usage:  c=uwpfbt(f,wt);
%           c=uwpfbt(f,wt,...);
%
%   Input parameters:
%         f   : Input data.
%         wt  : Wavelet Filterbank tree
%
%   Output parameters:
%         c   : Coefficients in a L xM matrix.
%
%   c=UWPFBT(f,wt) returns coefficients c obtained by applying the 
%   undecimated wavelet filterbank tree defined by wt to the input data 
%   f using the "a-trous" algorithm. Number of columns in c (*M*) is 
%   defined by the total number of outputs of each node. The outputs c(:,jj)
%   are ordered in the breadth-first node order manner.
%   In addition, the function returns struct. info containing the transform
%   parameters. It can be conviniently used for the inverse transform IUWPFBT
%   e.g. fhat = iUWPFBT(c,info). It is also required by the PLOTWAVELETS
%   function.
%
%   If f is a matrix, the transformation is applied to each of W columns
%   and the coefficients in c are stacked along the third dimension.
%
%   Please see help for WFBT description of possible formats of wt.
%
%   Examples:
%   ---------
%   
%   A simple example of calling the UWPFBT function using the "full
%   decomposition" wavelet tree:
% 
%     f = greasy;
%     J = 6;
%     [c,info] = uwpfbt(f,{'db10',J,'full'});
%     plotwavelets(c,info,44100,'dynrange',90);
%
%   See also: iuwpfbt, wfbtinit
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/uwpfbt.php

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

definput.import = {'wfbtcommon'};
[flags,kv]=ltfatarghelper({},definput,varargin);

% Initialize the wavelet tree structure
wt = wfbtinit(wt,flags.forder);
    
%% ----- step 1 : Verify f and determine its length -------
[f,Ls]=comp_sigreshape_pre(f,upper(mfilename),0);
if(Ls<2)
   error('%s: Input signal seems not to be a vector of length > 1.',upper(mfilename));  
end

%% ----- step 3 : Run computation
wtPath = nodesBForder(wt);
nodesUps = nodeFiltUps(wtPath,wt);
rangeLoc = rangeInLocalOutputs(wtPath,wt);
c = comp_uwpfbt(f,wt.nodes(wtPath),rangeLoc,nodesUps);

%% ----- Optional : Fill the info struct. -----
if nargout>1
   info.fname = 'uwpfbt';
   info.wt = wt;
   info.fOrder = flags.forder;
   info.isPacked = 0;
end
