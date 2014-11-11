function [c,info] = ufwt(f,w,J,varargin)
%UFWT  Undecimated Fast Wavelet Transform 
%   Usage:  c = ufwt(f,w,J);
%           [c,info] = ufwt(...);
%
%   Input parameters:
%         f     : Input data.
%         w     : Wavelet Filterbank. 
%         J     : Number of filterbank iterations.
%
%   Output parameters:
%         c     : Coefficients stored in L xJ+1 matrix.
%         info  : Transform paramaters struct.
%
%   c=UFWT(f,w,J) computes redundant time (or shift) invariant
%   wavelet representation c of the input signal f using the "a-trous"
%   algorithm. In addition, the function returns struct. info containing
%   the transform parameters. It can be conviniently used for the inverse 
%   transform IUFWT e.g. fhat = iUFWT(c,info). It is also required by 
%   the PLOTWAVELETS function.
%
%   The coefficents c are so called Undecimated Discrete Wavelet transform
%   of the input signal f, if w defines two-channel wavelet filterbank.
%   Other names for this version of the wavelet transform are: the
%   time-invariant wavelet transform, the stationary wavelet transform, 
%   maximal overlap discrete wavelet transform or even the "continuous" 
%   wavelet transform (as the time step is one sample). However, the 
%   function accepts any number filters (referred to as M) in the basic 
%   wavelet filterbank and the number of columns of c is then J(M-1)+1.
%
%   For all accepted formats of the parameter w see the FWT function.
%
%   For one-dimensional input f of length L, the coefficients c are 
%   stored as columns of a matrix. The columns are ordered with inceasing
%   central frequency of the corresponding effective filter frequency 
%   response or equivalently with decreasing wavelet scale.
%
%   If the input f is L xW matrix, the transform is applied 
%   to each column and the outputs are stacked along third dimension in the
%   L xJ(M-1)+1 xW data cube.
%
%   Boundary handling:
%   ------------------
%
%   c=UFWT(f,w,J) uses periodic boundary extension. The extensions are 
%   done internally at each level of the transform, rather than doing the 
%   prior explicit padding.
%
%   Examples:
%   ---------
%   
%   A simple example of calling the UFWT function:
% 
%     f = greasy;
%     J = 8;
%     [c,info] = ufwt(f,'db8',J);
%     %plotwavelets(c,info,16000,'dynrange',90);
%
%   See also: iufwt, plotwavelets
%
%   References:
%     M. Holschneider, R. Kronland-Martinet, J. Morlet, and P. Tchamitchian.
%     A real-time algorithm for signal analysis with the help of the wavelet
%     transform. In Wavelets. Time-Frequency Methods and Phase Space,
%     volume 1, page 286, 1989.
%     
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/ufwt.php

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

if nargin<3
  error('%s: Too few input parameters.',upper(mfilename));
end;

if ~isnumeric(J) || ~isscalar(J)
  error('%s: "J" must be a scalar.',upper(mfilename));
end;

if(J<1 && rem(a,1)~=0)
   error('%s: J must be a positive integer.',upper(mfilename)); 
end

% Initialize the wavelet filters structure
w = fwtinit(w);

%% ----- step 1 : Verify f and determine its length -------
% Change f to correct shape.
[f,Ls]=comp_sigreshape_pre(f,upper(mfilename),0);
if(Ls<2)
   error('%s: Input signal seems not to be a vector of length > 1.',upper(mfilename));  
end
 
%% ----- step 2 : Run computation
 c = comp_ufwt(f,w.h,J,w.a);
 
%% ----- Optionally : Fill info struct ----
if nargout>1
   info.fname = 'ufwt';
   info.wt = w;
   info.J = J;
end



