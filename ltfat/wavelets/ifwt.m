function f = ifwt(c,par,varargin)
%IFWT   Inverse Fast Wavelet Transform 
%   Usage:  f = ifwt(c,info)
%           f = ifwt(c,w,J,Ls)
%           f = ifwt(c,w,J,Ls,dim)
%
%   Input parameters:
%         c      : Wavelet coefficients.
%         info,w : Transform parameters struct/Wavelet filters definition.
%         J      : Number of filterbank iterations.
%         Ls     : Length of the reconstructed signal.
%         dim    : Dimension to along which to apply the transform.
%
%   Output parameters:
%         f     : Reconstructed data.
%
%   f = IFWT(c,info) reconstructs signal f from the wavelet coefficients
%   c using parameters from info struct. both returned by FWT
%   function.
%
%   f = IFWT(c,w,J,Ls) reconstructs signal f from the wavelet coefficients
%   c using J*-iteration synthesis filter bank build from the basic
%   filterbank defined by w. The Ls parameter is mandatory due to the 
%   ambiguity of lengths introduced by the subsampling operation and by
%   boundary treatment methods. Note that the same flag as in the FWT 
%   function have to be used, otherwise perfect reconstruction cannot be 
%   obtained.
%   
%   In both cases, the fast wavelet transform algorithm (Mallat's algorithm)
%   is employed. The format of c can be either packed, as returned by the
%   FWT function or cell-array as returned by WAVPACK2CELL function.
%
%   Please see the help on FWT for a detailed description of the parameters.
%
%   Examples:
%   ---------
%   
%   A simple example showing perfect reconstruction:
% 
%     f = gspi;
%     J = 8;
%     c = fwt(f,'db8',J);
%     fhat = ifwt(c,'db8',J,length(f));
%     % The following should give (almost) zero
%     norm(f-fhat)
%   
%   See also:  fwt, wavpack2cell, wavcell2pack
%
%   References:
%     S. Mallat. A wavelet tour of signal processing. Academic Press, San
%     Diego, CA, 1998.
%     
%     
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/ifwt.php

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

if  ~(iscell(c) || isnumeric(c)) || isempty(c)
  error('%s: Unrecognized coefficient format.',upper(mfilename));
end

if(isstruct(par)&&isfield(par,'fname'))
   if nargin>2
      error('%s: Too many input parameters.',upper(mfilename));
   end
   % process info struct
   w = fwtinit({'dual',par.wt});
   J = par.J;
   Lc = par.Lc;
   Ls = par.Ls;
   dim = par.dim;
   ext = par.ext;
else
   if nargin<4
      error('%s: Too few input parameters.',upper(mfilename));
   end;
   
   %% PARSE INPUT
   definput.import = {'fwt'};
   definput.keyvals.dim = [];
   definput.keyvals.Ls = [];
   definput.keyvals.J = [];
   [flags,~,J,Ls,dim]=ltfatarghelper({'J','Ls','dim'},definput,varargin);
 
   ext = flags.ext;
   %If dim is not specified use the first non-singleton dimension.
   if(isempty(dim))
      dim=find(size(c)>1,1);
   else
      if(~any(dim==[1,2]))
         error('%s: Parameter *dim* should be 1 or 2.',upper(mfilename));
      end
   end
   
   % Initialize the wavelet filters structure
   w = fwtinit(par);

   %% ----- Determine input data length.
   L = fwtlength(Ls,w,J,ext);

   %% ----- Determine number of ceoefficients in each subband
   Lc = fwtclength(L,w,J,ext);
end

   %% ----- Change c to correct shape according to the dim.
   if(isnumeric(c))
      %Check *Lc*
       if(sum(Lc)~=size(c,dim))
         error('%s: Coefficient subband lengths does not comply with parameter *Ls*.',upper(mfilename));
       end
      %Change format
      c =  wavpack2cell(c,Lc,dim);
   elseif(iscell(c))
      %Just check *Lc*
      if(~isequal(Lc,cellfun(@(x) size(x,1),c)))
         error('%s: Coefficient subband lengths does not comply with parameter *Ls*.',upper(mfilename));
      end
   else
      error('%s: Unrecognized coefficient format.',upper(mfilename)); 
   end;

   %% ----- Run computation 
   f = comp_ifwt(c,w.g,J,w.a,Ls,ext);

   %% ----- FINALIZE: Reshape back according to the dim.
   if(dim==2)
       f = f.';
   end
%END IFWT

