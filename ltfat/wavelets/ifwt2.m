function f = ifwt2(c,w,J,varargin)
%IFWT2   Inverse Fast Wavelet Transform 
%   Usage:  f = ifwt2(c,w,J)
%           f = ifwt2(c,w,J,Ls,...)
%
%   Input parameters:
%         c     : Coefficients stored in a matrix.
%         g     : Wavelet filters definition.
%         J     : Number of filterbank iterations.
%         Ls    : Length of the reconstructed signal.
%
%   Output parameters:
%         f     : Reconstructed data.
%
%   f = IFWT2(c,g,J) reconstructs signal f from the wavelet coefficients
%   c using a J*-iteration synthesis filter bank build from the basic synthesis
%   filterbank defined by g.
%
%   This function takes the same optional parameters as FWT2. Please see
%   the help on FWT2 for a description of the parameters.
%   
%   See also:  fwt2, fwtinit
%
%   Demos: demo_imagecompression
%
%   References:
%     S. Mallat. A wavelet tour of signal processing. Academic Press, San
%     Diego, CA, 1998.
%     
%     
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/ifwt2.php

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

if ~isnumeric(c)
  error('%s: Unrecognized coefficient format.',upper(mfilename));
end;

% Initialize the wavelet filters structure
w = fwtinit(w);


%% PARSE INPUT
definput.keyvals.Ls=[];    
definput.import = {'fwt','fwt2'};
[flags,kv,Ls]=ltfatarghelper({'Ls'},definput,varargin);

if (isempty(Ls))
   Ls = size(c);
end

if (numel(Ls)==1)
  Ls = [Ls,Ls];
end

Lcrows = fwtclength(Ls(1),w,J,'per');
Lccols = fwtclength(Ls(2),w,J,'per');
nFilts = numel(w.g);

if flags.do_standard
  Jstep = 1;
  for jj=1:J-1
    LcIdx =  jj*(nFilts-1)+2;
    colRange = 1:Lcrows(LcIdx);
    rowRange = 1:Lccols(LcIdx);
    c(colRange,rowRange) = ifwt(c(colRange,rowRange),w,Jstep,Lcrows(LcIdx),'dim',1,'per');
    c(colRange,rowRange) = ifwt(c(colRange,rowRange),w,Jstep,Lccols(LcIdx),'dim',2,'per');
  end

  c = ifwt(c,w,Jstep,Ls(1),'dim',1,'per');
  f = ifwt(c,w,Jstep,Ls(2),'dim',2,'per');
  
end;

if flags.do_tensor
  f = ifwt(c,w,J,Ls(1),'dim',1,'per');
  f = ifwt(f,w,J,Ls(2),'dim',2,'per');
end;

%% ----- step 1 : Run calc -----------
% filtNo = length(g.filts);
% subbNo = filtNo^2-1;
% cTmp = cell(filtNo,1);
% cTmp{1} = c{1};
% cJidxTmp = 2;
% for jj=1:J
%    if (jj==J)
%      cLs = Ls;
%    else
%      cLs = size(c{cJidxTmp+subbNo});  
%    end
% 
%    cTmp{1} = comp_ifwt_all({cTmp{1},c{cJidxTmp:cJidxTmp+filtNo-1}},g.filts,1,g.a,cLs(1),'dec',flags.ext).'; 
%    cJidxTmp = cJidxTmp+filtNo-1;
%    for cc= 2:filtNo
%       cTmp{cc} = comp_ifwt_all({c{cJidxTmp:cJidxTmp+filtNo-1}},g.filts,1,g.a,cLs(1),'dec',flags.ext).';
%       cJidxTmp = cJidxTmp+filtNo;
%    end
%    
%    cTmp{1} = comp_ifwt_all(cTmp,g.filts,1,g.a,cLs(2),'dec',flags.ext).';  
% end
% f = cTmp{1};








