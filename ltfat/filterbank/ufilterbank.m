function c=ufilterbank(f,g,a,varargin);  
%UFILTERBANK   Apply Uniform filterbank
%   Usage:  c=ufilterbank(f,g,a);
%
%   UFILTERBANK(f,g,a) applies the filter given in g to the signal
%   f. Each subband will be subsampled by a factor of a (the
%   hop-size). If f is a matrix, the transformation is applied to each
%   column.
%
%   The filters g must be a cell-array, where each entry in the cell
%   array corresponds to an FIR filter.
%
%   If f is a single vector, then the output will be a matrix, where each
%   column in f is filtered by the corresponding filter in g. If f is
%   a matrix, the output will be 3-dimensional, and the third dimension will
%   correspond to the columns of the input signal.
%
%   The coefficients c computed from the signal f and the filterbank
%   with windows g_m are defined by
%
%                   L-1
%      c(n+1,m+1) = sum f(l+1) * g_m (an-l+1)
%                   l=0
%
%
%   See also: ifilterbank, filterbankdual
%
%   References:
%     H. Bölcskei, F. Hlawatsch, and H. G. Feichtinger. Frame-theoretic
%     analysis of oversampled filter banks. Signal Processing, IEEE
%     Transactions on, 46(12):3256-3268, 2002.
%     
%
%   Url: http://ltfat.sourceforge.net/doc/filterbank/ufilterbank.php

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

definput.import={'pfilt'};
definput.keyvals.L=[];
[flags,kv,L]=ltfatarghelper({'L'},definput,varargin);

[f,Ls,W,wasrow,remembershape]=comp_sigreshape_pre(f,'UFILTERBANK',0);

a=a(1);

if isempty(L)
  L=filterbanklength(Ls,a);
end;

[g,info]=filterbankwin(g,a,L,'normal');
M=info.M;
N=L/a;

f=postpad(f,L);

c=zeros(N,M,W,assert_classname(f));
g = comp_filterbank_pre(g,info.a,L,kv.crossover);

ctmp=comp_filterbank(f,g,info.a);


for m=1:M    
    c(:,m,:)=ctmp{m};
end;

