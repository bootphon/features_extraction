function f = iwtfft(c,G,a,varargin)
%IWTFFT  Inverse Wavelet Transform in the frequency-domain
%   Usage: f=iwtfft(c,G,a);
%          f=iwtfft(c,G,a,...);
%
%   IWTFFT(c,G,a) computes XXX.
%
%   See also: wtfft
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/iwtfft.php

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


%% PARSE INPUT
definput.keyvals.L=[];    
definput.import = {'wtfft'};

[flags,kv,Ls]=ltfatarghelper({'L'},definput,varargin);

[Gr,Gc] = size(G);

if(isempty(a))
    a = ones(Gc,1);
end

L=filterbanklengthcoef(c,a);
[sigHalfLen,W] = size(c{end});
f = zeros(L,W);


N = zeros(Gc,1);
for gg=1:Gc
    N(gg)=size(c{gg},1);
end


for w=1:W
   for gg=1:Gc
      f(:,w)=f(:,w)+ifft(repmat(fft(c{gg}(:,w)),a(gg),1).*G(:,gg));
   end
end

if ~isempty(Ls)
  f=postpad(f,Ls);
else
  Ls=L;
end;


