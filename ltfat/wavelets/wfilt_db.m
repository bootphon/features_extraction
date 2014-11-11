function [h, g, a, info] = wfilt_db(N)
%WFILT_DB    Daubechies FIR filterbank
%   Usage:  [h,g] = wfilt_db(N);
%
%   Input parameters:
%         N     : Order of Daubechies filters. 
%   Output parameters:
%         H     : cell array of analysing filters impulse reponses
%         G     : cell array of synthetizing filters impulse reponses
%         a     : array of subsampling (or hop) factors accociated with
%                 corresponding filters
%
%   [H,G] = dbfilt(N) computes a two-channel Daubechies FIR filterbank
%   from prototype maximum-phase analysing lowpass filter obtained by
%   spectral factorization of the Lagrange interpolator filter.  N also
%   denotes the number of zeros at z=-1 of the lowpass filters of length
%   2N.  The prototype lowpass filter has the following form (all roots of
%   R(z) are outside of the unit circle):
%                 
%      H_l(z)=(1+z^-1)^N*R(z),
%
%   where R(z) is a spectral factor of the Legrange interpolator P(z)=2R(z)*R(z^{-1})
%   All subsequent filters of the two-channel filterbank are derived as
%   follows:
%
%      H_h(z)=H_l((-z)^-1)
%      G_l(z)=H_l(z^-1)
%      G_h(z)=-H_l(-z)
%
%   making them an orthogonal causal perfect-reconstruction QMF.
%
%   Examples:
%   ---------
%   :
%
%     wfiltinfo('db8');
%
%   References:
%     I. Daubechies. Ten Lectures on Wavelets. Society for Industrial and
%     Applied Mathematics, Philadelphia, PA, USA, 1992.
%     
%     
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfilt_db.php

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


if(nargin<1)
   error('%s: Too few input parameters.',upper(mfilename));
end

if(N<1)
    error('Minimum N is 1.');
end
if(N>20)
    warning('Instability may occur when N is too large.');
end


h = cell(2,1);
flen = 2*N;

% Calculating Lagrange interpolator coefficients
sup = [-N+1:N];
a = zeros(1,N);
for n = 1:N
    non  = sup(sup ~= n);
    a(n) = prod(0.5-non)/prod(n-non);
end
P = zeros(1,2*N-1);
P(1:2:end) = a;
P = [P(end:-1:1),1,P];

R = roots(P);
R = R(abs(R)<1 & real(R)>0);

% roots of the 2*conv(lo_a,lo_r) filter
hroots = [R(:);-ones(N,1)];


% building synthetizing low-pass filter from roots
h{1}= real(poly(hroots));
% normalize
h{1}= h{1}/norm(h{1});
% QMF modulation low-pass -> highpass
h{2}= (-1).^(1:flen).*h{1}(end:-1:1);

g=h;
a = [2;2];
info.istight=1;








