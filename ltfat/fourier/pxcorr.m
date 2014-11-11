function h=pxcorr(f,g,varargin)
%PXCORR  Periodic cross correlation
%   Usage:  h=pxcorr(f,g)
%
%   PXCORR(f,g) computes the periodix cross correlation of the input
%   signals f and g. The cross correlation is defined by
%
%               L-1
%      h(l+1) = sum f(k+1) * conj(g(k-l+1))
%               k=0
%
%   In the above formula, k-l is computed modulo L.
%
%   PXCORR(f,g,'normalize') does the same, but normalizes the output by
%   the product of the l^2-norm of f and g.
%
%   See also: dft, pfilt, involute
%
%   Url: http://ltfat.sourceforge.net/doc/fourier/pxcorr.php

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

%   AUTHOR: Peter L. Søndergaard

% Assert correct input.
if nargin<2
  error('%s: Too few input parameters.',upper(mfilename));
end;

if ~all(size(f)==size(g))
  error('f and g must have the same size.');
end;

definput.flags.type={'nonormalize','normalize'};

[flags,kv]=ltfatarghelper({},definput,varargin);

L=length(f);

if isreal(f) && isreal(g)
  h = ifftreal(fftreal(f).*conj(fftreal(g)),L);
else
  h = ifft(fft(f).*conj(fft(g)));
end;

if flags.do_normalize
  h = h/(norm(f)*norm(g));  
end;


