function f=idft(f,N,dim)
%IDFT  Inverse DFT
%   Usage: f=idft(f);
%          f=idft(f,N,dim);
%
%   This function computes a normalized inverse discrete Fourier transform.
%   This is nothing but a scaled version of the output from ifft. The
%   function takes exactly the same arguments as ifft. See the help on ifft
%   for a thorough description.
%
%   See also:  dft
%
%   Url: http://ltfat.sourceforge.net/doc/fourier/idft.php

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

%   AUTHOR : Peter L. Søndergaard
%   TESTING: OK
%   REFERENCE: OK

error(nargchk(1,3,nargin));

if nargin<3
  dim=[];  
end;

if nargin<2
  N=[];
end;

[f,N,Ls,W,dim,permutedsize,order]=assert_sigreshape_pre(f,N,dim,'IDFT');

% Force IFFT along dimension 1, since we have permuted the dimensions
% manually
f=ifft(f,N,1)*sqrt(N);

f=assert_sigreshape_post(f,dim,permutedsize,order);


