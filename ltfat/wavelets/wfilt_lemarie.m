function [h,g,a,info]=wfilt_lemarie(N)
%WFILT_LEMARIE  Battle and Lemarie filters
%   Usage: [h,g,a]=wfilt_lemarie(N)
%
%   Input parameters:
%         N     : Filter length.
%
%   [h,g,a]=WFILT_LEMARIE(N) calculates coeficients of orthonormal
%   Battle-Lemarie wavelets. Filter coefficients are obtainded by
%   frequency domain sampling and trunctating the impulse response.
%
%   Examples:
%   ---------
%   :
%
%     wfiltinfo('lemarie50');
%
%   References:
%     S. G. Mallat. A theory for multiresolution signal decomposition: The
%     wavelet representation. IEEE Trans. Pattern Anal. Mach. Intell.,
%     11(7):674-693, July 1989. [1]http ]
%     
%     References
%     
%     1. http://dx.doi.org/10.1109/34.192463
%     
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfilt_lemarie.php

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

% Original copyright goes to:
% Copyright (C) 1994, 1995, 1996, by Universidad de Vigo 
% Author: Jose Martin Garcia
% e-mail: Uvi_Wave@tsc.uvigo.es

num_coefs = N;
L = 1024;
H = wfreq_lemarie(L);
hh=real(ifft(H{1},L));
hh=[ hh(L-floor(num_coefs/2)+1:L) hh(1:ceil(num_coefs/2))];
hh=hh/norm(hh);

g{1} = fliplr(hh);
g{2} = -(-1).^(1:length(hh)).*g{1}(end:-1:1);
 
h=g;

a= [2;2];
info.istight = 1;





