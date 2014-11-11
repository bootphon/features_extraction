function [h,g,a] = wfilt_dtree(N)
%WFILT_DTREE  Dual-TREE complex wavelet transform filters
%   Usage: [h,g,a] = wfilt_dtree(N);
%
%   [h,g,a]=WFILT_DTREE(N) computes filters used in the dual-tree complex wavelet transform. 
%
%   References:
%     I. Selesnick, R. Baraniuk, and N. Kingsbury. The dual-tree complex
%     wavelet transform. Signal Processing Magazine, IEEE, 22(6):123 - 151,
%     nov. 2005.
%     
%
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfilt_dtree.php

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


switch(N)
case 1
harr = [
                  0                  0
  -0.08838834764832  -0.01122679215254
   0.08838834764832   0.01122679215254
   0.69587998903400   0.08838834764832
   0.69587998903400   0.08838834764832
   0.08838834764832  -0.69587998903400
  -0.08838834764832   0.69587998903400
   0.01122679215254  -0.08838834764832
   0.01122679215254  -0.08838834764832
                  0                  0
];
case 2
harr = [
  0.01122679215254                   0
   0.01122679215254                  0
  -0.08838834764832  -0.08838834764832
   0.08838834764832  -0.08838834764832
   0.69587998903400   0.69587998903400
   0.69587998903400  -0.69587998903400
   0.08838834764832   0.08838834764832
  -0.08838834764832   0.08838834764832
                  0   0.01122679215254
                  0  -0.01122679215254
];

case 3
harr = [
   0.03516384000000                  0
                  0                  0
  -0.08832942000000  -0.11430184000000
   0.23389032000000                  0
   0.76027237000000   0.58751830000000
   0.58751830000000  -0.76027237000000
                  0   0.23389032000000
  -0.11430184000000   0.08832942000000
                  0                  0
                  0  -0.03516384000000
];

case 4
harr = [
                  0  -0.03516384000000
                  0                  0
  -0.11430184000000   0.08832942000000
                  0   0.23389032000000
   0.58751830000000  -0.76027237000000
   0.76027237000000   0.58751830000000
   0.23389032000000                  0
  -0.08832942000000  -0.11430184000000
                  0                  0
   0.03516384000000                  0
];

 otherwise
        error('%s: No such Dual-Tree Complex Wavelet Transform Filters..',upper(mfilename));
end
a= [2;2];

h=mat2cell(harr.',[1,1],length(harr));

if(nargout>1)
    garr = harr(end:-1:1, :);
    g=mat2cell(garr.',[1,1],length(garr));
end
