function [h,g,a,info] = wfilt_matlabwtwrapper(wname)
%WFILT_MATLABWTWRAPPER Wrapper of the Matlab Wavelet Toolbox wfilters function
%   Usage: [h,g,a] = wfilt_matlabwtwrapper(wname);
%
%   [h,g,a]=WFILT_MATLABWTWRAPPER(wname) calls Matlab Wavelet Toolbox
%   function wfilters and passes the parameter wname. This function
%   requires the Matlab Wavelet Toolbox.
%
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/wfilt_matlabwtwrapper.php

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

a = [2;2];
[lo,hi,lo_s,hi_s] = wfilters(wname);

h=cell(2,1);
h{1} = lo(:);
h{2} = hi(:);

g=cell(2,1);
g{1} = flipud(lo_s(:));
g{2} = flipud(hi_s(:));

if all(h{1}==g{1}) && all(h{2}==g{2})
  info.istight = 1;
else
  info.istight = 0; 
end


