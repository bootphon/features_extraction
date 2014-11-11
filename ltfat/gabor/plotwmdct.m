function plotwmdct(coef,varargin)
%PLOTWMDCT  Plot WMDCT coefficients
%   Usage: plotwmdct(coef);
%          plotwmdct(coef,fs);
%          plotwmdct(coef,fs,dynrange);
%
%   PLOTWMDCT(coef) plots coefficients from WMDCT.
%
%   PLOTWMDCT(coef,fs) does the same assuming a sampling rate of
%   fs Hz of the original signal.
%
%   PLOTWMDCT(coef,fs,dynrange) additionally limits the dynamic
%   range.
%   
%   PLOTWMDCT supports all the optional parameters of TFPLOT. Please
%   see the help of TFPLOT for an exhaustive list.
%
%   See also:  wmdct, tfplot, sgram, plotdgt
%
%   Url: http://ltfat.sourceforge.net/doc/gabor/plotwmdct.php

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

%   AUTHOR : Peter L. Søndergaard.
%   TESTING: NA
%   REFERENCE: NA

if nargin<1
  error('%s: Too few input parameters.',upper(mfilename));
end;

definput.import={'ltfattranslate','tfplot'};

[flags,kv,fs]=ltfatarghelper({'fs','dynrange'},definput,varargin);

M=size(coef,1);

yr=[.5/M, 1-.5/M];

tfplot(coef,M,yr,'argimport',flags,kv);


