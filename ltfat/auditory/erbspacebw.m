function [y,n] = erbspacebw(flow,fhigh,varargin)
%ERBSPACEBW  Erbscale points specified by bandwidth
%   Usage: y=erbspacebw(flow,fhigh,bw,hitme);
%          y=erbspacebw(flow,fhigh,bw);
%          y=erbspacebw(flow,fhigh);
%
%   This is a wrapper around AUDSPACEBW that selects the erb-scale. Please
%   see the help on AUDSPACEBW for more information.
%
%   See also: audspacebw, freqtoaud
%
%   Url: http://ltfat.sourceforge.net/doc/auditory/erbspacebw.php

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
  
[y,n] = audspacebw(flow,fhigh,varargin{:},'erb');

