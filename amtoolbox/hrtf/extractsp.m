function varargout = extractsp(lat,hM,pos)
%EXTRACTSP Sagittal plane (SP) HRTFs from measurement data
%   Usage:    [sphrtfs,polangs] = extractsp( lat,hM,pos )
%
%   Input parameters:
%     lat     : lateral angle of the SP
%     hM      : matrix containing head-related impulse responses.
%               Dimensions: time,position,channel 
%               (for more details see doc: HRTF format description)
%     pos     : source-position matrix referring to 2nd dimension of hM and 
%               formated acc. to meta.pos (ARI format).
%               6th col: lateral angle. 7th col: polar angle
%
%   Output parameters:
%     sphrtfs : all available HRTFs in the current SP, sorted acc. to
%               ascending polar angle
%     polangs : corresponding polar angles
%
%   EXTRACTSP(...) extracts all HRTFs available for a specific SP or
%   lateral angle. In order to result in a representative HRTF template,
%   demands are made on:
%
%     1) lateral tolerance to be as small as possible, but min. 2 and
%        max. 5.
% 
%     2) polar angles to range from max. -30° to min. 210°,
%
%     3) gaps of polar angles to be max. 30° large.
%
%   Url: http://amtoolbox.sourceforge.net/doc//hrtf/extractsp.php

% Copyright (C) 2009-2013 Peter L. Søndergaard and others.
% This file is part of AMToolbox version 0.9.1
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
    
% AUTHOR: Robert Baumgartner, Acoustics Research Institute, Vienna, Austria

dlat = 2;      % initial lateral tolerance (+/-) in deg
pol = [0,0];   % initial polar angles

while (min(pol) > -30 || max(pol) < 210 ... % ensure that important polar range is included
        || max(diff(pol))>30)...            % and gaps are <= 30°
        && dlat <= 5                        % but keep tolerance smaller than 5 deg

  idx=find(pos(:,6)>=-(dlat+0.01)/2+lat & pos(:,6)<=(dlat+0.01)/2+lat);
  [pol,polidx]=unique(real(pos(idx,7)));   % sorted polar angles
  sphrtfs=double(hM(:,idx,:));  % unsorted DTFs of SP
  sphrtfs=sphrtfs(:,polidx,:);          % sorted DTFs of SP

  dlat = dlat + 1;  % increase dlat

end

varargout{1} = sphrtfs;
if nargout == 2
  varargout{2} = pol;
end

end
