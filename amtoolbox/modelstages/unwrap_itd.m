function itd = unwrap_itd(itd,ild,f_inst)
%UNWRAP_ITD unwraps the given itd using the sign of the corresponding ild
%   Usage: itd = unwrap(itd,ild)
%
%   Input parameters:
%       itd    : itd to unwrap
%       ild    : corresponding ild value
%       f_inst : instantaneous frequency
%
%   Output parameters:
%       itd    : unwrapped itd
%
%   UNWRAP_ITD(itd,ild) unwraps the given itd using the sign of the
%   corresponding ild value. Unwrapping means, that the ild value is used to
%   decide to which direction the itd belongs, which can be unclear for
%   large values, because of the pi limit (see Dietz et al. 2011).
%
%   See also: dietz2011
%
%   References:
%     M. Dietz, S. D. Ewert, and V. Hohmann. Auditory model based direction
%     estimation of concurrent speakers from binaural signals. Speech
%     Communication, 53(5):592-605, 2011. [1]http ]
%     
%     References
%     
%     1. http://www.sciencedirect.com/science/article/pii/S016763931000097X
%     
%
%   Url: http://amtoolbox.sourceforge.net/doc//modelstages/unwrap_itd.php

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

% AUTHOR: Hagen Wierstorf

%% ===== Checking of input parameters ===================================
nargmin = 3;
nargmax = 3;
error(nargchk(nargmin,nargmax,nargin));
isargnumeric(itd,ild,f_inst);


%% ===== Configuration ==================================================
% Only apply the unwrap mechanism for ild greater than a threshold value,
% because for values near 0 it could be wrong.
ild_threshold = 2.5;


%% ===== Calculation ====================================================
itd = itd + ...
    round( ... % this will be -1,0,1
    0.4*sign(round(ild(:,1:12)/2 / (abs(ild_threshold)+1e-9))) ...
    -0.4*sign(itd)...
    ) ./ f_inst;

