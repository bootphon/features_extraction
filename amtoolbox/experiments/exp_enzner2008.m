function exp_enzner2008(varargin) 
%EXP_ENZNER2008  Creates figures like [Enzner2008, Fig. 2], [Enzner2009, Fig. 4]
%   Usage: exp_enzner2008(flag)
%
%   The following flags can be specified:
%
%     'fig2_2008'   Plot Fig. 2 from Enzner et al. (2008)
%
%     'fig4_2009'   Plot Fig. 4 from Enzner et al. (2009)
%
%   Examples:
%   ---------
%
%   To display Figure 2 from the 2008 paper use :
%
%     exp_enzner2008('fig2_2008');
%
%   To display Figure 4 from the 2009 paper use :
%
%     exp_enzner2008('fig4_2009');
%
%   See also: enzner2008
%
%   Url: http://amtoolbox.sourceforge.net/doc//experiments/exp_enzner2008.php

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

  
%  Authors: Michael Weinert (Michael.Weinert@rub.de), Gerald Enzner (Gerald.Enzner@rub.de)
%  Date: 21-01-2013

%addpath(fullfile(amtbasepath,'hrtf','continuous-azimuth HRIR'))

enzner2008(1,1,varargin); % enzner2008(mu, delta_phi, varargin)



