function [detect,prob] = caspmdecide(mu,in_var,rule,numint)
%
%   Url: http://amtoolbox.sourceforge.net/doc//modelstages/caspmdecide.php

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

% Y = 1, signal is detected

if in_var <= 0
    error('CASP:mdecide', 'in_var must be > 0');
end

if rule(1) > 1
    error('CASP:mdecide','Only x-down, 1-up procedure have been implemented')
end


switch numint
    case 2
        prob = 1 - (erfc((((mu/in_var)*0.707) - 0)     * sqrt(2)/2) / 2);
    case 3
        prob = 1 - (erfc((((mu/in_var)*0.765) - 0.423) * sqrt(2)/2) / 2);
    case 4
        prob = 1 - (erfc((((mu/in_var)*0.810) - 0.668) * sqrt(2)/2) / 2);
    otherwise
        error('CASP:mdecide', 'Only 2-, 3- and 4-AFC procedures are implemented');
end;

if rule(1) == 1 && max(prob) > (1 / (2 .^ (1/rule(2))))
    detect = 1; % signal heard
else
    detect = 0; % no signal heard
end;

%OLDFORMAT

