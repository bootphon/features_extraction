clf;
hold all;

L=30;
dr=110;
magresp(firwin('hanning',L,'1'),'fir','dynrange',dr);
magresp(firwin('hamming',L,'1'),'fir','dynrange',dr);
magresp(firwin('blackman',L,'1'),'fir','dynrange',dr);
magresp(firwin('nuttall',L,'1'),'fir','dynrange',dr);
magresp(firwin('itersine',L,'1'),'fir','dynrange',dr);

legend('Hann','Hamming','Blackman','Nuttall','Itersine');


%
%   Url: http://ltfat.sourceforge.net/doc/demos/demo_firwin.php

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

