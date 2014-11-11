function angle = itd2angle(itd,ild,f_inst,tr)
% % % 20.july.12; slight update on the function call for polyfit
%
%   Url: http://amtoolbox.sourceforge.net/doc//general/itd2angle.php

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

load poly_lookup

unwrapped_itd = itd + round(0.4*sign(round(ild/2/(abs(tr)+1e-9)))-0.4*sign(itd))./f_inst;
angle = zeros(size(itd));

for n = 1:size(itd,2)
    %[p ,S ,MU]=polyfit(lookup.mitds(:,n),lookup.azi,9);
    angle(:,n)=polyval(p(:,n),unwrapped_itd(:,n),S{n} ,MU(:,n));
%   by calling the output S and MU, lookup.mitds is z-scored, thus improving the fitting
end
% neglect angles > 90°. WARNING => systematic underestimation for azi ~ 90°
angle(abs(angle)>90) = NaN;
%angle(abs(angle)>90) = 0*sign(angle(abs(angle)>90));
%disp('warning: temporary change')


