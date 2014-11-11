function outsig=comp_frsyn_fusion(F,insig)

W=size(insig,2);
L=size(insig,1)/framered(F);

outsig=zeros(L,W);

idx=0;    
for ii=1:F.Nframes
    coeflen=L*framered(F.frames{ii});
    outsig=outsig+frsyn(F.frames{ii},insig(idx+1:idx+coeflen,:))*F.w(ii);
    idx=idx+coeflen;
end;

%
%   Url: http://ltfat.sourceforge.net/doc/comp/comp_frsyn_fusion.php

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

