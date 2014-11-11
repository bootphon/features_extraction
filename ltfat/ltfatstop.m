function ltfatstop()
%LTFATSTOP   Stops the LTFAT toolbox
%   Usage:  ltfatstop;
%
%   LTFATSTOP removes all LTFAT subdirectories from the path.
%
%   See also:  ltfatstart
%
%   Url: http://ltfat.sourceforge.net/doc/ltfatstop.php

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

fullpath=strsplit(path,pathsep);

bp=ltfatbasepath;
% Remove the file separator at the end
bp=bp(1:end-1);
bplen=numel(bp);

for line=fullpath
    % Strip the cell array container away
    thispath=line{1};
    if numel(thispath)>=bplen && strcmp(thispath(1:bplen),bp)
        rmpath(thispath);
        disp(thispath)
    end;    
end;
    
  

