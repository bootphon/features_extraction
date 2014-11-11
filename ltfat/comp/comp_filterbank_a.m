function [a,info]=comp_filterbank_a(a,M,info)
%COMP_FILTERBANK_A  Return sanitized a
%   Usage:  [a,info]=comp_filterbank_a(a,M);
%   
%   [a,info]=COMP_FILTERBANK_A(a,M) returns a sanitized version of a*
%   expand to a Mx2 matrix, and update the information in info.
%
%   Url: http://ltfat.sourceforge.net/doc/comp/comp_filterbank_a.php

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

% FIXME: Not sufficiently safe in the case where there is only one
% channel, or when attempting to specify a uniform filterbank with
% fractional downsampling.
%


info.isfractional=0;
info.isuniform=0;

if M==1 && size(a,1)~=1
   error('%s: One channel, but more a.',upper(mfilename));
end

if M==1 && size(a,2)<2
   a = [a,1];
end

if isvector(a) && M~=1
    [a,~]=scalardistribute(a(:),ones(M,1));
    
    if all(a==a(1))
        info.isuniform=1;
    end;

    a=[a,ones(M,1)];
else
    % We need to check against the case where this routine has already
    % been run
    if isequal(a(:,2),ones(M,1))
        if all(a(:,1)==a(1))
            info.isuniform=1;
        end;        
    else
        info.isfractional=1;
    end;
    
    % If the filterbank uses fractional downsampling, it cannot be
    % treated by the uniform algorithms, even though the sampling rate is uniform.
    
    % FIXME: Fractional, uniform filterbanks are not handled, they are
    % not allowed.

end;

info.a=a;


