function H=comp_transferfunction(g,L)
%COMP_TRANSFERFUNCTION  Compute the transfer function
%
%   Url: http://ltfat.sourceforge.net/doc/comp/comp_transferfunction.php

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

l=(0:L-1).'/L;
if isfield(g,'h')
    
    % This is not safe when already having imp. resp. of length L
    % with zero delay (periodically wrapped).
 
    g_time=circshift(postpad(g.h,L),g.offset);

    
    if isfield(g,'fc')
       g_time = g_time.*exp(2*pi*1i*round(g.fc*L/2)*l);
    end
        
    if isfield(g,'realonly') && g.realonly
        g_time=real(g_time);
    end;
        
    H=fft(g_time);
    
elseif isfield(g,'H')
    if ~isnumeric(g.H)
        g.H=g.H(L);
        g.foff=g.foff(L);
    end;
    
    H=circshift(postpad(g.H,L),g.foff);
    
    if isfield(g,'delay')
       H = H.*exp(-2*pi*1i*round(g.delay)*l);
    end
    
    if isfield(g,'realonly') && g.realonly
        H=(H+involute(H))/2;
    end;
else
    error('%s: Unrecognized filter format. The struct should have either .h or .H field.',upper(mfilename));    
end;

