function c=comp_dgtreal(f,g,a,M,lt,phasetype)
%COMP_DGTREAL  Compute a DGTREAL
%   Usage:  c=comp_dgt_real(f,g,a,M,lt,phasetype);
%
%   Input parameters:
%         f     : Input data
%         g     : Window function.
%         a     : Length of time shift.
%         M     : Number of modulations.
%         L     : Length of transform to do.
%   Output parameters:
%         c     : M*N array of coefficients.
%
%
%   Url: http://ltfat.sourceforge.net/doc/comp/comp_dgtreal.php

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

L=size(f,1);
Lwindow=size(g,1);

W=size(f,2);
N=L/a;

M2=floor(M/2)+1;

if lt(2)==1
    if Lwindow<L
        % Do the filter bank algorithm
        % Periodic boundary conditions
        c=comp_dgtreal_fb(f,g,a,M);
        c=reshape(c,M2,N,W);
        
    else
        % Do the factorization algorithm 
        c=comp_dgtreal_long(f,g,a,M);
        c=reshape(c,M2,N,W);

    end;
    
    
    if phasetype==1
        
        TimeInd = (0:(N-1))*a;
        FreqInd = (0:(M2-1))/M;
        
        phase = FreqInd'*TimeInd;
        phase = exp(2*i*pi*phase);
    
        c=bsxfun(@times,c,phase);
        
    end;
    
else
    % Quinqux lattice
    c=comp_nonsepdgtreal_quinqux(f,g,a,M);            
end;





