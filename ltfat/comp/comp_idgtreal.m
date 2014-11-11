function f=comp_idgtreal(coef,g,a,M,lt,phasetype)
%COMP_IDGTREAL  Compute IDGTREAL
%   Usage:  f=comp_idgtreal(c,g,a,M,lt,phasetype);
%
%   Input parameters:
%         c     : Array of coefficients.
%         g     : Window function.
%         a     : Length of time shift.
%         M     : Number of modulations.
%         lt    : lattice type
%   Output parameters:
%         f     : Signal.
%
%
%   Url: http://ltfat.sourceforge.net/doc/comp/comp_idgtreal.php

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
%   TESTING: TEST_DGT
%   REFERENCE: OK

N=size(coef,2);
L=N*a;
b=L/M;

Lwindow=size(g,1);
W=size(coef,3);
M2=floor(M/2)+1;
M2short=ceil(M/2);

if lt(2)==1
    if phasetype==1
        TimeInd = (0:(N-1))/N;
        FreqInd = (0:(M2-1))*b;
        
        phase = FreqInd'*TimeInd;
        phase = exp(-2*i*pi*phase);
        
        % Handle multisignals
        coef = bsxfun(@times,coef,phase);
        
    end;
    
    if L==Lwindow
        % Do full-window algorithm.
        
        % Get the factorization of the window.
        gf = comp_wfac(g,a,M);      
        
        % Call the computational subroutine.
        % FIXME old input format
        f = comp_idgtreal_fac(reshape(coef,M2,N*W),gf,L,a,M);
        
    else
        % Do filter bank algorithm.
        % Call the computational subroutine.
        % FIXME old input format
        f = comp_idgtreal_fb(reshape(coef,M2,N*W),g,L,a,M);
    end;
    
else
    % Quinqux lattice
    f=comp_inonsepdgtreal_quinqux(coef,g,a,M);
    
end;


