function f=comp_idgt(coef,g,a,lt,phasetype,algns)
%COMP_IDGT  Compute IDGT
%   Usage:  f=comp_idgt(c,g,a,lt,phasetype);
%
%   Input parameters:
%         c     : Array of coefficients.
%         g     : Window function.
%         a     : Length of time shift.
%         lt    : Lattice type
%         phasetype : Type of phase
%   Output parameters:
%         f     : Signal.
%
%   Value of the algorithm chooser
%
%      algns=0 : Choose the fastest algorithm
%
%      algns=0 : Always choose multi-win
%
%      algns=1 : Always choose shear
%
%   Url: http://ltfat.sourceforge.net/doc/comp/comp_idgt.php

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

% AUTHOR : Peter L. Søndergaard.

M=size(coef,1);
N=size(coef,2);
W=size(coef,3);

L=N*a;

Lwindow=size(g,1);

% FIXME : Calls non-comp function 
if phasetype==1
    coef=phaseunlock(coef,a,'lt',lt);
end;

if lt(2)==1

    if L==Lwindow
        % Do full-window algorithm.
        
        % Get the factorization of the window.
        gf = comp_wfac(g,a,M);      

        % FIXME: This line is necessary because the mex and oct interfaces expect
        % a matrix as input.
        coef=reshape(coef,M,prod(size(coef))/M);

        
        % Call the computational subroutine.
        f  = comp_idgt_fac(coef,gf,L,a,M);
        
    else
        
        % Do filter bank algorithm.
        % Call the computational subroutine.

        % FIXME: This line is necessary because the mex and oct interfaces expect
        % a matrix as input.
        coef=reshape(coef,M,prod(size(coef))/M);

        f=comp_idgt_fb(coef,g,L,a,M);
        
    end;
    
else
    
    if (algns==1) || (algns==0 && lt(2)<=2) 
        
        % ----- algorithm starts here, split into sub-lattices ---------------
        
        mwin=comp_nonsepwin2multi(g,a,M,lt,L);
        
        % phase factor correction (backwards), for more information see 
        % analysis routine
        
        E = exp(2*pi*i*a*kron(0:N/lt(2)-1,ones(1,lt(2))).*...
                rem(kron(ones(1,N/lt(2)), 0:lt(2)-1)*lt(1),lt(2))/M);

        coef=bsxfun(@times,coef,E);
        
        % simple algorithm: split into sublattices and add the result from each
        % sublattice.
        f=zeros(L,W,assert_classname(coef,g));
        for ii=0:lt(2)-1
            % Extract sublattice
            sub=coef(:,ii+1:lt(2):end,:);
            f=f+comp_idgt(sub,mwin(:,ii+1),lt(2)*a,[0 1],0,0);  
        end;

    else

        g=fir2long(g,L);
      
        [s0,s1,br] = shearfind(L,a,M,lt);
        
        f=comp_inonsepdgt_shear(coef,g,a,s0,s1,br);
    end;

end;    
