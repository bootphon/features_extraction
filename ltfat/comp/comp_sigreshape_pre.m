function [f,fl,W,wasrow,remembershape]=comp_sigreshape_pre(f,callfun,do_ndim)
%COMP_SIGRESHAPE_PRE
%  
%
%   Url: http://ltfat.sourceforge.net/doc/comp/comp_sigreshape_pre.php

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
%   TESTING: OK
%   REFERENCE: OK
    
wasrow=0;

% Rember the shape if f is multidimensional.
remembershape=size(f);
fd=length(remembershape);


% Multi-dimensional mode, apply to first dimension.
if fd>2
	
  if (do_ndim>0) && (fd>do_ndim)
    error([callfun,': ','Cannot process multidimensional arrays.']);
  end;
  
  fl=size(f,1);
  W=prod(remembershape)/fl;

  % Reshape to matrix if multidimensional.
  f=reshape(f,fl,W);

else

  if size(f,1)==1
    wasrow=1;
    % Make f a column vector.
    f=f(:);
  end;
  
  fl=size(f,1);
  W=size(f,2);
  
end;






