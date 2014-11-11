function d=framediag(F,L);
%FRAMEDIAG  Compute the diagonal of the frame operator
%   Usage: d=framediag(F,L);
%
%   FRAMEDIAG(F,L) computes the diagonal of the frame operator for a
%   frame of type F of length L.
%
%   The diagonal of the frame operator can for instance be used as a
%   preconditioner.
%
%   See also: franaiter, frsyniter
%
%   Url: http://ltfat.sourceforge.net/doc/frames/framediag.php

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

if nargin<2
  error('%s: Too few input parameters.',upper(mfilename));
end;

if ~isstruct(F)
  error('%s: First agument must be a frame definition structure.',upper(mfilename));
end;

% Standard response, works for all tight and orthonormal systems
d=ones(L,1);

switch(F.type)
  case 'gen'
    d=diag(F.g*F.g');
    
  case {'dgt','dgtreal'}
    d=gabframediag(F.g,F.a,F.M,L,F.vars{:});  
  
  case {'dwilt','wmdct'}
    d=wilframediag(F.g,F.M,L);
    
  case {'filterbank','ufilterbank','filterbankreal','ufilterbankreal'}
    error('Not implemented yet.');

  case {'nsdgt','unsdgt','nsdgtreal','unsdgtreal'}
    d=nsgabframediag(F.g,F.a,F.M);
    
  case 'fusion'
    error('Not implemented yet.');
end;



