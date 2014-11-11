function outsig=frana(F,insig);
%FRANA  Frame analysis operator
%   Usage: c=frana(F,f);
%
%   c=FRANA(F,f) computes the frame coefficients c of the input
%   signal f using the frame F. The frame object F must have been
%   created using FRAME or FRAMEPAIR.
%
%   If f is a matrix, the transform will be applied along the columns
%   of f. If f is an N-D array, the transform will be applied along
%   the first non-singleton dimension.
%
%   The output coefficients are stored as columns. This is usually
%   *not* the same format as the 'native' format of the frame. As an
%   examples, the output from FRANA for a gabor frame cannot be
%   passed to IDGT without a reshape.
%
%   See also: frame, framepair, frsyn, plotframe
%
%   Url: http://ltfat.sourceforge.net/doc/frames/frana.php

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


%% ----- step 1 : Verify f and determine its length -------
% Change f to correct shape.
[insig,~,Ls,W,dim,permutedsize,order]=assert_sigreshape_pre(insig,[],[],upper(mfilename));
 
F=frameaccel(F,Ls);

insig=postpad(insig,F.L);

%% ----- do the computation ----

outsig=F.frana(insig);

%% --- cleanup -----

permutedsize=[size(outsig,1),permutedsize(2:end)];

outsig=assert_sigreshape_post(outsig,dim,permutedsize,order);

  

