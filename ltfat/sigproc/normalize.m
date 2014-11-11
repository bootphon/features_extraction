function f=normalize(f,varargin)
%NORMALIZE  Normalize input signal by specified norm
%   Usage:  h=normalize(f,...);
% 
%   NORMALIZE(f,...) will normalize the signal f by the specified norm.
%
%   The norm is specified as a string and may be one of:
%
%     '1'       Normalize the l^1 norm to be 1.
%
%     'area'    Normalize the area of the signal to be 1. This is exactly the same as '1'.
%
%     '2'       Normalize the l^2 norm to be 1.
%
%     'energy'  Normalize the energy of the signal to be 1. This is exactly
%               the same as '2'.
%
%     'inf'     Normalize the l^{inf} norm to be 1.
%
%     'peak'    Normalize the peak value of the signal to be 1. This is exactly
%               the same as 'inf'.
%
%     'rms'     Normalize the Root Mean Square (RMS) norm of the
%               signal to be 1.
%
%     's0'      Normalize the S0-norm to be 1.
%
%     'null'    Do NOT normalize, output is identical to input.
%
%   It is possible to specify the dimension:
%
%      'dim',d  Work along specified dimension. The default value of []
%                means to work along the first non-singleton one.
%
%   See also: rms, s0norm
%
%   Url: http://ltfat.sourceforge.net/doc/sigproc/normalize.php

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
  
if ~isnumeric(f) 
  error('%s: Input must be numerical.',upper(mfilename));
end;

if nargin<1
  error('%s: Too few input parameters.',upper(mfilename));
end;

definput.import={'normalize'};
definput.keyvals.dim=[];

[flags,kv]=ltfatarghelper({},definput,varargin);

if flags.do_null || isempty(f);
  return
end;

%% ------ Computation --------------------------
 
[f,L,Ls,W,dim,permutedsize,order]=assert_sigreshape_pre(f,[],kv.dim, ...
                                                  upper(mfilename));
y=zeros(permutedsize,assert_classname(f));

for ii=1:W  
  
  if flags.do_1 || flags.do_area
    f(:,ii)=f(:,ii)/norm(f(:,ii),1);
  end;

  if flags.do_2 || flags.do_energy
    f(:,ii)=f(:,ii)/norm(f(:,ii),2);
  end;

  if flags.do_inf || flags.do_peak
    f(:,ii)=f(:,ii)/norm(f(:,ii),Inf);
  end;

  if flags.do_rms
    f(:,ii)=f(:,ii)/rms(f(:,ii));
  end;
  
  if flags.do_s0 
    f(:,ii)=f(:,ii)/s0norm(f(:,ii));
  end;

end;


f=assert_sigreshape_post(f,kv.dim,permutedsize,order);

