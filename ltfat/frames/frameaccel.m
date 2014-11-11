function F=frameaccel(F,Ls);  
%FRAMEACCEL  Precompute structures
%   Usage: F=frameaccel(F,L);
%
%   F=FRAMEACCEL(F,Ls) precomputes certain structures that makes the basic
%   frame operations FRANA and FRSYN faster (like instantiating the
%   window from a textual description). If you only need to call the
%   routines once, calling FRAMEACCEL first will not provide any total
%   gain, but if you are repeatedly calling these routines, for instance in
%   an iterative algorithm, is will be a benefit.
%
%   Notice that you need to input the signal length Ls, so this routines
%   is only a benefit if Ls stays fixed.
%
%   If FRAMEACCEL is called twice for the same transform length, no
%   additional computations will be done.
%
%   See also: frame, frana, framelength, framelengthcoef
%
%   Url: http://ltfat.sourceforge.net/doc/frames/frameaccel.php

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
  
L=framelength(F,Ls);

if (isfield(F,'L') && (L==F.L))
  % Quick return, we have already accelerated
  return
end;

F.L=L;

if strcmp(F.type,'fusion')
    for ii=1:F.Nframes
        accel_frames{ii}=frameaccel(F.frames{ii},Ls);
    end;
    F=frame('fusion',F.w,accel_frames{:});
    F.L=L;
    return;
end;

if strcmp(F.type,'tensor')
    for ii=1:F.Nframes
        accel_frames{ii}=frameaccel(F.frames{ii},Ls);
    end;
    F=frame('tensor',accel_frames{:});
    F.L=L;
    return;
end;


if ~isfield(F,'g')
  % Quick exit, the frame does not use a window. In this case, the frame
  % always has a factorization
  
  % Default values for a lot of transforms
  F.isfac=1;

  return;
end;
  
% From this point and on, we are sure that F.g exists

if ~isempty(F.g)
    switch(F.type)
      case 'gen'
        F.isfac=~issparse(F.g);  
      case {'dgt','dgtreal'}
        [g, info] =  gabwin(F.g,F.a,F.M,L,F.kv.lt);
        F = frame(F.type,g,F.origargs{2:end});
        F.g_info = info;
        F.isfac=1;
      case {'dwilt','wmdct'}
        [g, info] = wilwin(F.g,F.M,L,upper(mfilename));
        F = frame(F.type,g,F.origargs{2:end});
        F.g_info = info;
        F.isfac=1;
      case {'filterbank','ufilterbank'}
        [g, info]  = filterbankwin(F.g,F.a,L);
        kv = arg_pfilt();
        g = comp_filterbank_pre(g,F.a,L,1000);
        F = frame(F.type,g,F.origargs{2:end});
        F.g_info = info;
        F.isfac=F.g_info.isfac;
      case {'filterbankreal','ufilterbankreal'}
        [g,info]  = filterbankwin(F.g,F.a,L,'real');
        kv = arg_pfilt();
        g = comp_filterbank_pre(g,F.a,L,1000);
        F = frame(F.type,g,F.origargs{2:end});
        F.g_info = info;
        F.isfac=F.g_info.isfac;
      case {'nsdgt','unsdgt','nsdgtreal','unsdgtreal'}
        [F.g,F.g_info]  = nsgabwin(F.g,F.a,F.M);
        F.isfac=F.g_info.isfac;
      case {'fwt','wfbt'}
        F.isfac = 1; 
    end;
end;

F.L=L;


