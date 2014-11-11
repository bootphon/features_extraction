function gf=filterbankresponse(g,a,L,varargin)
%FILTERBANKRESPONSE  Response of filterbank as function of frequency
%   Usage:  gf=filterbankresponse(g,a,L);
%      
%   FILTERBANKRESPONSE(g,a,L) computes the total response in frequency of
%   a filterbank specified by g and a for a signal length of
%   L. This corresponds to summing up all channels. The output is a
%   usefull tool to investigate the behaviour of the windows, as peaks
%   indicate that a frequency is overrepresented in the filterbank, while
%   a dip indicates that it is not well represented.
%
%   In mathematical terms, this function computes the diagonal of the
%   Fourier transform of the frame operator when the filterbank is painless.
%
%   FILTERBANKRESPONSE(g,a,L,'real') does the same for a filterbank
%   intended for positive-only filterbank.
%
%   FILTERBANKRESPONSE(g,a,L,fs) specifies the sampling rate fs. This
%   is only used for plotting purposes.
%
%   FILTERBANKRESPONSE takes the following optional parameters:
%
%      'fs',fs    Sampling rate, used only for plotting.
%
%      'complex'  Assume that the filters cover the entire frequency
%                 range. This is the default.
%
%      'real'     Assume that the filters only cover the positive
%                 frequencies (and is intended to work with real-valued
%                 signals only).
%
%      'noplot'   Don't plot the response, just return it.
%
%      'plot'     Plot the response using PLOTFFTREAL.
%
%   See also: filterbank, filterbankbounds
%
%   Url: http://ltfat.sourceforge.net/doc/filterbank/filterbankresponse.php

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
  
definput.flags.ctype={'complex','real'};
definput.flags.plottype={'noplot','plot'};
definput.keyvals.fs=[];
[flags,kv,fs]=ltfatarghelper({'fs'},definput,varargin);

[g,info]=filterbankwin(g,a,L,'normal');
M=info.M;

gf=comp_filterbankresponse(g,info.a,L,flags.do_real);

if flags.do_plot
    if flags.do_real
        plotfftreal(gf(1:floor(L/2)+1),fs,'lin');
    else
        plotfft(gf,fs,'lin');
    end;
end;

