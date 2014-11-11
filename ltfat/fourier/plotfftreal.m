function plotfftreal(coef,varargin)  
%PLOTFFTREAL  Plot the output from FFTREAL  
%   Usage: plotfftreal(coef);
%          plotfftreal(coef,fs);
%
%   PLOTFFTREAL(coef) plots the output from the FFTREAL function. The
%   frequency axis will use normalized frequencies between 0 and 1 (the
%   Nyquest frequency). It is assumed that the length of the original
%   transform was even.
%
%   PLOTFFTREAL(coef,fs) does the same for the FFTREAL of a signal
%   sampled at a sampling rate of fs Hz.
%
%   PLOTFFTREAL(coef,fs,dynrange) additionally limits the dynamic range of the
%   plot. See the description of the 'dynrange' parameter below.
%
%   PLOTFFTREAL accepts the following optional arguments:
%
%     'dynrange',r  Limit the dynamical range to r by using a colormap in
%                   the interval [chigh-r,chigh], where chigh is the highest
%                   value in the plot. The default value of [] means to not
%                   limit the dynamical range. 
%
%     'db'      Apply 20*log_{10} to the coefficients. This makes 
%               it possible to see very weak phenomena, but it might show 
%               too much noise. This is the default.
%
%     'dbsq'    Apply 10*log_{10} to the coefficients. Same as the
%               'db' option, but assumes that the input is already squared.  
%
%     'lin'     Show the coefficients on a linear scale. This will
%               display the raw input without any modifications. Only works for
%               real-valued input.
%
%     'linsq'   Show the square of the coefficients on a linear scale.
%
%     'linabs'  Show the absolute value of the coefficients on a linear
%               scale.
%     
%     'N',N     Specify the transform length N. Use this if you are
%               unsure if the original input signal was of even length.
%
%   In addition to these parameteres, PLOTFFTREAL accepts any of the flags
%   from NORMALIZE. The coefficients will be normalized as specified
%   before plotting.
%
%   See also: plotfft, fftreal
%
%   Url: http://ltfat.sourceforge.net/doc/fourier/plotfftreal.php

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

  
if nargin<1
  error('%s: Too few input parameters.',upper(mfilename));
end;

if ~isvector(coef)>1
  error('Input is multidimensional.');
end;

definput.import={'ltfattranslate','normalize'};
definput.importdefaults={'null'};

definput.flags.log={'db','dbsq','lin','linsq','linabs'};

definput.keyvals.fs=[];
definput.keyvals.dynrange=[];
definput.keyvals.opts={};

definput.keyvals.N=2*(length(coef)-1);
[flags,kv,fs]=ltfatarghelper({'fs','dynrange'},definput,varargin);

N=kv.N;

N2=floor(N/2)+1;
if N2~=length(coef)
  error('%s: Size mismatch.',upper(mfilename));
end;

coef=normalize(coef,flags.norm);

% Apply transformation to coefficients.
if flags.do_db
  coef=20*log10(abs(coef)+realmin);
end;

if flags.do_dbsq
  coef=10*log10(abs(coef)+realmin);
end;

if flags.do_linsq
  coef=abs(coef).^2;
end;

if flags.do_linabs
  coef=abs(coef);
end;

if flags.do_lin
  if ~isreal(coef)
    error(['Complex valued input cannot be plotted using the "lin" flag.',...
           'Please use the "linsq" or "linabs" flag.']);
  end;
end;
  
% 'dynrange' parameter is handled by thresholding the coefficients.
if ~isempty(kv.dynrange)
  maxclim=max(coef(:));
  coef(coef<maxclim-kv.dynrange)=maxclim-kv.dynrange;
end;

xr=(0:N2-1)*2/N;
if ~isempty(kv.fs)
  xr=xr*kv.fs/2;
end;

plot(xr,coef,kv.opts{:});
xlim([xr(1) xr(end)]);


if flags.do_db || flags.do_dbsq
  ylabel(sprintf('%s (dB)',kv.magnitude));
else
  ylabel(sprintf('%s',kv.magnitude));
end;

if ~isempty(kv.fs)
  xlabel(sprintf('%s (Hz)',kv.frequency));
else
  xlabel(sprintf('%s (%s)',kv.frequency,kv.normalized));
end;


