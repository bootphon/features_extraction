function [C] = plotwavelets(c,info,varargin)
%PLOTWAVELETS  Plot wavelet coefficients
%   Usage:  plotwavelets(c,info,fs) 
%           plotwavelets(c,info,fs,'dynrange',dynrange,...)
%
%   plowavelets(c,info) plots the wavelet coefficients c using
%   additional parameters from struct. info. Both parameters are returned
%   by any forward transform function in the wavelets directory.
%
%   plowavelets(c,info,fs) does the same plot assuming a sampling rate of fs Hz
%   of the original signal.
%
%   plowavelets(c,info,fs,'dynrange',dynrange) additionally limits the dynamic range.
%
%   C=plowavelets(...) returns the processed image data used in the
%   plotting. Inputting this data directly to imagesc or similar functions
%   will create the plot. This is usefull for custom post-processing of the
%   image data.
%
%   plowavelets supports optional parameters of TFPLOT. Please see
%   the help of TFPLOT for an exhaustive list.
%
%   See also: fwt, tfplot
%
%   Url: http://ltfat.sourceforge.net/doc/wavelets/plotwavelets.php

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

definput.import={'tfplot'};
definput.flags.fwtplottype = {'tfplot','stem'};
definput.keyvals.fs = [];
definput.keyvals.dynrange = [];
[flags,kv]=ltfatarghelper({'fs','dynrange'},definput,varargin);

if(flags.do_stem)
   error('%s: Flag %s not supported yet.',upper(mfilename),flags.fwtplottype);
end

maxSubLen = 800;
draw_ticks = 1;

if(strcmpi(info.fname,'fwt'))
%% FWT plot
   % Change to the cell format
   if(isnumeric(c))
       c = wavpack2cell(c,info.Lc,info.dim);
   end
   maxSubLen = max(info.Lc);
   
   % Only one channel signals can be plotted.
   if(size(c{1},2)>1)
      error('%s: Multichannel input not supported.',upper(mfilename));
   end

   subbNo = numel(c);
   w = fwtinit(info.wt);
   aBase = w.a;
   filtNo = numel(w.h);
   J = info.J;
   a = [aBase(1).^J, reshape(aBase(2:end)*aBase(1).^(J-1:-1:0),1,[])]';
elseif(strcmpi(info.fname,'ufwt'))
   
   % Only one channel signals can be plotted.
   if(ndims(c)>2)
      error('%s: Multichannel not supported.',upper(mfilename));
   end

   subbNo = size(c,2);
   a = ones(subbNo,1);

   w = fwtinit(info.wt);
   filtNo = numel(w.h);
   J = info.J; 
elseif(strcmpi(info.fname,'wfbt'))
   % Only one channel signals can be plotted.
   if(size(c{1},2)>1)
      error('%s: Multichannel input not supported.',upper(mfilename));
   end
   maxSubLen = max(cellfun(@(cEl) size(cEl,1),c));
   a = treeSub(info.wt);
   subbNo = numel(c);
   draw_ticks = 0;
elseif(strcmpi(info.fname,'uwfbt'))
   % Only one channel signals can be plotted.
   if(ndims(c)>2)
      error('%s: Multichannel not supported.',upper(mfilename));
   end

   subbNo = size(c,2);
   a = ones(subbNo,1);
   draw_ticks = 0;
elseif(strcmpi(info.fname,'wpfbt'))
   % Only one channel signals can be plotted.
   if(size(c{1},2)>1)
      error('%s: Multichannel input not supported.',upper(mfilename));
   end
   maxSubLen = max(cellfun(@(cEl) size(cEl,1),c));
   aCell = nodeSub(nodesBForder(info.wt),info.wt);
   a = cell2mat(cellfun(@(aEl) aEl(:)',aCell,'UniformOutput',0));
   draw_ticks = 0;
elseif(strcmpi(info.fname,'uwpfbt'))
   % Only one channel signals can be plotted.
   if(ndims(c)>2)
      error('%s: Multichannel not supported.',upper(mfilename));
   end

   subbNo = size(c,2);
   a = ones(subbNo,1);
   draw_ticks = 0;
else
   error('%s: Unknown function name %s.',upper(mfilename),info.fname);
end

% Use plotfilterbank
C=plotfilterbank(c,a,[],kv.fs,kv.dynrange,flags.plottype,...
  flags.log,flags.colorbar,flags.display,'fontsize',kv.fontsize,'clim',kv.clim,'xres',min([maxSubLen,800]));

if(draw_ticks)
   % Redo the yticks and ylabel
   yTickLabels = cell(1,subbNo);
   yTickLabels{1} = sprintf('a%d',J);
   Jtmp = ones(filtNo-1,1)*(J:-1:1);
   for ii=1:subbNo-1
      yTickLabels{ii+1} = sprintf('d%d',Jtmp(ii));
   end

   ylabel('Subbands','fontsize',kv.fontsize);
   set(gca,'ytick',1:subbNo);
   set(gca,'ytickLabel',yTickLabels,'fontsize',kv.fontsize);
end



