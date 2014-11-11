function p = langendijk(targets,template,varargin)
%LANGENDIJK Localization model according to Langendijk et al. (2002)
%   Usage:    p = langendijk(targets,template)
%             p = langendijk(targets,template,fs,bw,s,do,flow,fhigh)
%
%   Input parameters:
%     targets  : head-related impulse responses (HRIRs) of target sounds 
%                (sorted acc. ascending polar angle)
%     template : HRIRs of template
%
%   Output parameters:
%     p       : Predicted probability mass vectors (PMVs) of polar response
%               angles as a function of the polar target angle.
%
%   LANGENDIJK(targets,template,... ) results to a two dimensional matrix p.  The
%   first dimension represents all possible response positions in
%   increasing order and the second dimension all possible target
%   respectively source positions. Consequently each column represents the
%   predicted probability mass vector (PMV) of the polar response angle 
%   distribution for one special target position. If you want to plot this 
%   prediction matrix use PLOTLANGENDIJK.
%
%   LANGENDIJK accepts the following optional parameters.
%
%     'fs',fs        Sampling rate of the head-related impulse responses.
%  
%     'bw',bw        Bandwidth of filter bands as partial of an octave. The
%                    default value is 6.
%
%     'do',do        Differential order. The default value is 0.
%
%     's',s          Standard deviation of transforming Gaussian
%                    function, default value is 2.
%
%     'flow',flow    Start frequency of filter bank. min: 0,5kHz; default: 2kHz
%
%     'fhigh',fhigh  End frequency of filter bank; default: 16kHz
%
%   LANGENDIJK accepts the following flags.
%
%     'std'          Apply Gaussian transformed standard deviation of 
%                    inter-spectral differences for comparison process. 
%                    This is the default.
%  
%     'xcorr'        Apply crosscorrelation for comparison process.
%
%   See also: plotlangendijk
%
%   References:
%     E. Langendijk and A. Bronkhorst. Contribution of spectral cues to human
%     sound localization. J. Acoust. Soc. Am., 112:1583-1596, 2002.
%     
%     
%
%   Url: http://amtoolbox.sourceforge.net/doc//monaural/langendijk.php

% Copyright (C) 2009-2013 Peter L. Søndergaard and others.
% This file is part of AMToolbox version 0.9.1
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

% AUTHOR : Robert Baumgartner, OEAW Acoustical Research Institute
  
  
  definput.import={'langendijkcomp'};
  definput.keyvals.bw=6;
  definput.keyvals.flow=2000;
  definput.keyvals.fhigh=16000;
  definput.keyvals.stim=[];
  definput.keyvals.fs=48000;
  
  [flags,kv]=ltfatarghelper({'fs','bw','s','do','flow','fhigh'},definput,varargin);
  
  
  % Filter bank
  x = cqdft(targets,kv.fs,kv.flow,kv.fhigh,kv.bw);
  y = cqdft(template,kv.fs,kv.flow,kv.fhigh,kv.bw);
  
  % Comparison process
  si=zeros(size(template,2),size(targets,2),size(template,3)); % initialisation
  for ii=1:size(targets,2)
      si(:,ii,:) = langendijkcomp(x(:,ii,:),y,'argimport',flags,kv);
  end
  
  % Binaural average
  si = mean(si,3);
  
  % Normalization to PMV
  p = si ./ repmat(sum(si),size(si,1),1);
  
  
end
