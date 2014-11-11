function output = exp_langendijk2002(varargin)
%EXP_LANGENDIJK2002  Experiment from Langendijk & Bronkhorst 2002
%   Usage: output = exp_langendijk2002(flags);
%
%   exp_langendijk(flags) recreates figures from Langendijk & Bronkhorst
%   (2002)
%
%   The following flags can be specified;
%
%     'plot'    Plot the output of the experiment. This is the default.
%
%     'noplot'  Don't plot, only return data.
%
%     'fig7'    Listener P6
%
%     'fig9'    Listener P3
%
%   You can choose between two of his listeners P3 and P6. The required
%   data (DTF data and response patterns) will be provided by precalculated
%   mat-files due to high computing time (optionally data can be calculated
%   by using the data_langendijk2002('expdata') function. 
%
%   The following subfigures shows the probability density function (pdf)
%   and actual responses(°) for the chosen listener as a function of the target
%   position for different conditions. The shading of each cell codes the
%   probability density (light/dark is high/low probability):
%
%      Subfigure 1: Baseline condition
%      Subfigure 2: 2-octave condition (4-16kHz)
%      Subfigure 3: 1-octave condition (low 4-8kHz)
%      Subfigure 4: 1-octave condition (middle 5.7-11.3kHz)
%      Subfigure 5: 1-octave condition (high 8-16kHz)
%   
%   Subfigure 6 shows the likelihood statistics for the actual responses
%   (bars), the average and the 99% confidence interval of the expected
%   likelihood (dots and bars). See the paper for further details.
%
%   The output are the pdfs for the baseline condition.
%
%   Examples:
%   ---------
%
%   To display Figure 7 use :
%
%     exp_langendijk2002('fig7');
%
%   To display Figure 9 use :
%
%     exp_langendijk2002('fig9');
%
%   See also: langendijk, likelilangendijk, plotlangendijk, plotlikelilangendijk, data_langendijk2002
%
%   Url: http://amtoolbox.sourceforge.net/doc//experiments/exp_langendijk2002.php

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

%
% AUTHOR : Robert Baumgartner, OEAW Acoustic Research Institute


%% ------ Check input options --------------------------------------------

  definput.flags.type = {'missingflag','fig7','fig9'};
  definput.flags.plot = {'plot','noplot'};

  % Parse input options
  [flags,keyvals]  = ltfatarghelper({},definput,varargin);
  
  if flags.do_missingflag
    flagnames=[sprintf('%s, ',definput.flags.type{2:end-2}),...
               sprintf('%s or %s',definput.flags.type{end-1},definput.flags.type{end})];
    error('%s: You must specify one of the following flags: %s.',upper(mfilename),flagnames);
  end;


  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                               SETTINGS                                %
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if flags.do_fig7
    listener='P6';  % ID of listener (P3 or P6)
  elseif flags.do_fig9
    listener='P3';
  end

  fs = 48000;     % sampling frequency

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  path = mfilename('fullpath');
  dtfdata=load(['langendijk2002-' listener '.mat']);
  % loads hM data for all conditions 
  % data can be recalculated by calling data_langendijk2002('expdata')

  % pdf calcualtion
  %h = waitbar(0,'Please wait...');
  pb  = langendijk( dtfdata.medir,dtfdata.medir,fs); % baseline
  %waitbar(1/5)
  p2o = langendijk( dtfdata.medir2o,dtfdata.medir,fs); % 2-oct (4-16kHz)
  %waitbar(2/5)
  p1ol= langendijk( dtfdata.medir1ol,dtfdata.medir,fs); % 1-oct (low:4-8kHz)
  %waitbar(3/5)
  p1om= langendijk( dtfdata.medir1om,dtfdata.medir,fs); % 1-oct (middle:5.7-11.3kHz)
  %waitbar(4/5)
  p1oh= langendijk( dtfdata.medir1oh,dtfdata.medir,fs); % 1-oct (high:8-16kHz)
  %waitbar(5/5)

  % likelihood estimations
  la=zeros(5,1);le=zeros(5,1);ci=zeros(5,2);
  idb=1:2:length(dtfdata.targetb); % in order to get comparable likelihoods
  [la(1),le(1),ci(1,:)] = likelilangendijk( pb,dtfdata.pol,dtfdata.pol,dtfdata.targetb(idb),dtfdata.responseb(idb) );
  [la(2),le(2),ci(2,:)] = likelilangendijk( p2o,dtfdata.pol,dtfdata.pol,dtfdata.targetc,dtfdata.response2o );
  [la(3),le(3),ci(3,:)] = likelilangendijk( p1ol,dtfdata.pol,dtfdata.pol,dtfdata.targetc,dtfdata.response1ol );
  [la(4),le(4),ci(4,:)] = likelilangendijk( p1om,dtfdata.pol,dtfdata.pol,dtfdata.targetc,dtfdata.response1om );
  [la(5),le(5),ci(5,:)] = likelilangendijk( p1oh,dtfdata.pol,dtfdata.pol,dtfdata.targetc,dtfdata.response1oh );
  %close(h)

  output = pb;

  if flags.do_plot
    figure('Name',listener)
    clf
    % pdf plots with actual responses
    subplot(2,3,1)
    hold all;    
    plotlangendijk(pb,dtfdata.pol,dtfdata.pol,'nocolorbar');
    title(['Baseline']);    
    h=plot( dtfdata.targetb, dtfdata.responseb, 'ko');
    set(h, 'MarkerFaceColor','w');
    
    subplot(2,3,2)
    hold all;
    plotlangendijk(p2o,dtfdata.pol,dtfdata.pol,'nocolorbar');
    title(['2-oct (4-16kHz)']);
    h=plot( dtfdata.targetc, dtfdata.response2o, 'ko');
    set(h, 'MarkerFaceColor','w');
    
    subplot(2,3,3)
    hold all;
    plotlangendijk(p1ol,dtfdata.pol,dtfdata.pol,'nocolorbar');
    title(['1-oct (low: 4-8kHz)']);
    h=plot( dtfdata.targetc, dtfdata.response1ol, 'ko');
    set(h, 'MarkerFaceColor','w');
    
    subplot(2,3,4)
    hold all;
    plotlangendijk(p1om,dtfdata.pol,dtfdata.pol,'nocolorbar');
    title(['1-oct (middle: 5.7-11.3kHz)']);
    h=plot( dtfdata.targetc, dtfdata.response1om, 'ko');
    set(h, 'MarkerFaceColor','w');
    
    subplot(2,3,5)
    hold all;
    plotlangendijk(p1oh,dtfdata.pol,dtfdata.pol,'nocolorbar');
    title(['1-oct (high: 8-16kHz)']);
    h=plot( dtfdata.targetc, dtfdata.response1oh, 'ko');
    set(h,'MarkerFaceColor','w')
    
    % likelihood statistic
    subplot(2,3,6)
    plotlikelilangendijk(la,le,ci);
    set(gca,'XLim',[0.5 5.5])
  end



end


