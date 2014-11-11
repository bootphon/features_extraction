% Matlab code for computation of the distance-dependent feature
% BSMD STD (Binaural Spectral Magnitude Difference Standard Deviation)
%
% The feature can be derived from any dual-channel signal
% (binaural/stereo recordings).
%
%
%   Url: http://amtoolbox.sourceforge.net/doc//experiments/exp_georganti2013.php

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

% More information can be found in :
% [1] E. Georganti, T. May, S. van de Par, and J. Mourjopoulos. "Extracting
%     sound-source-distance information from binaural signals." In
%     J. Blauert, editor, The technology of binaural listening, chapter 7.
%     Springer, Berlin?Heidelberg?New York NY, 2013.

%
%   This is a very early version of the code. Updates are on their way!
%
%
%   Developed with Matlab 7.1.0.584 (R2010b). Please send bug reports to:
%
%   Author  :  Eleftheria Georganti
%              Phd student
%              Audio And Acoustic Technology Laboratoty
%              University of Patras, Rion, Greece
%              egeorganti@upatras.gr

%   History :
%   v.0.1   2013/01/21

% *************************************************************************

clc
close all
clear all



% SELECT SMALL OR LARGE ROOM BY COMMENTING/UNCOMMENTING
 num = 1      % Small Room -  RT = 0.15 sec
% num = 2      % Large Room -  RT = 0.9 sec





% Sampling frequency
P.fs = 44100;

% Frame size in seconds
P.timeFr = 1;

% Frame size in samples
P.sampleFr = P.fs * P.timeFr;

% Overlap
P.hop = P.sampleFr/2;

% FFT points
P.nFFT = P.sampleFr;

% Frequency index
P.freq = (P.fs/P.nFFT)*(0:(P.nFFT/2-1))

% Define frequency range for the BSDM STD feature calculation
P.fmin = 20;    % lower frequency in Hz
P.fmax = 2300; % upper frequency in Hz


% List the number of available distances for the wavfiles
% Wav files should be placed in separate folders - one for each distance


if num == 1
    nDist = listDirs(['wavFiles',filesep,'smallRoom']);  
elseif num == 2;
    nDist = listDirs(['wavFiles',filesep,'largeRoom']);   
    
end


fmin_id = min(find((P.freq>P.fmin)));
fmax_id = min(find((P.freq>P.fmax)));


idx = 1;

for ww = 1:length(nDist)
    
    pathWav = listFiles(nDist(ww).name,'*.wav');
    signal = wavread(pathWav(1).name);
    
    for kk = 1:P.hop:length(signal)-P.hop
        
        
        % Calculate magnitude spectrums in dB of the left & right signals
        leftFFT  = 20*log10(abs(fft(signal(kk:kk+P.hop-1,1))));
        rightFFT = 20*log10(abs(fft(signal(kk:kk+P.hop-1,2))));
        
        % Subtract the magnitude spectrums
        specDIF  = leftFFT(1:end/2)-rightFFT(1:end/2);
        
        % Calculate the differential standard deviation for the
        % frequency range of interest
        difSTD(ww,idx) = std(specDIF(fmin_id:fmax_id));
        
        clear leftFFT rightFFT specDIFF
        
        idx = idx+1;
        
    end
    
    idx = 1;
end



% Plot figures

if num == 1
    figure;
    subplot(1,3,[1 2])
    plot(difSTD(1,10:210),'k','LineWidth',2)
    hold on
    plot(difSTD(2,10:210),'Color',[0.6 0.6 0.6],'LineWidth',2)
    plot(difSTD(3,10:210),'--k','LineWidth',2)
    xlabel('Time (sec)','FontSize',16,'FontWeight','bold')
    ylabel('BSMD STD','FontSize',16,'FontWeight','bold')
    ylim([3 8])
    xlim([0 200])
    legend('0.5m','1m','1.5m','Location','NorthWest')
    set(gca,'FontSize',14)
    grid on
    box on
    
    subplot(1,3,3)
    [a1,b1] = hist(difSTD(1,10:210),[3:0.1:8]);
    [a2,b2] = hist(difSTD(2,10:210),[3:0.1:8]);
    [a3,b3] = hist(difSTD(3,10:210),[3:0.1:8]);
    plot(a1/201,b1,'LineWidth',2,'Color','k'); hold on
    set(gca,'FontSize',14)
    plot(a2/201,b2,'Color',[0.6 0.6 0.6],'LineWidth',2)
    plot(a3/201,b3,'LineWidth',2,'Color','k','LineStyle','--')
    xlabel('Frequency of occurence (%)','FontSize',16,'FontWeight','bold')
    legend('0.5m','1m','1.5m','Location','NorthEast')
    box on
    xlim([0 0.4])
    ylim([3 8])
    box on
    set(gca,'YTick',[])
    grid on
    
    print -depsc2 smallSpeechBSMD_STD  %Creates .eps file with the figure
    
    
elseif num == 2 
    
    figure;
    subplot(1,3,[1 2])
    plot(difSTD(1,10:210),'k','LineWidth',2)
    hold on
    plot(difSTD(2,10:210),'Color',[0.6 0.6 0.6],'LineWidth',2)
    plot(difSTD(3,10:210),'--k','LineWidth',2)
    xlabel('Time (sec)','FontSize',16,'FontWeight','bold')
    ylabel('BSMD STD','FontSize',16,'FontWeight','bold')
    ylim([2 8.5])
    xlim([0 200])
    legend('1m','2m','3m','Location','NorthWest')
    set(gca,'FontSize',14)
    grid on
    box on
    
    subplot(1,3,3)
    [a1,b1] = hist(difSTD(1,10:210),[3:0.1:8]);
    [a2,b2] = hist(difSTD(2,10:210),[3:0.1:8]);
    [a3,b3] = hist(difSTD(3,10:210),[3:0.1:8]);
    plot(a1/201,b1,'LineWidth',2,'Color','k'); hold on
    set(gca,'FontSize',14)
    plot(a2/201,b2,'Color',[0.6 0.6 0.6],'LineWidth',2)
    plot(a3/201,b3,'LineWidth',2,'Color','k','LineStyle','--')
    xlabel('Frequency of occurence (%)','FontSize',16,'FontWeight','bold')
    legend('1m','2m','3m','Location','NorthEast')
    box on
    xlim([0 0.4])
    ylim([2 8.5])
    box on
    set(gca,'YTick',[])
    grid on
    
    print -depsc2 largeSpeechBSMD_STD  %Creates .eps file with the figure
end


