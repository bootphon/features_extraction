function [hrir_data, hrir_angles, fs] = enzner2008(mu, delta_phi,varargin)
%ENZNER2008  Calculate HRIR set
%   Usage: [hrir_data,hrir_angles,fs] = enzner2008(mu,delta_phi,...)
%
%   Input parameters:
%     mu          : NLMS stepzize, e.g., 0.75 or 1
%     delta_phi   : azimuthal resolution (delta_phi) in degree to store
%                   hrir,e.g., 0.1 or 1 
%     varargin (not necessary, function will also run without): 
%                   'fig2_2008': plot figure like [Enzner2008, Fig.2] 
%                   'fig4_2009': plot figure like [Enzner2009, Fig.4]
%                   'fig2_2008', 'fig4_2009': plot both 
%  
%   Output parameters:
%     hrir_data   : sampled HRIR data at arbitrary azimuth-resolution delta_phi
%     hrir_angles : vector with azimuthal angles corresponding to the azimuthal
%                   index of hrir_data
%     fs          : sampling rate of the used binaural ear signal recording
%
%   ENZNER2008 calculates a set of HRIRs using the normalized LMS-algorithm.
%   A test signal in mono, e.g., white noise, perfect sweeps, or a reference
%   recording at the position in the middle of the listeners head is used as
%   the input of the algorithm, whereas the other input of the algorithm is
%   given by the corresponding spatially-continuous (i.e., dynamical) binaural
%   recording.
%  
%   This recording contains the measured ear signals along the trajectory of
%   interest, e.g., the horizontal plane, plus some symmetric overhead.  The
%   overhead is used to ensure capturing of all data of interest, to give the
%   algorithm a scope to adapt and to be able to shift the signals against
%   each other to ensure causality (see sys_latency). The binaural recording
%   of the ear signals has to begin/end at the rear of the subject.  Thus the
%   first recorded sample number subsequent to the required overhead (see
%   adapt) corresponds to an azimuth of phi = 180° (rear).
%  
%   From a given set of example files, the HRIR data will be calculated. Per
%   default the measured ear signals (stimulus: white noise) and the
%   corresponding reference recording will be used for the computation. If
%   you want to use the loudspeaker driving signals or a perfect sweep data
%   set, please uncomment only the case of interest in the section
%   "changeable parameters" in lines 74-104. In this section you can also
%   adjust the used filter length.
%  
%   The computation is performed continuously for each sample, in compliance with
%   a continuous-azimuth HRIR representation. The storage of the HRIR data is 
%   sampled with an arbitrary azimuth-spacing delta_phi. The HRIR data will be 
%   written into the array hrir_data.
%  
%   Structure of hrir_data:
%     hrir_data(filter coefficients, left/right, no. of channels, azimuthal index)
%         filter coefficients: see h_length
%         left/right: 1 = left, 2 = right
%         no. of channels: allways 1 (single channel NLMS-algorithm)
%         azimuthal index: corresponding to an azimuth phi
%                          Note: The rotational direction during the
%                          recording of the ear signals is counterclockwise!
%                          (1 = -180 °, 2 = -180 ° + delta_phi, end = 180 ° - delta_phi )
%  
%  
%  
%   Changeable parameters (See the section "Changeable parameters" in the
%   code:
%
%     h_length = filter length in samples, e.g., 256 (or 308 for single channel perfect sweeps)
%  
%     Adjust only if using own mwasurements:
%     rec_filename = recorded ear signals as WAV-file (stereo)
%     ref_filename = reference WAV-file (mono), e.g., reference recording or 
%                    playback (white noise, perfect sweeps,...)
%     adapt = No. of symmetrically overlapping samples of the recorded ear signals
%             arround phi = 180 °, used as adaptation buffer before HRIR data will
%             be stored, also used to shift the algorithm's input signals to enshure
%             causality, e.g, adapt = 20000 for the given examples
%     sys_latency = system latency, No. of samples to shift input signals against
%                   each other (ensure causality), e.g. 30, if using a reference
%                   recording or -290, if using loudspeaker driving signals
%                   whereupon the loudspeaker distance is approx. 2 m (fs = 44100)
%
%   Url: http://amtoolbox.sourceforge.net/doc//hrtf/enzner2008.php

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

%  Authors: Michael Weinert (Michael.Weinert@rub.de), Gerald Enzner (Gerald.Enzner@rub.de)
%  Date: 21-01-2013


%%%%%%%%%%%%%%%%%%%%%%%%%% changeable parameters %%%%%%%%%%%%%%%%%%%%%%%%%%
% Please choose a specific case (1.1, 1.2, 1.3 or 1.4) by commenting the others 
 
% option 1.1 using reference signals, measurement stimulus: white noise 
h_length = 256;
rec_filename = fullfile(amtbasepath,'hrtf','continuous-azimuth HRIR','measurement','example_1ch_white_noise_earsignals.wav');
ref_filename = fullfile(amtbasepath,'hrtf','continuous-azimuth HRIR','signals','reference','example_1ch_white_noise_reference.wav');
adapt = 20000; % depends on the recording (overhead at the end and the beginnig)
sys_latency = 30;

%   % option 1.2 using loudspeaker driving signals, measurement stimulus: white noise 
% h_length = 256;
% rec_filename = fullfile(amtbasepath,'hrtf','continuous-azimuth HRIR','measurement','example_1ch_white_noise_earsignals.wav');
% ref_filename = fullfile(amtbasepath,'hrtf','continuous-azimuth HRIR','signals','playback','example_1ch_white_noise_playback.wav');
% adapt = 20000; % depends on the recording (overhead at the end and the beginnig)
% sys_latency = -290;

% %   % option 1.3 using reference signals, measurement stimulus: perfect sweeps 
% h_length = 308;
% rec_filename = fullfile(amtbasepath,'hrtf','continuous-azimuth HRIR','measurement','example_1ch_PSWEEP308_earsignals.wav');
% ref_filename = fullfile(amtbasepath,'hrtf','continuous-azimuth HRIR','signals','reference','example_1ch_PSWEEP308_reference.wav');
% adapt = 20000; % depends on the recording (overhead at the end and the beginnig)
% sys_latency = 30;

%   % option 1.4 using loudspeaker driving signals, measurement stimulus: perfect sweeps 
% h_length = 308;
% rec_filename = fullfile(amtbasepath,'hrtf','continuous-azimuth HRIR','measurement','example_1ch_PSWEEP308_earsignals.wav');
% ref_filename = fullfile(amtbasepath,'hrtf','continuous-azimuth HRIR','signals','playback','example_1ch_PSWEEP308_playback.wav');
% adapt = 20000; % depends on the recording (overhead at the end and the beginnig)
% sys_latency = -290;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% main code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% read signals %%%
x = wavread(ref_filename);
[y, fs] = wavread(rec_filename);

%%% sample numbers at which HRIR will be saved %%%
%%% Note: Computation is continuous corresponding to every sample %%%
circle = 360;                                               % range of measured trajectory in degree
delta_phi_samples = (length(y)-2*adapt)/circle * delta_phi; % length of ear signals contains full circle plus some overhead (2 * adapt)
if delta_phi_samples < 1
    display(['delta_phi can not be smaller than ', num2str(circle/(length(y)-2*adapt)),' ° = 1 Sample']);
    display(['delta_phi is set to ', num2str(circle/(length(y)-2*adapt)),' ° = 1 Sample']);
    delta_phi_samples = 1;
end;
save_data_samples = zeros(round((length(y)-2*adapt)/delta_phi_samples),1);
for k = 1:length(save_data_samples)
    save_data_samples(k) = round((k-1)*delta_phi_samples)+1;
end;

%%% run NLMS-algorithm %%%
hrir_data = zeros(h_length,2,1,length(save_data_samples));  
hrir_angles = zeros(length(save_data_samples),1);
h0 = zeros(h_length,2);                            
error = zeros(length(x),2);                       
n = 1;                                                  % counter
k = adapt/2;                                            % counter, calculation begins at sample #adapt/2
                                                        % samples before adapt/2 are reserved for sys_latency
                                                        % samples from adapt/2 to adapt are used as adaptation time
k_phi90 = round((length(y)-2*adapt)/360*270)+1;         % equivalent sample to azimuth phi = 90 ° (used to plot like [Enzner2008, Fig.2])                                                       
tic;  
%wb = waitbar(0,'calculating, please wait...');
while k <= length(x)-sys_latency-adapt/2                % note that adapt is also an overhead at the end of measurement                        
    x_buffer = x(k+sys_latency:-1:k+sys_latency-h_length+1,:);    % input vector backwards, considering sys_latency
    y_estimate(1) = h0(:,1).' * x_buffer(:);            % estimated ear signal left
    y_estimate(2) = h0(:,2).' * x_buffer(:);            % estimated ear signal right
    error(k,1) = y(k,1) - y_estimate(1);                % error signal left
    error(k,2) = y(k,2) - y_estimate(2);                % error signal right
    x_norm = x_buffer' * x_buffer;        
    h0(:,1) = h0(:,1) + mu/(x_norm) .* error(k,1) .* x_buffer(:); % estimated left ear hrir at sample #k
    h0(:,2) = h0(:,2) + mu/(x_norm) .* error(k,2) .* x_buffer(:); % estimated right ear hrir at sample #k             
       
    if n <= length(save_data_samples)                     
        if (k-adapt == save_data_samples(n))            % store hrir at specified azimuth

            %if ishandle(wb)                                                                              
            %waitbar(((k-1-adapt)/delta_phi_samples)/(circle/ delta_phi),wb, ...
            %['calculating, please wait... ', num2str((k-1- adapt)/ ...
            %       delta_phi_samples*delta_phi),'/',num2str(circle)])
            %end;
            hrir_data(:,1,n) = h0(:,1);                 % left                
            hrir_data(:,2,n) = h0(:,2);                 % right     
            hrir_angles(n) = ((k-1-adapt)/delta_phi_samples*delta_phi)-180; % corresponding azimuth to stored hrir_data
            n = n+1;
        end;
    end; 
    
    if (k == k_phi90+adapt) && (sum(cell2mat(cellfun(@(x) strcmp(x,'fig2_2008'),varargin,'UniformOutput',false))) >= 1)  % plot like [Enzner2008, Fig.2] 
        figure
        subplot(2,1,1)
        plot(20*log10(abs(h0(:,1)./max(max(abs(h0))))))
        xlim([1 length(h0(:,1))])
        ylim([-80 3])
        ylabel('|h_1(\kappa,\theta_k)| [dB]')
        title(['azimuth \theta_k = ',num2str(round(360/(length(y)-2*adapt)*(k-adapt+1))),' °, left ear'])
        grid on
        subplot(2,1,2)
        plot(20*log10(abs(h0(:,2)./max(max(abs(h0))))))
        xlim([1 length(h0(:,2))])
        ylim([-80 3])
        ylabel('|h_2(\kappa,\theta_k)| [dB]') 
        xlabel('impulse response lag \kappa')
        title(['azimuth \theta_k = ',num2str(round(360/(length(y)-2*adapt)*(k-adapt+1))),' °, right ear'])
        grid on
    end;    
    
k = k+1; 
end;
toc;                

%if ishandle(wb)
%     close(wb);
%end; 

hrir_data = hrir_data./(max(max(max(max(abs(hrir_data))))));
% save hrir_data.mat hrir_data

%%% plot recorded earsignals vs. error signals like [Enzner2009, Fig.4] %%%
if sum(cell2mat(cellfun(@(x) strcmp(x,'fig4_2009'),varargin,'UniformOutput',false))) >= 1
    SNR_l = 10*log10(var(error(adapt+1:(length(y)-adapt),1))/var(y(adapt+1:(length(y)-adapt),1))/h_length);
    SNR_r = 10*log10(var(error(adapt+1:(length(y)-adapt),2))/var(y(adapt+1:(length(y)-adapt),2))/h_length);
    t = linspace(0, length(y)/fs, length(y));
    figure
    subplot(2,1,1)
    txt1 = ['(\sigma_{e^l}^2/\sigma_{y^l}^2)/N = ',num2str(SNR_l),' dB'];
    title({txt1});
    hold on
    p1 = plot(t, y(:,1),'b');
    p2 = plot(t, error(:,1),'m');
    xlim([(adapt+1)/fs (length(y)-adapt)/fs]);
    xlabel('time [s]');
    ylim([-0.5 0.5]);
    ylabel('amplitude');    
    legend([p1 p2],{'left ear recording y^l(k)' 'error signal e^l(k)'}, 'location', 'northwest') 
    grid on;
    subplot(2,1,2)
    txt2 = ['(\sigma_{e^r}^2/\sigma_{y^r}^2)/N = ',num2str(SNR_r),' dB'];
    title({txt2});
    hold on
    p1 = plot(t,y(:,2),'b');
    p2 = plot(t,error(:,2),'m');
    xlim([(adapt+1)/fs (length(y)-adapt)/fs]);
    xlabel('time [s]');
    ylim([-0.5 0.5]);
    ylabel('amplitude');
    legend([p1 p2],{'right ear recording y^r(k)' 'error signal e^r(k)'}, 'location', 'northeast') 
    grid on;
end;



