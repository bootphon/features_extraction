%
%   Url: http://amtoolbox.sourceforge.net/doc//experiments/exp_joergensen2011.php

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

material  = 'CLUE';
% Loads a cell array with 10 sentences from the CLUE material
load Danish_CLUE_10sentence_samples_22kHz
disp(['start: ' datestr(now, 'dd-mm-yyyy HH:MM:SS')])
x = 0;
clear res
res = {};
NSpeechsamples = 10;
if 1 ==1% if true: run simulation, false: load an earlier simulation
    for q = 1:NSpeechsamples
        
        x = sentenceArray{q}';
        fs = 22050;
        
        sentenceFileLevel = -26.00; % The RMS level of all CLUE sentence files (in dB relative to 1)
        SPL = 65; % speech presentation level
        SPL_sentence = dbspl(x,'noac');
        x = x*10^((SPL-sentenceFileLevel-100)/20);
        
        % the level of the sentence is set such that the long-term RMS of all
        % sentences are presented at a level of 65 dB SPL, using the
        % convention that a pure tone at 100 dB SPL has an RMS value of 1,
        % as in the AMToolbox.
        x_SPL = dbspl(x,'noac');
        
        Ts = 1/fs;
        T = length(x)/fs;
        t = 0:Ts:T;
        t = t(1:end-1);
        N = length(t);
        
        % load the noise file
        noise_glob = wavread('SSN_CLUE_22kHz.wav');
        
        Nsegments = floor(length(noise_glob)/N);
        % pick a random segment from the noise file
        startIdx = randi(Nsegments-2 ,1)*N;
        noise_glob = noise_glob(startIdx:startIdx+N -1)';
        
        
        SNRs = [ -9 -6 -3 0 3 6 9 ];
        %         
        conditions = [0 .5 1 2 4 8 ];
        
        for n = 1:length(conditions)
            
            for k = 1:length(SNRs)
                
                noise = setdbspl(noise_glob,SPL-SNRs(k));
                if size(noise) ~= size(x)
                    noise = noise';
                end
                test = noise + x;
                
                %        %% --------- spec sub parameters
                W=1024/2; % frame length
                padz=1024/2; 
                %zero padding (pad with padz/2 from the left and padz/2 from the right )
                % Note that (W+padz) is the final frame window and hence the
                % fft length (it is normally chose as a power of 2)
                SP=0.5; %Shift percentage is 50%
                
                %     all this goes into a function:
                %% --------- spec sub
                %               Spectral subtraction is applied to both the noisy speech and the noise alone
                factor = conditions(n);
                [test2 noise2] = joergensen2011specsub(test,noise,W,padz,SP,factor);
                
                
                %%         ---------------------------------------------------------        
                % It is important that the input to model is not zero-padded
                % of include silence (zeros) in the beginning and/or end of
                % the signal, since this leads to a large low-frequency
                % modulation, which will bias the model.  The SpecSub -
                % processing includes windowing which adds small periods of
                % silence in the beginning and end of the output.  The
                % output from the SpecSub is therefore truncated in the
                % beginning and the end. The truncation is inperceivable and
                % does not change the intelligibility of the sentence.
                test2_adj = test2(1.5*W:N); 
                noise2_adj = noise2(1.5*W:N);
                
                
                
                % ---------------- main model stages -------------------------- includes
                % gammatone analysis, modulation filterbank analysis and
                % calculation of SNRenv in each audio and modulation
                % channel. The output is a struct containing:
                %
                %   result.mod_fcs               : Center frequencies of the modulation filterbank
                %   result.outSNRenvs            : Matrix with an SNRenv value for each modulation filter in each gammatone filter;
                %
                %-----------------------------------------------------
                tmp =  joergensen2011multchansnrenv(test2_adj,noise2_adj,fs);
                
                %         All data of the internal representation is stored in a struct:
                res{n,k,q} = tmp; %{struct(['SNR_' num2str(k) '_cond_' num2str(n)] , tmp) };
                
            end
            
        end
        disp(['sentence nr: ' num2str(q) ' ' datestr(now, 'dd-mm-yyyy HH:MM:SS')]);
        
    end
    
    saveName = ['tmp_SNRenvs_specSub_' num2str(q) 'sent_' datestr(now, 'dd-mm-yyyy') '_1'];
    result.res = res;
    result.conditions =conditions;
    result.SNRs =SNRs;
    save(saveName, 'result') % the mulktichannel SNRenv is stored.
else
    load 'tmp_SNRenvs_specSub_10sent_20-09-2012_1'
    
end


% ------------------ combination of SNRenv across channels -----------------------

%%% The SNRenv values from the different audio and modulation channels are combined to a single value
%%% for each SNR and processing conditions:

selection = 1:length(result.conditions); %Select which conditions to use.

simOutput  =  joergensen2011combineinformation(result.res,result.SNRs,selection,NSpeechsamples);

% ------------------ ideal observer stage - converting SNrenv to percent correct  -----------------------

for q = 1:NSpeechsamples
    for n = selection
        [ Pc_est(:,n,q) SNRenvs(:,n,q)] = speechpercentcorrect(simOutput.combined_aud(n,:,q),material);
    end
end

% Average across speech samples
Pc_est_mean = mean(Pc_est,3);
Pc_est_std = std(Pc_est,0,3);
SNRenv_mean = mean(SNRenvs,3);

% ------------------  Estimating changes in SRTs based on the mean Pcorrect -------------
%   The first column of Pc_est_mean should always be the reference
conditions = result.conditions;
SNRs = result.SNRs;
dSRTs = dsrts_from_pc_mean(Pc_est_mean,SNRs,selection);


% ------------------ Plotting  -----------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%  plots of percent correct
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Center frequencies of the gammatone filters:
midfreq=[63    80   100  125  160  200    250    315  400   500  630 800  1000  1250  1600  2000 2500 3150 4000 5000 6300  8000];

if 1==1
    fnts = 14;
    colors = {'k','r','g','c','b','m',[2 1.8 .6]*.5,[1 1 1]*.5};
    figure
    subplot(1,2,1)
    %     plot(SNRs,Pc_est_mean,' -o','markersize',8), hold on
    for k = selection
        plot(SNRs,Pc_est_mean(:,k),'-- o','color',colors{k}, 'markersize', 10),hold on
    end
    data_CLUE = [0 7 34 71 90 100];
    plot([-8    -6    -4    -2     0     2],data_CLUE,'s','markerfacecolor','k')
    xlabel('SNRs (dB)','FontSize',fnts);
    ylabel('Estimated Percent correct %','FontSize',fnts);
    ylim([0 100])
    title({['Change in Percent correct vs spectral subtraction']})
    set(gca,'FontSize',fnts,'xtick',SNRs);
    
    subplot(1,2,2)
    for k = selection
        plot(SNRs,SNRenv_mean(:,k),' -o','markersize',8,'color',colors{k}), hold on
    end
    
    xlabel('SNRs (dB)','FontSize',fnts);
    ylabel('SNR_{env} (dB)','FontSize',fnts);
    ylim([0 20])
    xlim([-10 10])
    title({['Change in Percent correct vs reverberation time']})
    set(gca,'FontSize',fnts,'xtick',SNRs);
    
    for p = 1:length(conditions)
        lgnLables{p} = num2str(conditions(p));
    end
    legend(lgnLables)
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Figure 6: Comparison of measurements and predictions in SpecSub
%%%% including STI prediction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if 1==1
    xmin = 0;
    xmax = 7;
    ymin = -4.5;
    ymax = 4.5;
    ytickmax = 4;
    ytickmin = -4;
    
    fig=figure;
    
    sh = 400;
    set(fig,'Position',[2*sh, 0.15*sh, 1.2*sh, 1*sh]);
    
    
    line_sty = {'-','--','-.','-'};
    
    mark_sty = {'s','d','v','x','>','o'};
    mark_size = 10;%[6 6 6 7];
    mark_col(1,:) = [1 1 1]*0.01;
    mark_col(2,:) = [1 1 1]*0.4;
    mark_col(3,:) = [1 1 1]*0.3;
    mark_col(4,:) = [1 1 1]*0.5;
    mark_col(5,:) = [1 1 1]*0.2;
    fnts = 14;
    
    x = 1:6;
    clear y;
    
    y(:,1) = dSRTs;
    % loading the measured results:
    load measured_results_specsub;
    
    offset = [0.21 -0.21 0.25 -0.25];
    RMSE = sqrt(mean(abs(dSRTs- acrossSubdSRTs').^2));
    md = mean(abs(dSRTs- acrossSubdSRTs'));
    r_corr = corr(dSRTs',acrossSubdSRTs, 'type', 'Pearson');
    
    h = plot(0,0,'color',[1 1 1]);
    
    errorbar(x,acrossSubdSRTs,-std_acrossSubSRTs',std_acrossSubSRTs',...
             'linestyle',    'none',...
             'linewidth',    1,...
             'color',           mark_col(1,:),...
             'marker',           char(mark_sty(1)),...
             'markerfacecolor',  [1 1 1],...
             'markersize',       mark_size); hold on
    
    plot(x+offset(1),y(:,1),...
         'linestyle',    'none',...
         'linewidth',    1,...
         'color',           mark_col(1,:),...
         'marker',           char(mark_sty(1)),...
         'markerfacecolor',  mark_col(1,:),...
         'markersize',       mark_size);hold on
    
    
    load STI_SpecSub_44s_17-11-2010_1
    
    plot(x-offset(1),(STIs(:,3)-STIs(1,3))*-10,...
         'linestyle',    'none',...
         'linewidth',    1,...
         'color',           mark_col(4,:),...
         'marker',           char(mark_sty(6)),...
         'markerfacecolor',  mark_col(4,:),...
         'markersize',       mark_size);hold on
    
    hold off
    
    axis([xmin xmax ymin ymax]);
    
    set(gca,'xTick',1:6,'fontsize',fnts);
    set(gca,'yTick',ytickmin:ytickmax);
    set(gca,'xTickLabel',conditions);
    set(gca,'yTickLabel',ytickmin:ytickmax);
    
    xlabel('Over-subtraction factor \alpha','Fontsize',16)
    ylabel('\DeltaSRT (dB)','Fontsize',fnts);
    
    text(.6,3.5,{[' \rho = ',num2str(floor(r_corr*100)/100,2)],['RMSE = ',num2str(RMSE,2), ' dB']},'fontsize',fnts,'FontName', 'Times-Roman');
    
    pos= get(gca,'Position');
    set(gca,'box','on','position', [pos(1)*.8 pos(2) pos(3)*1 pos(4)*1 ])
    
    le = legend('Data','sEPSM','sSTI');
    set(le,'box','off','fontsize',14,'Location','southwest');
    %     legendshrink(0.1)
    pos= get(gca,'Position');
    ax3 = axes('Position',pos,...
               'XAxisLocation','bottom',...
               'YAxisLocation','right',...
               'Color','none',...
               'XColor','k','YColor','k',...
               'XTick',xaxis,'XTickLabel',[],...
               'YTick',-5:5,'YTickLabel',[1 0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1 0 ]);
    axis([xmin xmax ymin ymax]);
    set(gca,'fontsize',fnts);
    ylabel(ax3,'sSTI' ,'fontsize',fnts);
    
    set(gca, 'FontName', 'Times-Roman');
    set(gcf, 'Color', 'w');
    % export_fig fig6.eps  -cmyk
    %    export_fig fig6.eps fig6.pRdf -cmyk
    %
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Figure 7: Modulation excitation plots for four gammatone filters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if isfield(simOutput,'ExPatterns') == 1 && 1==1
    ExPatterns = simOutput.ExPatterns;
    fcs = [ 1 2 4 8 16 32 64 ];
    
    condNr = [1 ];
    xmin = .7;
    xmax = 90;
    ymin = -25;
    ymax = 8;
    ytickmax = 6;
    ytickmin = -22;
    fnts = 14;
    SNRidx = 4;
    fnts = 12;
    N_idx = 1;
    
    audFilter =  [7 10 13 17];
    
    
    fig=figure;
    
    sh = 350;
    set(fig,'Position',[0.05*sh, 0.15*sh, 1.7*sh, 1.5*sh]);
    
    E = 'East';
    W = 'West';
    %     N = 'North';
    S = 'South';
    NE = 'NorthEast';
    NW = 'NorthWest';
    SE = 'SouthEast';
    SW = 'SouthWest';
    
    le_pos = {NW NW;
              NW  NW;
              NW  NW};
    
    line_sty = {'-','--','-.','-'};
    line_width = 1;
    
    mark_sty = {'s','o','d','v','>'};
    mark_size = [6 6 6 7];
    mark_col(1,:) = [1 1 1]*0.11;
    mark_col(2,:) = [1 1 1]*0.4;
    mark_col(3,:) = [1 1 1]*.6;
    mark_col(4,:) = [1 1 1]*0.7;
    
    for idx_row = 1:2
        
        for idx_col=1:2%:3
            subplot_idx = (idx_row-1)*2+idx_col;
            subplot(2,2,subplot_idx)
            clear y tmp;
            %             average the excitation patterns across sentences
            for n = 1:NSpeechsamples
                tmp(:,:,n) = ExPatterns{condNr(1),SNRidx,n}(:,:,audFilter(subplot_idx));
            end
            tmp2= mean(tmp,3);
            y(:,:,1) = 10*log10(tmp2);
            x = fcs;
            hold all
            h(idx_row,idx_col) = plot(0,0,'color',[1 1 1]);
            for idx_stim = [1 2 3]
                
                h5(idx_stim,idx_row,idx_col)=  plot(x,y(:,idx_stim)',...
                                                    'linestyle',    '--',...
                                                    'linewidth',    1,...
                                                    'color',           mark_col(idx_stim,:),...
                                                    'marker',           char(mark_sty(idx_stim)),...
                                                    'markerfacecolor',  mark_col(idx_stim,:),...
                                                    'markersize',       mark_size(idx_stim));
                
            end
            %             plot(x,Threshold,'k--','LineWidth',1);
            hold off
            
            axis([xmin xmax ymin ymax]);
            
            
            set(gca,'xTick',0:20:100);
            set(gca,'yTick',0:20:100);
            set(gca,'xTickLabel',[]);
            set(gca,'yTickLabel',[]);
            pos= get(gca,'Position');
            set(gca,'position', [pos(1)*1 pos(2) pos(3)*1 pos(4)*1.1 ])
            
            set(gca,'box','on','Xscale','log','fontsize',fnts)
            
            if idx_row == 2
                set(gca,'xTick',fcs);
                set(gca,'xTickLabel',fcs);
            end
            
            if idx_row == 2 && idx_col == 1
                %makes a second axis
                set(gca,'yTick',ytickmin:4:ytickmax);
                set(gca,'yTickLabel',ytickmin:4:ytickmax);
                %
                h2 = ylabel('Envelope power (dB)','Fontsize',fnts);
                pos = get(h2,'pos'); % Read position [x y z]
                set(h2,'pos',pos+[0 13 0]) % Move label to right
                h2 =xlabel('Modulation filter f_{c} (Hz)','Fontsize',fnts);
                pos = get(h2,'pos'); % Read position [x y z]
                set(h2,'pos',pos+[50 0 0]) % Move label to right
            end
            %  set(axes,'OuterPosition',[0.05*sh, 0.15*sh, 3*sh, 1*sh])
            if idx_col == 1
                set(gca,'yTick',ytickmin:4:ytickmax);
                set(gca,'yTickLabel',ytickmin:4:ytickmax);
                %              ylabel('Envelope power [dB]');
                %makes a second axis
                %
            end
            if subplot_idx == 1
                h5 = title( ['SNR = ', num2str(SNRs(SNRidx)),' dB'],'Fontsize',16); % Create label
                pos = get(h5,'pos'); % Read position [x y z]
                set(h5,'pos',pos+[-6.5 -1 0]) % Move label to left
            end
        end
    end
    
    subplotspace('vertical',-21);
    subplotspace('horizontal',-24);
    
    
    for idx_col=1:2
        for idx_row = 1:2
            subplot_idx = (idx_row-1)*2+idx_col;
            le = legend(h(idx_row,idx_col), [num2str(midfreq(audFilter(subplot_idx))), ' Hz']);
            set(le,'Location',char(le_pos(idx_row,idx_col)));
            set(le,'box','off','fontsize',fnts);
            %                 legendshrink(0)
            
        end
    end
    
    ah=axes('position',get(gca,'position'),...
            'visible','off');
    le3 = legend(ah,h5([1 2 3],2,2), {'Clean speech','Noisy speech','Noise alone'});
    
    set(le3,'fontsize',10);
    
    pos = get(le3,'position'); % Read position [x y z]
    set(le3,'pos',pos+[0 0.15 0 0],'box','off') % Move label to right
    
end



