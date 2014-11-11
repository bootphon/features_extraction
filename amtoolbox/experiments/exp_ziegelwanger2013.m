function varargout=exp_ziegelwanger2013(varargin)
%EXP_ZIEGELWANGER2013   Figures from Ziegelwanger and Majdak (2013)
%   Usage: data = exp_ziegelwanger2013(flag)
%
%   EXP_ZIEGELWANGER2013(flags) reproduces figures of the paper from
%   Ziegelwanger and Majdak (2013).
%
%   Optional fields of output data structure:
%
%   The following flags can be specified:
%
%     'plot'    Plot the output of the experiment. This is the default.
%
%     'noplot'  Don't plot, print results in the console.
% 
%     'reload'	Reload previously calculated results. This is the default.
%
%     'recalc'	Recalculate results.
%
%     'fig2'    Reproduce Fig. 2:
%               
%               Left panel: 
%               Normalized HRIRs of NH64 (ARI database). Sound source was
%               placed 45° left in the horizontal plane.
%               Solid line: for the left ear
%               Dashed line: for the right ear
%               Vertical lines: Estimated TOAs
%               Arrows: Resulting ITDs from the corresponding TOAs
%               Circles, Red: Minimum-Phase Cross-Correlation Method
%               Triangle, Green: Time-Position of the HRIR-Maximum
%               Diamonds, Blue: Centroid of the HRIR
%               Squares, Magenta: Average Group Delay (1-5 kHz)
%               
%               Right panel: 
%               Estimated TOAs of NH64 (ARI database) in the horizontal
%               interaural plane.
%               Black: Minimum-Phase Cross-Correlation Method
%               Blue: Time-Position of the HRIR-Maximum
%               Green: Centroid of the HRIR
%               Red: Average Group Delay (1-5 kHz)
%
%     'fig3'    Reproduce Fig. 3:
%               
%               Left panel: 
%               Sagittal TOA deviations and averaged TOA variance for NH64
%               as function of the polar angle for all sagittal groups.
%               Dots, Blue: Sagittal TOA deviations
%               Line, Red: Averaged TOA variance
%               
%               Right panel: 
%               Estimated TOAs, detected outliers and outlier adjusted set
%               of TOAs for NH64 (ARI database) in the horizontal plane.
%               Line: Estimated TOAs
%               Triangles (down), Blue: Detected outliers for the azimuthal
%               slope criterion
%               Triangles (up), Blue: Detected outliers for the sagittal
%               variance criterion
%               Dots, Red: Outlier-adjusted set
%
%     'fig5'    Reproduce Fig. 5:
%               Model parameters (sphere radius, ear position) resulting
%               from fitting the on-axis model to HRTFs of a rigid sphere.
%               Squares: for the left ear
%               Diamonds: for the right ear
%               Dashed lines: set values used in the numerical HRTF
%               calculation
%
%     'fig6'    Reproduce Fig. 6:
%               TOA in interaural horizontal plane for the left ear HRTFs
%               of NH64.
%               Solid line, Black: On-axis model fitted to outlier-adjusted
%               set of TOAs
%               Circles, Red: Outlier-adjusted set of TOAs
%               Hexagrams, Blue: Detected outliers
%
%     'fig7'    Reproduce Fig. 7:
%               Model parameter (sphere radius) resulting from fitting the
%               on-axis model to HRTFs of human listeners. The listeners
%               are sorted by the ascending binaural average radius.
%               Blue: for the left ear
%               Green: for the right ear
%               Circles: ARI
%               Diamonds: CIPIC
%               Squares: LISTEN
%
%     'fig8'    Reproduce Fig. 8:
%               Relative TOAs for NH64 (ARI database) in the interaural
%               horizontal plane.
%               Dashed lines: for the right ear
%               Solid lines: for the right ear
%               Thin lines: TOAs estimated with the Minimum-phase
%               Cross-Correlation method
%               Thick lines: On-axis model fitted to the outlier-adjusted
%               sets of TOAs
%
%     'fig9'    Reproduce Fig. 9:
%               Model parameters (sphere radius) resulting from fitting the
%               on-axis model to HRTFs of an off-axis placed rigid sphere.
%               All other conventions as in Fig. 5.
%
%     'fig11'   Reproduce Fig. 11:
%               Model parameters (sphere radius, sphere center) resulting
%               from fitting the off-axis model to HRTFs of a rigid
%               sphere.
%               All other conventions as in Fig. 5.
%
%     'fig12'   Reproduce Fig. 12:
%               Model parameters (sphere radius, sphere center) resulting
%               from fitting the off-axis model to HRTFs of human
%               listeners.
%               All other conventions as in Fig. 7.
%
%   Examples:
%   ---------
%
%   To display Fig. 2, use :
%
%     exp_ziegelwanger2013('fig2');
%
%   To display Fig. 3, use :
%
%     exp_ziegelwanger2013('fig3');
%
%   To display Fig. 5, use :
%
%     exp_ziegelwanger2013('fig5');
%
%   To display Fig. 7, use :
%
%     exp_ziegelwanger2013('fig7');
%
%   To display Fig. 8, use :
%
%     exp_ziegelwanger2013('fig8');
%
%   To display Fig. 9, use :
%
%     exp_ziegelwanger2013('fig9');
%
%   To display Fig. 11, use :
%
%     exp_ziegelwanger2013('fig11');
%
%   To display Fig. 12, use :
%
%     exp_ziegelwanger2013('fig12');
%
%   See also: ziegelwanger2013, ziegelwanger2013onaxis,
%   ziegelwanger2013offaxis, data_ziegelwanger2013
%
%   References:
%     H. Ziegelwanger and P. Majdak. Continuous-direction model of the
%     time-of-arrival for head-related transfer funcitons. J. Acoust. Soc.
%     Am., submitted, 2013.
%     
%
%   Url: http://amtoolbox.sourceforge.net/doc//experiments/exp_ziegelwanger2013.php

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

% AUTHOR: Harald Ziegelwanger, Acoustics Research Institute, Vienna,
% Austria

%% ------ Check input options --------------------------------------------

  definput.flags.type = {'missingflag',...
    'fig2','fig3','fig5','fig6',...
    'fig7','fig8','fig9','fig11','fig12'};
  definput.flags.plot = {'plot','noplot'};
  definput.flags.results = {'reload','recalc'};

  % Parse input options
  [flags,kv]  = ltfatarghelper({},definput,varargin);
        
if flags.do_missingflag
  flagnames=[sprintf('%s, ',definput.flags.type{2:end-2}),...
             sprintf('%s or %s',definput.flags.type{end-1},definput.flags.type{end})];
  error('%s: You must specify one of the following flags: %s.',upper(mfilename),flagnames);
end;


%% Figure 2
if flags.do_fig2
    
    data=data_ziegelwanger2013('NH89');

    subplot(122)
    %---------------------------Threshold---------------------------
    temp=ziegelwanger2013(data.hM,data.meta,data.stimPar,1,0);
    MAX=temp.toa;

    %---------------------------Centroid----------------------------
    temp=ziegelwanger2013(data.hM,data.meta,data.stimPar,2,0);
    CTD=temp.toa;

    %---------------------------Groupdelay--------------------------
    temp=ziegelwanger2013(data.hM,data.meta,data.stimPar,3,0);
    AGD=temp.toa;

    %---------------------------Minimal-Phase-----------------------
    temp=ziegelwanger2013(data.hM,data.meta,data.stimPar,4,0);
    MCM=temp.toa;
    clear temp

    plotziegelwanger2013(MCM(:,1),4,[0 0 0]/255,0,1,1,data.meta,data.stimPar,'-',1);
    hold on
    plotziegelwanger2013(MAX(:,1),4,[0 0 80]/255,0,1,1,data.meta,data.stimPar,'--',1);
    plotziegelwanger2013(CTD(:,1),4,[50 220 50]/255,0,1,1,data.meta,data.stimPar,'-',1);
    plotziegelwanger2013(AGD(:,1),4,[250 80 80]/255,0,1,1,data.meta,data.stimPar,'--',1);
    xlim([-10 370])
    ylim([2.65 4.05])
    grid off
    legend(' MCM',' MAX',' CTD',' AGD','Location','NorthWest');
    legend boxoff
    xlabel('Azimuth (in deg) ')
    ylabel('TOA (ms) ')
%     set(gca,'FontSize',ticklabelsize,'LineWidth',bw,'LineWidth',bw);%,'YTick',[-0.8 -0.4 0 0.4 0.8]
    title('');
    
    subplot(121)
    time=(0:size(data.hM,1)-1)/data.stimPar.SamplingRate*1000;
    MAX=round(MAX);
    CTD=round(CTD);
    AGD=round(AGD);
    MCM=round(MCM);
    idx=ARI_FindPosition(data.meta,85,0);

    fprintf(['MAX: ITD is ' num2str(time(diff(MCM(idx,:),1,2))) ' ms\n'])
    fprintf(['CTD: ITD is ' num2str(time(diff(MAX(idx,:),1,2))) ' ms\n'])
    fprintf(['AGD: ITD is ' num2str(time(diff(CTD(idx,:),1,2))) ' ms\n'])
    fprintf(['MCM: ITD is ' num2str(time(diff(AGD(idx,:),1,2))) ' ms\n'])

    plot(time,data.hM(:,idx,1)/max(abs(data.hM(:,idx,1))),'k-');
    hold on
    plot(time,data.hM(:,idx,2)/max(abs(data.hM(:,idx,2))),'k--')
    h=stem([time(MCM(idx,1)) time(MCM(idx,1))],[-2 data.hM(MCM(idx,1),idx,1)/max(abs(data.hM(:,idx,1)))],'r-','BaseValue',-1);
    stem([time(CTD(idx,1)) time(CTD(idx,1))],[-2 data.hM(CTD(idx,1),idx,1)/max(abs(data.hM(:,idx,1)))],'g-','BaseValue',-1)
    stem([time(MAX(idx,1)) time(MAX(idx,1))],[-2 data.hM(MAX(idx,1),idx,1)/max(abs(data.hM(:,idx,1)))],'b-','BaseValue',-1)
    stem([time(AGD(idx,1)) time(AGD(idx,1))],[-2 data.hM(AGD(idx,1),idx,1)/max(abs(data.hM(:,idx,1)))],'m-','BaseValue',-1)
    stem([time(MCM(idx,2)) time(MCM(idx,2))],[-2 data.hM(MCM(idx,2),idx,2)/max(abs(data.hM(:,idx,2)))],'r-','BaseValue',-1)
    stem([time(CTD(idx,2)) time(CTD(idx,2))],[-2 data.hM(CTD(idx,2),idx,2)/max(abs(data.hM(:,idx,2)))],'g-','BaseValue',-1)
    stem([time(AGD(idx,2)) time(AGD(idx,2))],[-2 data.hM(AGD(idx,2),idx,2)/max(abs(data.hM(:,idx,2)))],'m-','BaseValue',-1)
    stem([time(MAX(idx,2)) time(MAX(idx,2))],[-2 data.hM(MAX(idx,2),idx,2)/max(abs(data.hM(:,idx,2)))],'b-','BaseValue',-1)
    plot(time(MAX(idx,1)),data.hM(MAX(idx,1),idx,1)/max(abs(data.hM(:,idx,1))),'b^','MarkerFaceColor','b')
    plot(time(MCM(idx,1)),data.hM(MCM(idx,1),idx,1)/max(abs(data.hM(:,idx,1))),'ro','MarkerFaceColor','r')
    plot(time(CTD(idx,1)),data.hM(CTD(idx,1),idx,1)/max(abs(data.hM(:,idx,1))),'gd','MarkerFaceColor','g')
    plot(time(AGD(idx,1)),data.hM(AGD(idx,1),idx,1)/max(abs(data.hM(:,idx,1))),'ms','MarkerFaceColor','m')
    plot(time(MAX(idx,2)),data.hM(MAX(idx,2),idx,2)/max(abs(data.hM(:,idx,2))),'b^','MarkerFaceColor','b')
    plot(time(MCM(idx,2)),data.hM(MCM(idx,2),idx,2)/max(abs(data.hM(:,idx,2))),'ro','MarkerFaceColor','r')
    plot(time(CTD(idx,2)),data.hM(CTD(idx,2),idx,2)/max(abs(data.hM(:,idx,2))),'gd','MarkerFaceColor','g')
    plot(time(AGD(idx,2)),data.hM(AGD(idx,2),idx,2)/max(abs(data.hM(:,idx,2))),'ms','MarkerFaceColor','m')
    xlim([2.4 4.1])
    ylim([-1.1 1.1])
    xlabel('Time (ms) ')
    ylabel('Amplitude ')
%     set(gca,'FontSize',ticklabelsize,'LineWidth',bw,'LineWidth',bw,'XTick',[2.5 3 3.5 4]);
    set(get(h,'Baseline'),'Visible','off')
end

%% Figure 3, Figure 6
if flags.do_fig3 || flags.do_fig6
        
    data=data_ziegelwanger2013('NH89');

    p0_onaxis=[[0.0875; pi/2; 0; 0.0001] [0.0875; -pi/2; 0; 0.0001]];
    p0_onaxis=transpose(p0_onaxis);
    p_onaxis=zeros(size(p0_onaxis));
    p0_offaxis=zeros(2,7);
    p_offaxis=p0_offaxis;

    toa=zeros(size(data.hM,2),size(data.hM,3));
    toaEst=zeros(size(data.hM,2),size(data.hM,3));
    indicator=zeros(size(data.hM,2),size(data.hM,3));
    indicator_hor=indicator;
    indicator_sag=indicator;
    data.meta.pos(:,8)=cumsum(ones(size(data.meta.pos,1),1));
    hM_min=ARI_MinimalPhase([data.hM; zeros(4096-size(data.hM,1),size(data.hM,2),size(data.hM,3))]);
    hM_min=hM_min(1:size(data.hM,1),:,:);
    for ii=1:size(data.hM,2)
        for jj=1:size(data.hM,3)
            if isnan(hM_min(1,ii,jj))
                hM_min(:,ii,jj)=ARI_MinimalPhase(data.hM(:,ii,jj));
            end
        end
    end
    corrcoeff=zeros(size(data.hM,2),size(data.hM,3));
    for ii=1:size(data.hM,2)
        for jj=1:size(data.hM,3)
            [c,lag]=xcorr(transpose(squeeze(data.hM(:,ii,jj))),transpose(squeeze(hM_min(:,ii,jj))),size(data.hM,1)-1,'none');
            [corrcoeff(ii,jj),idx]=max(abs(c));
            corrcoeff(ii,jj)=corrcoeff(ii,jj)/sum(data.hM(:,ii,jj).^2);
            toaEst(ii,jj)=lag(idx);
        end
    end
    
    for ch=1:size(data.hM,3)
        % Outlier detection: smooth TOA in horizontal planes
        epsilon=5;
        slope=zeros(size(data.hM,2),1);
        for ele=min(data.meta.pos(:,2)):epsilon:max(data.meta.pos(:,2)) %calculate slope for each elevation along azimut
            idx=find(data.meta.pos(:,2)>ele-epsilon/2 & data.meta.pos(:,2)<=ele+epsilon/2);
            if numel(idx)>1
                idx(length(idx)+1)=idx(1);
                slope(idx(1:end-1),1)=diff(toaEst(idx,ch))./abs(diff(data.meta.pos(idx,1)));
            end
        end
        sloperms=sqrt(sum(slope.^2)/length(slope));
        if sloperms<30/(length(find(data.meta.pos(:,2)==0))/2)
            sloperms=30/(length(find(data.meta.pos(:,2)==0))/2);
        end
        for ele=min(data.meta.pos(:,2)):epsilon:max(data.meta.pos(:,2))
            idx=find(data.meta.pos(:,2)>ele-epsilon/2 & data.meta.pos(:,2)<=ele+epsilon/2);
            for ii=1:length(idx)-1
                if abs(slope(idx(ii)))>sloperms
                    for jj=0:1
                        if ii+jj==0 || ii+jj==length(idx)
                            indicator_hor(idx(end),ch)=1;
                        else
                            indicator_hor(idx(mod(ii+jj,length(idx))),ch)=1;
                        end
                    end
                end
            end
            clear idx
        end
%         indicator2=indicator1(:,ch);

        % Outlier detection: constant TOA in sagittal planes
        epsilon=2;
        for ii=1:20
            sag_dev=zeros(size(data.hM,2),1);
            for lat=-90:epsilon:90
                idx=find(data.meta.pos(:,6)>lat-epsilon/2 & data.meta.pos(:,6)<=lat+epsilon/2 & indicator_hor(:,ch)==0);
                idx2=find(data.meta.pos(:,6)>lat-epsilon/2 & data.meta.pos(:,6)<=lat+epsilon/2 & indicator(:,ch)==0 & indicator_hor(:,ch)==0);
                if length(idx2)>2
                    sag_dev(idx,1)=toaEst(idx,ch)-mean(toaEst(idx2,ch));
                end
            end
            sag_var=sqrt(sum(sag_dev.^2)/length(sag_dev));
            if sag_var<2
                sag_var=2;
            end
            indicator(:,ch)=zeros(size(indicator,1),1);
            indicator_sag(:,ch)=zeros(size(indicator_sag,1),1);
            indicator_sag(abs(sag_dev)>sag_var,ch)=ones(length(find(abs(sag_dev)>sag_var)),1);
            indicator(abs(sag_dev)>sag_var | indicator_hor(:,ch)==1,ch)=ones(length(find(abs(sag_dev)>sag_var | indicator_hor(:,ch)==1)),1);
        end

        if flags.do_fig3 && ch==1 %Figure 3
            subplot(121)
            plot([-90; 270],[sag_var/data.stimPar.SamplingRate*1000000; sag_var/data.stimPar.SamplingRate*1000000],'r--');
            hold on
            plot(real(data.meta.pos(:,7)),abs(sag_dev)/data.stimPar.SamplingRate*1000000,'b.');
            xlim([-98 278])
            ylim([-5 max(abs(sag_dev)/data.stimPar.SamplingRate*1000000)+5])
            xlabel('Polar angle in degree')
            ylabel('Sagittal TOA deviation in µs')
            title('')
            set(gca,'XTick',[-90 0 90 180 270])
        end
        clear sag_dev; clear sag_var;

        if flags.do_fig3 && ch==1 %Figure 3
            subplot(122)
            plotziegelwanger2013(toaEst,4,'k',0,1,1,data.meta,data.stimPar,{'-'},1);
            hold on
            h=plotziegelwanger2013(indicator_sag.*toaEst,3,'w',0,1,1,data.meta,data.stimPar,{'^'},4);
            set(h,'MarkerFaceColor','w','MarkerEdgeColor','w');
            h=plotziegelwanger2013(indicator_hor.*toaEst,3,'w',0,1,1,data.meta,data.stimPar,{'v'},4);
            set(h,'MarkerFaceColor','w','MarkerEdgeColor','w');
            h=plotziegelwanger2013(indicator_sag.*toaEst,3,'b',0,1,1,data.meta,data.stimPar,{'^'},4);
            set(h,'LineWidth',2);
            h=plotziegelwanger2013(indicator_hor.*toaEst,3,'b',0,1,1,data.meta,data.stimPar,{'v'},4);
            set(h,'LineWidth',2);
            h=plotziegelwanger2013((-indicator+1).*toaEst,3,'r',0,1,1,data.meta,data.stimPar,{'o'},4);
            set(h,'MarkerFaceColor','r','MarkerEdgeColor','r');
            ylabel('TOA in ms')
            xlabel('Azimuth in degree')
            grid off
            xlim([-10 370])
            ylim([2.65 3.65])
            title('')
            set(gca,'YTick',[2.7 3 3.3 3.6])
        end
    end

    for ch=1:size(data.hM,3)
        p0_onaxis(ch,4)=min(toaEst(indicator(:,ch)==0,ch))/data.stimPar.SamplingRate;
        p0off_onaxis=[0.06 pi/4 pi/4 0.001];

        % Fit on-axis model to outlier adjusted set of estimated TOAs
        idx=find(indicator(:,ch)==0);
        x=data.meta.pos(idx,1:2)*pi/180;
        y=toaEst(idx,ch)/data.stimPar.SamplingRate;
        p_onaxis(ch,:)=lsqcurvefit(@ziegelwanger2013onaxis,p0_onaxis(ch,:),x,y,p0_onaxis(ch,:)-p0off_onaxis,p0_onaxis(ch,:)+p0off_onaxis,optimset('Display','off','TolFun',1e-6));
        toa(:,ch)=ziegelwanger2013onaxis(p_onaxis(ch,:),data.meta.pos(:,1:2)*pi/180)*data.stimPar.SamplingRate;
    end

    TolFun=[1e-5; 1e-6];
    % Fit off-axis model to outlier adjusted set of estimated TOAs
    for ii=1:size(TolFun,1)
        for ch=1:size(data.hM,3)
            idx=find(indicator(:,ch)==0);
            x=data.meta.pos(idx,1:2)*pi/180;
            y=toaEst(idx,ch)/data.stimPar.SamplingRate;
            p0_offaxis(ch,:)=[p0_onaxis(ch,1) 0 0 0 p0_onaxis(ch,4) p0_onaxis(ch,2) p0_onaxis(ch,3)];
            p0off_offaxis=[0.05 0.05 0.05 0.05 0.001 pi pi];
            p_offaxis(ch,:)=lsqcurvefit(@ziegelwanger2013offaxis,p0_offaxis(ch,:),x,y,p0_offaxis(ch,:)-p0off_offaxis,p0_offaxis(ch,:)+p0off_offaxis,optimset('Display','off','TolFun',TolFun(ii,1)));
            toa(:,ch)=ziegelwanger2013offaxis(p_offaxis(ch,:),data.meta.pos(:,1:2)*pi/180)*data.stimPar.SamplingRate;
        end
        if abs(diff(p_offaxis(:,1)))>0.003 || abs(diff(p_offaxis(:,3)))>0.003
            p_offaxis(:,[1 3])=p_offaxis([2 1],[1 3]);
            for ch=1:size(data.hM,3)
                idx=find(indicator(:,ch)==0);
                x=data.meta.pos(idx,1:2)*pi/180;
                y=toaEst(idx,ch)/data.stimPar.SamplingRate;
                p0_offaxis(ch,:)=[p_offaxis(ch,1) mean(p_offaxis(:,2)) p_offaxis(ch,3) mean(p_offaxis(:,4)) mean(p_offaxis(:,5)) p_offaxis(ch,6) p_offaxis(ch,7)];
                p0off_offaxis=[0.05 0.05 0.05 0.05 0.001 pi/2 pi/2];
                p_offaxis(ch,:)=lsqcurvefit(@ziegelwanger2013offaxis,p0_offaxis(ch,:),x,y,p0_offaxis(ch,:)-p0off_offaxis,p0_offaxis(ch,:)+p0off_offaxis,optimset('Display','off','TolFun',TolFun(ii,1)));
                toa(:,ch)=ziegelwanger2013offaxis(p_offaxis(ch,:),data.meta.pos(:,1:2)*pi/180)*data.stimPar.SamplingRate;
            end
        end
        if abs(diff(p_offaxis(:,1)))<0.003 && abs(diff(p_offaxis(:,2)))<0.003 && abs(diff(p_offaxis(:,3)))<0.003 && abs(diff(p_offaxis(:,4)))<0.003
            break
        end
    end

    if flags.do_fig6 %Figure 6
        h=plotziegelwanger2013(indicator.*toaEst,3,'b',0,1,1,data.meta,data.stimPar,{'^'},4);
        set(h,'LineWidth',2);
        hold on
        h=plotziegelwanger2013(indicator.*toaEst,3,'b',0,1,1,data.meta,data.stimPar,{'v'},4);
        set(h,'LineWidth',2);
        h=plotziegelwanger2013((-indicator+1).*toaEst,3,'r',0,1,1,data.meta,data.stimPar,{'o'},4);
        set(h,'MarkerFaceColor','r','MarkerEdgeColor','r');
        plotziegelwanger2013(toa,4,'k',0,1,1,data.meta,data.stimPar,{'-'},1);
        ylabel('TOA ms ')
        xlabel('Azimuth in degree')
        grid off
        xlim([-10 370])
        ylim([2.65 3.65])
        title('')
        set(gca,'YTick',[2.7 3 3.3 3.6])
    end
end

%% Figure 5
if flags.do_fig5

    sym='sdo'; %plot symbols
    clr=[0,0,255; 255,0,0; 255,255,67]/255; %plot colors
    meclr=[0,0,255; 255,0,0; 255,255,67]/255; %marker edge colors
    
    if flags.do_recalc
        data=data_ziegelwanger2013('SPHERE_ROT','recalc');
    else
        data=data_ziegelwanger2013('SPHERE_ROT');
    end
    
    % radii
    subplot(311)
    var=[squeeze(data.results.p_onaxis(1,1,:))*100 squeeze(data.results.p_onaxis(1,2,:))*100 data.radius(:)/10];
    err=abs([var(:,1) var(:,2)]-[var(:,3) var(:,3)]);
    err=reshape(err,numel(err),1);
    fprintf(['Radius: average err is ' num2str(mean(err)) ' cm\n']);
    fprintf(['        standard deviation is ' num2str(std(err)) ' cm\n']);
    for ch=1:size(data.results.p_onaxis,2)
        plot(var(:,ch),sym(ch),'MarkerEdgeColor',meclr(ch,:),'MarkerFaceColor',clr(ch,:));
        hold on
    end
    plot(var(:,3),'k--')
    clear var;
    ylabel('r in cm')

    %phi
    subplot(312)
    var=[squeeze(data.results.p_onaxis(2,1,:))/pi*180 squeeze(data.results.p_onaxis(2,2,:))/pi*180 -data.phi+ones(length(data.phi),1)*90 -data.phi-ones(length(data.phi),1)*90];
    err=abs([var(:,1) var(:,2)]-[var(:,3) var(:,4)]);
    err=reshape(err,numel(err),1);
    fprintf(['Phi: average err is ' num2str(mean(err)) 'deg\n']);
    fprintf(['     standard deviation is ' num2str(std(err)) 'deg\n']);
    for ch=1:size(data.results.p_onaxis,2)
        plot(var(:,ch),sym(ch),'MarkerEdgeColor',meclr(ch,:),'MarkerFaceColor',clr(ch,:));
        hold on
    end
    for ch=1:size(data.results.p_onaxis,2)
        plot(1:9,var(1:9,2+ch),'k--')
        plot(10:14,var(10:14,2+ch),'k--')
        plot(15:23,var(15:23,2+ch),'k--')
        plot(24:28,var(24:28,2+ch),'k--')
        plot(29:37,var(29:37,2+ch),'k--')
        plot(38:42,var(38:42,2+ch),'k--')
    end
    clear var;
    set(gca,'YTick',[-90 90])
    ylabel('\phi_e in deg')

    %theta
    subplot(313)
    var=[squeeze(data.results.p_onaxis(3,1,:))/pi*180 squeeze(data.results.p_onaxis(3,2,:))/pi*180 data.theta -data.theta];
    err=abs([var(:,1) var(:,2)]-[var(:,3) var(:,4)]);
    err=reshape(err,numel(err),1);
    fprintf(['Theta: average err is ' num2str(mean(err)) 'deg\n']);
    fprintf(['       standard deviation is ' num2str(std(err)) 'deg\n']);
    for ch=1:size(data.results.p_onaxis,2)
        plot(var(:,ch),sym(ch),'MarkerEdgeColor',meclr(ch,:),'MarkerFaceColor',clr(ch,:));
        hold on
    end
    for ch=1:size(data.results.p_onaxis,2)
        plot(1:9,var(1:9,2+ch),'k--')
        plot(10:14,var(10:14,2+ch),'k--')
        plot(15:23,var(15:23,2+ch),'k--')
        plot(24:28,var(24:28,2+ch),'k--')
        plot(29:37,var(29:37,2+ch),'k--')
        plot(38:42,var(38:42,2+ch),'k--')
    end
    clear var;
    ylabel('\theta_e in deg')
    xlabel('Condition')
end

%% Figure 7, Figure 12
if flags.do_fig7 || flags.do_fig12
    
    hrtf={'ARI','CIPIC','LISTEN'};
    sym='ods'; %plot symbols

    %-------------------------------Load Data----------------------------------
    for kk=1:length(hrtf)
        if flags.do_recalc
            data=data_ziegelwanger2013(hrtf{kk},'recalc');
        else
            data=data_ziegelwanger2013(hrtf{kk});
        end
        if kk==3
            data.results=data.results([1:27 29:end]);
        end
        temp1=zeros(size(data.results(1).meta.p_onaxis,1),size(data.results(1).meta.p_onaxis,2),length(data.results));
        temp3=zeros(size(data.results(1).meta.p_offaxis,1),size(data.results(1).meta.p_offaxis,2),length(data.results));
        temp4=zeros(length(data.results),4);
        temp6=zeros(length(data.results),4);
        temp7=zeros(length(data.results),4);
        for ii=1:length(data.results)
            temp1(:,:,ii)=data.results(ii).meta.p_onaxis;
            temp3(:,1:size(data.results(ii).meta.p_offaxis,2),ii)=data.results(ii).meta.p_offaxis;
            temp4(ii,:)=[data.results(ii).meta.performance(1).outliers data.results(ii).meta.performance(2).outliers data.results(ii).meta.performance(3).outliers data.results(ii).meta.performance(4).outliers];
            if isfield(data.results(ii).meta.performance(1)','outliersl')
                temp6(ii,:)=[data.results(ii).meta.performance(1).outliersl data.results(ii).meta.performance(2).outliersl data.results(ii).meta.performance(3).outliersl data.results(ii).meta.performance(4).outliersl];
                temp7(ii,:)=[data.results(ii).meta.performance(1).outliersr data.results(ii).meta.performance(2).outliersr data.results(ii).meta.performance(3).outliersr data.results(ii).meta.performance(4).outliersr];
            end
        end
        p_onaxis{kk}=temp1;
        p_offaxis{kk}=temp3;
        outliers{kk}=temp4;
        outliersear{kk}=[temp6; temp7];
    end
    
    if flags.do_fig7 %Figure 7
        fprintf('On-Axis Model:\n')
        fprintf(['Average Radius: ' num2str(mean([mean(squeeze(p_onaxis{1}(1,:,:)*100)) mean(squeeze(p_onaxis{2}(1,:,:)*100)) mean(squeeze(p_onaxis{3}(1,:,:)*100))])) ' cm\n'])
        fprintf(['Average Radius Difference: ' num2str(mean([mean(abs(diff(squeeze(p_onaxis{1}(1,:,:)*100)))) mean(abs(diff(squeeze(p_onaxis{2}(1,:,:)*100)))) mean(abs(diff(squeeze(p_onaxis{3}(1,:,:)*100))))])) 'cm\n'])
        fprintf(['Maximum Radius Difference: ' num2str(max([max(abs(diff(squeeze(p_onaxis{1}(1,:,:)*100)))) max(abs(diff(squeeze(p_onaxis{2}(1,:,:)*100)))) max(abs(diff(squeeze(p_onaxis{3}(1,:,:)*100))))])) ' cm\n'])
        temp=1;
        for kk=1:length(hrtf)
            var(temp:temp+size(p_onaxis{kk},3)-1,:)=[squeeze(mean(p_onaxis{kk}(1,:,:)*100,2)) kk*ones(size(p_onaxis{kk},3),1) transpose(1:size(p_onaxis{kk},3))];
            varl(temp:temp+size(p_onaxis{kk},3)-1,:)=[squeeze(p_onaxis{kk}(1,1,:)*100) kk*ones(size(p_onaxis{kk},3),1) transpose(1:size(p_onaxis{kk},3))];
            varr(temp:temp+size(p_onaxis{kk},3)-1,:)=[squeeze(p_onaxis{kk}(1,2,:)*100) kk*ones(size(p_onaxis{kk},3),1) transpose(1:size(p_onaxis{kk},3))];
            temp=size(var,1)+1;
        end
        [~,idx]=sort(var(:,1));
        var=var(idx,:);
        varl=varl(idx,:);
        varr=varr(idx,:);
        var(:,3)=transpose(1:size(var,1));
        varl(:,3)=transpose(1:size(var,1));
        varr(:,3)=transpose(1:size(var,1));
        for ii=1:size(varl,1)
            stm=stem(ii,varl(ii,1),'--k','BaseValue',varr(ii,1));
            hold on
        end
        baseline_handle = get(stm,'BaseLine');
        set(baseline_handle,'LineStyle','none')
        for kk=1:length(hrtf)
            h{kk}=plot(varl(varl(:,2)==kk,3),varl(varl(:,2)==kk,1),sym(kk),'MarkerEdgeColor','b','MarkerFaceColor','b');
            plot(varr(varr(:,2)==kk,3),varr(varr(:,2)==kk,1),sym(kk),'MarkerEdgeColor','g','MarkerFaceColor','g');
        end
        clear var varl varr stm;
        xlabel('Listeners ')
        ylabel('r in cm')
        xlim([-1.5 temp+1.5])
        ylim([4.1 12+0.7])
        legend([h{1},h{2},h{3}],'ARI','CIPIC','LISTEN','Location','NorthWest');
        legend boxoff
    end
    
    if flags.do_fig12 %Figure12
        %radii
        subplot(411)
        fprintf('Off-Axis Model:\n')
        fprintf(['Radius (Avg): ' num2str(mean([mean(squeeze(p_offaxis{1}(1,:,:)*100)) mean(squeeze(p_offaxis{2}(1,:,:)*100)) mean(squeeze(p_offaxis{3}(1,:,:)*100))])) ' cm\n'])
        fprintf(['Radius (Std): ' num2str(std([reshape(p_offaxis{1}(1,:,:)*100,1,numel(p_offaxis{1}(1,:,:))) reshape(p_offaxis{2}(1,:,:)*100,1,numel(p_offaxis{2}(1,:,:))) reshape(p_offaxis{3}(1,:,:)*100,1,numel(p_offaxis{3}(1,:,:)))])) ' cm\n'])
        fprintf(['Radius Difference (Avg): ' num2str(mean([mean(abs(diff(squeeze(p_offaxis{1}(1,:,:)*100)))) mean(abs(diff(squeeze(p_offaxis{2}(1,:,:)*100)))) mean(abs(diff(squeeze(p_offaxis{3}(1,:,:)*100))))])) 'cm\n'])
        fprintf(['Radius Difference (Std): ' num2str(std([abs(diff(squeeze(p_offaxis{1}(1,:,:)*100))) abs(diff(squeeze(p_offaxis{2}(1,:,:)*100))) abs(diff(squeeze(p_offaxis{3}(1,:,:)*100)))])) 'cm\n'])
        fprintf(['Radius Difference (Max): ' num2str(max([max(abs(diff(squeeze(p_offaxis{1}(1,:,:)*100)))) max(abs(diff(squeeze(p_offaxis{2}(1,:,:)*100)))) max(abs(diff(squeeze(p_offaxis{3}(1,:,:)*100))))])) ' cm\n'])
        temp=1;
        for kk=1:length(hrtf)
            var(temp:temp+size(p_onaxis{kk},3)-1,:)=[squeeze(mean(p_offaxis{kk}(1,:,:)*100,2)) kk*ones(size(p_onaxis{kk},3),1) transpose(1:size(p_onaxis{kk},3))];
            varl(temp:temp+size(p_onaxis{kk},3)-1,:)=[squeeze(p_offaxis{kk}(1,1,:)*100) kk*ones(size(p_onaxis{kk},3),1) transpose(1:size(p_onaxis{kk},3))];
            varr(temp:temp+size(p_onaxis{kk},3)-1,:)=[squeeze(p_offaxis{kk}(1,2,:)*100) kk*ones(size(p_onaxis{kk},3),1) transpose(1:size(p_onaxis{kk},3))];
            temp=size(var,1)+1;
        end
        [~,idx]=sort(var(:,1));
        var=var(idx,:);
        varl=varl(idx,:);
        varr=varr(idx,:);
        var(:,3)=transpose(1:size(var,1));
        varl(:,3)=transpose(1:size(var,1));
        varr(:,3)=transpose(1:size(var,1));
        for ii=1:size(varl,1)
            stm=stem(ii,varl(ii,1),'--k','BaseValue',varr(ii,1));
            hold on
        end
        baseline_handle = get(stm,'BaseLine');
        set(baseline_handle,'LineStyle','none')
        for kk=1:length(hrtf)
            h{kk}=plot(varl(varl(:,2)==kk,3),varl(varl(:,2)==kk,1),sym(kk),'MarkerEdgeColor','b','MarkerFaceColor','b');
            plot(varr(varr(:,2)==kk,3),varr(varr(:,2)==kk,1),sym(kk),'MarkerEdgeColor','g','MarkerFaceColor','g');
        end
        clear var varl varr;
        ylabel('r in cm')
        xlim([-1.5 temp+1.5])
        ylim([4.1 12+0.7])
        legend([h{1},h{2},h{3}],'ARI','CIPIC','LISTEN','Location','NorthWest');
        legend boxoff

        %lateral displacements
        % xM
        subplot(412)
        fprintf(['Average xM: ' num2str(mean([mean(abs(squeeze(p_offaxis{1}(2,:,:)*100))) mean(abs(squeeze(p_offaxis{2}(2,:,:)*100))) mean(abs(squeeze(p_offaxis{3}(2,:,:)*100)))])) ' cm\n'])
        fprintf(['Average xM Difference: ' num2str(mean([mean(abs(diff(squeeze(p_offaxis{1}(2,:,:)*100)))) mean(abs(diff(squeeze(p_offaxis{2}(2,:,:)*100)))) mean(abs(diff(squeeze(p_offaxis{3}(2,:,:)*100))))])) 'cm\n'])
        temp=1;
        for kk=1:length(hrtf)
            var(temp:temp+size(p_onaxis{kk},3)-1,:)=[squeeze(mean(p_offaxis{kk}(2,:,:)*100,2)) kk*ones(size(p_onaxis{kk},3),1) transpose(1:size(p_onaxis{kk},3))];
            varl(temp:temp+size(p_onaxis{kk},3)-1,:)=[squeeze(p_offaxis{kk}(2,1,:)*100) kk*ones(size(p_onaxis{kk},3),1) transpose(1:size(p_onaxis{kk},3))];
            varr(temp:temp+size(p_onaxis{kk},3)-1,:)=[squeeze(p_offaxis{kk}(2,2,:)*100) kk*ones(size(p_onaxis{kk},3),1) transpose(1:size(p_onaxis{kk},3))];
            temp=size(var,1)+1;
        end
        var=var(idx,:);
        varl=varl(idx,:);
        varr=varr(idx,:);
        var(:,3)=transpose(1:size(var,1));
        varl(:,3)=transpose(1:size(var,1));
        varr(:,3)=transpose(1:size(var,1));
        for ii=1:size(varl,1)
            stm=stem(ii,varl(ii,1),'--k','BaseValue',varr(ii,1));
            hold on
        end
        baseline_handle = get(stm,'BaseLine');
        set(baseline_handle,'LineStyle','none')
        for kk=1:length(hrtf)
            h{kk}=plot(varl(varl(:,2)==kk,3),varl(varl(:,2)==kk,1),sym(kk),'MarkerEdgeColor','b','MarkerFaceColor','b');
            plot(varr(varr(:,2)==kk,3),varr(varr(:,2)==kk,1),sym(kk),'MarkerEdgeColor','g','MarkerFaceColor','g');
        end
        clear var varl varr;
        ylabel('x_M in cm')
        xlim([-1.5 temp+1.5])
        ylim([-4.5 4.5])

        % yM
        subplot(413)
        fprintf(['yM (Avg): ' num2str(mean([mean(abs(squeeze(p_offaxis{1}(3,:,:)*100))) mean(abs(squeeze(p_offaxis{2}(3,:,:)*100))) mean(abs(squeeze(p_offaxis{3}(3,:,:)*100)))])) ' cm\n'])
        fprintf(['yM (Std): ' num2str(std([reshape(p_offaxis{1}(3,:,:)*100,1,numel(p_offaxis{1}(3,:,:))) reshape(p_offaxis{2}(3,:,:)*100,1,numel(p_offaxis{2}(3,:,:))) reshape(p_offaxis{3}(3,:,:)*100,1,numel(p_offaxis{3}(3,:,:)))])) ' cm\n'])
        fprintf(['yM Difference (Avg): ' num2str(mean([mean(abs(diff(squeeze(p_offaxis{1}(3,:,:)*100)))) mean(abs(diff(squeeze(p_offaxis{2}(3,:,:)*100)))) mean(abs(diff(squeeze(p_offaxis{3}(3,:,:)*100))))])) 'cm\n'])
        fprintf(['yM Difference (Std): ' num2str(std([abs(diff(squeeze(p_offaxis{1}(3,:,:)*100))) abs(diff(squeeze(p_offaxis{2}(3,:,:)*100))) abs(diff(squeeze(p_offaxis{3}(3,:,:)*100)))])) 'cm\n'])
        temp=1;
        for kk=1:length(hrtf)
            var(temp:temp+size(p_onaxis{kk},3)-1,:)=[squeeze(mean(p_offaxis{kk}(3,:,:)*100,2)) kk*ones(size(p_onaxis{kk},3),1) transpose(1:size(p_onaxis{kk},3))];
            varl(temp:temp+size(p_onaxis{kk},3)-1,:)=[squeeze(p_offaxis{kk}(3,1,:)*100) kk*ones(size(p_onaxis{kk},3),1) transpose(1:size(p_onaxis{kk},3))];
            varr(temp:temp+size(p_onaxis{kk},3)-1,:)=[squeeze(p_offaxis{kk}(3,2,:)*100) kk*ones(size(p_onaxis{kk},3),1) transpose(1:size(p_onaxis{kk},3))];
            temp=size(var,1)+1;
        end
        var=var(idx,:);
        varl=varl(idx,:);
        varr=varr(idx,:);
        var(:,3)=transpose(1:size(var,1));
        varl(:,3)=transpose(1:size(var,1));
        varr(:,3)=transpose(1:size(var,1));
        for ii=1:size(varl,1)
            stm=stem(ii,varl(ii,1),'--k','BaseValue',varr(ii,1));
            hold on
        end
        baseline_handle = get(stm,'BaseLine');
        set(baseline_handle,'LineStyle','none')
        for kk=1:length(hrtf)
            h{kk}=plot(varl(varl(:,2)==kk,3),varl(varl(:,2)==kk,1),sym(kk),'MarkerEdgeColor','b','MarkerFaceColor','b');
            plot(varr(varr(:,2)==kk,3),varr(varr(:,2)==kk,1),sym(kk),'MarkerEdgeColor','g','MarkerFaceColor','g');
        end
        clear var;
        ylabel('y_M in cm')
        xlim([-1.5 temp+1.5])
        ylim([-3.5 4.5])

        % zM
        subplot(414)
        fprintf(['Average zM: ' num2str(mean([mean(abs(squeeze(p_offaxis{1}(4,:,:)*100))) mean(abs(squeeze(p_offaxis{2}(4,:,:)*100))) mean(abs(squeeze(p_offaxis{3}(4,:,:)*100)))])) ' cm\n'])
        fprintf(['Average zM Difference: ' num2str(mean([mean(abs(diff(squeeze(p_offaxis{1}(4,:,:)*100)))) mean(abs(diff(squeeze(p_offaxis{2}(4,:,:)*100)))) mean(abs(diff(squeeze(p_offaxis{3}(4,:,:)*100))))])) 'cm\n'])
        temp=1;
        for kk=1:length(hrtf)
            var(temp:temp+size(p_onaxis{kk},3)-1,:)=[squeeze(mean(p_offaxis{kk}(4,:,:)*100,2)) kk*ones(size(p_onaxis{kk},3),1) transpose(1:size(p_onaxis{kk},3))];
            varl(temp:temp+size(p_onaxis{kk},3)-1,:)=[squeeze(p_offaxis{kk}(4,1,:)*100) kk*ones(size(p_onaxis{kk},3),1) transpose(1:size(p_onaxis{kk},3))];
            varr(temp:temp+size(p_onaxis{kk},3)-1,:)=[squeeze(p_offaxis{kk}(4,2,:)*100) kk*ones(size(p_onaxis{kk},3),1) transpose(1:size(p_onaxis{kk},3))];
            temp=size(var,1)+1;
        end
        var=var(idx,:);
        varl=varl(idx,:);
        varr=varr(idx,:);
        var(:,3)=transpose(1:size(var,1));
        varl(:,3)=transpose(1:size(var,1));
        varr(:,3)=transpose(1:size(var,1));
        for ii=1:size(varl,1)
            stm=stem(ii,varl(ii,1),'--k','BaseValue',varr(ii,1));
            hold on
        end
        baseline_handle = get(stm,'BaseLine');
        set(baseline_handle,'LineStyle','none')
        for kk=1:length(hrtf)
            h{kk}=plot(varl(varl(:,2)==kk,3),varl(varl(:,2)==kk,1),sym(kk),'MarkerEdgeColor','b','MarkerFaceColor','b');
            plot(varr(varr(:,2)==kk,3),varr(varr(:,2)==kk,1),sym(kk),'MarkerEdgeColor','g','MarkerFaceColor','g');
        end
        clear var;
        xlabel('Listeners')
        ylabel('z_M in cm')
        xlim([-1.5 temp+1.5])
        ylim([-4.5 6.5])
    end
    
end

%% Figure 8
if flags.do_fig8
    
    data=data_ziegelwanger2013('NH89');
    
    temp=ziegelwanger2013(data.hM,data.meta,data.stimPar,4,1);
    data.meta.toa5=temp.toa;
    fprintf(['Radii for left and right ear: ' num2str(temp.p_onaxis(1,:)*100) ' cm\n'])
    temp=ziegelwanger2013(data.hM,data.meta,data.stimPar,4,0,0);
    data.meta.toa6=temp.toa;
    fprintf(['Maximum TOA difference left: ' num2str((max(data.meta.toa5(:,1))-min(data.meta.toa5(:,1)))/data.stimPar.SamplingRate*1000) ' ms\n'])
    fprintf(['Maximum TOA difference right: ' num2str((max(data.meta.toa5(:,2))-min(data.meta.toa5(:,2)))/data.stimPar.SamplingRate*1000) ' ms\n'])

    plotziegelwanger2013(data.meta.toa6-min(min(data.meta.toa5)),4,'k',0,1,1,data.meta,data.stimPar,'--',1);
    plotziegelwanger2013(data.meta.toa5-min(min(data.meta.toa5)),4,'k',0,1,1,data.meta,data.stimPar,'--',2);
    plotziegelwanger2013(data.meta.toa6-min(min(data.meta.toa5)),4,'k',0,2,1,data.meta,data.stimPar,'-',1);
    plotziegelwanger2013(data.meta.toa5-min(min(data.meta.toa5)),4,'k',0,2,1,data.meta,data.stimPar,'-',2);

    xlim([-5 365])
    ylim([-0.05 0.95])
    grid off
    xlabel('Azimuth in degree')
    ylabel('Relative TOA in ms');
    title('')
    
end

%% Figure 9, Figure 11
if flags.do_fig9 || flags.do_fig11
    
    sym='sdo'; %plot symbols
    clr=[0,0,255; 255,0,0; 255,255,67]/255; %plot colors
    meclr=[0,0,255; 255,0,0; 255,255,67]/255; %marker edge colors
    
    if flags.do_recalc
        data=data_ziegelwanger2013('SPHERE_DIS','recalc');
    else
        data=data_ziegelwanger2013('SPHERE_DIS');
    end
    
    if flags.do_fig9 %Figure 9
        p1=data.results.p_onaxis(:,:,[1:3 length(data.xM)/3+1:length(data.xM)/3+3 length(data.xM)/3*2+1:length(data.xM)/3*2+3]);
        r1=data.radius([1:3 length(data.xM)/3+1:length(data.xM)/3+3 length(data.xM)/3*2+1:length(data.xM)/3*2+3]);
        yM1=data.yM([1:3 length(data.xM)/3+1:length(data.xM)/3+3 length(data.xM)/3*2+1:length(data.xM)/3*2+3]);
        
        %radii
        subplot(211)
%         ymax2=round(max(max(squeeze(p1(1,:,:)*100))))+1;
        var=[squeeze(p1(1,1,:))*100 squeeze(p1(1,2,:))*100 r1/10];
        for ch=1:size(p1,2)
            plot(var(:,ch),sym(ch),'MarkerEdgeColor',meclr(ch,:),'MarkerFaceColor',clr(ch,:));
            hold on
        end
        plot(r1/10,'k--')
        clear var;
        ylabel('r in cm')

        %yM
        subplot(212)
        plot(-yM1*100,'k--')
        clear var;
        xlabel('Condition')
        ylabel('y_M in cm ')
    end
    
    if flags.do_fig11 %Figure 11
        center=[data.xM(1:length(data.xM)/3) data.yM(1:length(data.yM)/3) data.zM(1:length(data.zM)/3)];
        [~,idx3]=sort(squeeze(center(:,3)));
        [~,idx2]=sort(squeeze(center(:,1)));
        [~,idx1]=sort(squeeze(center(:,2)));
        idx=idx3(idx2(idx1));
        idx=[idx; idx+length(data.xM)/3; idx+length(data.xM)/3*2];
        data.radius=data.radius(idx);
        p_offaxis=data.results.p_offaxis(:,:,idx);
        data.xM=data.xM(idx);
        data.yM=data.yM(idx);
        data.zM=data.zM(idx);

        %radii
        subplot(411)
        ymax2=round(max(max(squeeze(p_offaxis(1,:,:)*100))))+1;
        var=[squeeze(p_offaxis(1,1,:))*100 squeeze(p_offaxis(1,2,:))*100 data.radius/10];
        err=abs([var(:,1) var(:,2)]-[var(:,3) var(:,3)]);
        err=reshape(err,numel(err),1);
        temp1=err;
        fprintf(['Radius: average err is ' num2str(mean(err)) ' cm\n']);
        fprintf(['        standard deviation is ' num2str(std(err)) ' cm\n']);
        fprintf(['        maximum is ' num2str(max(abs(var(:,1)-var(:,2)))) ' cm\n']);
        fprintf(['        average is ' num2str(mean(abs(var(:,1)-var(:,2)))) ' cm\n']);
        for ch=1:size(p_offaxis,2)
            plot(var(:,ch),sym(ch),'MarkerEdgeColor',meclr(ch,:),'MarkerFaceColor',clr(ch,:));
            hold on
        end
        plot(var(:,3),'k--')
        ylabel('r in cm')
        xlim([0 size(var,1)+1])
        ylim([4.5 ymax2])
        clear var;

        %xM
        subplot(412)
        var=[squeeze(p_offaxis(2,1,:))*100 squeeze(p_offaxis(2,2,:))*100 -data.xM*100];
        err=abs([var(:,1) var(:,2)]-[var(:,3) var(:,3)]);
        err=reshape(err,numel(err),1);
        fprintf(['xM: average err is ' num2str(mean(err)) ' cm\n']);
        fprintf(['    standard deviation is ' num2str(std(err)) ' cm\n']);
        fprintf(['    maximum is ' num2str(max(abs(var(:,1)-var(:,2)))) ' cm\n']);
        fprintf(['    average is ' num2str(mean(abs(var(:,1)-var(:,2)))) ' cm\n']);
        for ch=1:size(p_offaxis,2)
            plot(var(:,ch),sym(ch),'MarkerEdgeColor',meclr(ch,:),'MarkerFaceColor',clr(ch,:));
            hold on
        end  
        plot(var(:,3),'k--')
        ylabel('x_M in cm ')
        xlabel([0 size(var,1)+1])
        ylim([-2.5 0.5])
        clear var;

        %yM
        subplot(413)
        var=[squeeze(p_offaxis(3,1,:))*100 squeeze(p_offaxis(3,2,:))*100 -data.yM*100];
        err=abs([var(:,1) var(:,2)]-[var(:,3) var(:,3)]);
        err=reshape(err,numel(err),1);
        temp1=[temp1; err];
        fprintf(['yM: average err is ' num2str(mean(err)) ' cm\n']);
        fprintf(['    standard deviation is ' num2str(std(err)) ' cm\n']);
        fprintf(['    maximum is ' num2str(max(abs(var(:,1)-var(:,2)))) ' cm\n']);
        fprintf(['    average is ' num2str(mean(abs(var(:,1)-var(:,2)))) ' cm\n']);
        for ch=1:size(p_offaxis,2)
            plot(var(:,ch),sym(ch),'MarkerEdgeColor',meclr(ch,:),'MarkerFaceColor',clr(ch,:));
            hold on
        end
        plot(var(:,3),'k--')
        ylabel('y_M in cm ')
        xlim([0 size(var,1)+1])
        ylim([-0.5 2.5])
        clear var;

        %zM
        subplot(414)
        var=[squeeze(p_offaxis(4,1,:))*100 squeeze(p_offaxis(4,2,:))*100 -data.zM*100];
        err=abs([var(:,1) var(:,2)]-[var(:,3) var(:,3)]);
        err=reshape(err,numel(err),1);
        temp1=[temp1; err];
        fprintf(['zM: average err is ' num2str(mean(err)) ' cm\n']);
        fprintf(['    standard deviation is ' num2str(std(err)) ' cm\n']);
        fprintf(['    maximum is ' num2str(max(abs(var(:,1)-var(:,2)))) ' cm\n']);
        fprintf(['    average is ' num2str(mean(abs(var(:,1)-var(:,2)))) ' cm\n']);
        for ch=1:size(p_offaxis,2)
            plot(var(:,ch),sym(ch),'MarkerEdgeColor',meclr(ch,:),'MarkerFaceColor',clr(ch,:));
            hold on
        end
        plot(var([1 3],3),'k--')
        plot(var([2 4],3),'k--')
        set(gca,'YTick',[-1 0])
        xlabel('Condition')
        ylabel('z_M in cm')
        xlim([0 size(var,1)+1])
        ylim([-1.5 0.5])
        clear var;

        
        fprintf(['offset: average err is ' num2str(mean(temp1)) ' cm\n']);
        fprintf(['        standard deviation is ' num2str(std(temp1)) ' cm\n']);
        fprintf(['        maximum is ' num2str(max(abs(temp1))) ' cm\n']);
    end
end

end

function idx=ARI_FindPosition(meta,azimuth,elevation)
    psi=sin(elevation/180*pi).*sin(meta.pos(:,2)/180*pi) + ...
        cos(elevation/180*pi).*cos(meta.pos(:,2)/180*pi).*...
        cos(azimuth/180*pi-meta.pos(:,1)/180*pi);
    [~,idx]=min(acos(psi));
end

function out=ARI_MinimalPhase(in)
    n=size(in,1);
    itnr=size(in,2);
    rec=size(in,3);
    out=zeros(size(in));

    for jj=1:rec
        for ii=1:itnr
            h=squeeze(in(:,ii,jj));
            % decompose signal
            amp1=abs(fft(h));

            % transform
            amp2=amp1;
            an2u=-imag(hilbert(log(amp1))); % minimal phase

            % reconstruct signal from amp2 and an2u
            % build a symmetrical phase 
            an2u=an2u(1:floor(n/2)+1);
            an2u=[an2u; -flipud(an2u(2:end+mod(n,2)-1))];
            an2=an2u-round(an2u/2/pi)*2*pi;  % wrap around +/-pi: wrap(x)=x-round(x/2/pi)*2*pi
            % amplitude
            amp2=amp2(1:floor(n/2)+1);
            amp2=[amp2; flipud(amp2(2:end+mod(n,2)-1))];
            % back to time domain
            h2=real(ifft(amp2.*exp(1i*an2)));
            out(:,ii,jj)=h2;
        end
    end
end
