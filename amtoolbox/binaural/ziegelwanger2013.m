function meta=ziegelwanger2013(hM,meta,stimPar,method,correct,p0_onaxis)
%ZIEGELWANGER2013 Time of arrival estimates
%   usage: meta=ziegelwanger2013(hM,meta,stimPar,method,correct,p0_onaxis) 
%
%   Input:
%       hM: data matrix with impulse respnoses (IR): 
%           dim 1: time in samples
%           dim 2: each IR
%           dim 3: each record channel
%
%       meta:
% 
%       stimPar:
% 
%       method (optional): select one of the estimation methods
%           1: Threshold-Detection
%           2: Centroid of squared IR
%           3: Mean Groupdelay
%           4: Minimal-Phase Cross-Correlation (Max) (default)
%           5: Minimal-Phase Cross-Correlation (Centroid)
%           6: Zero-Crossing
%
%       correct (optional): correct estimated toa, using geometrical TOA-Model
%           0:
%           1: Correct TOA (default)
%
%       p0_onaxis (optional): startvalues for lsqcurvefit
%           dim 1: [sphere-radius in m,
%                 azimut of ear in radiants,
%                 elevation of ear in radiants, 
%                 direction-independent delay in seconds]
%           dim 2: each record channel
% 
%   Output:
%       meta.toa: data matrix with time of arrival (TOA) for each impulse response (IR):
%           dim 1: each toa in samples
%           dim 2: each record channel
%       meta.p_onaxis: estimated on-axis model-parameters
%           dim 1: [sphere-radius in m,
%                 azimut of ear in radiants,
%                 elevation of ear in radiants,
%                 direction-independent delay in seconds]
%           dim 2: each record channel
%       meta.p_offaxis: estimated off-axis model-parameters
%           dim 1: [sphere-radius in m,
%                 xM in m,
%                 yM in m,
%                 zM in m,
%                 direction-independent delay in seconds,
%                 channel (starting at 1),
%                 azimut of ear in radiants,
%                 elevation of ear in radiants]
%           dim 2: each record channel
%
%   Estimates the Time-of-Arrival for each column in input data hM and corrects 
%   the results with a geometrical model of the head.
%
%   Examples:
%   ---------
% 
%   To calculate the model parameters for the on-axis time-of-arrival model
%   (p_onaxis) and for the off-axis time-of-arrival model (p_offaxis) for a
%   given HRTF set (hM,meta,stimPar) with the minimum-phase
%   cross-correlation method, use:
%
%       meta=ziegelwanger2013(hM,meta,stimPar,4,1);
%
%   See also: ziegelwanger2013onaxis, ziegelwanger2013offaxis,
%   data_ziegelwanger2013, exp_ziegelwanger2013
%
%   References:
%     H. Ziegelwanger and P. Majdak. Continuous-direction model of the
%     time-of-arrival for head-related transfer funcitons. J. Acoust. Soc.
%     Am., submitted, 2013.
%     
%
%   Url: http://amtoolbox.sourceforge.net/doc//binaural/ziegelwanger2013.php

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

%% ----------------------------check variables-----------------------------
if ~exist('method','var')
    method=4;
else if isempty(method)
        method=4;
    end
end

if ~exist('correct','var')
    correct=1;
else if isempty(correct)
        correct=1;
    end
end

if ~exist('p0_onaxis','var')
    p0_onaxis=[[0.0875; pi/2; 0; 0] [0.0875; -pi/2; 0; 0]];
end

%% -------------------------initialize variables---------------------------
p0_onaxis=transpose(p0_onaxis);
p_onaxis=zeros(size(p0_onaxis));
p0_offaxis=zeros(2,7);
p_offaxis=p0_offaxis;

toa=zeros(size(hM,2),size(hM,3));
toaEst=zeros(size(hM,2),size(hM,3));
indicator=zeros(size(hM,2),size(hM,3));
indicator_hor=indicator;
indicator_sag=indicator;
meta.pos(:,8)=cumsum(ones(size(meta.pos,1),1));
hM_min=ARI_MinimalPhase([hM; zeros(4096-size(hM,1),size(hM,2),size(hM,3))]);
hM_min=hM_min(1:size(hM,1),:,:);
for ii=1:size(hM,2)
    for jj=1:size(hM,3)
        if isnan(hM_min(1,ii,jj))
            hM_min(:,ii,jj)=ARI_MinimalPhase(hM(:,ii,jj));
        end
    end
end

%% -----------------------estimate time-of-arrival-------------------------
switch method
    case 1 %---------------------------Threshold---------------------------
        for ii=1:size(hM,2)
            for jj=1:size(hM,3)
                toaEst(ii,jj)=find(abs(hM(:,ii,jj))==max(abs(hM(:,ii,jj))),1);
            end
        end
    case 2 %---------------------------Centroid----------------------------
        for ii=1:size(hM,2)
            for jj=1:size(hM,3)
                toaEst(ii,jj)=find(cumsum(hM(:,ii,jj).^2)>(sum(hM(:,ii,jj).^2)/2),1);
            end
        end
    case 3 %---------------------------Groupdelay--------------------------
        for ii=1:size(hM,2)
            for jj=1:size(hM,3)
                [Gd,F]=grpdelay(transpose(double(hM(:,ii,jj))),1,size(hM,1),stimPar.SamplingRate);
                toaEst(ii,jj)=median(Gd(find(F>500):find(F>2000)));
            end
        end
    case 4 %---------------------------Minimal-Phase-----------------------
        corrcoeff=zeros(size(hM,2),size(hM,3));
        for ii=1:size(hM,2)
            for jj=1:size(hM,3)
                [c,lag]=xcorr(transpose(squeeze(hM(:,ii,jj))),transpose(squeeze(hM_min(:,ii,jj))),size(hM,1)-1,'none');
                [corrcoeff(ii,jj),idx]=max(abs(c));
                corrcoeff(ii,jj)=corrcoeff(ii,jj)/sum(hM(:,ii,jj).^2);
                toaEst(ii,jj)=lag(idx);
            end
        end
end

%% ----------------------Fit-Models-to-estimated-TOA-----------------------
for ch=1:size(hM,3)

    % Outlier detection: smooth TOA in horizontal planes
    epsilon=5;
    slope=zeros(size(hM,2),1);
    for ele=min(meta.pos(:,2)):epsilon:max(meta.pos(:,2)) %calculate slope for each elevation along azimuth
        idx=find(meta.pos(:,2)>ele-epsilon/2 & meta.pos(:,2)<=ele+epsilon/2);
        if numel(idx)>1
            idx(length(idx)+1)=idx(1);
            slope(idx(1:end-1),1)=diff(toaEst(idx,ch))./abs(diff(meta.pos(idx,1)));
        end
    end
    sloperms=sqrt(sum(slope.^2)/length(slope));
    if sloperms<30/(length(find(meta.pos(:,2)==0))/2)
        sloperms=30/(length(find(meta.pos(:,2)==0))/2);
    end
    for ele=min(meta.pos(:,2)):epsilon:max(meta.pos(:,2))
        idx=find(meta.pos(:,2)>ele-epsilon/2 & meta.pos(:,2)<=ele+epsilon/2);
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

    % Outlier detection: constant TOA in sagittal planes
    epsilon=2;
    for ii=1:20
        sag_dev=zeros(size(hM,2),1);
        for lat=-90:epsilon:90
            idx=find(meta.pos(:,6)>lat-epsilon/2 & meta.pos(:,6)<=lat+epsilon/2); 
            idx2=find(meta.pos(:,6)>lat-epsilon/2 & meta.pos(:,6)<=lat+epsilon/2 & indicator_hor(:,ch)==0 & indicator(:,ch)==0);
            if length(idx2)>2
                sag_dev(idx,1)=toaEst(idx,ch)-mean(toaEst(idx2,ch));
            end
        end
        sag_var=sqrt(sum(sag_dev.^2)/length(sag_dev));
        if sag_var<2
            sag_var=2;
        end
        indicator_sag(:,ch)=abs(sag_dev)>sag_var;
        indicator(:,ch)=(abs(sag_dev)>sag_var | indicator_hor(:,ch));
    end
    clear sag_dev; clear sag_var;
end

performance.indicator=indicator;
performance.outliers=sum(sum(indicator))/size(hM,2)/2*100;
performance.outliersl=sum(indicator(:,1))/size(hM,2)*100;
performance.outliersr=sum(indicator(:,2))/size(hM,2)*100;

if correct
    % Fit on-axis model to outlier adjusted set of estimated TOAs
    for ch=1:size(hM,3)
        p0_onaxis(ch,4)=min(toaEst(indicator(:,ch)==0,ch))/stimPar.SamplingRate;
        p0offset_onaxis=[0.06 pi/4 pi/4 0.001];

        idx=find(indicator(:,ch)==0);
        x=meta.pos(idx,1:2)*pi/180;
        y=toaEst(idx,ch)/stimPar.SamplingRate;
        if isoctave
            [~,p_onaxis(ch,:)]=leasqr(x,y,p0_onaxis(ch,:),@ziegelwanger2013onaxis);
        else
            p_onaxis(ch,:)=lsqcurvefit(@ziegelwanger2013onaxis,p0_onaxis(ch,:),x,y,p0_onaxis(ch,:)-p0offset_onaxis,p0_onaxis(ch,:)+p0offset_onaxis,optimset('Display','off','TolFun',1e-6));
        end
        toa(:,ch)=ziegelwanger2013onaxis(p_onaxis(ch,:),meta.pos(:,1:2)*pi/180)*stimPar.SamplingRate;
    end

    % Fit off-axis model to outlier adjusted set of estimated TOAs
    TolFun=[1e-5; 1e-6];
    for ii=1:size(TolFun,1)
        for ch=1:size(hM,3)
            idx=find(indicator(:,ch)==0);
            x=meta.pos(idx,1:2)*pi/180;
            y=toaEst(idx,ch)/stimPar.SamplingRate;
            p0_offaxis(ch,:)=[p0_onaxis(ch,1) 0 0 0 p0_onaxis(ch,4) p0_onaxis(ch,2) p0_onaxis(ch,3)];
            p0offset_offaxis=[0.05 0.05 0.05 0.05 0.001 pi pi];
            if isoctave
                [~,p_offaxis(ch,:)]=leasqr(x,y,p0_offaxis(ch,:),@ziegelwanger2013offaxis);
            else
                p_offaxis(ch,:)=lsqcurvefit(@ziegelwanger2013offaxis,p0_offaxis(ch,:),x,y,p0_offaxis(ch,:)-p0offset_offaxis,p0_offaxis(ch,:)+p0offset_offaxis,optimset('Display','off','TolFun',TolFun(ii,1)));
            end
            toa(:,ch)=ziegelwanger2013offaxis(p_offaxis(ch,:),meta.pos(:,1:2)*pi/180)*stimPar.SamplingRate;
        end
        if abs(diff(p_offaxis(:,1)))>0.003 || abs(diff(p_offaxis(:,3)))>0.003
            p_offaxis(:,[1 3])=p_offaxis([2 1],[1 3]);
            for ch=1:size(hM,3)
                idx=find(indicator(:,ch)==0);
                x=meta.pos(idx,1:2)*pi/180;
                y=toaEst(idx,ch)/stimPar.SamplingRate;
                p0_offaxis(ch,:)=[p_offaxis(ch,1) mean(p_offaxis(:,2)) p_offaxis(ch,3) mean(p_offaxis(:,4)) mean(p_offaxis(:,5)) p_offaxis(ch,6) p_offaxis(ch,7)];
                p0offset_offaxis=[0.05 0.05 0.05 0.05 0.001 pi/2 pi/2];
                if isoctave
                    [~,p_offaxis(ch,:)]=leasqr(x,y,p0_offaxis(ch,:),@ziegelwanger2013offaxis);
                else
                    p_offaxis(ch,:)=lsqcurvefit(@ziegelwanger2013offaxis,p0_offaxis(ch,:),x,y,p0_offaxis(ch,:)-p0offset_offaxis,p0_offaxis(ch,:)+p0offset_offaxis,optimset('Display','off','TolFun',TolFun(ii,1)));
                end
                toa(:,ch)=ziegelwanger2013offaxis(p_offaxis(ch,:),meta.pos(:,1:2)*pi/180)*stimPar.SamplingRate;
            end
        end
        if abs(diff(p_offaxis(:,1)))<0.003 && abs(diff(p_offaxis(:,2)))<0.003 && abs(diff(p_offaxis(:,3)))<0.003 && abs(diff(p_offaxis(:,4)))<0.003
            break
        end
    end
else
    toa=toaEst;
    p_offaxis=p0_offaxis;
end

meta.toa=toa;
meta.p_onaxis=transpose(p_onaxis);
meta.p_offaxis=transpose(p_offaxis);
meta.performance=performance;

end %of function

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
