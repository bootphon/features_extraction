function out = may2011(input,fs)
%may2011   GMM-based localization module.
%   Usage: out = GFB2Azimuth(input,fs)
%
%   Input arguments:
%     input : binaural input signal [nSamples x 2 channels]
%        fs : sampling frequency in Hertz
%
%   Output arguments:
%     out : localization structure containing the following fields:
%        .param    = processing parameters
%        .azFrames = frame-based azimuth estimate    [Frames x 1]
%        .azimuth  = time-frequency azimuth estimate [nFilter x nFrames]
%        .rangeAZ  = azimuth grid                    [nAzDir x 1]
%        .prob     = 3D probability map              [nFilter x nFrames x nAzDir]
%        .loglik   = 3D log-likelihood map           [nFilter x nFrames x nAzDir]
%        .itd      = interaural time difference      [nFilter x nFrames]
%        .ild      = interaural level difference     [nFilter x nFrames]
%        .ic      = interaural coherence             [nFilter x nFrames]
%  
%  *Note**: The NETLAB toolbox is required for the GMM modeling.
% 
%
%   Url: http://amtoolbox.sourceforge.net/doc//binaural/may2011.php

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

%REFERENCES
%   [1] T. May, S. van de Par and A. Kohlrausch, "A Probabilistic Model for
%       Robust Localization Based on a Binaural Auditory Front-End", IEEE
%       Transactions on Audio, Speech and Language Processing, Vol 19,
%       Issue 1, pp. 1-13, 2011.


%   Developed with Matlab 7.8.0.347 (R2009a). Please send bug reports to:
%
%   Author  :  Tobias May, © 2009-2013 
%              University of Oldenburg and TU/e Eindhoven   
%              tobias.may@uni-oldenburg.de   t.may@tue.nl
%
%   History :
%   v.1.0   2009/04/05
%   v.1.1   2009/05/19 ensure that loglikelihoods are finite
%   ***********************************************************************

% Initialize persistent memory
persistent PERC PERmethod


%% ***********************  CHECK INPUT ARGUMENTS  ************************
% 
% 
% Check for proper input arguments
error(nargchk(2,2,nargin));


%% *******************  INITIALIZE AZIMUTH CLASSIFIER  ********************
% 
% 
% Block parameter 
blockSec  = 20e-3; % Block size in seconds
hopSec    = 10e-3; % Step size in seconds
winType   = 'rectwin';
methodGMM = 'default'; 

% Select GMM preset 
switch lower(methodGMM)
    case 'default'
        % Room 5 -90:5:90
        A.gmmModel = 'GMMMultiPosRoom5Roman_20100622-1525';   
    otherwise
        error(['GMM module ''',lower(methodGMM),'''is not available.'])
end

% Check if localization module can be re-loaded from persistent memory
if ~isempty(PERC) && ~isempty(PERmethod) && isequal(methodGMM,PERmethod)
    % Re-load GMMs
    C = PERC;
else
    % Load localization module
    load(A.gmmModel);
    
    % Store classifier to persistent memory
    PERC = C; PERmethod = methodGMM;
end

% Store classifier
A.GMM = C;

% Extract gammatone filterbank settings from the classifier
GFB = A.GMM.featSpace(1).GFB;
                 
% Adapt fs in GFB structure according to input signal
if ~isequal(GFB.fs,fs)
    GFB.fs = fs;
end

% Haircell processing
if isfield(C.featSpace(1),'neuralTransduction')
    A.haircellModel = C.featSpace(1).neuralTransduction;
else
    A.haircellModel = 'roman';
end

% Cross-correlation method
if isfield(C.featSpace(1),'xcorrMethod') && ...
   ~isempty(C.featSpace(1).xcorrMethod)
    A.xcorrMethod = C.featSpace(1).xcorrMethod;
else
    A.xcorrMethod = 'power';
end

% ITD parameter
A.bXcorrNorm    = true;
A.bXcorrDetrend = true;
A.maxDelay      = round(1e-3 * fs);
A.lags          = (-A.maxDelay:1:A.maxDelay).';
A.domain        = 'time';

% Resolution of GMM models
stepSizeGMM = 1;

% For short access
gmmFinal = C.classifier.gmmFinal;

% Build azimuth grid
azIdx  = 1:stepSizeGMM:length(C.azimuth);
azGrid = C.azimuth(1:stepSizeGMM:end);
azStep = diff(azGrid(1:2));

% Number of different azimuth directions 
nAz = length(azGrid);

% Determine length of input signal
nSamples = size(input,1);

% Block processing parameters
blockSamples = 2 * round(fs * blockSec / 2);
hopSamples   = 2 * round(fs * hopSec / 2); 
overSamples  = blockSamples - hopSamples;

% Calculate number of frames
nFrames = fix((nSamples-overSamples)/hopSamples);

% Allocate memory
prob   = zeros(GFB.nFilter,nFrames,nAz);
loglik = zeros(GFB.nFilter,nFrames,nAz);
feat   = zeros(nFrames,2);
itd    = zeros(GFB.nFilter,nFrames);
ild    = zeros(GFB.nFilter,nFrames);
ic     = zeros(GFB.nFilter,nFrames);


%% ************************  GAMMATONE FILTERING  *************************
%
% 
% Perform gammatone filtering
bm = may2011gammatone(input,GFB);


%% ********************  BINAURAL FEATURE EXTRACTION  *********************
%
% 
% Loop over number of channels
for ii = 1 : GFB.nFilter
    
    % Neural transduction
    left  = may2011neuraltransduction(bm(:,ii,1), fs, A.haircellModel);
    right = may2011neuraltransduction(bm(:,ii,2), fs, A.haircellModel);
    
    % Framing
    frameL = frameData(left, blockSamples, hopSamples, winType);
    frameR = frameData(right,blockSamples, hopSamples, winType);
    
    % =====================================================================
    % ITD ESTIMATE
    % =====================================================================
    % 
    % Cross-correlation analysis
    switch lower(A.xcorrMethod)
        case 'power'
            % Time-based
            xcorr = xcorrNorm(frameL,frameR,A.maxDelay,A.bXcorrDetrend,...
                              A.bXcorrNorm);
        case 'unbiased'
            % FFT-based
            xcorr = calcXCorr(frameL,frameR,A.maxDelay,A.xcorrMethod);
    end
    
    % Maximum search per frame
    [a,b] = findLocalPeaks(xcorr,2); %#ok
    
    % Warp indices
    bM = mod(b-1,2*A.maxDelay+1)+1;
      
    % Refine lag position by applying parabolic interpolation
    [delta,currIC] = interpolateParabolic(xcorr,bM);
    
    % Calculate ITD with fractional part
    feat(:,1) = A.lags(bM)/fs + delta * (1/fs);
    
    % =====================================================================
    % ILD ESTIMATE
    % =====================================================================
    %     
    % Calculate energy per frame
    bmEnergyL = sum(abs(frameL).^2)/blockSamples;
    bmEnergyR = sum(abs(frameR).^2)/blockSamples;
    
    % Compute ILD
    feat(:,2) = calcILD(bmEnergyL,bmEnergyR);
    
    % Compensate for square-root compression
    if isequal(A.haircellModel,'roman')
        % Reverse effect of sqrt() in decibel domain
        feat(:,2) = 2 * feat(:,2);
    end
    
    % =====================================================================
    % CREATE 3-DIMENSIONAL AZIMUTH-DEPENDENT LIKELIHOOD FUNCTION
    % =====================================================================
    %     
    % Classify azimuth
    prob(ii,:,:) = classifyGMM(feat,gmmFinal{ii}(azIdx));
    
    % Normalize
    prob(ii,:,:) = prob(ii,:,:) ./ repmat(sum(prob(ii,:,:),3),[1 1 nAz]);
    
    % Store log-likelihood
    loglik(ii,:,:) = log(prob(ii,:,:));
    
    % Store ITD
    itd(ii,:) = feat(:,1);
    
    % Store ILD
    ild(ii,:) = feat(:,2);
    
    % Store IC
    ic(ii,:) = currIC;
end

% Ensure that loglikelihood is finite
loglik(isinf(loglik) | isnan(loglik)) = 0;


%% ***********************  LOCALIZATION ESTIMATE  ************************
% 
% 
% =========================================================================
% TIME/FREQUENCY-BASED AZIMUTH ESTIMATE
% =========================================================================
% 
% 
% Maximum search
[tmp,maxIdx] = max(loglik,[],3); %#ok

% Warp to azimuth indices
azimuth = azGrid(maxIdx);

% =========================================================================
% FRAME-BASED AZIMUTH ESTIMATE
% =========================================================================
% 
% 
% Integrate log-likelihoods across channels
intChanGMM = squeeze(mean(loglik,1));

% Maximum search
[tmp,maxIdx] = max(intChanGMM,[],2); %#ok

% Warp to azimuth indices
azFrames = azGrid(maxIdx);

% Apply interpolation to increase azimuth resolution
delta = interpolateParabolic(exp(intChanGMM).',maxIdx);

% Find overall azimuth estimate
azFrames(:) = azFrames(:) + (azStep*delta(:));


%% ************************  CREATE RESULT STRUCT  ************************
%
% 
% Create output structure
out.param      = A;
out.azFrames   = azFrames;
out.azimuth    = azimuth;
out.rangeAZ    = azGrid;
out.prob       = prob;
out.loglik     = loglik;
out.itd        = itd;
out.ild        = ild;
out.ic         = ic;

