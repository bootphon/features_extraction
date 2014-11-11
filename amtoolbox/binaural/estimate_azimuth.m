function [phi,phi_std,itd,ild,cfreqs] = estimate_azimuth(sig,lookup,model,do_spectral_weighting,fs)
%ESTIMATE_AZIMUTH Estimate the perceived azimuth using a binaural model
%   Usage: [phi,itd,ild,cfreqs] = estimate_azimuth(sig,lookup,model,do_spectral_weighting,fs)
%          [phi,itd,ild,cfreqs] = estimate_azimuth(sig,lookup,model,do_spectral_weighting)
%          [phi,itd,ild,cfreqs] = estimate_azimuth(sig,lookup,model)
%          [phi,itd,ild,cfreqs] = estimate_azimuth(sig,lookup)
%
%   Input parameters:
%       sig                   : binaural singal
%       lookup                : lookup table to map ITDs to angles (struct)
%       model                 : model to use:
%                                   'dietz' (default)
%                                   'lindemann'
%       do_spectral_weighting : apply spectral weighting of ITD values after
%                               Raatgever (1980) (default: false)
%       fs                    : sampling rate (default: 44100) (Hz)
%
%   Output parameters:
%       phi     : estimated azimuth (rad)
%       itd     : calculated ITD (s)
%       ild     : calculated ILD (dB)
%       cfreqs  : center frequencies of used auditory filters (Hz)
%
%   ESTIMATE_AZIMUTH(sig,lookup,model,do_spectral_weighting,fs) uses a binaural
%   model to estimate the perceived direction for a given binaural signal.
%   Therefore, it needs the struct lookup, which maps ITD values to the
%   corresponding angles. This can be created with the LOOKUP_TABLE function.
%   If do_spectral_weighting is set to true, a spectral weighting of the single
%   ITD values after Raatgever is applied. He has done some measurements to see
%   what is the spectral domincance region for lateralization by the ITD and
%   found a region around 600 Hz. Stern et al. have fitted his data with a formula
%   used in this function.
%
%       Raatgever (1980) - On the binaural processing of stimuli with different
%           interaural phase relations, TU delft
%       Stern et al. (1980) - Lateralization of complex binaural stimuli: A
%           weighted-image model, JASA 84(1)
%
%   see also: lookup_table
%
%   Url: http://amtoolbox.sourceforge.net/doc//binaural/estimate_azimuth.php

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

% AUTHOR: Hagen Wierstorf


%% ===== Checking of input  parameters ==================================
nargmin = 2;
nargmax = 5;
error(nargchk(nargmin,nargmax,nargin));

if nargin==2
    model = 'dietz';
    do_spectral_weighting = false;
    fs = 44100;
elseif nargin==3
    do_spectral_weighting = false;
    fs = 44100;
elseif nargin==4
    fs = 44100;
end
if ~ischar(model)
    error('%s: %s need to be a string.',upper(mfilename),model);
end
if ~isnumeric(fs) || ~isscalar(fs) || fs<0
    error('%s: %s need to be a positive scalar.',upper(mfilename),fs);
end


%% ===== Computation ====================================================
%
% === Dietz model ===
if strcmpi('dietz',model)

    ic_threshold=0.98;
    cn = 10; % channel number for time plot

    % Run the Dietz model on signal
    [fine, modulation, cfreqs, ild_tmp] = dietz2011(sig,fs);

    % Unwrap ITDs and get the azimuth values
    itd = unwrap_itd(fine.itd(:,1:12),ild_tmp,fine.f_inst);
    phi = itd2azimuth(itd,lookup);

    % Calculate the median over time for every frequency channel of the azimuth
    for n = 1:size(phi,2)
        idx = fine.ic(:,n)>ic_threshold&[diff(fine.ic(:,n))>0; 0];
        angle = phi(idx,n);
        idx = ~isnan(angle);
        if size(angle(idx),1)==0
            azimuth(n) = NaN;
        else
            azimuth(n) = median(angle(idx));
        end
    end
    % Calculate ITD and ILD values
    ild = median(ild_tmp,1);
    itd = median(itd,1);

elseif strcmpi('lindemann',model)

    % run Lindemann model on signal
    c_s = 0.3; % stationary inhibition
    w_f = 0; % monaural sensitivity
    M_f = 6; % decrease of monaural sensitivity
    T_int = inf; % integration time
    N_1 = 1764; % sample at which first cross-correlation is calculated
    [cc_tmp,dummy,ild,cfreqs] = lindemann1986(sig,fs,c_s,w_f,M_f,T_int,N_1);
    clear dummy;
    cc_tmp = squeeze(cc_tmp);
    % Calculate tau (delay line time) axes
    tau = linspace(-1,1,size(cc_tmp,1));
    % find max in cc
    itd = zeros(1,size(cc_tmp,2));
    for jj = 1:size(cc_tmp,2)
        [v,idx] = max(cc_tmp(:,jj));
        itd(jj) = tau(idx)/1000;
    end
    azimuth = itd2azimuth(itd,lookup);
end

% Remove outliers
[azimuth,cfreqs] = remove_outlier(azimuth,itd,cfreqs);
% Calculate mean about frequency channels
if length(azimuth)==0
    phi = rad(90);
elseif do_spectral_weighting
    w = spectral_weighting(cfreqs);
    phi = sum(azimuth.*w)/sum(w);
else
    phi = median(azimuth);
    phi_std = std(azimuth);
end

end % of main function

%% ===== Subfunctions ====================================================
function [azimuth,cfreqs] = remove_outlier(azimuth,itd,cfreqs)
    cfreqs = cfreqs(1:12);
    % remove unvalid ITDs
    azimuth = azimuth(abs(itd(1:12))<0.001);
    cfreqs = cfreqs(abs(itd(1:12))<0.001);
    % remove NaN
    azimuth = azimuth(~isnan(azimuth));
    cfreqs = cfreqs(~isnan(azimuth));
    % remove outliers more than 30deg away from median
    if length(azimuth)>0
        cfreqs = cfreqs(abs(azimuth-median(azimuth))<rad(30));
        azimuth = azimuth(abs(azimuth-median(azimuth))<rad(30));
    end
end
function w = spectral_weighting(f)
    % Calculate a spectral weighting after Stern1988, after the data of
    % Raatgever1980
    b1 = -9.383e-2;
    b2 =  1.126e-4;
    b3 = -3.992e-8;
    w = 10.^( -(b1*f+b2*(f).^2+b3*(f).^3)/10 );
end

