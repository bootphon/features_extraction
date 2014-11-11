function lookup = lookup_table(irs,model,fs)
%LOOKUP_TABLE generates an azimuth lookup table for the given irs set
%   Usage: lookup = lookup_table(irs,model,fs);
%          lookup = lookup_table(irs,model);
%          lookup = lookup_table(irs);
%
%   Input parameters:
%       irs    : HRTF data set (TU Berlin irs format)
%       model  : binaural model to use:%                   
%       fs     : sampling rate, default: 44100) (Hz)
%
%   Output parameters:
%       lookup : struct containing lookup data
%
%   LOOKUP_TABLE(irs) creates a lookup table from the given IR data set. This
%   lookup table can be used by the dietz binaural model to predict the
%   perceived direction of arrival of an auditory event.
%   
%
%
%   Url: http://amtoolbox.sourceforge.net/doc//modelstages/lookup_table.php

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


%% ===== Checking of input parameters ===================================
nargmin = 1;
nargmax = 3;
error(nargchk(nargmin,nargmax,nargin));

if nargin==1
    model = 'dietz';
    fs = 44100;
elseif nargin==2
    fs = 44100;
end


%% ===== Configuration ==================================================
% time of noise used for the calculation (samples)
nsamples = fs;
% noise type to use
noise_type = 'white';


%% ===== Calculation ====================================================
% generate noise signal
sig_noise = noise(nsamples,1,noise_type);
% get only the -90 to 90 degree part of the irs set
idx = (( irs.apparent_azimuth>-pi/2 & irs.apparent_azimuth<pi/2 & ...
    irs.apparent_elevation==0 ));
irs = slice_irs(irs,idx);
% iterate over azimuth angles
nangles = length(irs.apparent_azimuth);
% create an empty mod_itd, because the lindemann model didn't use it
mod_itd = [];

if strcmpi('dietz',model)

    itd = zeros(nangles,12);
    mod_itd = zeros(nangles,23);
    ild = zeros(nangles,23);
    for ii = 1:nangles
        % generate noise coming from the given direction
        ir = get_ir(irs,irs.apparent_azimuth(ii));
        sig = auralize_ir(ir,sig_noise);
        % calculate binaural parameters
        [fine, modulation, cfreqs, ild_tmp] = dietz2011(sig,fs);
        % unwrap ITD
        itd_tmp = unwrap_itd(fine.itd(:,1:12),ild_tmp,fine.f_inst);
        % calculate the mean about time of the binaural parameters and store
        % them
        itd(ii,:) = median(itd_tmp,1);
        mod_itd(ii,:) = median(modulation.itd,1);
        ild(ii,:) = median(ild_tmp,1);
    end

elseif strcmpi('lindemann',model)

    itd = zeros(nangles,36);
    ild = zeros(nangles,36);
    for ii = 1:nangles
        % generate noise coming from the given direction
        ir = get_ir(irs,irs.apparent_azimuth(ii));
        sig = auralize_ir(ir,sig_noise);
        % Ten fold upsampling to have a smoother output
        %sig = resample(sig,10*fs,fs);
        % calculate binaural parameters
        c_s = 0.3; % stationary inhibition
        w_f = 0; % monaural sensitivity
        M_f = 6; % decrease of monaural sensitivity
        T_int = inf; % integration time
        N_1 = 17640; % sample at which first cross-correlation is calculated
        [cc_tmp,dummy,ild(ii,:),cfreqs] = lindemann1986(sig,fs,c_s,w_f,M_f,T_int,N_1);
        clear dummy;
        cc_tmp = squeeze(cc_tmp);
        % Calculate tau (delay line time) axes
        tau = linspace(-1,1,size(cc_tmp,1));
        % find max in cc
        for jj = 1:size(cc_tmp,2)
            [v,idx] = max(cc_tmp(:,jj));
            itd(ii,jj) = tau(idx)/1000;
        end
    end

end

% Create lookup struct
lookup.itd = itd;
lookup.ild = ild;
lookup.mod_itd = mod_itd;
lookup.cfreq = cfreqs;
lookup.azimuth = irs.apparent_azimuth;

