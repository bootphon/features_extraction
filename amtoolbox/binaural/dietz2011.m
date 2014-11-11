function [fine, env, fc, ild] = dietz2011(insig,fs,varargin)
%DIETZ2011  Dietz 2011 binaural model
%   Usage: [...] = dietz(insig,fs);
%
%   Input parameters:
%       insig       : binaural signal for which values should be calculated
%       fs          : sampling rate (Hz)
%
%   Output parameters:
%       fine        : Information about the fine structure (see below)
%       env         : Information about the envelope (see below)
%
%   DIETZ2011(insig,fs) calculates interaural phase, time and level
%   differences of fine- structure and envelope of the signal, as well as
%   the interaural coherence, which can be used as a weighting function.
%
%   The output structures fine and env have the following fields:
%
%     s1
%       left signal as put in the binaural processor
%
%     s2
%       right signal as put in the binaural processor
%
%     fc        
%       center frequencies of the channels (*f_carrier or f_mod*)
%
%     itf
%       transfer function
%
%     itf_equal
%       transfer function without amplitude
%
%     ipd
%       phase difference in rad
%
%     ipd_lp
%       based on lowpass-filtered itf, phase difference in rad
%
%     ild
%       level difference in dB
%
%     itd, itd_C, itd_lp, itd_C_lp
%       time difference based on instantaneous
%       and central frequencies, with and without low-passed itf
%
%     f_inst_1
%       instantaneous frequencies in the channels of the filtered s1
%
%     f_inst_2
%       instantaneous frequencies in the channels of the filtered s2
%
%     f_inst
%       instantaneous frequencies (average of f_inst1 and 2)
%  
%   The steps of the binaural model to calculate the result are the
%   following (see also Dietz et al., 2011):
%
%     1) Middle ear filtering (500-2000 Hz 1st order bandpass)
%
%     2) Auditory bandpass filtering on the basilar membrane using a
%        4th-order all-pole gammatone filterbank, employing 23 filter
%        bands between 200 and 5000 Hz, with a 1 ERB spacing. The filter
%        width was set to correspond to 1 ERB.
%
%     3) Cochlear compression was simulated by power-law compression with
%        an exponent of 0.4.
%
%     4) The transduction process in the inner hair cells was modelled
%        using half-wave rectification followed by filtering with a 770-Hz
%        5th order lowpass.
%
%   The interaural temporal disparities are then extracted using a
%   second-order complex gammatone bandpass (see paper for details).
%
%   DIETZ2011 accepts the following optional parameters:
%
%     'flow',flow    Set the lowest frequency in the filterbank to
%                    flow. Default value is 200 Hz.
%
%     'fhigh',fhigh  Set the highest frequency in the filterbank to
%                    fhigh. Default value is 5000 Hz.
%
%     'basef',basef  Ensure that the frequency basef is a center frequency
%                    in the filterbank. The default value  is 1000.
%
%     'filters_per_ERB', filters_per_erb
%                    Filters per erb. The default value is 1.
%
%     'middle_ear_thr',r
%                    Bandpass freqencies for middle ear transfer. The
%                    default value is [500 2000].
%
%     'middle_ear_order',n 
%                    Order of middle ear filter. Only even numbers are
%                    possible. The default value is 2.
%
%     'haircell_lp_freq',hlpfreq
%                    Cutoff frequency for haircell lowpass filter.
%                    The default value is 770.
%
%     'haircell_lp_order',hlporder
%                    Order of haircell lowpass filter. The default value is 5.
%
%     'compression_power',cpwr
%                    XXX. The default value is 0.4.
%
%     'alpha',alpha  Internal noise strength. Convention FIXME 65dB =
%                    0.0354. The default value is 0.
%
%     'int_randn'    Internal noise XXX. This is the default.
%
%     'int_mini'     Internal noise XXX.
%
%     'filter_order',fo
%                    Filter order for output XXX. Used for both 'mod' and 'fine'.
%                    The default value is 2.
%
%     'filter_attenuation_db',fadb
%                    XXX. Used for both 'mod' and 'fine'. The default value is 10.
% 
%     'fine_filter_finesse',fff
%                    Only for finestructure plugin. The default value is 3.
%
%     'mod_center_frequency_hz',mcf_hz
%                    XXX. Only for envelope plugin. The default value is 135.
%
%     'mod_filter_finesse',mff
%                    XXX. Only for envelope plugin. The default value is 8.
% 
%     'level_filter_cutoff_hz',lfc_hz
%                    XXX. For ild- or level-plugin. The default value is 30.
%
%     'level_filter_order',lforder
%                    XXX. For ild- or level-plugin. The default value is 2.
%
%     'coh_param',coh_param
%                    This is a structure used for the localization
%                    plugin. It has the following fields:
%  
%                      max_abs_itd
%                         XXX. The default value is 1e-3.
%
%                      tau_cycles
%                         XXX. The default value is 5. 
%
%                      tau_s
%                         XXX. The default value is 10e-3.
%
%     'signal_level_dB_SPL',signal_level
%                    Sound pressure level of left channel. Used for data
%                    display and analysis. Default value is 70.
%
%   See also: dietz2011interauralfunctions
%
%   References:
%     M. Dietz, S. D. Ewert, and V. Hohmann. Auditory model based direction
%     estimation of concurrent speakers from binaural signals. Speech
%     Communication, 53(5):592-605, 2011. [1]http ]
%     
%     References
%     
%     1. http://www.sciencedirect.com/science/article/pii/S016763931000097X
%     
%
%   Url: http://amtoolbox.sourceforge.net/doc//binaural/dietz2011.php

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

% AUTHOR: Mathias Dietz, Martin Klein-Hennig (implementation for AMT)

%   Copyright (C) 2002-2012   AG Medizinische Physik,
%                             Universitaet Oldenburg, Germany
%                             http://www.physik.uni-oldenburg.de/docs/medi
%
%   Authors: Tobias Peters (tobias@medi.physik.uni-oldenburg.de) 2002
%            Mathias Dietz (mathias.dietz@uni-oldenburg.de)      2006-2009
%            Martin Klein-Hennig (martin.klein.hennig@uni-oldenburg.de) 2011
 
  
if nargin<2
  error('%s: Too few input parameters.',upper(mfilename));
end;

if ~isnumeric(insig) || min(size(insig))~=2
    error('%s: insig has to be a numeric two channel signal!',upper(mfilename));
end

if ~isnumeric(fs) || ~isscalar(fs) || fs<=0
    error('%s: fs has to be a positive scalar!',upper(mfilename));
end

  
% Gammatone filterbank parameters
definput.keyvals.flow = 200;
definput.keyvals.fhigh = 5000;
definput.keyvals.basef = 1000;
definput.keyvals.filters_per_ERB = 1;

% Preprocessing parameters
definput.keyvals.middle_ear_thr = [500 2000]; % Bandpass freqencies for middle ear transfer
definput.keyvals.middle_ear_order = 2;        % Only even numbers possible
definput.keyvals.haircell_lp_freq = 770;      % Cutoff frequency for haircell lowpass
definput.keyvals.haircell_lp_order = 5;       % Order of haircell lowpass
definput.keyvals.compression_power = 0.4;
definput.keyvals.alpha = 0;                   % Internal noise strength
                                              % 65dB = 0.0354
% randn: add random noise with rms = alpha
% mini: set all values < alpha to alpha
definput.flags.int_noise_case = {'int_randn','int_mini'};

% Parameters for filtering the haircell output
definput.keyvals.filter_order = 2;            % Used for both mod and fine
definput.keyvals.filter_attenuation_db = 10;  % Used for both mod and fine

% Only for finestructure plugin
definput.keyvals.fine_filter_finesse = 3;

% Only for envelope plugin
definput.keyvals.mod_center_frequency_hz = 135;
definput.keyvals.mod_filter_finesse = 8;

% For ild- or level-plugin
definput.keyvals.level_filter_cutoff_hz = 30;
definput.keyvals.level_filter_order = 2;

% Parameters for localization plugin
definput.keyvals.coh_param.max_abs_itd = 1e-3;
definput.keyvals.coh_param.tau_cycles  = 5;   % in cycles
definput.keyvals.coh_param.tau_s       = 10e-3; % in s for faller

% parameters for data display and analysis
definput.keyvals.signal_level_dB_SPL = 70; % sound pressure level of left channel

% display current process
displ                = 0;   % display current process (0 = do not)


[flags,kv]  = ltfatarghelper({},definput,varargin);

%% Model processing starts here

siglen=length(insig);

t = (0:siglen-1)/fs.';

%% ---- middle ear band pass filtering ------
if displ 
  disp(['band pass filtering of input according to middle ear transfer ' ...
        'charact. -> signal_me']); 
end

[b,a] = butter(kv.middle_ear_order,kv.middle_ear_thr(1)/(fs/2),'low');
low_filtered= filter(b,a,insig);

[b,a] = butter(kv.middle_ear_order,kv.middle_ear_thr(2)/(fs/2),'high');
signal_me = filter(b,a,low_filtered);


%% Filterbank analysis
if displ 
  disp('splitting signal into frequency channels -> signal_filtered');
end

% create filterbank
analyzer = gfb_analyzer_new(fs, kv.flow, kv.basef, kv.fhigh,...
                            kv.filters_per_ERB);
channels = length(analyzer.center_frequencies_hz);
fc=analyzer.center_frequencies_hz;
analyzer_sh = analyzer;

% apply filterbank
[signal_filtered, analyzer] = gfb_analyzer_process(analyzer, signal_me(:,1));
[signal_sh_filtered, analyzer_sh] = gfb_analyzer_process(analyzer_sh,...
                                                  signal_me(:,2));

% get number of channels
channels = length(fc);

% determine lowpass parameter
tau = kv.coh_param.tau_cycles./fc;

% rectification, comression, and lowpass filtering of filtered signals
% (haircell processing)
if displ 
  disp('haircell processing of frequency bands -> hairc');
end

% haircell processing
hairc = haircell(signal_filtered', kv.haircell_lp_freq,...
                 kv.haircell_lp_order,kv.compression_power, fs);

hairc_sh = haircell(signal_sh_filtered',kv.haircell_lp_freq,...
                    kv.haircell_lp_order,kv.compression_power,fs);

% also haircell processing, but no 770 Hz filter for fine-structure channel
hairc_nolp = haircell(signal_filtered', [],[],...
                      kv.compression_power, fs);

hairc_nolp_sh = haircell(signal_sh_filtered', [],[],...
                         kv.compression_power, fs);

% adding internal noise
if flags.do_int_randn
  if displ 
    disp('adding internal random noise -> hairc');
  end
  hairc = hairc + (randn(length(hairc),channels)*kv.alpha);
  hairc_sh = hairc_sh + (randn(length(hairc_sh),length(fc))*kv.alpha);
  hairc_nolp = hairc_nolp + (randn(length(hairc),channels)*kv.alpha);
  hairc_nolp_sh = hairc_nolp_sh + (randn(length(hairc_sh), ...
                                         length(fc))*kv.alpha);
end;

if flags.do_int_mini
  if displ 
    disp('adding internal noise via minimum -> hairc');
  end
  hairc = max(hairc,kv.alpha);
  hairc_sh = max(hairc_sh,kv.alpha);
  hairc_nolp = max(hairc,kv.alpha);
  hairc_nolp_sh = max(hairc_sh,kv.alpha);
end

% processing the hairc output with a modulation frequency filter
%cmin = min(find(fc>2*mod_center_frequency_hz)); % lowest freq. band for envelope detection
if displ 
  disp('enveloping haircell output -> hairc_mod'); 
end

mod_filter_bandwidth_hz = kv.mod_center_frequency_hz/kv.mod_filter_finesse;
[hairc_mod, hairc_sh_mod] =... %gfb_envelope_filter(hairc(:,cmin:end), hairc_sh(:,cmin:end), fs,...
    gfb_envelope_filter(hairc, hairc_sh, fs,...
    kv.mod_center_frequency_hz, mod_filter_bandwidth_hz, ...
    kv.filter_attenuation_db, kv.filter_order);

% calculation of interaural functions from haircell modulation
if displ disp('calculating interaural functions from haircell modulation'); end
env = dietz2011interauralfunctions(...
    hairc_mod, hairc_sh_mod, tau,kv.mod_center_frequency_hz+0*fc,...
    kv.signal_level_dB_SPL, kv.compression_power, kv.coh_param.tau_cycles, fs);

% processing the hairc output with a fine structure filter
if displ disp('deriving fine structure of haircell output -> hairc_fine'); end
[hairc_fine, hairc_sh_fine] =...
    gfb_envelope_filter(hairc_nolp, hairc_nolp_sh, fs, fc,...
    fc/kv.fine_filter_finesse, kv.filter_attenuation_db, kv.filter_order);

% calculation of interaural functions from haircell fine structure
if displ 
  disp('calculating interaural functions from haircell fine structure');
end
fine = dietz2011interauralfunctions(...
    hairc_fine, hairc_sh_fine, tau, fc,...
    kv.signal_level_dB_SPL, kv.compression_power, kv.coh_param.tau_cycles, fs);

% determine ILD of the hairc output
if displ disp('determining ild of the haircell output -> ild'); end
ild = ild_filter(hairc,hairc_sh,kv.level_filter_cutoff_hz,...
                       kv.level_filter_order,kv.compression_power,fs);

% remove finestructure information > 1400 Hz
fine.f_inst(:,fc>1400)=[];
fine.ic(:,fc>1400)=[];
fine.ipd_lp(:,fc>1400)=[];
fine.itd_lp(:,fc>1400)=[];


if displ 
  disp('finished');
end

end



%% gfb_envelope_filter %%%%%%%%%%%%%%%%%
function [envelopes_filtered, envelopes_sh_filtered] = gfb_envelope_filter(s1, s2, sampling_rate_hz, center_frequency_hz,...
    bandwidth_hz, attenuation_db, gamma_filter_order)
% [envelopes_filtered, envelopes_sh_filtered] =...
%   gfb_envelope_filter(s1, s2, sampling_rate_hz, center_frequency_hz, bandwidth_hz, attenuation_db, gamma_filter_order);
%
% Filters each row of s1 and s2 with the gammatone filter defined by the input parameters.
% Takes both vectors and matrices.
%
% Input
%   s1, s2 - signals to be filtered
%   sampling_rate_hz - sampling frequency / Hz
%   center_frequency_hz - centre frequency of the gammatone filter / Hz
%   bandwidth_hz - bandwidth of the gammatone filter at the level attenuation_db / Hz
%   attenuation_db - attenuation in dB at which the filter has bandwidth_hz
%   gamma_filter_order - order of the filter

s1 = s1';
s2 = s2';

[M, N] = size(s1);
if length(center_frequency_hz) == 1
    center_frequency_hz = center_frequency_hz * ones(1,M);
end
if isempty(bandwidth_hz) % default: width = 1 ERB
    recip_width1erb = diff(gfb_hz2erbscale(1:N/2));
    bandwidth_hz = round(1./recip_width1erb(round(center_frequency_hz)));
elseif length(bandwidth_hz) == 1
    bandwidth_hz = bandwidth_hz * ones(1,M);
end

for i = 1:M
    filter = gfb_filter_new(sampling_rate_hz, center_frequency_hz(i),...
        bandwidth_hz(i), attenuation_db, gamma_filter_order);
    [envelopes_filtered(:,i),    filter_obj] = ...
        gfb_filter_process(filter, s1(i,:));

    [envelopes_sh_filtered(:,i), filter_obj] = ...
        gfb_filter_process(filter, s2(i,:));
end
end



%% haircell
function output = haircell(input,cutoff,order,compress_power,fs)

% half-wave rectification
rect = max(real(input),0);

% compression
output = rect.^compress_power;

% lowpass filtering, only if desired
if (~isempty(cutoff) || ~isempty(order))
    fNorm = cutoff*(1/sqrt((2^(1/order)-1))) / (fs/2);
    [b,a] = butter(1,fNorm,'low');
    for k = 1:order
        output= filter(b,a,output);
    end
end

end


%% ild_filter
function output = ild_filter(hairc,hairc_sh,lp_threshold_freq,lp_order,compression,fs)

% lowpass filtering
[b,a] = butter(lp_order,lp_threshold_freq/(fs/2),'low');
hclp    = filter(b,a,hairc);
hclp_sh = filter(b,a,hairc_sh);

output = 20*log10(max(hclp_sh,1e-4)./max(hclp,1e-4))/compression;
end

