function [fcs_EPSM outSNRenvs ] = joergensen2011snrenv(MixEnv,NoiseEnv,fs)
%SNRENV  SNRenv of processed signal
% 
%   Input parameters:
%     MixEnv      : The envelope of a signal which have been processed
%                   in some way
%     NoiseEnv    : The noise envelope with/wothout processing
%    fs           : Sampling frequency
%    outSNRenvs   : 1x7 vector of overall SNRenv's in dB,
%                   one for each modulation filter center frequency
%    fcs_EPSM     : center-frequencies of the modulation filterbank
%    sEPSM_ExPtns : Envelope power excitation patterns for of the input
% 
%  [fcs_EPSM,outSNRenvs]=JOERGENSEN2011SNRENV(MixEnv,NoiseEnv,fs)
%  calculates the SNRenv for the processed signal.
%
%   Url: http://amtoolbox.sourceforge.net/doc//speech/joergensen2011snrenv.php

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

% AUTHOR: May 2012 S�ren J�rgensen 

% Center frequencies of the modulation filters
% fcs = [1 2 4 8 16 32 64 ];

% Calculation of envelope power in 7 modulation filters
[fcs_EPSM Mix_ExcPtn]   =  modfilterbankepsm(MixEnv,0,fs);
[fcs_EPSM Noise_ExcPtn] =  modfilterbankepsm(NoiseEnv,0,fs);

% NaN values are set to zero
idx_nans =  isnan(Noise_ExcPtn);
for k = 1:length(idx_nans)
    if idx_nans(k) == 1
        Noise_ExcPtn = 0;
    end
end

% Noisefloor cannot exceed the mix, since they exist at the same time 
Noise_ExcPtn = min(Mix_ExcPtn,Noise_ExcPtn); 

% The noisefloor restricted to minimum 0.01 reflecting and internal noise
% threshold
Noise_ExcPtn = max(Noise_ExcPtn,0.01);
Mix_ExcPtn = max(Mix_ExcPtn,0.01);
% calculation of SNRenv
outSNRenvs = 10*log10(((Mix_ExcPtn-Noise_ExcPtn ) ./Noise_ExcPtn));

% SNRenv - values are truncated to minimum -30 dB.
outSNRenvs = max(-30,outSNRenvs);

% Excitation patterns
sEPSM_ExPtns = [Mix_ExcPtn'  Noise_ExcPtn'];



