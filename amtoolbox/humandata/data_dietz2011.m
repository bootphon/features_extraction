function [stim,fs]  = data_dietz2011(varargin)
%DATA_DIETZ2011 Stimuli from Dietz et al. (2011)
%   Usage: [stim,fs] = data_dietz2011;
%
%   [stim,fs]=DATA_DIETZ2011(flag) returns the speech stimuli and sampling
%   frequency from Dietz et al. 2011. The stimuli are speech signals from
%   the TIMIT database, convolved with head-related impulse responses 
%   from Kayser et al. 2009. The possible values of flag are:
%   'five_speakers', 'one_of_three', 'one_speaker_reverb', 'three_of_three',
%   'bNoise', 'two_of_three', or 'two_speakers'.
%
%   See also: dietz2011, exp_dietz2011
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
%   Url: http://amtoolbox.sourceforge.net/doc//humandata/data_dietz2011.php

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

definput.flags.wavfile={'missingflag','five_speakers','one_of_three',...
                    'one_speaker_reverb','three_of_three',...
                    'two_of_three','two_speakers','bNoise'};

[flags,keyvals]  = ltfatarghelper({},definput,varargin);

if flags.do_missingflag
    flagnames=[sprintf('%s, ',definput.flags.wavfile{2:end-2}),...
               sprintf('%s or %s',definput.flags.wavfile{end-1},definput.flags.wavfile{end})];
    error('%s: You must specify one of the following flags: %s.',upper(mfilename),flagnames);
end;
  
s=[amtbasepath,'humandata',filesep,'dietz2011_',flags.wavfile,'.wav'];

[stim,fs] = wavread(s);

  

