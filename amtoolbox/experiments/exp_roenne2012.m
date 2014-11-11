function [waveVamp, waveVlat] = exp_roenne2012(varargin)
%EXP_ROENNE2012 Figures from Rønne et al. (2012)
%   Usage: output = exp_roenne2012(flag)
%
%   EXP_ROENNE2012(flag) reproduces the results for the figure given
%   by flag from the Rønne et al. (2012) paper. Outputs are the ABR wave V
%   amplitude and latency of all datapoints in that given figure.
%   
%   The following flags can be specified;
%
%     'plot'     Plot the specified figure from Rønne et al. (2012). This is
%                the default. 
%
%     'noplot'   Don't plot, only return data.
%
%     'plot2'    Plot extra figures for all individual simulated points.
%                Note that this creates lots of extra figures (3 for each
%                simulated data point)
%
%     'auto '    Redo the experiment if a cached dataset does not exist. This is the default.
%
%     'refresh'  Always recalculate the experiment.
%
%     'cached'   Always use the cached version. This throws an error if the
%                file does not exist.
%
%     'fig5'     Plot Fig. 5 (Rønne et al., 2012). Latency of simulated ABR
%                wave V's compared to Neely et al. (1988) and Harte et al.
%                (2009) reference data.
%
%     'fig6'     Plot Fig. 6 (Rønne et al., 2012). Amplitude of simulated
%                wave V compared to Elberling et al. (2010) reference data.
%
%     'fig7'     Plot Fig. 7 (Rønne et al., 2012). Latency of simulated wave
%                V compared to Elberling et al. (2010) reference data.
%
%   Examples:
%   ---------
%
%   To display Figure 5 use :
%
%     exp_roenne2012('fig5');
%
%   To display Figure 6 use :
%
%     exp_roenne2012('fig6');
%
%   To display Figure 7 use :
%
%     exp_roenne2012('fig7');
%
%   References:
%     C. Elberling, J. Callø, and M. Don. Evaluating auditory brainstem
%     responses to different chirp stimuli at three levels of stimulation. J.
%     Acoust. Soc. Am., 128(1):215-223, 2010.
%     
%     J. Harte, G. Pigasse, and T. Dau. Comparison of cochlear delay
%     estimates using otoacoustic emissions and auditory brainstem responses.
%     J. Acoust. Soc. Am., 126(3):1291-1301, 2009.
%     
%     S. Neely, S. Norton, M. Gorga, and J. W. Latency of auditory brain-stem
%     responses and otoacoustic emissions using tone-burst stimuli. J.
%     Acoust. Soc. Am., 83(2):652-656, feb 1988.
%     
%     F. Rønne, J. Harte, C. Elberling, and T. Dau. Modeling auditory evoked
%     brainstem responses to transient stimuli. J. Acoust. Soc. Am., accepted
%     for publication, 2012.
%     
%
%   ---------
%
%   Please cite Rønne et al. (2012) and Zilany and Bruce (2007) if you use
%   this model.
%
%
%   Url: http://amtoolbox.sourceforge.net/doc//experiments/exp_roenne2012.php

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
definput.import={'amtredofile'};
definput.flags.type={'fig5','fig6','fig7'};
definput.flags.plot={'plot','noplot'};

[flags,keyvals]  = ltfatarghelper({},definput,varargin);

save_format='-v6';

%% ------ FIG 5 -----------------------------------------------------------
if flags.do_fig5
  
  s = [mfilename('fullpath'),'_fig5.mat'];

  waveVamp = 0;

  stim_level = 40:10:100; % Default stimulus levels
  
  if amtredofile(s,flags.redomode);

    [click_amplitude, click_latency]    = roenne2012click(stim_level);     

    waveVlat = roenne2012tonebursts(stim_level);
    
    save(s,'waveVlat','click_latency',save_format);
  
  else
    
    s = load(s);
    waveVlat      = s.waveVlat;
    click_latency = s.click_latency;

  end;
  
  if flags.do_plot;
    plotroenne2012tonebursts(waveVlat,click_latency);
  end  ;  

end;

%% ------ FIG 6 -----------------------------------------------------------
if flags.do_fig6;

  stim_level    = (20:20:60)+35.2;
  
  % Default chirp numbers. 1 = click, 2 to 6 = chirp 1 to 5.
  chirp_number  = 1:6;              

  s = [mfilename('fullpath'),'_fig6.mat'];

  if amtredofile(s,flags.redomode)

    [waveVamp, waveVlat] = roenne2012chirp(stim_level, chirp_number);

    save(s,'waveVamp','waveVlat',save_format);
  else

    s = load(s);
    waveVamp = s.waveVamp;
    waveVlat = s.waveVlat;
  end;
        
  if flags.do_plot
    plotroenne2012chirp(waveVamp, waveVlat,'amponly');
  end
end;

%% ------ FIG 7 -----------------------------------------------------------
if flags.do_fig7;

  stim_level    = (20:20:60)+35.2;
  
  % Default chirp numbers. 1 = click, 2 to 6 = chirp 1 to 5.
  chirp_number  = 1:6;              

  s = [mfilename('fullpath'),'_fig7.mat'];

  if amtredofile(s,flags.redomode)

    [waveVamp, waveVlat] = roenne2012chirp(stim_level, chirp_number);

    save(s,'waveVamp','waveVlat',save_format);
  else

    s = load(s);
    waveVamp = s.waveVamp;
    waveVlat = s.waveVlat;
  end;
        
  if flags.do_plot
    plotroenne2012chirp(waveVamp, waveVlat,'latonly');
  end
end;


