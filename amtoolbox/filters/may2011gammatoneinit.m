function GFB = may2011gammatoneinit(fs,lowFreq,upFreq,nFilter,bUseEar,bAlign,bInfo)
%MAY2011GAMMATONEINIT   Initialize gammatone filterbank structure
%USAGE:
%   GFB = gammatoneInit(FS,FLOW,FUP,NFILTER,BEAR,BALIGN,BINFO)
% 
%INPUT PARAMETER
%        FS : sampling frequency in Hz
%      FLOW : center frequency of lowest auditory filter 
%             (default, FLOW = 80)
%       FUP : center frequency of highest auditory filter 
%             (default, FUP = 8e3)
%   NFILTER : number of auditory filters which will be spaced linear on the 
%             ERB scale. If NFILTER is not a scalar but a vector, the first 
%             value is assumed to represent the number of auditory channels
%             and the following values represents the indices of the 
%             filters which should be processed. This can be useful if a 
%             large number of channels is required as MATLAB might run out 
%             of memory for a longer exerpt if all filters are computed in 
%             one step. (default, NFILTER = round(freq2erb(FS/2))
%      BEAR : Adjust gain coefficients of the auditory channels to 
%             incorporate middle ear effects. Note that this feature can 
%             only be used if the center frequencies of the auditory 
%             channels are above 20 hertz and below 12500 hertz.
%             (default, BEAR = true)
%    BALIGN : time-aligned gammatone output (non-causal output)
%             (default, BALIGN = false)
%     BINFO : info flag printing gammatone parameters on the screen
%             (default, BINFO = false)
% 
%OUTPUT PARAMETER
%       GFB : gammatone parameter structure which can be passed as second
%             input argument to the function "gammatone".
%
%EXAMPLE
%   nSamples = 500;
%   % Initialize gammatone parameter structure
%   GFB = gammatoneInit(20e3);
%   % Filter impulse with gammatone filtering 
%   bm = gammatone([1; zeros(nSamples-1,1)],GFB);
%   % Plot result
%   waveplot(1:nSamples,GFB.cf,bm);
%
%   See also gammatone and isGFB.
%
%   Url: http://amtoolbox.sourceforge.net/doc//filters/may2011gammatoneinit.php

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

%   Developed with Matlab 7.4.0.287 (R2007a). Please send bug reports to:
%   
%   Author  :  Tobias May, © 2008 
%              TUe Eindhoven and Philips Research  
%              t.may@tue.nl      tobias.may@philips.com
%
%   History :  
%   v.1.0   2008/01/21
%   v.1.1   2008/02/04 added phase alignment 
%   v.1.2   2008/02/08 fixed bug related to center frequency calculation
%   ***********************************************************************

% Check for proper input arguments
checkInArg(1,7,nargin,mfilename);

% Set default values ...   
if nargin < 2 || isempty(lowFreq);  lowFreq = 80;                    end
if nargin < 3 || isempty(upFreq);   upFreq  = min(5e3,fs/2);         end
if nargin < 4 || isempty(nFilter);  nFilter = round(freq2erb(fs/2)); end
if nargin < 5 || isempty(bUseEar);  bUseEar = false;                 end
if nargin < 6 || isempty(bAlign);   bAlign  = false;                 end
if nargin < 7 || isempty(bInfo);    bInfo   = false;                 end

% First value corresponds to the total number of auditory filters
nTotalFilter = nFilter(1);

% Figure out how many auditory filters should be processed
if length(nFilter) == 1
    filter2Process = 1:nTotalFilter;
else
    filter2Process = nFilter(2:end);
end

% Transform frequencies to erb domain
lowerERB = freq2erb(lowFreq);
upperERB = freq2erb(upFreq);

% Calculate center frequencies being linear spaced on the ERB scale
cf = erb2freq(linspace(lowerERB,upperERB,nTotalFilter));

% Bandwidth correction factor (related to 4rth order gamma function)
bandwidthCorrection = 1.019;

% Bandwidth
bw = erb(cf) * bandwidthCorrection;

% Compute gammatone envelope delay in samples (for 4th order)
delay = (3 * fs)./(bw * 2 * pi);

% Phase compensation factor
pc = -cf*3./bw;

% Store gammatone-related parameter
GFB = struct('object','4th order gammatone filterbank',             ...
             'fcnHandle','gammatoneMEX','fs',fs,                    ...
             'nFilter',nTotalFilter,                                ...
             'filter2Process',[nTotalFilter filter2Process],        ...
             'lowerFreq',lowFreq,'upperFreq',upFreq,'cf',cf,        ...
             'bw',bw,'delay',delay,'phaseCorrection',pc,            ...
             'bOuterMiddleEar',bUseEar,'bPhaseAlign',bAlign,'bInfo',bInfo);

         
%   ***********************************************************************
%   This program is free software: you can redistribute it and/or modify
%   it under the terms of the GNU General Public License as published by
%   the Free Software Foundation, either version 3 of the License, or
%   (at your option) any later version.
% 
%   This program is distributed in the hope that it will be useful,
%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%   GNU General Public License for more details.
% 
%   You should have received a copy of the GNU General Public License
%   along with this program.  If not, see <http://www.gnu.org/licenses/>.
%   ***********************************************************************
