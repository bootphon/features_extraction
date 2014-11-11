function [ varargout ] = pmv2ppp( p,varargin )
%PMV2PPP PMV to PPP conversion
%   Usage:  [ qe,pe,pb ] = pmv2ppp( p,tang,rang );
%           [ qe,pe,pb ] = pmv2ppp( p,tang,rang,exptang );
%
%   Input parameters:
%     p          : prediction matrix (response PMVs)
%     tang       : possible polar target angles. As default, ARI's MSP 
%                  polar angles in the median SP is used.
%     rang       : polar angles of possible response angles.
%                  As default regular 5°-sampling is used (-30:5:210).    
%
%   Output parameters:
%     qe         : quadrant error rate
%     pe         : local polar RMS error in degrees
%     pb         : polar bias in degrees
%
%   PMV2PPP(...) retrieves commonly used PPPs (Psychoacoustic performance
%   parameters) for sagittal-plane (SP) localization like quadrant error
%   (QEs), local polar RMS error (PE), and local polar bias (PB) from
%   response PMVs (probability mass vectors) predicted by a localization
%   model. PPPs are retreived either for a specific polar target angle or as
%   an average across all available target angles. The latter is the
%   default.
%
%   PMV2PPP needs the following optional parameter in order to retrieve
%   the PPPs for a specific (set of) target angles:
%
%     'exptang', exptang   experimental polar target angles
%
%   PMV2PPP accepts the following flag:
%
%     'print'      Display the outcomes.
%
%   Example:
%   ---------
%
%   To evaluate chance performance of QE and PE use :
%
%     [qe,pe] = pmv2ppp(ones(49,44));
%
%   References:
%     R. Baumgartner. Modeling sagittal-plane sound localization with the
%     application to subband-encoded head related transfer functions.
%     Master's thesis, University of Music and Performing Arts, Graz, June
%     2012.
%     
%     R. Baumgartner, P. Majdak, and B. Laback. Assessment of Sagittal-Plane
%     Sound Localization Performance in Spatial-Audio Applications,
%     chapter 4, page expected print date. Springer-Verlag GmbH, accepted for
%     publication, 2013.
%     
%
%   Url: http://amtoolbox.sourceforge.net/doc//modelstages/pmv2ppp.php

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

% AUTHOR : Robert Baumgartner

definput.flags.print = {'noprint','print'};
definput.keyvals.rang=-30:5:210;
definput.keyvals.tang=[-30:5:70,80,100,110:5:210];
definput.keyvals.exptang=[];
[flags,kv]=ltfatarghelper({'tang','rang','exptang'},definput,varargin);

    
p = p./repmat(sum(p),length(kv.rang),1);  % ensure probability mass vectors
tang = kv.tang(:);
rang = kv.rang(:);
nt = length(tang);

qet = zeros(nt,1); % QE for each target angle
pet = zeros(nt,1); % PE for each target angle
pbt = zeros(nt,1); % PB for each target angle
for ii = 1:nt % for all target positions
    d = tang(ii)-rang;                 % wraped angular distance between tang & rang
    iduw = (d < -180) | (180 < d);     % 180°-unwrap indices
    d(iduw) = mod(d(iduw) + 180,360) - 180; % 180° unwrap
    d = abs(d);                        % absolut distance
    qet(ii) = sum( p(d>=90,ii) );
    pc = p(d<90,ii);                   % pmv for conditional probability excluding QEs
    pc = pc/sum(pc);                   % normalization to sum=1
    pet(ii) = sqrt( sum( pc .* (d(d<90)).^2 )); % RMS of expected difference
    pbt(ii) = sum( pc .* rang(d<90) ) - tang(ii); % expectancy value of rang - tang
end

if ~isempty(kv.exptang)
    extang = [-90; tang(:); 270]; % extended tang for targets outside
    qetb = (qet(1)+qet(end))/2;  % boundaries for extang
    etb = (pet(1)+pet(end))/2;
    pbtb = (pbt(1)+pbt(end))/2;
    qet = interp1(extang,[qetb; qet(:); qetb],kv.exptang);
    pet = interp1(extang,[etb; pet(:); etb],kv.exptang);
    pbt = interp1(extang,[pbtb; pbt(:); pbtb],kv.exptang);
end

qe = mean(qet)*100;
pe = mean(pet);
pb = mean(pbt);

varargout{1} = qe;
varargout{2} = pe;
varargout{3} = pb;


if flags.do_print
    fprintf('Quadrant errors (%%) \t\t %4.1f \n',qe)
    if nargout > 1
      fprintf('Local polar RMS error (deg) \t %4.1f \n',pe)
    end
    if nargout > 2
      fprintf('Local polar bias (deg) \t\t %4.1f \n',pb)
    end
end

end
