function plotlikelilangendijk(la,le,ci,tit,horlin)
%PLOTLIKELILANGENDIJK likelihood statistics according to Langendijk et al. (2002)
%   Usage:           plotlikelilangendijk(la,le,ci)
%                    plotlikelilangendijk(la,le,ci,tit)
%                    plotlikelilangendijk(la,le,ci,dynlin)
%
%   Input parameters:
%     la     : actual likelihood
%     le     : expected likelihood
%     ci     : confidence interval for expected likelihood
%     tit    : set to 'spatstrat' optionally
%     horlin : vector with dynamic values for reference lines
%
%   plotlikelangendijk(la,le,ci) plots likelihood statistics according to
%   Langendijk et al. (2002). XXX DESCRIPTION OF PARAMETERS
%
%   See also: likelilangendijk, langendijk
%
%   References:
%     E. Langendijk and A. Bronkhorst. Contribution of spectral cues to human
%     sound localization. J. Acoust. Soc. Am., 112:1583-1596, 2002.
%     
%     
%
%   Url: http://amtoolbox.sourceforge.net/doc//monaural/plotlikelilangendijk.php

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
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% AUTHOR : Robert Baumgartner, OEAW Acoustical Research Institute
% latest update: 2010-08-25
% Modifications: Sebastian Grill 2011-9
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if ~exist('tit','var')
    tit='';
end

% figure('Name','Likelihood statistic','NumberTitle','off');
% clf
hold on
if ~exist('horlin','var')
    plot(0.5:length(la)+0.5,275*ones(length(la)+1,1),'k:') % unimodal
    plot(0.5:length(la)+0.5,350*ones(length(la)+1,1),'k:') % bimodal
    plot(0.5:length(la)+0.5,400*ones(length(la)+1,1),'k:') % trimodal
    plot(0.5:length(la)+0.5,437*ones(length(la)+1,1),'k:') % flat
else
    plot(0.5:length(la)+0.5,horlin(1)*ones(length(la)+1,1),'k:') % unimodal
    plot(0.5:length(la)+0.5,horlin(2)*ones(length(la)+1,1),'k:') % bimodal
    plot(0.5:length(la)+0.5,horlin(3)*ones(length(la)+1,1),'k:') % trimodal
    plot(0.5:length(la)+0.5,horlin(4)*ones(length(la)+1,1),'k:') % flat
end
h=bar(la);
set(gca,'XTick',1:length(la))
if strcmp('spatstrat',tit)==1
    set(gca,'XTickLabel',{'baseline';'dummy';'warped'})
end
set(gca,'YLim',[200 550],'Layer','top')
set(gca,'Box','on')
set(h,'FaceColor','white','BarWidth',0.5)
ylabel('Likelihood')
xlabel('Condition')
errorbar(le,(ci(:,1)-ci(:,2))/2,'k.');
hold off

end

