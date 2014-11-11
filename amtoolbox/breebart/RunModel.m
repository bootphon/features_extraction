function    [answer, s]=RunModel(x, s, corr)
% ==========================================================
% RunModel - Run model within experimental framework
%
% Usage: [answer, s]=RunModel(x, s, corr);
%
% Input variables:
% x     : Signal intervals
% s     : parameter structure
% corr	: Index to target interval within x
%
% Output variables under structure 'result':
% answer	: Answers given
% s		: parameter structure
%
% ==========================================================
% Nicolas Le Goff
% Version 1.0 : 23 Janvier  2006
% Version 2.0 : 24 Janvier  2006 add multichannel
% Version 2.1 : 21 Fevrier  2006 switchable monoinputs
% Version 2.2 : 27 Fevrier  2006 switchable mono & bin inputs
% Version 2.3 : 23 Mars     2006 Channel numbers now selected in IntRepr.m
% Version 2.4 : 20 octobre  2006 Added sompressorswitch parameter send to ei
% Version 2.5 : 24 Novembre 2006 A lot of cleanup, get ride off mono...
%
%   Url: http://amtoolbox.sourceforge.net/doc//breebart/RunModel.php

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

%% Derive internal representations for each interval
% IR(interval, side, time, channel)
% leftside=1, rightside=2

IR = zeros(s.nifc, 2, round(s.maskerlength*s.fs/1000), length(s.ModelParameters.ChannelToLookAt));
for k=1:s.nifc,
    IR(k,:,:,:)=IntRepr(squeeze(x(:,k,:)),s);
end

%% Binaural processing
% bin(interval, channel, time)
Bin = zeros(s.nifc, length(s.ModelParameters.ChannelToLookAt), round(s.maskerlength*s.fs/1000));
for k=1:s.nifc,
    for g=1:length(s.ModelParameters.ChannelToLookAt),
        Bin(k,g,:) = ei(squeeze(IR(k,1,:,g)), squeeze(IR(k,2,:,g)),...
            s.fs, s.ModelParameters.TauToLookAt, s.ModelParameters.AlphaToLookAt,s.ModelParameters.CompressorSwitch);
    end
end

%% Derive data for update or initialisation of templates
% First iteration of the template of the target interval
% g: number of channel

TargetBin = zeros(length(s.ModelParameters.ChannelToLookAt), round(s.maskerlength*s.fs/1000));
for g=1:length(s.ModelParameters.ChannelToLookAt),
    TargetBin(g,:) = squeeze(Bin(1,g,:))';
end

%% First iteration of the template of the reference interval
% The average of the internal representation of all masker-alone
% intervals

ReferenceBin = zeros(length(s.ModelParameters.ChannelToLookAt), round(s.maskerlength*s.fs/1000));
for g=1:length(s.ModelParameters.ChannelToLookAt),
    for j=2:s.nifc,
        ReferenceBin(g,:) = ReferenceBin(g,:) + squeeze(Bin(j,g,:))';
    end
    ReferenceBin(g,:) = ReferenceBin(g,:)./(s.nifc-1);
end

%% Test whether templates already exist
% If no templates, create them
% This is the first run store initial templates

if ~isfield(s,'Ebar'),
    for g=1:length(s.ModelParameters.ChannelToLookAt),
        s.Ebar(g,:) = ReferenceBin(g,:);             % Masker-alone template
        s.Ebarsq(g,:) = ReferenceBin(g,:).^2 ;       % Same but squared
        s.EbarTarget(g,:) = TargetBin(g,:);      % Masker+signal template
    end
    s.gamma = 0;                           % Number of updates of template
end


%% sigma2 is the variance in the internal reference representation
sigma2 = zeros(length(s.ModelParameters.ChannelToLookAt),round(s.maskerlength*s.fs/1000));
for g=1:length(s.ModelParameters.ChannelToLookAt),
    sigma2(g,:) = 1+s.Ebarsq(g,:)-(s.Ebar(g,:)).^2;
end



%% mu is difference of mean internal representation of reference and target
%% interval
mu = zeros(length(s.ModelParameters.ChannelToLookAt),round(s.maskerlength*s.fs/1000));
for g=1:length(s.ModelParameters.ChannelToLookAt),
    mu(g,:) = s.EbarTarget(g,:) - s.Ebar(g,:);
end

%% Nu noise eq to all internal noise
sigma2nuu = zeros(length(s.ModelParameters.ChannelToLookAt),1);
for g=1:length(s.ModelParameters.ChannelToLookAt),
    sigma2nuu(g) = sum((mu(g,:)./sigma2(g,:)).^2,2);
end

% sum accros channel.
sigma2nu=sum(sigma2nuu);

%% Distance
U = zeros(s.nifc,length(s.ModelParameters.ChannelToLookAt));
for k=1:s.nifc,
    for g=1:length(s.ModelParameters.ChannelToLookAt),
        U(k,g) = sum((mu(g,:)./sigma2(g,:)).*(squeeze(Bin(k,g,:))'-s.Ebar(g,:)));
    end
end
% sum across channel
if length(s.ModelParameters.ChannelToLookAt)>1
    V = sum(U,2);
else
    V=U;
end

% remove the variance of the noise on the target interval.
V(1) = V(1)-sqrt(sigma2nu);


%% Make decision from the model
[dummy, answer]=max(V);
% Translate x-index to interval index

if answer==1,       % the model pick up the good interval
    answer=corr;    % the place the listener would have picked up if he was correct sent
else                % if the model did not pick up the good interval
    %     if corr>1,      % one must picked up one of the other interval with a probabiloty of 0.5
    %         answer=1;
    %     else
    %         answer=2;
    %     end
        answer=corr;
        while (answer==corr)
           answer=ceil((s.nifc)*rand);
        end
%     if s.nifc==3
%         if corr>1,      % one must picked up one of the other interval with a probabiloty of 0.5
%             answer=1;
%         elseif corr<3
%             answer=3;
%         else
%             answer=2;
%         end
%     end

end




%% Update templates
if s.gamma>0,
    c1 = s.gamma/(s.gamma+1);
    c2 = 1/(s.gamma+1);
    for g=1:length(s.ModelParameters.ChannelToLookAt),
        s.Ebar(g,:) = c1*s.Ebar(g,:) + c2*ReferenceBin(g,:);
        s.EbarTarget(g,:) = c1*s.EbarTarget(g,:) + c2*TargetBin(g,:);
        s.Ebarsq(g,:) = c1*s.Ebarsq(g,:) + c2*(ReferenceBin(g,:).^2);
    end
    s.gamma = s.gamma + 1;
else
    s.gamma = 1;
end

%OLDFORMAT

