function y = SeneffEar(x, fs, plotChannel)

% Compute the response of Stephanie Seneff's Auditory Model.  The
% input waveform x (with sampling rate fs) is converted into fourty
% channels of auditory firing probabilities.
%
% This m-function is based on data from the following paper:
%	Benjamin D. Bryant and John D. Gowdy, "Simulation of Stages
%	I and II of Seneff's Auditory Model (SAM) Using Matlab", and 
%	published in the Proceedings of the 1993 Matlab User's Group
%	Conference.
% Thanks to Benjamin Bryant for supplying us with his filter 
% coefficients and the initial organization of this implementation.

% (c) 1998 Interval Research Corporation  

global SeneffFS SeneffPreemphasis SeneffFilterBank
global SeneffForward SeneffBackward

if nargin < 2; fs = 16000; end
if nargin < 3; plotChannel = 0; end

if exist('SeneffFS') ~= 1 | length(SeneffFS) == 0 | SeneffFS ~= fs
	[SeneffPreemphasis, SeneffFilterBank, ...
	 SeneffForward, SeneffBackward] = SeneffEarSetup(fs);
	SeneffFS = fs;
end

y=soscascade(filter(SeneffPreemphasis, [1], x), ...
			SeneffFilterBank);

[width,channels] = size(SeneffForward);
for j=1:channels
	y(j,:) = filter(SeneffForward(:,j),SeneffBackward(:,j),y(j,:));
end

if plotChannel > 0
	clf
	subplot(4,2,1);
	plot(y(plotChannel,:))
	subplot(4,2,2);
	f=find(y(plotChannel,:)>max(y(plotChannel,:))/20);
	plotStart = max(1,f(1)-10);
	plotEnd = min(plotStart+84,length(y(plotChannel,:)));
	plot(y(plotChannel,plotStart:plotEnd));
	ylabel('Filter Bank')
end

% Implement Seneff's detector non-linearity.
hwrA = 10;
hwrB = 65;
hwrG = 2.35;

y = hwrA*atan(hwrB*max(0,y))+exp(hwrA*hwrB*min(0,y));
if plotChannel > 0
	subplot(4,2,3);
	plot(y(plotChannel,:))
	subplot(4,2,4);
	plot(y(plotChannel,plotStart:plotEnd));
	ylabel('HWR');
end

% Implement Seneff's short-term adaptation (a reservoir hair cell
% model.)
start_Cn = 442.96875;
Tua = 58.3333/fs;
Tub = 8.3333/fs;

len = length(x);
Cn = start_Cn*ones(channels,1)*0;		% Does this work non zero?
for j=1:len
	Sn = y(:,j);
	flow = max(0,Tua*(Sn-Cn));
	Cn = Cn + flow - Tub*Cn;
	y(:,j) = flow;
end

% Implement Seneff's Low Pass filter (to model the loss of
% Synchrony.

lpAlpha = .209611;
lpPoly = poly([lpAlpha lpAlpha lpAlpha lpAlpha]);
Glp = sum(lpPoly);

for j=1:channels
	y(j,:) = filter([Glp],lpPoly,y(j,:));
end

if plotChannel > 0
	subplot(4,2,5);
	plot(y(plotChannel,:))
	subplot(4,2,6);
	plot(y(plotChannel,plotStart:plotEnd));
	ylabel('Adaptation');
end

% Finally implement Seneff's Adaptive Gain Control.  This computes
%				y(n) = y(n)/(1+k y'(n))
% where y'(n) is a low-pass filtered version of the input.  Note, 
% this is a single channel AGC.

initial_yn = 0.23071276;
alpha_agc = 0.979382181;
kagc = 0.002;

for j=1:channels
	averageY(j,:) = filter([0 1-alpha_agc],[alpha_agc],y(j,:),initial_yn);
end
y = y./(1+kagc*averageY);

if plotChannel > 0
	subplot(4,2,7);
	plot(y(plotChannel,:))
	subplot(4,2,8);
	plot(y(plotChannel,plotStart:plotEnd));
	ylabel('AGC');
end

