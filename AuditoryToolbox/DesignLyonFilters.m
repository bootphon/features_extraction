% [filters, freqs] = DesignLyonFilters(fs,EarQ,StepFactor)
% Design the cascade of second order filters and the front filters
% (outer/middle and compensator) needed for Lyon's Passive Short Wave
% (Second Order Sections) cochlear model.  The variables used here come
% from Apple ATG Technical Report #13 titled "Lyon's Cochlear Model".
%
% Most of the parameters are hardwired into this m-function.  The user
% settable parameters are the digital sampling rate, the basic Q of the
% each stage (usually 8 or 4), and the spacing factor between channels
% (usually .25 and .125, respectively.)
%
% The result is returned as rows of second order filters; three coefficients
% for the numerator and two for the denomiator.  The coefficients are
% [A0 A1 A2 B1 B2]..........................(Malcolm 1 Feb. 1993)
%

% (c) 1998 Interval Research Corporation  
function [filters, CenterFreqs, gains] = DesignLyonFilters(fs,EarQ,StepFactor)

if nargin < 2
	EarQ = 8;
end

if nargin < 3
	StepFactor=EarQ/32;
end

Eb = 1000.0;
EarZeroOffset = 1.5;
EarSharpness = 5.0;
EarPremphCorner = 300;

% Find top frequency, allowing space for first cascade filter.
topf = fs/2.0;
topf = topf - (sqrt(topf^2+Eb^2)/EarQ*StepFactor*EarZeroOffset)+ ...
					sqrt(topf^2+Eb^2)/EarQ*StepFactor;

% Find place where CascadePoleQ < .5
lowf = Eb/sqrt(4*EarQ^2-1);
NumberOfChannels = floor((EarQ*(-log(lowf + sqrt(lowf^2 + Eb^2)) + ...
         log(topf + sqrt(Eb^2 + topf^2))))/StepFactor);

% Now make an array of CenterFreqs..... This expression was derived by
% Mathematica by integrating 1/EarBandwidth(cf) and solving for f as a 
% function of channel number.
cn = 1:NumberOfChannels;
CenterFreqs = (-((exp((cn*StepFactor)/EarQ)*Eb^2)/ ...
          (topf + sqrt(Eb^2 + topf^2))) + ...
       (topf + sqrt(Eb^2 + topf^2))./exp((cn*StepFactor)/EarQ))/2;

% OK, now we can figure out the parameters of each stage filter.
EarBandwidth = sqrt(CenterFreqs.^2+Eb^2)/EarQ;
CascadeZeroCF = CenterFreqs +	EarBandwidth * StepFactor * EarZeroOffset;
CascadeZeroQ = EarSharpness*CascadeZeroCF./EarBandwidth;
CascadePoleCF = CenterFreqs;
CascadePoleQ = CenterFreqs./EarBandwidth;

% Now lets find some filters.... first the zeros then the poles
zerofilts = SecondOrderFilter(CascadeZeroCF, CascadeZeroQ, fs);
polefilts = SecondOrderFilter(CascadePoleCF, CascadePoleQ, fs);
filters = [zerofilts polefilts(:,2:3)];

% Now we can set the DC gain of each stage.
dcgain(2:NumberOfChannels)=CenterFreqs(1:NumberOfChannels-1)./ ...
					CenterFreqs(2:NumberOfChannels);
dcgain(1) = dcgain(2);
for i=1:NumberOfChannels
     filters(i,:) = SetGain(filters(i,:), dcgain(i), 0, fs);
end

% Finally, let's design the front filters.
front(1,:) = SetGain([0 1 -exp(-2*pi*EarPremphCorner/fs) 0 0], 1, fs/4, fs);
topPoles = SecondOrderFilter(topf,CascadePoleQ(1), fs);
front(2,:) = SetGain([1 0 -1 topPoles(2:3)], 1, fs/4, fs);

% Now, put them all together.
filters = [front; filters];

if nargout > 2
	% Compute the gains needed so that everything is flat
	% when we do the filter inversion.
	[channels, width] = size(filters);
	len = length(CenterFreqs);
	clear cfs2;
	cfs2(1) = fs/2;
	cfs2(1:2:2*len) = CenterFreqs;
	cfs2(2:2:2*len-1) = (CenterFreqs(1:len-1) + CenterFreqs(2:len))/2;
%	cfs2 = CenterFreqs;
	
	resp = zeros(length(cfs2), channels);
	for i=1:channels
		resp(:,i) = FreqResp(filters(i,:), cfs2, fs)';
	end
	% Each column vector of resp contains one channel's response
	% across all frequencies (in dB).
	cumresp = cumsum(resp')';
	% cumresp now contains the total gain at output of i'th channel at 
	% frequency f
	a=10.^(cumresp(:,3:channels)/20);
	a = a.* a;			% Have to go through each filter twice.
	gains = [0; 0; a \ ones(length(cfs2),1)];
end
