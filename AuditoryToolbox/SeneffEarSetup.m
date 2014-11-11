function [SeneffPreemphasis, SeneffFilterBank, SeneffForward, SeneffBackward] ...
			= SeneffEarSetup(fs)
			
% This m-function is based on data from the following paper:
%	Benjamin D. Bryant and John D. Gowdy, "Simulation of Stages
%	I and II of Seneff's Auditory Model (SAM) Using Matlab", and 
%	published in the Proceedings of the 1993 Matlab User's Group
%	Conference.
% Thanks to Benjamin Bryant for supplying us with his filter 
% coefficients and the initial organization of this implementation.

% (c) 1998 Interval Research Corporation  

% Set the following variable to a non-zero value to see a summary
% of the filter bank's behaviour.
plotTests = 0;

% The following values were taken from Figure 2 of Bryant's paper.
PreemphasisRTheta = [0.86 3.1148863;0.99 0; 0.5 0; 0.95 3.14159];

% The following values were taken from Table 1 of Bryant's paper.
% They represent the cascade zeros (R-z and Theta-z), and the 
% second order poles (radius and theta) and zeros (radius and theta/2).
%
%    R-z       Theta-z       Radius       Theta       R-z2
FilterBankRTheta = [
     0         3.14159       0.740055     2.633909    0.8
     0.86      2.997077      0.753637     2.178169    0.8
     0.86      2.879267      0.775569     1.856744    0.8
     0.86      2.761458      0.798336     1.617919    0.8
     0.86      2.643648      0.819169     1.433496    0.8
     0.86      2.525839      0.837158     1.286795    0.8
     0.8       2.964876      0.852598     1.167321    0.8
     0.86      2.408029      0.865429     1.068141    0.8
     0.86      2.29022       0.876208     0.984489    0.8
     0.86      2.17241       0.885329     0.912985    0.8
     0.86      2.054601      0.893116     0.851162    0.8
     0.86      1.936791      0.899823     0.797179    0.8
     0.8       2.788161      0.906118     0.749633    0.8
     0.86      1.818981      0.911236     0.70744     0.8
     0.86      1.701172      0.915747     0.669742    0.8
     0.86      1.583362      0.919753     0.635858    0.8
     0.86      1.465552      0.923335     0.605237    0.8
     0.86      1.347743      0.926565     0.57743     0.8
     0.8       2.611447      0.929914     0.552065    0.8
     0.86      1.229933      0.932576     0.528834    0.8
     0.86      1.112123      0.944589     0.487783    0.75
     0.86      0.994314      0.957206     0.452645    0.660714
     0.86      0.876504      0.956548     0.42223     0.672143
     0.86      0.758694      0.956653     0.395644    0.682143
     0.8       2.434732      0.956518     0.372208    0.690966
     0.86      0.640885      0.956676     0.351393    0.69881
     0.86      0.523075      0.956741     0.316044    0.712143
     0.8       2.258018      0.956481     0.287157    0.723052
     0.8       2.081304      0.956445     0.263108    0.732143
     0.8       1.904589      0.956481     0.242776    0.739835
     0.86      0.405265      0.958259     0.217558    0.749384
     0.8       1.727875      0.963083     0.197086    0.757143
     0.8       1.55116       0.969757     0.175115    0.769048
     0.8       1.374446      0.97003      0.153697    0.780662
     0.8       1.197732      0.970382     0.134026    0.791337
     0.8       1.021017      0.970721     0.118819    0.799596
     0.8       1.5           0.970985     0.106711    0.8
     0.8       1.2           0.971222     0.096843    0.8
     0.8       1             0.97144      0.088645    0.8
     0.8       0.9           0.971645     0.081727    0.8];

% Let's plot the cascade zero locations and the locations of the
% pole and zeros in the resonator.
if plotTests
	clf;
	subplot(3,3,1);
	plot(FilterBankRTheta(:,1).*exp(i*FilterBankRTheta(:,2)))
	axis([-1 1 0 1])
	title('Cascade Zero Locations')

	subplot(3,3,2);
	plot([FilterBankRTheta(:,3).*exp(i*FilterBankRTheta(:,4)) ...
		  FilterBankRTheta(:,5).*exp(i*FilterBankRTheta(:,4)/2)],'+')
	title('Resonator Pole/Zero')
	drawnow;
end

% Convert r-theta form, first into a list of roots, then a polynomial
roots=exp(i*PreemphasisRTheta(:,2)).*PreemphasisRTheta(:,1);
SeneffPreemphasis=real(poly([roots;conj(roots)]));

% Plot the preemphasis filter response, if desired
if plotTests
	subplot(3,3,3);
	freqScale=(0:255)/256*8000;
	freqresp = FreqResp(SeneffPreemphasis,[1], freqScale, 16000);
	semilogx(freqScale,freqresp)
	title('Preemphasis Response');
	axis([100 10000 -60 20])
	drawnow;
end

% Now figure out the second order sections that make up the main
% filter bank cascade.  We put the zeros into the numerator (b's)
% and there are no poles.  Just to keep things simpler, we adjust
% the gain of each filter to keep it unity gain at DC.
[channels,width] = size(FilterBankRTheta);
roots=exp(i*FilterBankRTheta(:,2)).*FilterBankRTheta(:,1);
SeneffFilterBank = zeros(channels,5);
for j=1:channels
	SeneffFilterBank(j,1:3) = poly([roots(j) conj(roots(j))]);
	SeneffFilterBank(j,1:3) = SeneffFilterBank(j,1:3)/sum(SeneffFilterBank(j,1:3));
end

% Plot the cascade zero responses, if desired.
if plotTests
	subplot(3,3,4);
	y=soscascade([1 zeros(1,511)],SeneffFilterBank);
	freqresp=20*log10(abs(fft(y(1:5:40,:)')));
	freqScale=(0:511)/512*16000;
	semilogx(freqScale(1:256),freqresp(1:256,:))
	axis([100 10000 -150 0]);
	title('Cascade Response');
	drawnow;
end

% Now figure out the resonating filters.  Each of these resonators
% is a double pole-zero pair.
zlocs = FilterBankRTheta(:,5).*exp(i*FilterBankRTheta(:,4)/2);
plocs = FilterBankRTheta(:,3).*exp(i*FilterBankRTheta(:,4));
SeneffForward = zeros(5,channels);
SeneffBackward = zeros(5,channels);

for j=1:channels
	SeneffForward(:,j) = real(poly([zlocs(j) conj(zlocs(j)) ...
										zlocs(j) conj(zlocs(j))]))';
	SeneffBackward(:,j) = real(poly([plocs(j) conj(plocs(j)) ...
										plocs(j) conj(plocs(j))]))';
end

% Now plot the frequency response of just the resonating filters.
% These are all bandpass filters.
if plotTests
	subplot(3,3,5);
	impulse = [1 zeros(1,255)];
	y=zeros(256,channels);
	for j=1:40
		y(:,j) = filter(SeneffForward(:,j),SeneffBackward(:,j),impulse)';
	end
	freqresp=20*log10(abs(fft(y(:,1:5:40))));
	freqScale=(0:255)/256*16000;
	semilogx(freqScale(1:128),freqresp(1:128,:))
	axis([100 10000 -30 40]);
	title('Resonators Response')
	drawnow;
end

% The plot below shows the overall response of the preemphasis filters
% along with the just-designed cascade of zeros.
if plotTests
	subplot(3,3,6);
	impulse = [1 zeros(1,511)];
	y=soscascade(filter(SeneffPreemphasis, [1], impulse), ...
				SeneffFilterBank);
	freqresp=20*log10(abs(fft(y(1:5:40,:)')));
	freqScale=(0:511)/512*16000;
	semilogx(freqScale(1:256),freqresp(1:256,:))
	axis([100 10000 -100 25]);
	title('Preemphasis+Cascade');
	drawnow;
end

% Now we need to normalize the gain of each channel.  We run an impulse
% through the preemphasis filter, and then through the cascade of zeros.
% Finally, we run it through each of the resonator filters.
impulse = [1 zeros(1,255)];
y=soscascade(filter(SeneffPreemphasis, [1], impulse), ...
			SeneffFilterBank);
for j=1:channels
	y(j,:) = filter(SeneffForward(:,j),SeneffBackward(:,j),y(j,:));
end

% Now we have impulse responses from each filter.   We can find the FFT
% and then find the gain peak.  We divide each forward polynomial by the
% maximum gain (to normalize) and then multiply by the desired low
% frequency roll-off.  The Bryant paper says that the last 24 channels
% should be cut at 6dB per octave and that this occurs at 1600 Hz, but 
% it looks to me like the gain change happens at 3200 Hz.
freqresp=abs(fft(y'));
gain = ones(1,channels)./max(freqresp);
cfs = FilterBankRTheta(:,4)/pi*fs/2;
rolloff = min(cfs/1600,1);
	
for j=1:channels
	SeneffForward(:,j)=SeneffForward(:,j)*gain(j)*rolloff(j);
end
	
% All Done. The figure below should match Figure 3 of Bryant's paper.
if plotTests
	subplot(3,3,8);
	impulse = [1 zeros(1,511)];
	y=soscascade(filter(SeneffPreemphasis, [1], impulse), ...
				SeneffFilterBank);
	for j=1:channels
		y(j,:) = filter(SeneffForward(:,j),SeneffBackward(:,j),y(j,:));
	end

	freqresp=20*log10(abs(fft(y(1:5:40,:)')));
	freqScale=(0:511)/512*16000;
	plot(freqScale(1:256),freqresp(1:256,:))
	axis([100 10000 -120 0]);
	title('Magnitude Response vs. Linear Frequency');
	drawnow;
end


function mag=FreqResp(b,a,f,fs)
cf = exp(i*2*pi*f/fs);
num = 0;
for i=1:length(b)
	num = num + b(end-i+1)*cf.^i;
end

denom = 0;
for i=1:length(a)
	denom = denom + a(end-i+1)*cf.^i;
end
mag = 20*log10(abs(num./denom));
