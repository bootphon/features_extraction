function y=rasta(x,fs,low,high)
% function y=rasta(x,fs) where x is the input data (rows of time data), 
% and fs is the frame rate (sampling rate) in Hz.  This is a modified 
% version of the original filter. Here the RASTA filter is approximated
% by a simple fourth order Butterworth bandpass filter.  See pages 50-51
% of my second IRC logbook for the derivation.
%
% Hermansky and Morgan, "RASTA Processing of Speech."  IEEE Transactions
% on Speech and Audio Processing.  vol. 2, no. 4, October 1994
%

% (c) 1998 Interval Research Corporation  
% Malcolm Slaney, January 30, 1996, Interval Research Corporation

if (nargin < 2); fs=100; end
if (nargin < 3); low=.9; end
if (nargin < 4); high=12.8; end

if (low == 0 & high == 0)			% Original Filter
	num = .1*[2 1 0 -1 -2];
	denum = [1 -.94];
else						% New fourth order 
						% Butterworth BP filter
	w1=low/fs*2*pi;
	w2=high/fs*2*pi;
	theta=1;
	
	a=cos((w1+w2)/2)/cos((w2-w1)/2);
	k=cot((w2-w1)/2)*tan(theta/2);
	
	num = [1 0 -2 0 1];
	denum = [(1 + 2*2^(1/2)*k + 4*k^2) ...
			(-4*2^(1/2)*a*k - 16*a*k^2) ...
			(-2 + 8*k^2 + 16*a^2*k^2) ...
			(4*2^(1/2)*a*k - 16*a*k^2) ...
			(1 - 2*2^(1/2)*k + 4*k^2)];
	scale = denum(1);			% Scale by a(1) component
	num = num/scale;
	denum = denum/scale;
end

if (0)
	len = 1024;
	impulse = zeros(1,len);
	impulse(1) = 1;
	
	y=filter(num,denum,impulse);
	ym = abs(fft(y));
	ym=20*log10(ym);
	f=(0:(len-1))/len*fs;
	semilogx(f(1:len/2),ym(1:len/2));
	drawnow;
end

if (length(x) == size(x,1)*size(x,2))
	y = filter(num,denum,x,x(1)*[-1 -1 1 1]);
else
	y = zeros(size(x));
	for i=1:size(x,1)
		y(i,:) = filter(num,denum,x(i,:),x(i,1)*[-1 -1 1 1]/scale);
	end
end
