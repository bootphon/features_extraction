% mag = FreqResp(filter, f, fs)
% Find the frequency response (in dB) of a filter (1x5 vector) at
% frequency f and sampling rate fs.

% (c) 1998 Interval Research Corporation  

function mag=FreqResp(filter,f,fs)
cf = exp(i*2*pi*f/fs);
mag = (filter(3) + filter(2)*cf + filter(1)*cf.^2) ./ ...
	(filter(5) + filter(4)*cf + cf.^2);
mag = 20*log10(abs(mag));
