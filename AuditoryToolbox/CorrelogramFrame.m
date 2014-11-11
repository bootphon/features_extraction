% pic = CorrelogramFrame(data, picWidth, start, winLen)
% Compute one frame of a correlogram.  The input data is a
% two-dimensional array of cochlear data, each row representing
% firing probabilities from one cochlear channel.  The output
% picture is a two dimensional array of width "picWidth".
%
% The correlogram is computed with autocorrelation using 
% data from the input array.  For each channel, the data from
% is extracted starting at column "start" and extending for 
% "winLength" time steps.  

% (c) 1998 Interval Research Corporation

function pic = CorrelogramFrame(data, picWidth, start, winLen)

if nargin < 2
	disp('Syntax: pic=CorrelogramFrame(data, picWidth[, start, len])');
	return
end

if nargin < 3
	start = 1;
end

if nargin < 4
	[channels, winLen] = size(data);
end
	
[channels, dataLen] = size(data);
start = max(1, start);
last = min(dataLen, start+winLen-1);

pic = zeros(channels, picWidth);
fftSize = 2^(nextpow2(max(picWidth, winLen))+1);
% disp(['CorrelogramFrame fftSize is ' int2str(fftSize)]);

a = .54;
b = -.46;
wr = sqrt(64/256);
phi = pi/winLen;
ws = 2*wr/sqrt(4*a*a+2*b*b)*(a + b*cos(2*pi*(0:winLen-1)/winLen + phi));

for i=1:channels
	f = zeros(1, fftSize);
%	d = zeros(1, winLen);
%	d(1:(last-start+1)) = data(i,start:last) .* ws(1:(last-start+1));
%	f(1:(winLen/2)) = d(winLen/2+1:winLen);
%	f(fftSize-(winLen/2)+1:fftSize) = d(1:min(length(d),winLen/2));
	f(1:(last-start+1)) = data(i,start:last) .* ws(1:(last-start+1));
	f = fft(f);
	f = ifft(f.*conj(f));
	pic(i,:) = real(f(1:picWidth));
	if pic(i,1) > pic(i,2) & pic(i,1) > pic(i,3)
		pic(i,:) = pic(i,:)/sqrt(pic(i,1));
	else
		pic(i,:) = zeros(1,picWidth);
	end
end
