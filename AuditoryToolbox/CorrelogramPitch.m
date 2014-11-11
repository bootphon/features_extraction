function [pitch,salience]=CorrelogramPitch(correlogram, width, sr, low, high);
% pitch=CorrelogramPitch(correlogram, width [, sr, lowPitch, highPitch]) 
% computes the pitch of a correlogram sequence by finding the time lag
% with the largest correlation energy. 
%  
% (c) 1998 Interval Research Corporation

if nargin < 3; sr=22254.54; end;
if nargin < 4; low=0; end;
if nargin < 5; high=inf; end;

dropLow = floor(sr/high);
if low > 0
	dropHigh = min(width,ceil(sr/low));
else
	dropHigh = width;
end

[pixels frames] = size(correlogram);
channels = pixels/width;
if channels < 1 | floor(channels) ~= channels
	error('Correlogram Size Error');
end

pitch = zeros(1,frames);
salience = zeros(1,frames);
for j=1:frames
	% Get one frame from the correlogram, reshape it, and compute
	% the sum (as a function of time lag) across all channels.
	if channels == 1
		summary=reshape(correlogram(:,j),channels,width);
	else
		summary=sum(reshape(correlogram(:,j),channels,width));
	end
	zeroLag = summary(1);
	% Now we need to find the first pitch past the peak at zero
	% lag.  The following lines smooth the summary pitch a bit, then
	% look for the first point where the summary goes back up.  
	% Everything up to this point is zeroed out.
	windowLength=16;
	sumfilt=filter(ones(1,windowLength),[1],summary);
	sumdif=sumfilt(2:width)-sumfilt(1:width-1);
	sumdif(1:windowLength) = zeros(1,windowLength);
	valleys=find(sumdif>0);
	summary(1:valleys(1)) = zeros(1,valleys(1));
	summary(1:dropLow) = zeros(1,dropLow);
	summary(dropHigh:width) = zeros(1,width-dropHigh+1);
	plot(summary);
	drawnow;
	% Now find the location of the biggest peak and call this the pitch
	[m p] = max(summary);
	pitch(j) = sr/(p-1);
	salience(j) = m/zeroLag;
end
