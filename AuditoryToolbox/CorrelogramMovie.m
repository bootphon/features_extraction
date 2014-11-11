function movie = CorrelogramMovie(data, sr, frameRate, width)
% function movie = CorrelogramMovie(data, sr, frameRate, width)
% Compute a Matlab movie of a sound array called "data" which has
% a sampling rate of sr Hz.  Compute frameRate frames per second, each
% time taking "width" samples for analysis.

% (c) 1998 Interval Research Corporation

if nargin < 2, sr = 16000; end
if nargin < 3, frameRate = 12; end
if nargin < 4, width = 256; end

[channels, len] = size(data);
frameIncrement = fix(sr/frameRate);
frameCount = floor((len-width)/frameIncrement)+1;

movie = moviein(frameCount);
for i=1:frameCount
	start = (i-1)*frameIncrement + 1;
	pic = CorrelogramFrame(data, width, start, frameIncrement*2);
		minimum = min(min(pic));
		maximum = max(max(pic));
		image((pic-minimum)/(maximum-minimum)*length(colormap));
		drawnow;
	movie(:,i) = getframe;
end
