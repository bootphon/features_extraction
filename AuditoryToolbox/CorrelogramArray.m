function movie = CorrelogramArray(data, sr, frameRate, width)
% function movie = CorrelogramArray(data, sr, frameRate, width)
% Compute an array of correlogram frames, from the sound file data with
% a sampling rate of sr Hz.  Compute frameRate frames per second, using
% a window size of "width" samples.

% (c) 1998 Interval Research Corporation

if nargin < 2, sr = 16000; end
if nargin < 3, frameRate = 12; end
if nargin < 4, width = 256; end

[channels, len] = size(data);
frameIncrement = fix(sr/frameRate);
frameCount = floor((len-width)/frameIncrement)+1;
fprintf('Correlogram spacing is %g samples per frame.\n', frameIncrement);

movie = zeros(channels*width, frameCount);
for i=1:frameCount
	start = (i-1)*frameIncrement + 1;
	pic = CorrelogramFrame(data, width, start, frameIncrement*4);
		minimum = min(min(pic));
		maximum = max(max(pic));
		image((pic-minimum)/(maximum-minimum)*length(colormap));
		drawnow;
	movie(:,i) = reshape(pic, channels*width, 1);
end
