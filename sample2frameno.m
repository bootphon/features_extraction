function n = sample2frameno(x, fs, wintime, steptime)
% find the frame number matching the sample number, given
% the sampling frequency fs, window length wintime and window
% shift steptime.
n = floor((x/fs)/steptime) + 1;
