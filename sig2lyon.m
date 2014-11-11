function [] = sig2lyon(sig, fs, outfile, conf)

wlen = getfield(conf, 'wlen');
frate = getfield(conf, 'frate');
decimation_factor = round((size(sig,1)/fs) / wlen);
spec = LyonPassiveEar(sig, fs, decimation_factor);
features = spec';
duration = numel(sig)/fs;
center_times = linspace(0, duration, size(features, 1));
save('-7', [outfile '.mat'], 'features', 'center_times');
