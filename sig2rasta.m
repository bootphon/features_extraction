function [] = sig2rasta(sig, fs, outfile, conf)

wlen = getfield(conf, 'wlen');
frate = getfield(conf, 'frate');
steptime = 1 / frate;
[ceps, spec] = rastaplp(sig, fs, getfield(conf, 'dorasta'),
	getfield(conf, 'modelorder'), wlen, steptime);
features = spec';
duration = numel(sig)/fs;
center_times = linspace(0, duration, size(features, 1));
save('-7', [outfile '.mat'], 'features', 'center_times');
