function [] = sig2drnl(sig, fs, outfile, conf)

wlen = getfield(conf, 'wlen');
frate = getfield(conf, 'frate');
dec_factor = round((size(sig,1)/fs) / wlen);

spec = drnl(sig, fs, 'jepsen2008');
spec = ihcenvelope(spec, fs, 'ihc_dau');

spec = spec';

% decimate
spec = spec(:, 1:dec_factor:size(spec,2));
features = spec';

duration = numel(sig)/fs;
center_times = linspace(0, duration, size(features, 1));
save('-7', [outfile '.mat'], 'features', 'center_times');
