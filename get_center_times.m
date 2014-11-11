function [center_times] = get_center_times(sig, fs, frate, wlen);

duration = numel(sig)/fs;
center_times = linspace(0, duration, numel(sig))

% wshift_smp = fs / frate;
% wlen_smp = wlen * fs;
% nframes = size(sig, 1) / wshift_smp + 1;
% center_times = zeros(nframes, 1);
% for fr = 1:nframes
%     start_smp = round(fr * wshift_smp);
%     end_smp = min(size(sig, 1), start_smp + wlen_smp);
%     start_ms = start_smp / fs;
%     end_ms = end_smp / fs;
%     center_times(fr) = (start_ms + end_ms) / 2;
% end