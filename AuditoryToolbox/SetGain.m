% filter = SetGain(filter, desired, f, fs)
% Set the gain of a filter (1x5 vector) to any desired gain
% at any desired frequency (f).

% (c) 1998 Interval Research Corporation  

function filter = SetGain(filter, desired, f, fs)
oldGain = 10^(FreqResp(filter, f, fs)/20);
filter(1:3) = filter(1:3)*desired/oldGain;
