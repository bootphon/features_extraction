function y=EpslionFromTauFS(tau, fs)
% EpsilonFromTauFS - Find first order filter coefficient
% as a function of time constant (tau) and sample rate (fs)

% (c) 1998 Interval Research Corporation  
y=1-exp(-1/tau/fs);
