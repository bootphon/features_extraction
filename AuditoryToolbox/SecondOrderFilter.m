% filter = SecondOrderFilter(f, q, fs)
% Design a second order digital filter with a center frequency of
% f, filter quality of q, and digital sampling rate of fs (Hz).

% (c) 1998 Interval Research Corporation  

function filts = SecondOrderFilter(f,q,fs)
cft = f'/fs;
rho = exp(- pi * cft ./ q');
theta = 2 * pi * cft .* sqrt(1-1 ./(4*q'.^2));
filts = [ ones(size(rho)) -2*rho.*cos(theta) rho.^2 ];
