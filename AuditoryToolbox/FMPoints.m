function points=FMPoints(len, freq, fmFreq, fmAmp, fs)
% points=FMPoints(len, freq, fmFreq, fmAmp, fs)
% Generates (fractional) sample locations for frequency-modulated impulses
%     len         = number of samples
%     freq        = pitch frequency (Hz)
%     fmFreq      = vibrato frequency (Hz)  (defaults to 6 Hz)
%     fmAmp       = max change in pitch  (defaults to 5% of freq)
%     fs          = sample frequency     (defaults to 22254.545454 samples/s)
%
% Basic formula: phase angle = 2*pi*freq*t + (fmAmp/fmFreq)*sin(2*pi*fmFreq*t)
%     k-th zero crossing approximately at sample number
%     (fs/freq)*(k - (fmAmp/(2*pi*fmFreq))*sin(2*pi*k*(fmFreq/freq)))

% (c) 1998 Interval Research Corporation  

if nargin<2,
   fprintf('Format: sig=fmPoints(len, freq [, fmAmp, fmFreq, sampleRate])\n');
   return;
   end;
if nargin<5,
   fs=22254.545454;
   end;
if nargin<4,
   fmAmp=0.05*freq;
   end;
if nargin<3,
   fmFreq=6;
end;

kmax=fix(freq*(len/fs));
points=0:kmax-1;
points=1+(fs/freq)*(points-(fmAmp/(2*pi*fmFreq))* ...
sin(2*pi*(fmFreq/freq)*points));
