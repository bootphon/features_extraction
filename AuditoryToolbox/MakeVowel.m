function y=MakeVowel(len, pitch, sampleRate, f1, f2, f3)
%  MakeVowel(len, pitch [, sampleRate, f1, f2, f3]) - Make a vowel with
%    "len" samples and the given pitch.  The sample rate defaults to
%    be 22254.545454 Hz (the native Mactinosh Sampling Rate).  The
%    formant frequencies are f1, f2 & f3.  Some common vowels are
%               Vowel       f1      f2      f3
%                /a/        730    1090    2440
%                /i/        270    2290    3010
%                /u/        300     870    2240
%
% The pitch variable can either be a scalar indicating the actual
%      pitch frequency, or an array of impulse locations. Using an
%      array of impulses allows this routine to compute vowels with
%      varying pitch.
%
% Alternatively, f1 can be replaced with one of the following strings
%      'a', 'i', 'u' and the appropriate formant frequencies are
%      automatically selected.
%  Modified by R. Duda, 3/13/94

% (c) 1998 Interval Research Corporation  

if nargin < 2,
   fprintf('Format: y = MakeVowel(len, pitch [, sampleRate, f1, f2, f3])\n');
   return;
end;

if nargin < 6; f3 = 0; end;
if nargin < 5; f2 = 0; end;
if nargin < 4,
   f1 = 0;
else
    if isstr(f1)
        if f1 == 'a' | f1 == '/a/'
               f1=730; f2=1090; f3=2440;
        elseif f1 == 'i' | f1 == '/i/'
               f1=270; f2=2290; f3=3010;
        elseif f1 == 'u' | f1 == '/u/'
               f1=300; f2=870; f3=2240;
        end
   end;
end;

if nargin < 3,
   sampleRate = 22254.545454;
elseif sampleRate < 1000,         % Apparently for test purposes
   sampleRate = 22254.545454;
end;

%  GlottalPulses(pitch, fs, len) - Generate a stream of
%    glottal pulses with the given pitch (in Hz) and sampling
%    frequency (sampleRate).  A vector of the requested length is returned.
y=zeros(1,len);
if length(pitch) > 1,            % If true, use to determine points
   points=pitch;                 % Check for valid sequence of points
   if any(points~=sort(points)),
      error('Values in pitch array must be in ascending order.')
   end;
   if points(1) < 1,
      error('Values in pitch array cannot be less than 1.');
   end;
   kmax=sum(points <= len);
   if kmax == 0,
      error('All values in pitch array exceed "len"; none should.');
   elseif kmax < length(points),
      fprintf('Some values in pitch array exceed "len"; truncating.\n');
      points=points(1:kmax);
   end;
else
    points=1:sampleRate/pitch:len;
end;
indices=floor(points);

%  Use a triangular approximation to an impulse function.  The important
%  part is to keep the total amplitude the same.
y(indices) = (indices+1)-points;
y(indices+1) = points-indices;

%  GlottalFilter(x,fs) - Filter an impulse train and simulate the glottal
%    transfer function.  The sampling interval (sampleRate) is given in Hz.
%    The filtering performed by this function is two first-order filters
%    at 250Hz.
a = exp(-250*2*pi/sampleRate);
%y=filter([1,0,-1],[1,-2*a,a*a],y);      %  Not as good as one below....
y=filter([1],[1,0,-a*a],y);

%  FormantFilter(input, f, fs) - Filter an input sequence to model one
%    formant in a speech signal.  The formant frequency (in Hz) is given
%    by f and the bandwidth of the formant is a constant 50Hz.  The
%    sampling frequency in Hz is given by fs.
if f1 > 0
        cft = f1/sampleRate;
        bw = 50;
        q = f1/bw;
        rho = exp(-pi * cft / q);
        theta = 2 * pi * cft * sqrt(1-1/(4 * q*q));
        a2 = -2*rho*cos(theta);
        a3 = rho*rho;
        y=filter([1+a2+a3],[1,a2,a3],y);
end;

%  FormantFilter(input, f, fs) - Filter an input sequence to model one
%    formant in a speech signal.  The formant frequency (in Hz) is given
%    by f and the bandwidth of the formant is a constant 50Hz.  The
%    sampling frequency in Hz is given by fs.
if f2 > 0
        cft = f2/sampleRate;
        bw = 50;
        q = f2/bw;
        rho = exp(-pi * cft / q);
        theta = 2 * pi * cft * sqrt(1-1/(4 * q*q));
        a2 = -2*rho*cos(theta);
        a3 = rho*rho;
        y=filter([1+a2+a3],[1,a2,a3],y);
end;

%  FormantFilter(input, f, fs) - Filter an input sequence to model one
%    formant in a speech signal.  The formant frequency (in Hz) is given
%    by f and the bandwidth of the formant is a constant 50Hz.  The
%    sampling frequency in Hz is given by fs.
if f3 > 0
        cft = f3/sampleRate;
        bw = 50;
        q = f3/bw;
        rho = exp(-pi * cft / q);
        theta = 2 * pi * cft * sqrt(1-1/(4 * q*q));
        a2 = -2*rho*cos(theta);
        a3 = rho*rho;
        y=filter([1+a2+a3],[1,a2,a3],y);
end;
