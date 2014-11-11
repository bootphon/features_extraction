function [aCoeff,resid,pitch,G,parcor,stream] = proclpc(data,sr,L,fr,fs,preemp)
% USAGE: [aCoeff,resid,pitch,G,parcor,stream] = proclpc(data,sr,L,fr,fs,preemp)
%
% This function computes the LPC (linear-predictive coding) coefficients that
% describe a speech signal.  The LPC coefficients are a short-time measure of
% the speech signal which describe the signal as the output of an all-pole
% filter.  This all-pole filter provides a good description of the speech
% articulators; thus LPC analysis is often used in speech recognition and 
% speech coding systems.  The LPC parameters are recalculated, by default in
% this implementation, every 20ms.
%
% The results of LPC analysis are a new representation of the signal
%	s(n) = G e(n) - sum from 1 to L a(i)s(n-i)
% where s(n) is the original data.  a(i) and e(n) are the outputs of the LPC 
% analysis with a(i) representing the LPC model. The e(n) term represents 
% either the speech source's excitation, or the residual: the details of the 
% signal that are not captured by the LPC coefficients.  The G factor is a
% gain term.
%
% LPC analysis is performed on a monaural sound vector (data) which has been
% sampled at a sampling rate of "sr".  The following optional parameters modify
% the behaviour of this algorithm.
% L - The order of the analysis.  There are L+1 LPC coefficients in the output
%	array aCoeff for each frame of data.  L defaults to 13.
% fr - Frame time increment, in ms.  The LPC analysis is done starting every
% 	fr ms in time.  Defaults to 20ms (50 LPC vectors a second)
% fs - Frame size in ms.  The LPC analysis is done by windowing the speech
% 	data with a rectangular window that is fs ms long.  Defaults to 30ms
% preemp - This variable is the epsilon in a digital one-zero filter which 
%	serves to preemphasize the speech signal and compensate for the 6dB
%	per octave rolloff in the radiation function.  Defaults to .9378.
%
% The output variables from this function are
% aCoeff - The LPC analysis results, a(i).  One column of L numbers for each
%	frame of data
% resid - The LPC residual, e(n).  One column of sr*fs samples representing
%	the excitation or residual of the LPC filter.
% pitch - A frame-by-frame estimate of the pitch of the signal, calculated
%	by finding the peak in the residual's autocorrelation for each frame.
% G - The LPC gain for each frame.
% parcor - The parcor coefficients.  The parcor coefficients give the ratio
%	between adjacent sections in a tubular model of the speech 
%	articulators.  There are L parcor coefficients for each frame of 
%	speech.
% stream - The LPC analysis' residual or excitation signal as one long vector.
%	Overlapping frames of the resid output combined into a new one-
%	dimensional signal and post-filtered.
%
% The synlpc routine inverts this transform and returns the original speech
% signal.
%
% This code was graciously provided by:
% 	Delores Etter (University of Colorado, Boulder) and 
%	Professor Geoffrey Orsak (Southern Methodist University) 
% It was first published in
%	Orsak, G.C. et al. "Collaborative SP education using the Internet and
%	MATLAB" IEEE SIGNAL PROCESSING MAGAZINE  Nov. 1995. vol.12, no.6, pp.
%	23-32.
% Modified and debugging plots added by Kate Nguyen and Malcolm Slaney

% A more complete set of routines for LPC analysis can be found at
%	http://www.ee.ic.ac.uk/hp/staff/dmb/voicebox/voicebox.html

% (c) 1998 Interval Research Corporation  
   

if (nargin<3), L = 13; end
if (nargin<4), fr = 20; end
if (nargin<5), fs = 30; end
if (nargin<6), preemp = .9378; end

[row col] = size(data);
if col==1 data=data'; end

nframe = 0;						
msfr = round(sr/1000*fr);			% Convert ms to samples
msfs = round(sr/1000*fs);			% Convert ms to samples
duration = length(data);
speech = filter([1 -preemp], 1, data)';		% Preemphasize speech
msoverlap = msfs - msfr;
ramp = [0:1/(msoverlap-1):1]';			% Compute part of window


for frameIndex=1:msfr:duration-msfs+1		% frame rate=20ms
  frameData = speech(frameIndex:(frameIndex+msfs-1));	% frame size=30ms
  nframe = nframe+1;
  autoCor = xcorr(frameData);			% Compute the cross correlation
  autoCorVec = autoCor(msfs+[0:L]);

						% Levinson's method
  err(1) = autoCorVec(1);
  k(1) = 0;
  A = [];
  for index=1:L
    numerator = [1 A.']*autoCorVec(index+1:-1:2);
    denominator = -1*err(index);
    k(index) = numerator/denominator;		% PARCOR coeffs
    A = [A+k(index)*flipud(A); k(index)];     			
    err(index+1) = (1-k(index)^2)*err(index);
  end

  aCoeff(:,nframe) = [1; A];
  parcor(:,nframe) = k';

 						% Calculate the filter 
						% response
						% by evaluating the 
						% z-transform
  if 0
    gain=0;
    cft=0:(1/255):1;
    for index=1:L
      gain = gain + aCoeff(index,nframe)*exp(-i*2*pi*cft).^index;
    end
    gain = abs(1./gain);
    spec(:,nframe) = 20*log10(gain(1:128))';
    plot(20*log10(gain));
    title(nframe);
    drawnow;
  end

						% Calculate the filter response
						% from the filter's impulse
						% response (to check above).
  if 0
    impulseResponse = filter(1, aCoeff(:,nframe), [1 zeros(1,255)]);
    freqResp = 20*log10(abs(fft(impulseResponse)));
    plot(freqResp);
  end

  errSig = filter([1 A'],1,frameData);		% find excitation noise
  
  G(nframe) = sqrt(err(L+1));			% gain
  autoCorErr = xcorr(errSig);			% calculate pitch & voicing 
						% information
  [B,I] = sort(autoCorErr);
  num = length(I);
  if B(num-1) > .3*B(num)
    pitch(nframe) = abs(I(num) - I(num-1));
  else
    pitch(nframe) = 0;
  end

  resid(:,nframe) = errSig/G(nframe);
  if(frameIndex==1)				% add residual frames using a
    stream = resid(1:msfr,nframe); 		% trapezoidal window
  else
    stream = [stream; 
              overlap+resid(1:msoverlap,nframe).*ramp; 
              resid(msoverlap+1:msfr,nframe)];
  end
  if(frameIndex+msfr+msfs-1 > duration)
    stream = [stream; resid(msfr+1:msfs,nframe)];
  else
    overlap = resid(msfr+1:msfs,nframe).*flipud(ramp);	
  end	
end
stream = filter(1, [1 -preemp], stream)';
