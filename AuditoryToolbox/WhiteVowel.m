function [output,aCoeff] = WhiteVowel(data,sr,L,pos)
% function [output,aCoeff] = WhiteVowel(data,sr,L,pos)
%
% Speech is often described as having spectral peaks or formants which 
% identify the phonetic signal. An interesting experiment, first proposed by
% XXX, filters a speech signal to remove all the formant information at one
% time during the speech. If there are no formant peaks, how can the speech
% be understood?  It turns out that processing, much like RASTA, means that
% relative changes in spectrum are the most important, thus the speech signal
% is understood because the formant transitions carry the information.  This
% gives speech an important transparency due 
%
% This function takes a speech signal (data) with a given sampling rate (sr).
% It then finds the L-order LPC filter that describes the speech at the given
% position (pos ms).  The entire speech signal is then filtered with the
% inverse of the LPC filter, effectively turning the speech spectrum at the 
% given time white (flat).

% Chris Pal, Interval, May 1997
% (c) 1998 Interval Research Corporation  

fr = 20; fs = 30; preemp = .9378;			% LPC defaults

[row col] = size(data);
if col==1 data=data'; end

nframe = 0;
msfr = round(sr/1000*fr);
msfs = round(sr/1000*fs);
duration = length(data);
msoverlap = msfs - msfr;
frameNumber = floor(pos/1000*sr/msfr);

frameStart = round(pos/1000*sr - msfs/2);	                
frameData = data(frameStart:(frameStart+msfs-1));   
aCoeff = proclpc(frameData, sr, L, fr, fs, preemp);
                                % Calculate the filter response
                                % by evaluating the z-transform
spec=lpc_spec(aCoeff);
subplot(2,3,1);
plot(spec);
title('LPC Spectral Slice');
ylabel('Original')

								% Now do the actual whitening filter
output = filter(aCoeff,1,data)';

frameData = output(frameStart:(frameStart+msfs-1));   
bCoeff = proclpc(frameData, sr, L, fr, fs,  preemp);
spec=lpc_spec(bCoeff);
subplot(2,3,4);
plot(spec);
ylabel('Whitened'); xlabel('FFT Bin');

% 256-DFT
origSpec = 20*log10(abs(specgram(data,512,sr,msfs,msoverlap)));
subplot(2,3,2),imagesc(origSpec); axis xy; colormap(1-gray);
title('Spectrogram');

synSpec = 20*log10(abs(specgram(output,512,sr,msfs,msoverlap)));
subplot(2,3,5),imagesc(synSpec); axis xy; colormap(1-gray);
xlabel('Frame #');

origloc = origSpec(:,frameNumber); origloc=origloc-max(origloc);origmin=min(origloc);
subplot(2,3,3),plot(origloc),title('Spectrogram'),
axis([1 length(origloc) origmin 0]);

filloc = synSpec(:,frameNumber); filloc=filloc-max(filloc);
subplot(2,3,6),plot(filloc);ylabel('db');
axis([1 length(origloc) origmin 0]);
xlabel('FFT Bin');

function spec=lpc_spec(aCoeff)
gain=0;
cft=0:(1/255):1;
for index=1:size(aCoeff,1)
 gain = gain + aCoeff(index)*exp(-i*2*pi*cft).^index;
end
gain = abs(1./gain);
spec = 20*log10(gain(1:128))';
