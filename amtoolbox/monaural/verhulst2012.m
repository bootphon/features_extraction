function output = verhulst2012(insig,fs,fc,spl)
%VERHULST2012 Process a signal with the cochlear model by Verhulst et. al. 2012
%   Usage: output = verhulst2012(insig,fs,fc,spl)
%
%   Input parameters:
%        insig : the monaural/binaural input signal to be processed
%        fs    : sampling rate
%        fc    : list of frequencies specifying the probe positions on
%                the basilar membrane
%        spl   : the sound pressure level that corresponds to the rms of
%                one of the input signal
%
%   Output parameters:
%        output : structure consisting of either two of the first listed
%                 below or of all the listed elements, depending on
%                 whether the input is monaural or binaural
%
%                    velocityLeft  : velocity of the basilar membrane
%                                     movement at different positions in
%                                     the left ear canal signal
%                    displaceLeft  : displacement of the basilar membrane
%                                     movement at different positions in
%                                     the left ear canal signal
%                    velocityRight : velocity of the basilar membrane
%                                     movement at different positions in
%                                     the right ear canal signal
%                    displaceRight : displacement of the basilar membrane
%                                     movement at different positions in
%                                     the right ear canal signal
%
%   This function divides the binaural or monaural input signal into non-
%   overlapping time frames and computes the basilar membrane displacement
%   and the velocity of the movement at different positions employing the 
%   nonlinear time-domain model of cochlea by Verhulsts, Dau, Shera 2012.
%
%   It shold be noted that this MATLAB code needs to be in the same folder 
%   as the cochlear model as the model is called from this function and the
%   parameters of the function are modified during the processing of this
%   function. Moreover, the subfolders stim and out must exist in the 
%   same folder as well. Furthermore, one is advised to remove the
%   separation into two freqeuncy regions if a maximum of 20 probe
%   frequencies are required.
%
%   The processing is implemented as follows:
%
%     1) the input signal is resampled to the 400 kHz sampling rate
%        employed in the cochlea model
%
%     2) the list of frequencies in fc are converted in to probe
%        positions in a manner that the frequencies are divided evenly into
%        low and high frequency categories. 
%
%     3) the signal is processed by processing first the left ear canal
%        signal
%
%             - the two frequency regions are measured separately
%
%             - the processing is done in 90-ms long non-overlapping time
%               frames
%
%     4) the values obtained are resampled back to the original sampling
%        rate
%               
%   See also: verhulst2012
%
%   References:
%     S. Verhulst, T. Dau, and C. A. Shera. Nonlinear time-domain cochlear
%     model for transient stimulation and human otoacoustic emission. J.
%     Acoust. Soc. Am., 132(6):3842 - 3848, 2012.
%     
%
%   Url: http://amtoolbox.sourceforge.net/doc//monaural/verhulst2012.php

% Copyright (C) 2009-2013 Peter L. Søndergaard and others.
% This file is part of AMToolbox version 0.9.1
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

%   AUTHOR: Sarah Verhulst, Marko Takanen (MATLAB code to run the model)

basedir = [amtbasepath,filesep,'verhulst',filesep];

oldpath = pwd;
cd(basedir);

%% ------ Set the parameters ---------------------------------------------
% the cochlear model applies a sampling rate of 400 kHz, hence the input is
% resampled while maintaining the level of the input as original
fsNew = 400000; % sampling rate employed in the cochlear model
maxStimLength = 0.09;%length of the time frame in seconds
%as the T_MAX setting in the parameters.dat

% maximum of 20 probe positions are used in each run
nProbes = round(length(fc)/2); 
nRemProbes = length(fc)-nProbes;
dims = size(insig);


%% ------ Resample the input ---------------------------------------------
sigResampled = resample(insig,fsNew,fs);
scale1= max(abs(insig(:)));scale2=max(abs(sigResampled(:)));
%ensure that the resampling did not change the level of the signal
sigResampled = sigResampled.*(scale1/scale2);

%count the number of non-overlapping time frames
ncol= max(1,ceil(length(sigResampled)/(maxStimLength*fsNew)));

%the input signal can be either monaural or binaural signal
output.velocityLeft = zeros(size(sigResampled,1),length(fc));
output.displaceLeft = zeros(size(sigResampled,1),length(fc));
if (dims(2)>1)
    output.velocityRight = zeros(size(sigResampled,1),length(fc));
    output.displaceRight = zeros(size(sigResampled,1),length(fc));
end

%% ------ Specify the measurement positions ------------------------------
% this is a modified version of the one in probepointgenerator by Verhulst
Csections=1000;
% start with highest freq first and fill in in decending order
%the program then calculates the probing points that correspond to these
%frequencies, with the Greenwood map for a human cochlea!
BMlength=35e-3 - 1e-3;
dx=BMlength/Csections;
Ga=20682;
Galpha=61.765;
Gbeta=140.6;

%this gives the location in meters
loc=(log10((sort(fc,'descend')+Gbeta)/Ga))/-Galpha; %turned around
%this is then converted in a section
probes=round(loc/dx);


%check the amount of probe frequencies
if(length(fc)<=40)
        %two character arrays are generated for the low and high frequencies
        probListHigh = [];
        for ind = 1:nProbes
            if(ind<nProbes)
                probListHigh = [probListHigh num2str(probes(ind)) ', '];
            else
                probListHigh = [probListHigh num2str(probes(ind))];
            end
        end
        probListLow = [];
        for ind = (nProbes+1):length(fc)
            if(ind<length(fc))
                probListLow = [probListLow num2str(probes(ind)) ', '];
            else
                probListLow = [probListLow num2str(probes(ind))];
            end
        end
else
    disp('The maximum number of probe frequencies (40) was exceeded');
    return;
end


%% ------ Start the actual processing ------------------------------------
for ind=1:2*dims(2)
    %the output parameters are computed separately for the low and high
    %frequency areas, and the (left and right) channel(s) of the input are
    %also processed separately
    
    %at the beginning of each run, the original parameters of the cochlear
    %model are stored into a backup file
    system('cp parameters.dat parameters2.dat.old');
    
    startP =1;
    endP = min(maxStimLength*fsNew,size(sigResampled,1));
    for frameInd=1:ncol
        sLength = endP-startP+1;
        % the 90 ms long sample of the input is stored to a wav-file for
        % processing
        sample = sigResampled(startP:endP,:);
        % the first two values of the parameter ind refer to the left ear
        % signals and the last two to the right ear signals
        if ind<=2
            %note that the name of the file in the parameters.dat file is
            %modified to match this
            wavwrite(sample(:,1),fsNew,[basedir,'stim/sig1L.wav']);
        else
            wavwrite(sample(:,2),fsNew,[basedir,'stim/sig1L.wav']);
        end
        %the cochlear model parameters are modified during the first run
        if frameInd==1
            %set the spl level that the rms of signal equals to
            system(['sed ''s/''''AUDIOFILELEVEL = 80''''/''''AUDIOFILELEVEL = ' num2str(round(spl)) '''''/'' <parameters.dat> temp1.dat']);
            %set the file name to match the one used above
            system('sed ''s/.*AUDIOFILENAME.*/'''' AUDIOFILENAME   = "stim\/sig1L.wav",/'' <temp1.dat> temp.dat');
            %ensure that the length of the time frame is correct
            system('sed ''s/.*T_MAX.*/'''' T_MAX   =  90.00000000000000E-003,/'' <temp.dat> temp1.dat');
            %the cochlear model is set to store the membrane status for the
            %next time frame
            system('sed ''s/''''STOREMEMBRANESTATUS     = F''''/''''STOREMEMBRANESTATUS  = T''''/'' <temp1.dat> temp.dat');
            %how the parameters are modified depends on whether the low or
            %high frequencies are analyzed on left or right ear signals
            if mod(ind,2)==1
                system(['sed ''s/.*PROBES.*/'''' PROBES = ' probListLow '/'' <temp.dat> parameters.dat']);
            else
                system(['sed ''s/.*PROBES.*/'''' PROBES = ' probListHigh '/'' <temp.dat> parameters.dat']);
            end
            % the cochlear model is called here
            
            system('./verhulst2012');
            % and the model is set to continue from the membrane status at
            % the end of the current file when processing the next time
            % frame
            system('sed ''s/''''RETRIEVEMEMBRANESTATUS  = F''''/''''RETRIEVEMEMBRANESTATUS  = T''''/'' <parameters.dat> temp.dat');
            system('cp temp.dat parameters.dat');
        else
            system('./verhulst2012');
        end
        %the cochlear model outputs are loaded from a file and stored
        %to the corresponding locations in the output arrays
        load 'out/probing.dat';
        temp = probing;
        
        %make sure that the upsampling has not increased the size
        temp = temp(1:sLength,:);
        switch ind
            case 1
                output.displaceLeft(startP:endP,1:nRemProbes) = temp(:,nRemProbes:-1:1);
                output.velocityLeft(startP:endP,1:nRemProbes) = temp(:,(2*nRemProbes):-1:(nRemProbes+1));
            case 2
                output.displaceLeft(startP:endP,(nRemProbes+1):length(fc)) = temp(:,nProbes:-1:1);
                output.velocityLeft(startP:endP,(nRemProbes+1):length(fc)) = temp(:,(2*nProbes):-1:(nProbes+1));
            case 3
                output.displaceRight(startP:endP,1:nRemProbes) = temp(:,nProbes:-1:1);
                output.velocityRight(startP:endP,1:nRemProbes) = temp(:,(2*nRemProbes):-1:(nRemProbes+1));
            case 4
                output.displaceRight(startP:endP,(nRemProbes+1):length(fc)) = temp(:,nProbes:-1:1);
                output.velocityRight(startP:endP,(nRemProbes+1):length(fc)) = temp(:,(2*nProbes):-1:(nProbes+1));
        end
        startP = endP+1;
        endP = min(endP+maxStimLength*fsNew,size(sigResampled,1));
    end
    %the parameter values are reset before the next frequency area or
    %channel is processed
    system('cp parameters2.dat.old parameters.dat');
end

%% ------ Resample the output back to original rate -----------------------
output.velocityLeft = resample(output.velocityLeft,fs,fsNew);
output.displaceLeft = resample(output.displaceLeft,fs,fsNew);
if (dims(2)>1)
    output.displaceRight = resample(output.displaceRight,fs,fsNew);
    output.velocityRight = resample(output.velocityRight,fs,fsNew);
end

cd(oldpath);
