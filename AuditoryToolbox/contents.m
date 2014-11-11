% Auditory Toolbox
% by Malcolm Slaney
% (c) 1998 Interval Research Corporation  
% Technical Report #1998-010
%
% Lyon's Cochlear Model.
%   agc                - Adaptation process for Lyon's Passive 
%                        longwave cochlear model
%   DesignLyonCascade  - Designs the filters needed to implement
%                        Lyon's passive cochlear model
%   EpsilonFromTauFs   - Calculate the first order decay 
%                        coefficient (tau)
%   FreqResp           - Evaluate frequency response of the filter
%   LyonPassiveEar     - Calculate auditory nerve responses using
%                        Lyon's passive cochlear model
%   SecondOrderSection - Design a second order filter section
%   SetGain            - Set the gain of a second order system
%   soscascade         - Implement a cascade of second order filters
%   sosfilters         - Implement a bank of second order filters
%
% Patterson/Meddis Models
%   ERBSpace           - Calculate frequencies for ERB filters
%   MakeERBFilters     - Design for ERB cochlear model
%   ERBFilterBank      - Implement a bank of ERB Gammatone filters
%   MeddisHairCell     - Implement Meddis' Inner Hair Cell Model
%
% Seneff Auditory Model
%   SeneffEar          - Implement Stages I/II of Seneff's Auditory Model
%   SeneffEarSetup     - Design filters for Seneff's model
%
% Correlogram Processing
%   CorrelogramArray   - Compute an array of correlogram frames
%   CorrelogramFrame   - Compute a single correlogram frame
%   CorrelogramMovie   - Compute a Matlab movie of a correlogram
%   CorrelogramPitch   - Compute the pitch of a signal with a correlogram
%
% Signal Processing.
%   mfcc               - Mel-frequency cepstral coefficient transform of
%                        an audio signal
%   spectrogram        - Compute the spectrogram of a signal
%   rasta              - Implement RelAtive SpecTrAl filtering
%   proclpc            - Perform Linear Preditive Coding (LPC) analysis
%   synlpc             - Synthesize speech from LPC coefficients
%
% Demonstrations
%   MakeVowel          - Synthesize a vowel sound
%   FMPoints           - Create a changing pitch signal for vowel synthesis
%   WhiteVowel         - Filter a speech signal so formants disappear
