function [] = features(wavfiles, out_folder, feat, config, force);

pkg load signal;

addpath('ltfat');
addpath('amtoolbox');
ltfatstart;
amtstart;

addpath('jsonlab');
conf = loadjson(config);
rmpath('jsonlab');

for idx = 1:numel(wavfiles)
  wavfile = wavfiles{idx};
  [folder, basename] = fileparts(wavfile);
  fprintf('processing file %s (%d/%d)\n',basename, idx, numel(wavfiles));

  [sig, fs] = wavread(wavfile);

  if (fs ~= getfield(conf, 'fs'))
    if (force == 1)
      sig = resample(sig, getfield(conf, 'fs'), fs);
      fs = getfield(conf, 'fs');
    else
      fprintf(['Samplerate mismatch, expected %d, got %d, in %s.\nUse ' ...
               'option -f to force resampling of the audio file. ' ...
               'Note that you should use force with parsimony, it ' ...
               'is better to adjust the sampling rate to your wav ' ...
               'files'], getfield(conf, 'fs'), fs, wavfile);
      break;
    end
  end

  % set loudness such that loudness of vowel is at 50dB
  ref_lvl = 50;
  sig = gaindb(sig/rms(sig),ref_lvl-dbspl(1));
  
  out_file = strcat(out_folder, basename);

  if strcmp(feat, 'rasta')
      %addpath('rastamat');            
      %fprintf('\trasta-plp\n');           
      sig2rasta(sig, fs, out_file, conf);
      %rmpath('rastamat');

  elseif strcmp(feat, 'lyon')
      addpath('AuditoryToolbox');
      %fprintf('\tlyonpassiveear\n');
      sig2lyon(sig, fs, out_file, conf);
      rmpath('AuditoryToolbox');
      % fprintf('\tmelfcc\n');
      % sig2mfcc(sig, fs, interval, basename, out_folder);
  elseif strcmp(feat, 'drnl')
      %fprintf('\tdrnl\n');
      sig2drnl(sig,fs,out_file, conf);
  end
end
pkg unload signal;
