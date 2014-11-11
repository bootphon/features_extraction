function [outsig,fs] = read_hrir(elev_r,azim_r,database);
%READ_HRIR  Read HRIR from selected databases
%   Usage:  hrir = read_hrir(elev,azim,database);
%  
%   Input parameters:
%     elev : (-90 -> +90) elevation of source with respect to head in degrees
%            Both databases have a range of elevations, but more limited in 
%            the Oldenburg case.
%     azim : (-360 -> +360) azimuth of source with respect to head in degrees
%
%     database : Type of database (see below)
%
%   Output parameters:
%     hrir : the requested HRIR 
%
%   READ_HRIR(elev,azim,database) retrieves the HRIR closest to the
%   specified azimuth and elevation from the MIT Kemar (Gardner and Martin,
%   1995) or Oldenburg Siemens Acuris Kayser et al. (2009) databases.
%
%   The value of the database parameter determines which dataset to
%   access:
%
%     'kemar'     MIT KEMAR database. 
%
%     'siemens0'  Oldenburg HATS database. 
%
%     'siemens1'  Front microphone from the Siemens Acuris microphones.
%
%     'siemens2'  As above, but the middle microphone.
%
%     'siemens3'  As above, but the read microphone.
%             
%     'cardioid'  For a simple cardioid directional microphone
%                 constructred from the Siemens Acruris mikes.
%
%   References:
%     W. Gardner and K. Martin. Hrtf measurements of a kemar. J. Acoust. Soc.
%     Am., 97:3907-3908, 1995.
%     
%     H. Kayser, S. Ewert, J. Anemüller, T. Rohdenburg, V. Hohmann, and
%     B. Kollmeier. Database of multichannel in-ear and behind-the-ear
%     head-related and binaural room impulse responses. EURASIP Journal on
%     Advances in Signal Processing, 2009:6, 2009.
%     
%
%   Url: http://amtoolbox.sourceforge.net/doc//hrtf/read_hrir.php

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

  % Base path of where to store the data
  s=[amtbasepath,'hrtf/hrir/'];

  [elev_r,azim_r]=scalardistribute(elev_r,azim_r);  
  
  nexps=numel(elev_r);
  
  switch (database)
   case 'hats'        % HATS
    mik_nums = [1 2];
   case 'siemens1'        % front mike
    mik_nums = [3 4];
   case 'siemens2'        % middle mike
    mik_nums = [5 6];
   case 'siemens3'        % rear mike
    mik_nums = [7 8];
   case 'kemar'
    outsig=zeros(512,nexps,2);
    for ii=1:nexps
      azim=azim_r(ii);
      elev=elev_r(ii);
        
      % fix out of range cases
      if azim < 0
        azim = 360 + azim;
      elseif azim >= 360 
        azim = azim-360;
      end
      
      % prefix zeros for KEMAR databases
      if azim < 10           
        azim_str = sprintf('00%d',azim);
      elseif azim < 100
        azim_str = sprintf('0%d',azim);  
      else
        azim_str = sprintf('%d',azim);
      end  

      
      ipfile = fopen(sprintf('%skemar/elev%d/L%de%sa.dat',s,elev,elev,azim_str));
      outsig(:,ii,1) = fread(ipfile, 512, 'int16',0,'b');
      fclose(ipfile);

      ipfile = fopen(sprintf('%skemar/elev%d/R%de%sa.dat',s,elev,elev,azim_str));
      outsig(:,ii,2) = fread(ipfile, 512, 'int16',0,'b');
      fclose(ipfile);
    end;
  end

if strncmp(database,'siemens',7) || strcmp(database,'cardioid') || strcmp(database,'hats')
  if azim > 180            % fix out of range cases
    azim = azim - 360;
  end    
  filename = sprintf('%ssiemens/anechoic_distcm_300_el_%d_az_%d',s,elev,azim);
  set = wavread(filename);
  if strcmp(database,'cardioid')
    op = set(2:length(set),3:4);
    op = op - set(1:length(set)-1,5:6);
  else
    % select channels of wav file to get different mike 
    op = set(:,mik_nums(1):mik_nums(2));
  end
end

if strncmp(database,'siemens',7)
    fs = 48000;
else
    fs = 44100;
end

% Remove unnecessary dimension if only one dataset was requested
outsig=squeeze(outsig);
