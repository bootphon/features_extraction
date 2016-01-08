function amtstart()
%AMTSTART   Start the Auditory Modelling Toolbox
%   Usage:  amtstart;
%
%   AMTSTART starts the Auditory Modelling toolbox. This command must be
%   run before using any of the function in the toolbox.
%
%   The auditory modelling toolbox depends on the Linear Time Frequency
%   Analysis Toolbox (LTFAT) to properly function. Therefore, you must issue
%   the ltfatstart command before you start AMT.
%
%   To configure default options for functions, you can use the
%   ltfatsetdefaults function in your startup script. A typical startup
%   file could look like:
%
%     addpath('/path/to/my/work/ltfat');
%     addpath('/path/to/my/work/amtoolbox');
%     ltfatstart;
%     amtstart;
%     ltfatsetdefaults('audspecgram','classic');
%
%   The last command wil configure AUDSPECGRAM to display a classic
%   auditory spectrogram by default.
%
%   See also:  amthelp
%
%   References:
%     P. L. Søndergaard, B. Torrésani, and P. Balazs. The Linear Time
%     Frequency Analysis Toolbox. International Journal of Wavelets,
%     Multiresolution Analysis and Information Processing, 10(4), 2012.
%
%
%   Url: http://amtoolbox.sourceforge.net/doc//amtstart.php

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

%   AUTHOR : Peter L. Søndergaard.

% Verify that LTFAT has been installed
if ~exist('ltfatarghelper','file')
    disp('');
    disp('--- AMTOOLBOX - The Auditory Modelling toolbox. ---');
    disp('')
    error(['The toolbox require the LTFAT toolbox to properly function. ' ...
         'Please download and install it from http://ltfat.sourceforge.net,' ...
         'and then call the LTFATSTART command BEFORE you call ' ...
          'AMTSTART.'])
end;

% Check for the correct version
% Required version is given by:
major_rq  = 1;
minor_rq  = 0;
bugfix_rq = 9;


s=ltfathelp('version');

if isempty(s)
  error(['ltfathelp("version") returns an empty array. Please check your '...
         installation.']);
end;

% Split into major, minor and bugfix version.
stops=find(s=='.');
major_no  = str2num(s(1:stops(1)));
if numel(stops)==1
  minor_no  = str2num(s(stops(1)+1:end));
  bugfix_no = 0;
else
  minor_no  = str2num(s(stops(1)+1:stops(2)));
  bugfix_no = str2num(s(stops(2)+1:end));
end;

% Do the check, multiply by some big number to make the check easy
if major_rq*1000000+minor_rq*1000+bugfix_rq>major_no*1000000+minor_no*1000+ ...
            bugfix_no
  error(['Your version of LTFAT is too old for this version of AMToolbox ' ...
         'to function proberly. Your need at least version %i.%i.%i of LTFAT.'],major_rq,minor_rq,bugfix_rq);
end;

% --- general settings ---
% Print the banner at startup?
printbanner=1;

% ----------------------------------------------------
% -------   do not edit below this line   ------------
% ----------------------------------------------------

% Get the basepath as the directory this function resides in.
% The 'which' solution below is more portable than 'mfilename'
% becase old versions of Matlab does not have "mfilename('fullpath')"
basepath=which('amtstart');
% Kill the function name from the path.
basepath=basepath(1:end-11);

% add the base path
if exist('addpath')>0
  addpath(basepath);
else
  path(path,basepath);
end

bp=[basepath,filesep];

% Load the version number
[FID, MSG] = fopen ([bp,'amtoolbox_version'],'r');
if FID == -1
    error(MSG);
else
    amt_version = fgetl (FID);
    fclose(FID);
end

% -----------  install the modules -----------------

modules={};
nplug=0;

% List all files in base directory
d=dir(basepath);

for ii=1:length(d)
  if d(ii).isdir
    if ~(d(ii).name(1)=='.')

      name=d(ii).name;

      % The file is a directory and it does not start with '.' This could
      % be a module
      if exist([bp,name,filesep,name,'init.m'],'file')
	% Set 'status' to zero if the module forgets to define it.
	status=0;
	module_version=amt_version;
        addpath([bp,name]);

	eval([name,'init']);
        if status>0
          if status==1
            nplug=nplug+1;
            modules{nplug}.name=name;
            modules{nplug}.version=module_version;
          end;
	else
	  rmpath([bp,name]);
	end;
      end;

    end;
  end;
end;

% Check if Octave was called using 'silent'
%if isoctave
%  args=argv;
%  for ii=1:numel(args)
%    s=lower(args{ii});
%    if strcmp(s,'--silent') || strcmp(s,'-q')
%      printbanner=0;
%    end;
%  end;
%end;

## if printbanner
##   disp(['AMT version ',amt_version,'. Copyright 2012 Peter L. Søndergaard. For help, please type "amthelp".'])
## end;



%% ---------- load information into ltfathelp ------------

% As comp is now in the path, we can call ltfatarghelper
ltfatsetdefaults('amthelp','versiondata',amt_version,...
                 'modulesdata',modules);
