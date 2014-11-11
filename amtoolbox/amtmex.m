function amtmex(varargin)
%AMTMEX   Compile Mex/Oct interfaces
%   Usage:  amtmex;
%           amtmex(...);
%
%   AMTMEX compiles the C backend in order to speed up the execution of
%   the toolbox. The C backend is linked to Matlab and Octave through mex
%   and Octave C++ interfaces.
%
%   The action of AMTMEX is determined by one of the following flags:
%
%     'compile'  Compile stuff. This is the default.
%
%     'clean'    Removes the compiled functions.
%
%   Url: http://amtoolbox.sourceforge.net/doc//amtmex.php

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

%   AUTHOR : Peter Søndergaard.
%   TESTING: NA
%   REFERENCE: NA

bp=mfilename('fullpath');
bp=bp(1:end-6);

defnopos.flags.command={'compile','clean'};
[flags,kv]=ltfatarghelper({},defnopos,varargin);

% Remember the current directory.
curdir=pwd;

if isoctave
  extname='oct';
  ext='oct';
else
  extname='mex';
  ext=mexext;
end;

% -------------- Handle cleaning --------------------------------
  
if flags.do_clean
    
  if ~isoctave
    % Delete files permanently (bypass trashcan on Windows/Mac
    % but remember the old state
    oldstate = recycle('off');
  end;
  
  fprintf('========= Cleaning %s interfaces ==========\n', extname);
  if isoctave
    deletefiles([bp,'oct'],'*.oct');
    deletefiles([bp,'oct'],'*.o');
  else
    deletefiles([bp,'mex'],['*.',mexext]);
  end;

  if ~isoctave
    recycle(oldstate);
  end;  
  
end;

% -------------- Handle compiling  --------------------------------

if flags.do_compile

  fprintf('========= Compiling %s interfaces ==========\n', extname);
  if compile_amt(bp)>1;                
    fprintf('ERROR: The %s interfaces was not built.\n', extname);
  else
    disp('Done.');
  end;

end;

% Jump back to the original directory.
cd(curdir);


function deletefiles(base,files)

L=dir([base,filesep,files]);
for ii=1:numel(L)
    s=[base,filesep,L(ii).name];
    delete(s);
end;


function status=compile_amt(bp)

% If we exit early, it is because of an error, so set status=1
status=1;

if isoctave
    cd([bp,'oct']);
    
    ext='oct';
    
    % Get the list of files.
    L=dir('*.cc');
    
    endchar=2;
else
    
    cd([bp,'mex']);
    
    ext=mexext;
    
    % Get the list of files.
    L=dir('comp_*.c');
    
    endchar=1;
end;

for ii=1:numel(L)
    filename = L(ii).name;
    objname  = [filename(1:end-endchar),ext];
    objdirinfo = dir(objname);
    
    % Make-like behaviour: build only the files where the src file is
    % newer than the object file, or the object file is missing.
    
    if isempty(objdirinfo) || (objdirinfo.datenum<L(ii).datenum)
        
        fprintf('Compiling %s\n',filename);
        
        if isoctave
          mkoctfile('-I.','-I../src','-L../src',filename);
        else
          mex('-I.','-I../src','-L../src',filename);
        end;                
        
    end;        
    
end;

status=0;

