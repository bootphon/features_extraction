function data = data_ziegelwanger2013(varargin)
%DATA_ZIEGELWANGER2013  Data from Ziegelwanger and Majdak (2013)
%   Usage: data = data_ziegelwanger2013(flag)
%
%   DATA_ZIEGELWANGER2013(flag) returns results for different HRTF
%   databases from Ziegelwanger and Majdak (2013).
%
%   The flag may be one of:
%  
%     'ARI'         ARI database 
%                        data.results:     Results for all HRTF sets
%
%                        data.subjects:    IDs for HRTF sets
%  
%     'CIPIC'       CIPIC database:
%                        data.results      Results for all HRTF sets
%                        data.subjects     IDs for HRTF sets
%  
%     'LISTEN'      LISTEN database: 
%                        data.results      Results for all HRTF sets
%                        data.subjects     IDs for HRTF sets
%  
%     'SPHERE_ROT'  HRTF sets for a rigid sphere placed in the center of
%                   the measurement setup and varying rotation
%                        data.results      Results for all HRTF sets
%                        data.subjects     IDs for HRTF sets
%                        data.phi          Azimuth of ear position
%                        data.theta        Elevation of ear position
%                        data.radius       sphere radius
%  
%     'SPHERE_DIS'  HRTF sets for a rigid sphere with various positions in
%                   the measurement setup
%                        data.results      Results for all HRTF sets
%                        data.subjects     IDs for HRTF sets
%                        data.xM           x-coordinate of sphere center
%                        data.yM           y-coordinate of sphere center
%                        data.zM           z-coordinate of sphere center
%                        data.radius       sphere radius
%  
%     'NH89'        HRTF set of listener NH89 of the ARI database
%                        data.hM           Matrix of IRs
%                        data.meta         Meta data
%                        data.stimPar     
%  
%     'reload'      Reload previously calculated results    
%  
%     'recalc'      Recalculate the results  
% 
%   Examples:
%   ---------
% 
%   To get results fot the ARI database, use:
%
%     data=data_ziegelwanger2013('ARI');
%
%   See also: ziegelwanger2013, ziegelwanger2013onaxis,
%   ziegelwanger2013offaxis, exp_ziegelwanger2013
%
%   References:
%     H. Ziegelwanger and P. Majdak. Continuous-direction model of the
%     time-of-arrival for head-related transfer funcitons. J. Acoust. Soc.
%     Am., submitted, 2013.
%     
%
%   Url: http://amtoolbox.sourceforge.net/doc//humandata/data_ziegelwanger2013.php

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

% AUTHOR: Harald Ziegelwanger, Acoustics Research Institute, Vienna,
% Austria

%% ------ Check input options --------------------------------------------

% Define input flags
definput.flags.type = {'missingflag','ARI','CIPIC','LISTEN','SPHERE_DIS','SPHERE_ROT','NH89'};
definput.flags.results = {'reload','recalc'};

% Parse input options
[flags,keyvals]  = ltfatarghelper({},definput,varargin);

if flags.do_missingflag
    flagnames=[sprintf('%s, ',definput.flags.type{2:end-2}),...
        sprintf('%s or %s',definput.flags.type{end-1},definput.flags.type{end})];
    error('%s: You must specify one of the following flags: %s.',upper(mfilename),flagnames);
else
    hpath = which('hrtfinit');  % find local path of hrtf repository
    hpath = hpath(1:end-10);
  
    if exist([hpath 'ziegelwanger2013'],'dir') ~= 7
        fprintf([' Sorry! Before you can run this script, you have to download the HRTF Database from \n http://www.kfs.oeaw.ac.at/hrtf/database/amt/ziegelwanger2013.zip , \n unzip it, and move it into your HRTF repository \n ' hpath ' .\n' ' Then, press any key to quit pausing. \n'])
        pause
    end
    
    hpath = [hpath 'ziegelwanger2013' filesep];
end

%% ARI database
if flags.do_ARI
    
    if flags.do_recalc || ~exist([hpath 'ARI' filesep 'results.mat'],'file')
        data=load([hpath 'ARI' filesep 'subjects.mat']);
        for ii=1:length(data.subjects)
            load([hpath 'ARI' filesep data.subjects{ii} filesep 'hrtf_M_hrtf.mat'])
            idx=find(mod(meta.pos(:,2),10)==0);
            hM=hM(:,idx,:);
            meta.pos=meta.pos(idx,:);

            results(ii).meta=ziegelwanger2013(hM,meta,stimPar,4,1);
            results(ii).meta.performance(4)=results(ii).meta.performance;
            temp=ziegelwanger2013(hM,meta,stimPar,1,0,0);
            results(ii).meta.performance(1)=temp.performance;
            temp=ziegelwanger2013(hM,meta,stimPar,2,0,0);
            results(ii).meta.performance(2)=temp.performance;
            temp=ziegelwanger2013(hM,meta,stimPar,3,0,0);
            results(ii).meta.performance(3)=temp.performance;
            clear hM; clear meta; clear stimPar;
        end
        save([hpath 'ARI' filesep 'results.mat'],'results');
        data.results=results;
    else
        data=load([hpath 'ARI' filesep 'subjects.mat']);
        load([hpath 'ARI' filesep 'results.mat']);
        data.results=results;
    end
    
end

%% CIPIC database
if flags.do_CIPIC
    
    if flags.do_recalc || ~exist([hpath 'CIPIC' filesep 'results.mat'],'file')
        data=load([hpath 'CIPIC' filesep 'subjects.mat']);
        for ii=1:length(data.subjects)
            load([hpath 'CIPIC' filesep data.subjects{ii} filesep 'hrtf_M_hrtf.mat'])

            results(ii).meta=ziegelwanger2013(hM,meta,stimPar,4,1);
            results(ii).meta.performance(4)=results(ii).meta.performance;
            temp=ziegelwanger2013(hM,meta,stimPar,1,0,0);
            results(ii).meta.performance(1)=temp.performance;
            temp=ziegelwanger2013(hM,meta,stimPar,2,0,0);
            results(ii).meta.performance(2)=temp.performance;
            temp=ziegelwanger2013(hM,meta,stimPar,3,0,0);
            results(ii).meta.performance(3)=temp.performance;
            clear hM; clear meta; clear stimPar;
        end
        save([hpath 'CIPIC' filesep 'results.mat'],'results');
        data.results=results;
    else
        data=load([hpath 'CIPIC' filesep 'subjects.mat']);
        load([hpath 'CIPIC' filesep 'results.mat']);
        data.results=results;
    end
    
end

%% LISTEN database
if flags.do_LISTEN
    
    if flags.do_recalc || ~exist([hpath 'LISTEN' filesep 'results.mat'],'file')
        data=load([hpath 'LISTEN' filesep 'subjects.mat']);
        for ii=1:length(data.subjects)
            if ~strcmp(data.subjects{ii},'34')
                load([hpath 'LISTEN' filesep data.subjects{ii} filesep 'hrtf_M_hrtf.mat'])
                stimPar.SamplingRate=48000;
                
                results(ii).meta=ziegelwanger2013(hM,meta,stimPar,4,1);
                results(ii).meta.performance(4)=results(ii).meta.performance;
                temp=ziegelwanger2013(hM,meta,stimPar,1,0,0);
                results(ii).meta.performance(1)=temp.performance;
                temp=ziegelwanger2013(hM,meta,stimPar,2,0,0);
                results(ii).meta.performance(2)=temp.performance;
                temp=ziegelwanger2013(hM,meta,stimPar,3,0,0);
                results(ii).meta.performance(3)=temp.performance;
                clear hM; clear meta; clear stimPar;
            end
        end
        save([hpath 'LISTEN' filesep 'results.mat'],'results');
        data.results=results;
    else
        data=load([hpath 'LISTEN' filesep 'subjects.mat']);
        load([hpath 'LISTEN' filesep 'results.mat']);
        data.results=results;
    end
    
end

%% SPHERE (Displacement) database
if flags.do_SPHERE_DIS
    
    if flags.do_recalc || ~exist([hpath 'Sphere' filesep 'Displacement' filesep 'results.mat'],'file')
        data=load([hpath 'Sphere' filesep 'Displacement' filesep 'subjects.mat']);
        results.p_onaxis=zeros(4,2,length(data.subjects));
        results.p_offaxis=zeros(7,2,length(data.subjects));
        for ii=1:length(data.subjects)
            load([hpath 'Sphere' filesep 'Displacement' filesep data.subjects{ii} filesep 'hrtf_M_hrtf.mat']);
            meta=ziegelwanger2013(hM,meta,stimPar,4,1);
            results.p_onaxis(:,:,ii)=meta.p_onaxis;
            results.p_offaxis(:,:,ii)=meta.p_offaxis;
        end
        save([hpath 'Sphere' filesep 'Displacement' filesep 'results.mat'],'results');
        data.results=results;
    else
        data=load([hpath 'Sphere' filesep 'Displacement' filesep 'subjects.mat']);
        load([hpath 'Sphere' filesep 'Displacement' filesep 'results.mat']);
        data.results=results;
    end
    
end

%% SPHERE (Rotation) database
if flags.do_SPHERE_ROT
    
    if flags.do_recalc || ~exist([hpath 'Sphere' filesep 'Rotation' filesep 'results.mat'],'file')
        data=load([hpath 'Sphere' filesep 'Rotation' filesep 'subjects.mat']);
        results.p=zeros(4,2,length(data.phi));
        for ii=1:length(data.subjects)
            load([hpath 'Sphere' filesep 'Rotation' filesep data.subjects{ii} filesep 'hrtf_M_hrtf.mat']);
            meta=ziegelwanger2013(hM,meta,stimPar,4,1);
            results.p_onaxis(:,:,ii)=meta.p_onaxis;
        end
        save([hpath 'Sphere' filesep 'Rotation' filesep 'results.mat'],'results');
        data.results=results;
    else
        data=load([hpath 'Sphere' filesep 'Rotation' filesep 'subjects.mat']);
        load([hpath 'Sphere' filesep 'Rotation' filesep 'results.mat']);
        data.results=results;
    end
    
end

%% ARI database (NH89)
if flags.do_NH89
    
    data=load([hpath 'ARI' filesep 'NH89' filesep 'hrtf_M_hrtf.mat']);
    
end
