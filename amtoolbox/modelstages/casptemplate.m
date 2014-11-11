function [template,ir_reference]=casptemplate(target,reference,modelname,modelpars)
%CASPTEMPLATE  Generate a template for the optimal detector
%
%  CASPTEMPLATE(target,reference,modelname,modelpars) generates the template
%  needed for the optimal detector. CASPTEMPLATE will run the model
%  specifief by modelname on the signals stored in target and reference
%  and generate the template from this.
%
%  If target or reference is a matrix, each column will be considered a
%  signal, and averaging will be done. This is usefull for stochastic
%  signals.
%
%   Url: http://amtoolbox.sourceforge.net/doc//modelstages/casptemplate.php

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

if nargin<4
  modelpars={};
end;

ntargets    = size(target,2);
nreferences = size(reference,2);

%% ----- Compute average internal representation of the targets
ir_target=feval(modelname,target(:,1),modelpars{:});

for ii=2:ntargets
  ir_target = ir_target + feval(modelname,target(:,ii),modelpars{:});
end;

ir_target=ir_target/ntargets;

%% ----- Compute average internal representation of the references
ir_reference=feval(modelname,reference(:,1),modelpars{:});

for ii=2:nreferences
  ir_reference = ir_reference + feval(modelname,reference(:,ii),modelpars{:});
end;

ir_reference=ir_reference/nreferences;

% Compute the template as the difference between the average
% representation of the targets and references.
template = ir_target - ir_reference;

%% ----- Normalize to compensate for the increase in level ----

% Normalize across all dimenstions of the internal representation.
template=template/rms(tempate(:));
%OLDFORMAT

