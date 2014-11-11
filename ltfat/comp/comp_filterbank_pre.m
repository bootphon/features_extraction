function g = comp_filterbank_pre(g,a,L,crossover)
%COMP_FILTERBANK_PRE Return sanitized filterbank
%
%   The purpose of this function is to evauate all parameters of the
%   filters, which can be evaluated knowing L.
%
%   This function ensures that g will be in the following format:
%
%      If g has field .h and it is a vector with length < crossover:
%         Impulse response is modulated.
%
%   Url: http://ltfat.sourceforge.net/doc/comp/comp_filterbank_pre.php

% Copyright (C) 2005-2013 Peter L. Søndergaard <soender@users.sourceforge.net>.
% This file is part of LTFAT version 1.4.3
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

M=numel(g);

% Divide filters to time domain and frequency domain groups
mFreq = 1:M;
mTime = mFreq(cellfun(@(gEl,aEl) isfield(gEl,'h') && numel(gEl.h)<=crossover,g(:),num2cell(a(:,1)))>0); 
mFreq(mTime) = [];

% Prepare time-domain filters
for mId=1:numel(mTime)
   m = mTime(mId);
   
   % Handle .fc parameter
   if isfield(g{m},'fc') && g{m}.fc~=0
      l = (g{m}.offset:g{m}.offset+numel(g{m}.h)-1).'/L;
      g{m}.h = g{m}.h.*exp(2*pi*1i*round(g{m}.fc*L/2)*l);
      g{m}.fc = 0;
   end

   if isfield(g{m},'realonly') && g{m}.realonly
      g{m}.h = real(g{m}.h);
      g{m}.realonly = 0;
   end
   
   % Do zero padding when the offset is big enough so the initial imp. resp.
   % support no longer intersects with zero
   if g{m}.offset > 0
      g{m}.h = [zeros(g{m}.offset,1);g{m}.h(:)];
      g{m}.offset = 0;
   end
   
   if g{m}.offset < -(numel(g{m}.h)-1)
      g{m}.h = [g{m}.h(:);zeros(-g{m}.offset-numel(g{m}.h)+1,1)];
      g{m}.offset = -(numel(g{m}.h)-1);
   end
   
end

% Prepare frequency-domain filters
%l=(0:L-1).'/L;
for mId=1:numel(mFreq)
    m = mFreq(mId);
    if isfield(g{m},'h')
       g{m}.H=comp_transferfunction(g{m},L);
       g{m}=rmfield(g{m},'h');
       g{m}=rmfield(g{m},'offset');
       if isfield(g{m},'fc'), g{m}=rmfield(g{m},'fc'); end;
       % The following parameters have to be set to zeros, because they
       % have already been incorporated in the freq. resp. calculation.
       g{m}.foff = 0;
    elseif isfield(g{m},'H') && ~isnumeric(g{m}.H)
       g{m}.H=g{m}.H(L);
       g{m}.foff=g{m}.foff(L);
    end
    
    if isfield(g{m},'H') && isfield(g{m},'delay') && g{m}.delay~=0
       % handle .delay parameter
       lrange = mod(g{m}.foff:g{m}.foff+numel(g{m}.H)-1,L).'/L;
       g{m}.H=g{m}.H.*exp(-2*pi*1i*round(g{m}.delay)*lrange); 
       g{m}.delay = 0;
    end

    % Treat full-length .H
    if numel(g{m}.H)==L
       if isfield(g{m},'foff') && g{m}.foff~=0 
          % handle .foff parameter for full-length freq. resp.
          g{m}.H = circshift(g{m}.H,g{m}.foff);
          % to avoid any other moving
          g{m}.foff = 0;
       end

       if isfield(g{m},'realonly') && g{m}.realonly
          % handle .realonly parameter for full-length freq. resp.
          g{m}.H=(g{m}.H+involute(g{m}.H))/2;
          g{m}.realonly = 0;
       end;
    end
end;





