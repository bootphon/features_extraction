function [gu,au,info]=nonu2ufilterbank(g,a)
%NONU2UFILTERBANK   Non-uniform to Uniform filterbank transform
%   Usage:  [gu,au]=nonu2ufilterbank(g,a)
%
%   [gu,au]=NONU2UFILTERBANK(g,a) calculates uniform filterbank gu, 
%   au=lcm(a) which is identical to the (possibly non-uniform) filterbank
%   g,*a in terms of the equal output coefficients. Each filter g{k} 
%   is replaced by p=au/a(k) delayed versions of itself
%   z^{-ma(k)}G_k(z) for m=0,...,p-1.
%
%   This allows using the factorisation algorithm when determining
%   filterbank frame bounds in FILTERBANKBOUNDS and
%   FILTERBANKREALBOUNDS and in the computation of the dual filterbank 
%   in FILTERBANKDUAL and FILTERBAKREALDUAL which do not work 
%   with non-uniform filterbanks.
%
%   See also: ufilterbank, filterbank, filterbankbounds, filterbankdual
%
%
%   Url: http://ltfat.sourceforge.net/doc/filterbank/nonu2ufilterbank.php

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

%   References: TBD

if nargin<2
  error('%s: Too few input parameters.',upper(mfilename));
end;

if ~isnumeric(a)
  error('%s: a must be numeric.',upper(mfilename));
end;

if ~iscell(g) || ...
   ~all(cellfun(@(gEl) isstruct(gEl) && (isfield(gEl,'h')||isfield(gEl,'H')),g))
  error('%s: a must be a cell array of structs containing filter definition.',upper(mfilename));
end;

a = comp_filterbank_a(a,numel(g));

if size(a,2)==2 && ~all(a(:,2)==1) && rem(a(:,1),1)~=0
   error('%s: Filterbanks with fractional subsampling are not supported.',upper(mfilename)); 
end

au=filterbanklength(1,a);

pk = au./a(:,1);

gu=cell(sum(pk),1);

auIdx = 1;
for m=1:numel(g)
   for ii=0:pk(m)-1
      gu{auIdx} = g{m};
      if(isfield(gu{auIdx},'H'))
         if(~isfield(gu{auIdx},'delay'))
            gu{auIdx}.delay = 0;
         end
         gu{auIdx}.delay = gu{auIdx}.delay-a(m)*ii;
      end
      
      if(isfield(gu{auIdx},'h'))
         if(~isfield(gu{auIdx},'offset'))
            gu{auIdx}.offset = 0;
         end
         gu{auIdx}.offset = gu{auIdx}.offset-a(m)*ii;
      end
      auIdx = auIdx+1;
   end
end

if nargout>2
   % Array of lengths of identical filterbanks of each channel
   info.p = pk;
end

