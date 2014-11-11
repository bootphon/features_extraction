% LTFAT - Block processing
%
%  Zdenek Prusa, 2013.
%
%  Basic methods
%    BLOCK          - Setup a new block-stream
%    BLOCKDEVICES   - List available audio I/O devices
%    BLOCKREAD      - Read samples from file/device
%    BLOCKPLAY      - Play block (sound output)
%    BLOCKPANEL     - Block-stream control GUI
%    BLOCKPANELGET   - Obtain parameter(s) from GUI
%    BLOCKDONE      - Closes block-stream and frees resources
%
%  Block-adapted transforms
%    BLOCKFRAMEACCEL     - Prepare a frame for a block-stream processing
%    BLOCKFRAMEPAIRACCEL - Prepare a pair of frames for a block-stream processing
%    BLOCKANA            - Block analysis
%    BLOCKSYN            - Block synthesis
%
%  Running visualisation
%    BLOCKFIGURE   - Initialize figure for redrawing
%    BLOCKPLOT     - Append coefficients to the running plot
%
%  Helper functions
%    BLOCK_FWT      - FWT processing
%    BLOCK_IFWT     - IFWT processing
%
%  For help, bug reports, suggestions etc. please send email to
%  ltfat-help@lists.sourceforge.net
%
%   Url: http://ltfat.sourceforge.net/doc/blockproc/Contents.php

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

