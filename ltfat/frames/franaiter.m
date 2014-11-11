function [outsig,relres,iter]=franaiter(F,insig,varargin)
%FRANAITER  Iterative analysis
%   Usage:  c=franaiter(F,f);
%           [c,relres,iter]=franaiter(F,f,...);
%
%   Input parameters:
%         F       : Frame.
%         f       : Signal.
%         Ls      : Length of signal.
%   Output parameters:
%         c       : Array of coefficients.    
%         relres  : Vector of residuals.
%         iter    : Number of iterations done.
%
%   c=FRANAITER(F,f) computes the frame coefficients c of the signal f*
%   using an iterative method such that perfect reconstruction can be
%   obtained using FRSYN. FRANAITER always works, even when FRANA
%   cannot generate perfect reconstruction coefficients.
%
%   [c,relres,iter]=FRANAITER(...) additionally returns the relative
%   residuals in a vector relres and the number of iteration steps iter.
%  
%   *Note:* If it is possible to explicitly calculate the canonical dual
%   frame then this is usually a much faster method than invoking
%   FRANAITER.
%
%   FRANAITER takes the following parameters at the end of the line of
%   input arguments:
%
%     'tol',t      Stop if relative residual error is less than the
%                  specified tolerance. Default is 1e-9 (1e-5 for single precision)
%
%     'maxit',n    Do at most n iterations.
%
%     'pg'        Solve the problem using the Conjugate Gradient
%                  algorithm. This is the default.
%
%     'pcg'        Solve the problem using the Preconditioned Conjugate Gradient
%                  algorithm.
%
%     'print'      Display the progress.
%
%     'quiet'      Don't print anything, this is the default.
%
%   Examples
%   --------
%
%   The following example shows how to rectruct a signal without ever
%   using the dual frame:
%
%      f=greasy;
%      F=frame('dgtreal','gauss',40,60);
%      [c,relres,iter]=franaiter(F,f,'tol',1e-14);
%      r=frsyn(F,c);
%      norm(f-r)/norm(f)
%      semilogy(relres);
%      title('Conversion rate of the CG algorithm');
%      xlabel('No. of iterations');
%      ylabel('Relative residual');
%
%   See also: frame, frana, frsyn, frsyniter
%
%   Url: http://ltfat.sourceforge.net/doc/frames/franaiter.php

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
  
% AUTHORS: Peter L. Søndergaard
    
if nargin<2
    error('%s: Too few input parameters.',upper(mfilename));
end;

tolchooser.double=1e-9;
tolchooser.single=1e-5;

definput.keyvals.Ls=[];
definput.keyvals.tol=tolchooser.(class(insig));
definput.keyvals.maxit=100;
definput.flags.alg={'cg','pcg'};
definput.keyvals.printstep=10;
definput.flags.print={'quiet','print'};

[flags,kv,Ls]=ltfatarghelper({'Ls'},definput,varargin);

%% ----- step 1 : Verify f and determine its length -------
% Change f to correct shape.
[insig,~,Ls,W,dim,permutedsize,order]=assert_sigreshape_pre(insig,[],[],upper(mfilename));

F=frameaccel(F,Ls);
L=F.L;

%% -- run the iteration 

A=@(x) F.frsyn(F.frana(x));

% An explicit postpad is needed for the pcg algorithm to not fail
insig=postpad(insig,L);

if flags.do_pcg
    d=framediag(F,L);
    M=spdiags(d,0,L,L);
    
    [fout,flag,~,iter,relres]=pcg(A,insig,kv.tol,kv.maxit,M);
else
    
    [fout,flag,~,iter,relres]=pcg(A,insig,kv.tol,kv.maxit);          
end;

outsig=F.frana(fout);

if nargout>1
    relres=relres/norm(fout(:));
end;


%% --- cleanup -----

permutedsize=[size(outsig,1),permutedsize(2:end)];

outsig=assert_sigreshape_post(outsig,dim,permutedsize,order);


