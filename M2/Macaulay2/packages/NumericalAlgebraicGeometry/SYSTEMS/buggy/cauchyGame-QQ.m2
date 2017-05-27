{*
David Eisenbud <de@msri.org>
MENGYUAN ZHANG <myzhang@berkeley.edu>,
Fri, Apr 7, 2017 at 2:56 PM
*}

restart
errorDepth = 0
needsPackage "NumericalAlgebraicGeometry"
R = QQ[x,y,z];
--R = CC[x,y,z]; -- works
I = minors(2,matrix{gens R, gens R / (v->v^2)});
J = I + ideal(random(1,R)+1_R);  -- Since I is 1 dim I add random linear slice to cut down to 0 dim
NAGtrace 3
solveSystem J_*
