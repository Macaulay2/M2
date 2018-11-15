-*
David Eisenbud <de@msri.org>
MENGYUAN ZHANG <myzhang@berkeley.edu>,
Fri, Apr 7, 2017 at 2:56 PM
*-

restart
needsPackage "NumericalAlgebraicGeometry"
R = QQ[x,y,z,w];
M = matrix{{x,y,z,w},{(y+11*w)*(x+209*y+201*z-13*w),(161*x+y-838*z+33*w)*(x+12*w),-161*(y+51*w)*(x-161*w),0}};
I = minors(2,M) + ideal(random(1,R)+1_R);
setDefault(Software=>BERTINI) -- override the default Software=>M2engine
numericalIrreducibleDecomposition minors(2,M) 
numericalIrreducibleDecomposition I 
#components oo

decompose I
sum(oo/degree)
