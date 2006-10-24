-- Fix primary decomposition routines
restart
debug PrimaryDecomposition
saturation(Ideal,RingElement) := (I,F) -> (
     facs := factors F;
     ret := minSatPPD(I,facs);
     (ret#0, ret#2))

R = ZZ/3[x,y,u,s,t]
I = ideal(
    x^27,
    y^27,
    u^27,
    u^5-x*y*(x-y)*(s*x-t*y))

gbTrace=1
(Q,J) = GTZ0 I
(Q1,J1) = GTZ0 J;
Q1 = trim Q1;
betti Q1
transpose gens Q1
L1 = substitute(Q1,{x=>0,y=>0,u=>0})
L1 = trim L1
factor L1_0
intersect(Q,Q1,J1)
codim J1
time C =  GTZ1 I;
ms = independentSets I

H = flatt(I,s*t)
F = product unique flatten apply(values H, f -> factors f_0)
oo/print;

time Q = saturate(I,F);
J = ideal F + I
codim J
gbTrace=1
C = primaryDecomposition J
