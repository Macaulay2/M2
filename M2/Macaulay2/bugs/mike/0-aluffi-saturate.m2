load "~/src/M2/Macaulay2/packages/Macaulay2Doc/test/saturate3.m2"

-- bug reported by Paolo Aluffi <aluffi@mail.math.fsu.edu>, april 2009

end
-- Below is playing witht his example

restart
kk = QQ
kk = ZZ/32003 -- original problem is for over QQ
A = kk[e1,e2,e3,e4,e5,e6];
I=ideal(e1*e2*e3*e4+e1*e2*e3*e5+e1*e2*e3*e6+e1*e2*e4*e6+e1*e2*e5*e6
    +e1*e3*e4*e5+e1*e3*e5*e6+e1*e4*e5*e6+e2*e3*e4*e5+e2*e3*e4*e6
    +e2*e4*e5*e6+e3*e4*e5*e6);
J=ideal jacobian I;
m=numgens J;
R=(ring J)[t_0 .. t_(m-1),Join=>false];
II = substitute(J,R);
JJ =ideal apply(0..(m-2), i -> apply((i+1)..(m-1), j -> (II_i*t_j-II_j*t_i)));

errorDepth=0
saturate(JJ,II_0,Strategy => Bayer);  -- gives error right away
saturate(JJ,II_0,Strategy => Iterate); -- just sort of gets hung up

(A,F) = flattenRing R
J = F JJ
I = F II

gbTrace=3
saturate(J,I_0, Strategy=>Bayer)

B = (coefficientRing A)[gens A, z]
J = sub(J,B)
I = sub(I,B)
I_0
L = J + ideal(z^3-I_0)
gbTrace=3
time gens gb(L, Algorithm=>LinearAlgebra);

-- From an email of Paolo Aluffi, April 20, 2009.
-- This seems to simply not finish...  Possibly it is related to towers.

loadPackage "ReesAlgebra"
gbTrace=3
time RJ = reesAlgebra J;
