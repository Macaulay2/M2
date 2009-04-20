loadPackage "IntegralClosure"
kk=ZZ/32003
S=kk[x,y,z,t]
mm=ideal vars S
f = ideal"xy-(z-t2)(z-t3)(z-t4)"
J = ideal jacobian f
gens f % J
--JJ=integralClosure J

loadPackage "ReesAlgebra"
R = reesAlgebra J
gens R
netList (ideal R)_*

--minors(3, jacobian R);
--minors(3, jacobian presentation R);
--codim ideal presentation R
--sub(o20,R);

(A,F) = flattenRing R
codim A
--time B = integralClosure A;


jacobian A
gbTrace=3
minors(3,jacobian A);

JA  = randomMinors(20,3,jacobian A);
JA = trim ideal JA;
betti JA
factor lift(JA_0, ambient A)
factor lift(JA_1, ambient A)
factor lift(JA_2, ambient A)
factor lift(JA_3, ambient A)

use ring ideal A
P = trim sub(ideal A, y => 0)
betti P
P_*/factor//netList

I = trim ideal A
codim I
I_0
I_1
codim ideal(I_0,I_1,I_2)
