S = ZZ/32003[a,b,c,d,x,y,z,u]
I = ideal(
   a*x-b*y,
   b*u^7+b*u^6-2*b*z*u^4+b*u^5-2*b*z*u^3-2*b*z*u^2+3*b*z^2+c*x,
   a*u^7+a*u^6-2*a*z*u^4+a*u^5-2*a*z*u^3-2*a*z*u^2+3*a*z^2+c*y,
   b*z*u^6+9142*b*z*u^5+13715*b*z^2*u^3-9143*b*z*u^4-9145*b*u^5-13716*b*z^2*u^2-13712*b*z^2*u-13713*b*z*u^2+4568*b*z^2+9145*c*x*u-9145*c*x+4572*d*x,
   a*z*u^6+9142*a*z*u^5+13715*a*z^2*u^3-9143*a*z*u^4-9145*a*u^5-13716*a*z^2*u^2-13712*a*z^2*u-13713*a*z*u^2+4568*a*z^2+9145*c*y*u-9145*c*y+4572*d*y,
   c*u^8+7111*c*z*u^6+3556*d*u^7+10667*c*z*u^5+3556*d*u^6+14224*c*z^2*u^3+14223*c*z*u^4-7112*d*z*u^4+3556*d*u^5+10668*c*z^2*u^2-7112*d*z*u^3+7112*c*z^2*u-7112*d*z*u^2+10668*d*z^2);
R = S/I
time R' = integralClosure R
time R' = integralClosure(R, Strategy=>{RadicalCodim1})

end

kk=ZZ/32003
S=kk[x,y,z,u]
I = ideal(y,x,-u^7-u^6+2*z*u^4-u^5+2*z*u^3+2*z*u^2-3*z^2,9*u^8-7*z*u^6-6*z*u^5+4*z^2*u^3-5*z*u^4+3*z^2*u^2+2*z^2*u)
J = first flattenRing ideal reesAlgebra I
S1 = kk[a..d,x,y,z,u]
J1 = sub(J,vars S1)
trim J1
R = S1/J1
time R'= integralClosure(R, Verbosity=>3);

L = trim ideal R'
-- now let's compute the jacobian of L, and see if it has codim >= 2...
codim L
singL = L + minors(codim L, jacobian L);



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
time B = integralClosure(A, Verbosity=>2, Strategy=>{SimplifyFractions});



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
