kk = ZZ/101
R = kk[a..c]
I = ideal random(R^1, R^{-2,-2})
time gens gb(I, Algorithm=>F4, GBDegrees=>{1,1,1})
collectGarbage()

I = ideal flatten entries gens I
time gens gb I
F1 = I_0
F2 = I_1
F1 = 1_kk/42_kk * F1
F2 = 1_kk/50_kk * F2

F1
F2 = F1-F2
F2 = 1_kk/(-17_kk) * F2

G = b*F1-a*F2 + 11*b*F2
(1_kk/48_kk) * G
-----------------------------------
R = ZZ/101[a..e]
I = ideal random(R^1, R^{-2,-2,-2})
time gens gb(I, Algorithm=>F4, GBDegrees=>{1,1,1,1,1})
time I = gens gb I
gbTrace=3
time gens gb(I, Algorithm=>F4, GBDegrees=>{1,1,1,1,1})
------------------------------
J = ideal"a2-6ab-39b2+37ac+6bc-37c2-7ad-35bd+44cd-49d2-22ae+45be-26ce-24de+48e2,
    ab-6b2-24ac+21bc-35c2-4ad-23bd-4cd+28d2+22ae+26be+9ce-29de-23e2,
    b2+16ac-37bc+44c2-50ad-41bd+16cd-47d2-3ae-36be-14ce-20de-8e2,
    ac2+10bc2+46c3-9a2d+42abd+10b2d-48acd+32bcd-26c2d-38ad2-43bd2-8cd2-a2e-43abe+48b2e+15ace-38bce+2c2e-40ade+9bde-7cde+24ae2+12be2-6ce2,
    bc2-48c3-34a2d+49abd-8b2d-14acd+47bcd-30c2d+29ad2+21bd2-3cd2-40a2e+30abe+32b2e-41ace+28bce+11c2e+17ade+31bde-37cde+30ae2-19be2+23ce2"
(gens J) % I

F = poly"ac2+10bc2+46c3-9a2d+42abd+10b2d-48acd+32bcd-26c2d-38ad2-43bd2-8cd2-a2e-43abe+48b2e+15ace-38bce+2c2e-40ade+9bde-7cde+24ae2+12be2-6ce2"

-- scratch work on above problem:
B = flatten entries I

B_0
B_1
B_2
B_3
B_4
B_5

F = (g) -> set apply(terms(g),t -> leadMonomial t)
F(b*B_1) + F(a*B_0) + F(b*B_2) + F(a*B_1)

   1 [0 0 1 0 0 0]
   0 [0 1 0 0 0 0]
   2 [0 0 1 0 0 0]
   1 [0 1 0 0 0 0]

   1 [1 0 0 1 0 0]
   0 [0 0 0 1 0 0]
   1 [1 0 0 0 1 0]
   0 [0 0 0 0 1 0]
   1 [1 0 0 0 0 1]
   0 [0 0 0 0 0 1]
   2 [2 0 0 1 0 0]
   2 [2 0 0 0 1 0]
   2 [2 0 0 0 0 1]

kk = ZZ/101
M = matrix"42, -50,-22,39, 50, -39,9 , 45,30, -38,-15,-29,19, 2,  -4;
-36,-16,-32,-6, -38,-42,-32,31,-50,15, 31, 24, -41,17, -28 ;
37, -22,-31,-19,-9,4,  45, 32,-2, 1,  -8, -4, 24, -45,15"
M = substitute(M,kk)
M = mutableMatrix M
rowMult(M,0,-12_kk)
rowAdd(M,1,36_kk,0)
rowMult(M,1,37_kk)
rowAdd(M,2,-37_kk,0)
rowAdd(M,2,2_kk,1)
rowMult(M,2,-14_kk^-1)
------------------------------
R = ZZ/101[a,b,c,d]
I = ideal(a^2, a*b, a*c, b^2,a*d^3, b^4)
gens gb(I, Algorithm=>F4, GBDegrees=>{1,1,1,1})

R = ZZ/101[a,b,c,d]
I = ideal(a^2-b*c, a*b)
gens gb(I, Algorithm=>F4, GBDegrees=>{1,1,1,1})

------------------------------
R = ZZ/101[a,b,c,d,e,f]
I = ideal(a*b*c,b*c*d,c*d*e)
gens gb(I, Algorithm=>F4)

------------------------------
R = ZZ/101[a,b,c,d,e,f]
I = ideal basis(3,R)
gens gb(I, Algorithm=>F4)
------------------------------
R = ZZ/101[a..h]
I = ideal random(R^1, R^{-3,-3,-3})
I = gens gb I;
gbTrace=3
gens gb(I, Algorithm=>F4, GBDegrees=>{1,1,1,1,1,1,1,1});
------------------------------
R = ZZ/101[a..h,MonomialSize=>8]
I = ideal random(R^1, R^{-3,-4,-4,-5});
J = ideal"a3,b4,c4,d5"
gbTrace=3
time I = gens gb(I, Hilbert => poincare J);
I = matrix entries I;
time gens gb(I, Algorithm=>F4, GBDegrees=>{1,1,1,1,1,1,1,1});
I1 = ideal  flatten entries gens I;
time gens gb(I1, Algorithm=>F4, GBDegrees=>{1,1,1,1,1,1,1,1});
time gens gb(I1, Algorithm=>LinearAlgebra)
------------------------------
R = ZZ/101[vars(0..17)]
m1 = genericMatrix(R,a,3,3)
m2 = genericMatrix(R,j,3,3)
J = ideal flatten(m1*m2-m2*m1)
J1 = ideal flatten(m1*m2-m2*m1)
time gens gb J
time gens gb(J1, Algorithm=>F4, GBDegrees=>toList(18:1))

time gens gb ideal(m1^4);
time gens gb(ideal(m1^4), Algorithm=>F4, GBDegrees=>toList(18:1));

