-- This code is old, but the question we need to check:
-- Is this yet implemented for det over RR or CC?
-- MES: I think it is, but need to check (8-12-09)

M = matrix table(120,120,x->random 10.);
time det M -- -1.41915*10^155
N = matrix table(125,125,x->random 10.);
time det N -- 0.0

M1 = mutableMatrix M;
time LUdecomposition M1;
methods LU

M = matrix table(125,125,x->random 10.);
M1 = mutableMatrix M;
(P,L,U) = LU M1;
det M
d = 1.0; for i from 0 to numColumns U - 1 do d = d * U_(i,i); d
for i from 0 to numColumns U - 1 list U_(i,i)


determinantRR = (M) -> (
     -- only up to sign!
     M1 := mutableMatrix M;
     (P,L,U) = LUdecomposition M1;
     d := 1.0; for i from 0 to numColumns U - 1 do d = d * U_(i,i);
     d)

M = matrix table(125,125,x->random 10.);
determinantRR M
det M

N = 216
time M = matrix table(N,N,x->random 10.);
time determinantRR M
det M

N = 510
time M = matrix table(N,N,x->random 1.);
time determinantRR M

N = 3
time M = matrix table(N,N,x->random 1.0 + ii * random 1.0);
time M = matrix table(N,N,x->random 10 + ii * random 10);
M
time determinantRR M
det M
det(M, Strategy=>Bareiss)
det(M, Strategy=>Cofactor)
determinantRR M
toString M
entries M

N = 2
time M = matrix table(N,N,x->random 10 + ii * random 10);
M

M11 = matrix{{7+8*ii}}
M22 = matrix{{9+3*ii}}
M11*M22
M12 = matrix{{9+2*ii}}
M21 = matrix{{5+5*ii}}
M12*M21
Mm = mutableMatrix M
LU oo


M = random(CC^10,CC^10)
M1 = promote(M,CC_200);
time det M1
determinantRR M1
