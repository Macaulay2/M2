numgens ZZ
A = ZZ[a,b,c];
numgens A
KA = frac A
numgens KA
B = A[x,y];
numgens B
C = KA[x,y];
numgens C
g = generators(B, CoefficientRing=>ZZ)
#g
K = GF(9,Variable=>a)
numgens K
R = ambient K
numgens R
