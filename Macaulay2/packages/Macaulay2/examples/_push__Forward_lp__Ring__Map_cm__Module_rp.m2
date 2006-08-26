R4 = ZZ/32003[a..d];
R5 = ZZ/32003[a..e];
R6 = ZZ/32003[a..f];
M = coker genericMatrix(R6,a,2,3)
pdim M
G = map(R6,R5,{a+b+c+d+e+f,b,c,d,e})
F = map(R5,R4,random(R5^1, R5^{4:-1}))
P = pushForward(G,M)
pdim P
Q = pushForward(F,P)
pdim Q
P3 = QQ[a..d];
M = comodule monomialCurveIdeal(P3,{1,2,3})
P2 = QQ[a,b,c];
F = map(P3,P2,random(P3^1, P3^{-1,-1,-1}))
N = pushForward(F,M)
hilbertPolynomial M
hilbertPolynomial N
ann N
