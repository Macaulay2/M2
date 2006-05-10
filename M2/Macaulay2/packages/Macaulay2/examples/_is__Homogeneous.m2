isHomogeneous(ZZ)
isHomogeneous(ZZ[x])
isHomogeneous(ZZ[x]/(x^3-x-3))
A = QQ[a,b,c];
B = A[x,y];
isHomogeneous B
isHomogeneous ideal(a*x+y,y^3-b*x^2*y)
R = QQ[a,b,c,Degrees=>{{1,1},{1,0},{0,1}}];
I = ideal(a-b*c);
isHomogeneous I
isHomogeneous(R/I)
isHomogeneous(R/(a-b))
S = QQ[a,b];
F = S^{-1,2}
isHomogeneous F
G = S^{1,2}
phi = random(G,F)
isHomogeneous phi
degree phi
M = coker phi
isHomogeneous(a*M)
isHomogeneous((a+1)*M)
