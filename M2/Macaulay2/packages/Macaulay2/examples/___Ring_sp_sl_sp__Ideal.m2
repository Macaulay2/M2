ZZ[x]/367236427846278621
A = QQ[u,v];
I = ideal random(A^1, A^{-2,-2,-2})
B = A/I;
use A;
C = A/(u^2-v^2,u*v);
D = GF(9,Variable=>a)[x,y]/(y^2 - x*(x-1)*(x-a))
ambient D
ZZ/2 === ZZ/(4,6)
R = ZZ/101[t]
R/t === R/t
