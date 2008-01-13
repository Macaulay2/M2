R = QQ[a,b,c,d]/(a^4+b^4+c^4+d^4);
X = Proj R
Omega = cotangentSheaf X
HH^1(Omega)
F = sheaf coker matrix {{a,b}}
module F
