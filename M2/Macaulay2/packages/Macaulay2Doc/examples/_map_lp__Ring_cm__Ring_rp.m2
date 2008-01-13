A = QQ[a..e];
B = A[x,y];
C = QQ[a..e,x,y];
map(B,A)
map(A,B)
F = map(B,C)
G = map(C,B)
F*G
oo == id_B
G*F
oo == id_C
D = QQ[x,y,z];
E = D/(x^2-z-1,y);
F = map(E,D)
G = map(D,E)
x^3
G x^3
