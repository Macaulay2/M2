R = ZZ/1277[x,y];
I = ideal(x^3 - 2*x*y, x^2*y - 2*y^2 + x);
(x^3 - 2*x) % I
(x^3) % I
S = ZZ[x,y];
144*x^2*y^2 % (7*x*y-2)
S = QQ[a..f]
J = ideal(a*b*c-d*e*f,a*b*d-c*e*f, a*c*e-b*d*f)
C = res J
F = syz transpose C.dd_4
G = transpose C.dd_3
G % F
F % G
A = QQ[x,y,z]/(x^3-y^2-3)
I = ideal(x^4, y^4)
J = ideal(x^3*y^2, x^2*y^3)
(gens J) % I
kk = frac(ZZ[a,b])
B = kk[x,y,z]
I = ideal(a*x^2-b*x-y-1, 1/b*y^2-z-1)
gens gb I
x^2*y^2 % I
