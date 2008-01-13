F = frac ZZ
F = frac (ZZ[a,b])
R = ZZ/101[x,y];
gens gb ideal(x^2*y - y^3)
K = frac R;
gens gb ideal(x^2*y - y^3)
a*b/b^4
f = (x-y)/(x^6-y^6)
(x^3 - y^3) * f
numerator f
denominator f
liftable(1/f,R)
liftable(f,R)
lift(1/f,R)
S = K[u,v];
I = ideal(y^2*u^3 + x*v^3, u^2*v, u^4);
gens gb I
Ires = res I
Ires.dd_2
A = ZZ/101[a,b,c];
f = map(K, A, {x^3/y^4, x^2/y^2, (x^2+y^2)/y^4});
kernel f
