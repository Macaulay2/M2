R = QQ[a..d];
I = ideal (a^2*b-c^2, a*b^2-d^3, c^5-d);
J = ideal (a^2,b^2,c^2,d^2);
I:J
P = quotient(I,J)
Q = quotient(I,J,MinimalGenerators => false)
Q == P
R = ZZ/32003[a..d];
I = ideal(a^3-b, a^4-c)
Ih = homogenize(I,d)
saturate(Ih,d)
saturate(Ih,d,Strategy => Bayer)
