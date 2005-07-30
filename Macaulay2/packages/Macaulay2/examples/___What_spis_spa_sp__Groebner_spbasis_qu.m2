R = ZZ/1277[a..d]
F = 5*a^3 + d^2 + a*d + b*c + 1
leadTerm F
R = ZZ/1277[x,y];
I = ideal(x^3 - 2*x*y, x^2*y - 2*y^2 + x);
leadTerm I
gens gb I
R = ZZ/1277[x,y, MonomialOrder => Lex];
I = ideal(x^3 - 2*x*y, x^2*y - 2*y^2 + x);
gens gb I
