S = ZZ/101[a,b,c];
I = ideal(a^3+b^3+c^3)
R = S/I
eulers(R)
J = substitute(ideal(b,a+c),R)
eulers(R/J)
