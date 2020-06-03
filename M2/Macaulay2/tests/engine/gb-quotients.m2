-- Test of GB and quotients of polynomial rings

R = ZZ/32003[a..d]
I = ideal((a*d-b*c)*a*(b-c))
A = R/a
B = A/b -- not allowed yet.
I : sub((b-c), R)
gens oo
A = R/I
J = ideal(a^3,b^3,c^3)
gens gb J

