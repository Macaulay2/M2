restart
needsPackage "NCAlgebra"

kk = ZZ/32003
R = kk{a,b,c,d}
I1 = ncIdeal apply(subsets(gens R, 2), x -> - x#0 * x#1 + x#1 * x#0)
homogDual I1
I2 = ncIdeal {a^2+a*b,a*c+b*d,a*d,b^2,c^2,c*d+d^2}
I = I1 + I2
J = homogDual I

J = ncIdeal {b*a+a*b-a^2, c*b+b*c, d*b-c*a+b*d-a*c, d^2-d*c-c*d}
methods ncGroebnerBasis
options ncGroebnerBasis
JGB = ncGroebnerBasis(J, DegreeLimit=>10);
(gens JGB)/degree//tally

restart
debug needsPackage "PolynomialAlgebra"
kk = ZZ/32003
R = kk{d,c,b,a}
J = ideal {b*a+a*b-a^2, c*b+b*c, d*b-c*a+b*d-a*c, d^2-d*c-c*d}
gbTrace=2
elapsedTime NCGB(J, 3);
select((ideal oo)_*, f -> degree f == {3})
netList oo

elapsedTime NCGB(J, 15);
(ideal oo)_*/degree//tally
F = d^2-d*c-c*d
F*d-d*F - F*c + c*F

J = ideal(d^2-d*c-c*d)
elapsedTime NCGB(J, 3)

J = ideal(d^2-c^2)
elapsedTime NCGB(J, 3)
