-- This is not a bug, but an example, see Frietag, 1506.00892v1.pdf
restart
kk = ZZ/32003
R = kk[y_0..y_3, x_0..x_3]
I = ideal(y_0^2 - (x_0^2+x_1^2+x_2^2+x_3^2),
    y_1^2 - (x_0^2-x_1^2+x_2^2-x_3^2),
    y_2^2 - (x_0^2+x_1^2-x_2^2-x_3^2),
    y_3^2 - (x_0^2-x_1^2-x_2^2+x_3^2))
jacobian I
singI = trim(I + minors(4, oo))
decompose singI
phi = select(singI_*, f -> degree f == {4})
-- consider, in the product of P^7 x P^57
S = kk[gens R, z_0..z_57]
gr0 = minors(2,sub(matrix{phi}, S) || matrix{{z_0..z_57}})
gr1 = sum(gr0_*/(f -> saturate(ideal f, product toList (x_0..y_3))));
gr = sub(I,S) + minors(2,sub(matrix{phi}, S) || matrix{{z_0..z_57}})
--gr1 = saturate(gr, sub(phi_0, S));
loadPackage "MinimalPrimes"
elapsedTime C = minprimes(gr, Verbosity=>2);

gbTrace=3
L = flatten entries gens gb gr1;
J = ideal(L/(f -> (facs := factorize f; product for v in facs list if first degree v#1 > 1 then v#1 else continue)));
J = trim J;
codim J -- 57

IJ = sub(I,S) + J;
gens gb(IJ, DegreeLimit=>4);
La = flatten entries oo;
Ja = ideal(La/(f -> (facs := factorize f; product for v in facs list if first degree v#1 > 1 then v#1 else continue)));
Ja = trim Ja;
betti Ja

codim IJ

