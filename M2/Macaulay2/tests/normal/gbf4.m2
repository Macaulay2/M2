-- Some tests for LinearAlgebra variant
I = ideal vars(QQ[x])
assert try (elapsedTime gb(I, Algorithm => LinearAlgebra); false) else true

R = ZZ/32003[x]
I = ideal vars R
elapsedTime result = gens gb(I, Algorithm => LinearAlgebra);
assert(result == vars R)

kk = ZZ/101;
R1 = kk[a..g, MonomialSize=>8];
J1 = ideal random(R1^1, R1^{-5,-5,-5,-6});
elapsedTime gens gb(ideal J1_*, Algorithm => LinearAlgebra, DegreeLimit => 10);
elapsedTime groebnerBasis(ideal J1_*, Strategy => "F4");


kk = GF 125;
R1 = kk[a..g, MonomialSize=>8];
J1 = ideal random(R1^1, R1^{-5,-5,-5,-5});
elapsedTime gbA = flatten entries gens gb(ideal J1_*, DegreeLimit => 10);
elapsedTime gbB = flatten entries gens gb(ideal J1_*, Algorithm => LinearAlgebra, DegreeLimit => 10);
--elapsedTime groebnerBasis(ideal J1_*, Strategy => "F4"); -- BUG: this should be disallowed...!
assert(gbA == sort gbB) -- TODO: make sure gbB is sorted!

-- GB for lex order
-- 
restart
kk = ZZ/101;
R1 = kk[a..f];

setRandomSeed 42
J1 = ideal random(R1^1, R1^{-2, -2, -2, -2});
elapsedTime gbA = flatten entries gens gb(J1);
elapsedTime gbB = flatten entries gens gb(ideal J1_*, Algorithm => LinearAlgebra);
elapsedTime gbC = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
needsPackage "Msolve"
elapsedTime gbD = flatten entries msolveGB(ideal J1_*, Verbosity => 2, Threads => 18);
#gbA == 12
#gbA == #gbB
#gbA == #gbC
#gbA == #gbD
assert(gbA == gbB) -- fails, just not sorted correctly...
assert(gbA == gbC)
assert(gbA == gbD)
assert(sort gbA == gbA)
assert(sort gbB == gbA) -- true.  So GB is correct, but is in incorrect order.

-- now previous example, transferred to lex --
R2 = kk[a..f, MonomialOrder => Lex];
J2 = sub(J1, R2)
elapsedTime gbA = flatten entries gens gb(J2); -- 38.1 sec
--elapsedTime gens gb(ideal J1_*, Algorithm => LinearAlgebra); -- fails for lex order...
elapsedTime gbC = flatten entries groebnerBasis(ideal J2_*, Strategy => "F4"); -- 1.1 sec

needsPackage "GroebnerWalk"
setWalkTrace 2
elapsedTime gbB = flatten entries gens groebnerWalk(gb J1, R2); -- -- 14 sec

-- these all pass
assert(#gbA == 269)
assert(#gbB == 269)
assert(#gbC == 269)
assert(gbA == gbB)
assert(gbA == gbC)

