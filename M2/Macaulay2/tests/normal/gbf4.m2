-- To be fixed:
-- Output needs to be sorted
-- Weighted polynomial rings?
-- Different monomial orders

-- Doesn't yet work:
-- Inhomogeneous
-- Exterior variables
-- Quotient Rings
-- Weyl Algebras

-- Other observations:
-- giving wrong HF in standard algorithm crashes the system

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
elapsedTime gens gb(ideal J1_*, Algorithm => LinearAlgebra);
elapsedTime groebnerBasis(ideal J1_*, Strategy => "F4");

kk = GF 125;
R1 = kk[a..g, MonomialSize=>8];
J1 = ideal random(R1^1, R1^{-5,-5,-5,-5});
elapsedTime gbA = flatten entries gens gb(ideal J1_*, DegreeLimit => 10);
elapsedTime gbB = flatten entries gens gb(ideal J1_*, Algorithm => LinearAlgebra, DegreeLimit => 10);
--elapsedTime groebnerBasis(ideal J1_*, Strategy => "F4"); -- BUG: this should be disallowed...!
assert(gbA == sort gbB) -- TODO: make sure gbB is sorted!

-- GB for lex order
restart
kk = ZZ/101;
R1 = kk[a..f];

setRandomSeed 42
J1 = ideal random(R1^1, R1^{-2, -2, -2, -2});
elapsedTime gbA = flatten entries gens gb(J1);
elapsedTime gbB = flatten entries gens gb(ideal J1_*, Algorithm => LinearAlgebra);
elapsedTime gbC = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
needsPackage "Msolve"
elapsedTime gbD = flatten entries msolveGB(ideal J1_*, Verbosity => 2, Threads => 8);
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
-- elapsedTime gens gb(ideal J1_*, Algorithm => LinearAlgebra); -- fails for lex order...
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

-- weighted projective space, graded reverse lex
-- restart
kk = ZZ/101;
R1 = kk[a..f,Degrees => {1,2,3,4,4,4}];
setRandomSeed 42
J1 = ideal random(R1^1, R1^{-5, -5, -5, -5});
elapsedTime gbA = flatten entries gens gb(J1);
elapsedTime gbB = flatten entries gens gb(ideal J1_*, Algorithm => LinearAlgebra);
elapsedTime gbC = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
needsPackage "Msolve"
elapsedTime gbD = flatten entries msolveGB(ideal J1_*, Verbosity => 2, Threads => 8);
#gbA == 12
#gbA == #gbB  -- incorrect
#gbA == #gbC
#gbA == #gbD
assert(gbA == gbB) -- fails, just not sorted correctly...
assert(gbA == gbC)
assert(gbA == gbD)
assert(sort gbA == gbA)
assert(sort gbB == gbA) -- true.  So GB is correct, but is in incorrect order.

-- exterior algebra
-- restart
kk = ZZ/101
R1 = kk[a,b,c,d,e,f, SkewCommutative=>true]
setRandomSeed 42
J1 = ideal random(R1^1, R1^{-2,-2,-2,-2});
elapsedTime gbA = flatten entries gens gb(J1);
elapsedTime gbB = flatten entries gens gb(ideal J1_*, Algorithm => LinearAlgebra);
elapsedTime gbC = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
assert(gbA == gbB) -- fails, gbB seems to compute over polynomial ring instead...
assert(gbA == gbC)
assert(gbA == gbD)

-- hilbert driven gb computation
--restart
setRandomSeed 42
kk = ZZ/101
R1 = kk[a..g, MonomialSize=>8];
K1 = ideal (a^5,b^5,c^5,d^5,e^5)
hfJ = poincare K1
J1 = ideal random(R1^1, R1^{-5,-5,-5,-5,-5});
elapsedTime gbA = flatten entries gens gb(ideal J1_*, Hilbert => hfJ);
elapsedTime gbB = flatten entries gens gb(ideal J1_*,
                                          Algorithm => LinearAlgebra,
					  Hilbert => hfJ);
elapsedTime gbC = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
-- assert(gbA == sort gbB) 
assert(sort gbB == gbC)

-- GB over a quotient ring
-- restart
setRandomSeed 42
kk = ZZ/101
R1 = kk[a,b,c,d,e,f]/ideal(a^2 - b*c)
J1 = ideal random(R1^1, R1^{-5,-5,-5,-5});
elapsedTime gbA = flatten entries gens gb(ideal J1_*);
elapsedTime gbB = flatten entries gens gb(ideal J1_*,
                                          Algorithm => LinearAlgebra);
elapsedTime gbC = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
assert(gbA == sort gbB) -- fails

