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

----------------------------------------------
-- Test: QQ coefficients ---------------------
----------------------------------------------

----------------------------------------------
-- Test: Quasi-homogeneous -------------------
----------------------------------------------

----------------------------------------------
-- Test: Inhomogeneous     -------------------
----------------------------------------------

----------------------------------------------
-- Test: Other monomial orders ---------------
----------------------------------------------

----------------------------------------------
-- Test: Quotient rings    -------------------
----------------------------------------------

----------------------------------------------
-- Test: Skew commuting    -------------------
----------------------------------------------

----------------------------------------------
-- Test: Weyl Algebrta     -------------------
----------------------------------------------

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



