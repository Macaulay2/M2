-- BUGS.md 8: sosdecTernary ignores its options in the last step.  With
-- RoundTol => infinity the decomposition must be real, as for solveSOS.
R = QQ[x,y,z];
f = x^4 + y^4 + z^4 + x^2*y^2;
setRandomSeed 0;
(p, q) = sosdecTernary(f, RoundTol => infinity);
assert(#p == 1 and instance(coefficientRing ring p#0, RealField))
f = library("Motzkin", R);
setRandomSeed 0;
(p, q) = sosdecTernary(f, RoundTol => infinity);
assert(all(p | q, a -> instance(coefficientRing ring a, RealField)))
