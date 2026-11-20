-- doc-FINDING-core-odd-power.m2 -- open finding, needs discussion (independent
-- tester, core).  Expected to FAIL on the original package.
-- The documentation lists (symbol ^, SOSPoly, ZZ) and says that sums of
-- squares "support many common operations with polynomials".  Odd powers of a
-- sum of squares are sums of squares (s^(2k+1) = s * (s^k)^2), and s^1 = s.
-- On the original, s^1 and s^3 raise "error: power should be an even
-- integer" (and s^0 "power should be a positive integer", although 1 = 1^2).
-- Asserted here: s^1 and s^3 work and have the right values.
setRandomSeed 0;  -- fixed seed for anything random in the solver path
R = QQ[x,y];
s = sosPoly(R, {x+1,y}, {2,3});
s1 = try s^1 else null;
assert instance(s1, SOSPoly);
assert(value s1 == value s);
s3 = try s^3 else null;
assert instance(s3, SOSPoly);
assert(value s3 == (value s)^3);
assert all(coefficients s3, c -> c > 0);
-- (lead, after the decision on BUGS.md 28, LOG.md 2.10: (b)) every exponent
-- >= 0; s^0 = 1 = 1^2; even exponents unchanged; negative ones an error
s0 = s^0;
assert instance(s0, SOSPoly);
assert(value s0 == 1_R);
s5 = s^5;
assert(instance(s5, SOSPoly) and value s5 == (value s)^5);
assert all(coefficients s5, c -> c > 0);
s2 = s^2;
assert(gens s2 == {value s} and coefficients s2 == {1});
assert((try s^-1 else "error") === "error");
