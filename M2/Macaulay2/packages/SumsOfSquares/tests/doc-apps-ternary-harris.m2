-- Tests written from the documentation alone (independent tester, applications).
-- Topic: sosdecTernary on the Harris form, a nonnegative ternary form of
-- degree 10 with 30 projective zeros (doc node library; [Har99]).  By
-- [Har99] (a ternary m-ic with finitely many zeros that is SOS has at most
-- m^2/4 zeros) it is not SOS, so a denominator of positive degree is needed.
-- Separate file because it takes about 15 s.

RRR = RR[x,y,z];
hrNorm = g -> max(apply(terms g, t -> abs leadCoefficient t) | {0});
R = QQ[x,y,z];
f = library("Harris", R);
assert(isHomogeneous f and first degree f == 10);
-- some of its real zeros: (1,1,0) and its permutations
assert(sub(f, {x => 1, y => 1, z => 0}) == 0 and sub(f, {x => 1, y => 0, z => 1}) == 0 and sub(f, {x => 0, y => 1, z => 1}) == 0);
setRandomSeed 0;
(p, q) = sosdecTernary f;
assert(instance(p, List) and instance(q, List) and #p >= 1);
assert(all(p | q, s -> instance(s, SOSPoly)));
assert(all(p | q, s -> all(coefficients s, c -> c > 0)));
assert(10 + sum(q, s -> first degree value s) == sum(p, s -> first degree value s));
assert(any(q, s -> first degree value s > 0));
lhs = sub(f, RRR) * product(q, s -> sub(value s, RRR));
rhs = product(p, s -> sub(value s, RRR));
-- 1e-4: the decomposition may be floating point; its accuracy is that of
-- the solver, which depends on the machine (1.6e-6 on an Apple M5, CSDP 6.2.0)
assert(hrNorm(lhs - rhs) < 1e-4 * hrNorm lhs);
-- f(1,1,0) = 0 and f * prod q = prod p, so some numerator factor vanishes
-- at (1,1,0)
hrAt = g -> abs lift(sub(sub(g, RRR), {RRR_0 => 1, RRR_1 => 1, RRR_2 => 0}), RR);
assert(any(p, s -> hrAt(value s) < 1e-4 * hrNorm sub(value s, RRR)));
