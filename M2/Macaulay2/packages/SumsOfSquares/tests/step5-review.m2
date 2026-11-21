-- Findings of the review of step 5 (notes/review-step5-code.md, LOG.md 5.6).
-- 1: RoundTol <= 0 in optimisation problems (crashed after the first version
-- of the rounding of step 5; the original rounds to multiples of 2^-d)
R = QQ[x];
assert(first lowerBound(x^2 - 4*x, RoundTol => 0) == -4);
b = first lowerBound(x^2 - 4*x, RoundTol => -3);
assert(b <= -4 and b >= -4 - 2^3);
S = QQ[x][t];
assert((solveSOS(x^2 - 4*x - t, -t, RoundTol => 0))#Parameters == matrix(QQ, {{-4}}));
-- 2: a user-given monomial vector whose forced zeros leave a problem on which
-- CSDP gets stuck (BUGS.md 15): solved once more without the reductions
S = QQ[x,y][t];
for mon in {matrix{{x^2},{x*y},{y^2},{x},{y},{1}}, matrix{{x^2},{x*y},{y^2},{1}}} do (
    sol := solveSOS(x^4 + t*x^2*y^2 + y^4, t, mon);
    assert(sol#Parameters =!= null and abs(sol#Parameters_(0,0) + 2) < 1e-6));
