-- BUGS.md 20: an unbounded objective returns a real "Gram matrix" that does
-- not represent the polynomial.  Correct: no Gram matrix and no parameters
-- when the solver reports no solution (the status is kept).
R = QQ[x][t];
f = x^2 - 3*x - t;                        -- SOS iff t <= -9/4; min t is unbounded
sol = solveSOS(f, t, RoundTol => 12);
assert(sol#GramMatrix === null and sol#Parameters === null and sosPoly sol === null)
assert instance(sol#Status, String)
-- the same through lowerBound(f, h, D) with an infeasible relaxation (with
-- D = 2 the multiplier is a constant and -y cannot be matched); found by the
-- tester of the applications (O3)
R = QQ[x,y];
(b, sol, mult) = lowerBound(10 - x^2 - y, matrix{{x^2 + 1}}, 2);
assert(b === null and sol#GramMatrix === null)
