-- Findings of the review of step 3 (notes/review-step3-code.md 2-4, LOG.md 3.7).
out = cmd -> (
    (err, o) := capture cmd;
    assert(not err);
    concatenate select(lines o, l -> not match("experimental computation over inexact field|results not reliable|^ *$|^i[0-9]+ :|^o[0-9]+ ", l)))
-- 2: a far minimizer of a rational function: the moment of 1 is at the
-- accuracy of the solver, so recoverSolution warns (BUGS.md 24)
R = QQ[x];
(b, sol) = lowerBound((x-100)^2/(x^4+1), RoundTol => infinity);
assert match("rank", out "recoverSolution sol;")
-- no warning for a polynomial with a far minimizer
(b, sol) = lowerBound((x-100)^2+1, RoundTol => infinity);
assert(not match("rank", out "recoverSolution sol;"))
assert(abs(sub(x, recoverSolution sol) - 100) < 1e-2)
-- 3: a degree bound below the degree of every generator: no multiples, no SOS
(sol, mult) = sosInIdeal(matrix{{x^4 + 1}}, 2);
assert(sol#GramMatrix === null and mult === null)
-- 4: an objective on the zero polynomial with parameters is unbounded
S = QQ[x][symbol t];
sol = solveSOS(0_S, -S_0);
assert(sol#GramMatrix === null and sol#Status == "SDP solved, primal infeasible")
