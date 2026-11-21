-- BUGS.md 32 (found by the lead in step 4): x is not a sum of squares in
-- QQ[x]/(x^2) (x = q00 + 2 q01 x forces q00 = 0, hence q01 = 0), but the SDP
-- on (1, x) is weakly infeasible and CSDP reports success; the package then
-- returns a real near-certificate of x + 8.9e-10.  Correct: no Gram matrix.
S = QQ[x]/ideal(x^2);
sol = solveSOS(x, matrix{{1_S},{x}});
assert(sol#GramMatrix === null)
sol = solveSOS(x, 2);
assert(sol#GramMatrix === null)
