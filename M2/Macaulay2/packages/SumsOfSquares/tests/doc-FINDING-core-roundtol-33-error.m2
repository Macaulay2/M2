-- doc-FINDING-core-roundtol-33-error.m2 -- open finding (independent tester,
-- core).  Expected to FAIL on the original package.
-- RoundTol is the rounding precision in binary digits (doc of RoundTol); any
-- value should give a result.  On the original, RoundTol => 33 raises
-- "error: expected true or false" for every problem, even x^2+y^2+1, while
-- 32 gives a rational and 34 a real result.
-- Correct behaviour asserted here: an SDPResult with a valid certificate.
setRandomSeed 0;  -- fixed seed for anything random in the solver path
R = QQ[x,y];
for f in {x^2 + y^2 + 1, 2*x^4+5*y^4-2*x^2*y^2+2*x^3*y} do (
    sol := try solveSOS(f, RoundTol=>33) else null;
    assert instance(sol, SDPResult);
    assert(sol#Status == "SDP solved, primal-dual feasible");
    s := sosPoly sol;
    assert(if ring sol#GramMatrix === QQ then value s == f
           else clean(1e-6, value s - sub(f, ring s)) == 0);
    );
-- also with parameters: min x^2 - 3x = -9/4
S = QQ[x][t];
sol = try solveSOS(x^2 - 3*x - t, -t, RoundTol=>33) else null;
assert instance(sol, SDPResult);
assert(abs((sol#Parameters)_(0,0) + 9/4) < 1e-6);
