-- doc-FINDING-core-zero-polynomial.m2 -- open finding, needs discussion
-- (independent tester, core).  Expected to FAIL on the original package.
-- solveSOS(f) "attempts to find a ... positive semidefinite matrix Q and a
-- vector of monomials mon such that f = mon' Q mon" (doc of solveSOS).  The
-- zero polynomial is a sum of squares (the empty sum; Q = 0).
-- On the original, solveSOS(0_R) raises "error: Expected nonzero inputs."
-- (This can occur in user code, e.g. solveSOS(f - g) with f == g.)
-- Asserted here: an SDPResult; if it has a Gram matrix, its SOSPoly is 0.
setRandomSeed 0;  -- fixed seed for anything random in the solver path
R = QQ[x,y];
sol = try solveSOS(0_R) else null;
assert instance(sol, SDPResult);
if sol#GramMatrix =!= null then assert(value sosPoly sol == 0);
-- (lead, after the decision on BUGS.md 27, LOG.md 2.10: (b)) the same
-- result as solveSOS(0_R, 2): the monomial 1, the Gram matrix 0, the empty
-- sum, whose value is 0 in R (with the fix of BUGS.md 5)
assert(sol#Status == "SDP solved, primal-dual feasible");
assert(sol#GramMatrix == 0 and ring sol#GramMatrix === QQ);
assert(sol#Monomials == matrix{{1_R}});
assert(length sosPoly sol == 0 and ring value sosPoly sol === R);
S = RR[x,y];
solR = solveSOS(0_S);
assert(solR#GramMatrix =!= null and value sosPoly solR == 0);
