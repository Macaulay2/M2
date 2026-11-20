-- Open finding (independent tester, applications): recoverSolution fails on
-- the rank one moment matrix of the origin.
-- Doc node recoverSolution: "checking if the moment matrix is rank one.  In
-- this case it factorizes the matrix and returns the defining vector."
-- Original: recoverSolution(matrix{{1},{x},{y}}, X) with
-- X = | 1 0 0 ; 0 0 0 ; 0 0 0 | (positive semidefinite, rank one, the moment
-- matrix of the point (0,0)) stops with "error: Moment matrix is not
-- positive semidefinite".  The same for 2*X, for X over QQ and for
-- monomials (1, x, x^2).  Every other rank one v v^T tried works (also
-- v = (1, 0, 1), v = (1, .001, 0)).
-- Expected: {x => 0, y => 0}.
-- This file FAILS on the original (version 2.2).

R = RR[x,y];
mon = matrix {{1},{x},{y}};
X = matrix(RR, {{1,0,0},{0,0,0},{0,0,0}});
pt = try recoverSolution(mon, X) else "error";
print("recovered " | toString pt);
assert(instance(pt, List));
assert(all(pt, o -> abs(o#1) < 1e-9));
