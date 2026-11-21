-- Open finding (independent tester, applications): recoverSolution returns a
-- point that is not optimal, without any warning, when the moment matrix has
-- rank greater than one (several minimizers).
-- Doc node recoverSolution: "attempts to find the optimizing solutions of a
-- sum-of-squares problem by checking if the moment matrix is rank one.  In
-- this case it factorizes the matrix and returns the defining vector."
-- Original: for f = x^2 (x-1)^2 (x+1)^2 (minimum 0 at x = -1, 0, 1, bound 0
-- found), the moment matrix in (1, x, x^2, x^3) has eigenvalues 1.570,
-- 1.407, .133, 7e-10 (rank 3), yet recoverSolution returns {x => .838637}
-- (f = .0619) and prints nothing.  (For the circle example of lowerBound,
-- eigenvalues 1.25, .75, 0, it does print "Moment matrix is not rank one,
-- solution might not be correct.")
-- Expected: either no point is returned (null or {}) for a moment matrix
-- that is not rank one, or the returned point is a minimizer, or a warning
-- that the moment matrix is not rank one (as in the multivariate case; added
-- by the lead after the test audit, since BUGS.md 24 leaves this open).
-- This file FAILS on the original (version 2.2).

R = QQ[x];
f = x^2*(x - 1)^2*(x + 1)^2;
(bound, sol) = lowerBound f;
assert(bound <= 0 and bound >= -1e-3);
(err, out) = capture "pt = recoverSolution sol;";
assert(not err)
print("recovered " | toString pt);
assert(pt === null or pt === {} or abs(sub(f, x => pt#0#1)) < 1e-3 or match("rank", out));
