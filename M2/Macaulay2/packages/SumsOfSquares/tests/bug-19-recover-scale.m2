-- BUGS.md 19: recoverSolution ignores the scale of the moment matrix.  The
-- minimizer of (x^2-x)/(x^2+1) is sqrt 2 - 1 = 0.414214 (the documented
-- output 0.382683 is wrong).
R = QQ[x];
(b, sol) = lowerBound((x^2 - x)/(x^2 + 1), RoundTol => infinity);
pt = recoverSolution sol;
assert(abs(sub(x, pt) - (sqrt 2 - 1)) < 1e-4)
(b, sol) = lowerBound((x^2 - x)/(x^2 + 1));
pt = recoverSolution sol;
assert(abs(sub(x, pt) - (sqrt 2 - 1)) < 1e-2)
