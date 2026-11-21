-- BUGS.md 30 (found by the test audit of step 2, G2): the SDP sent to the
-- solver depends on RoundTol before any rounding.  lowerBound(1/x, {x - 1}, 2)
-- gives no bound ("SDP failed") with the default RoundTol, but the correct 1
-- with RoundTol => infinity and with D = 4.  On {x = 1}, 1/x = 1.
R = QQ[x];
(b, sol, mult) = lowerBound(1/x, matrix{{x - 1}}, 2);
assert(b =!= null and b <= 1 and b >= 1 - 2^-3)
(b, sol, mult) = lowerBound(1/x, matrix{{x - 1}}, 2, RoundTol => infinity);
assert(b =!= null and abs(b - 1) < 1e-6)
-- Macaulay2 issue #4107 (closed upstream as a CSDP bug; reproduced on this
-- machine in step 4): the same class with RoundTol => infinity.  The minimum
-- is 26/3 at (-2/3, 1/3).
R = QQ[x,y];
(b, sol) = lowerBound(x^2 + x*y + y^2 + x + 9, RoundTol => infinity);
assert(b =!= null and abs(b - 26/3) < 1e-6)
(b, sol) = lowerBound(x^2 + x*y + y^2 + x + 9);
assert(b =!= null and b <= 26/3 and b >= 26/3 - 2^-3)
