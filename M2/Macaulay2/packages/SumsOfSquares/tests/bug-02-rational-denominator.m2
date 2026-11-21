-- BUGS.md 2: lowerBound of a rational function whose denominator changes
-- sign returns false bounds.  Decided (LOG.md 1.7 item 4): certify den >= 0
-- by an SOS decomposition, else use num*den - t*den^2.  Both functions below
-- are unbounded below, so no bound is correct; the documented example (a
-- positive denominator) keeps its certified bound.
R = QQ[x];
(b, sol) = lowerBound(1/x);
assert(b === null)
(b, sol) = lowerBound((x^2 + 1)/x);       -- f(-1) = -2, unbounded below
assert(b === null)
(b, sol) = lowerBound((x - 1)^2/(x^3 + 1));
assert(b === null)                        -- -> -infinity as x -> -1 from below
-- with equations (decision: the denominator must be nonnegative on {h = 0}):
-- on {x^2 = 1} the function 1/x takes the values 1 and -1 (test audit, G1;
-- the original returns 1)
(b, sol, mult) = lowerBound(1/x, matrix{{x^2 - 1}}, 2);
assert(b === null or b <= -1)
(b, sol) = lowerBound((x^2 - x)/(x^2 + 1));
assert(b =!= null and b <= 1/2 - 1/sqrt 2)
-- after the review of step 3: the fallback relaxation is infeasible when f is
-- unbounded below, and CSDP did not stop on it; the denominator's sign
-- change is detected without an SDP (each call returns in milliseconds)
for g in {x/(x + 1), (x^2 - x)/(x^2 - 1), x/(x - 2)} do (
    (bg, solg) := lowerBound g;
    assert(bg === null))
-- a denominator that is negative everywhere: the signs are flipped
(b, sol) = lowerBound((x^2 - x)/(-x^2 - 1));
assert(b =!= null and b <= -1/2 - 1/sqrt 2)
