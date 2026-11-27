-- doc-radical-ring.m2
-- binomialRadical returns its result in the ring of I, also for non-cellular
-- inputs whose minimal primes need roots of unity: the radical of I is
-- defined over the ring of I.
-- Documented: "the radical of I, in the ring of I".
-- Example: R = QQ[x,y]; I = ideal(x*(y^3-1)).
-- Expected: an ideal of R equal to radical I.

R = QQ[x,y];
for I in {ideal(x*(y^3-1)), ideal(x^2*(y^4-1), x*y^2*(y-1))} do (
    r := binomialRadical I;
    assert(ring r === R);
    assert(r == radical I));
