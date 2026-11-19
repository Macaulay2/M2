-- doc-FINDING-radical-ring.m2
-- FINDING: binomialRadical returns its result in a different ring (with a
-- cyclotomic coefficient field) for non-cellular inputs whose minimal primes
-- need roots of unity, although the radical of I is defined over the ring of I.
-- Documentation: Outputs "the radical of I"; nothing about field extensions.
-- Reproduction: R = QQ[x,y]; I = ideal(x*(y^3-1)).
-- Observed: binomialRadical I = ideal(x*y^3-x) in
--   ((QQ[ww_3]/(ww_3^2+ww_3+1))[])[x..y], so  binomialRadical I == radical I
--   raises "expected ideals for the same ring".
-- Expected: an ideal of R equal to radical I.

R = QQ[x,y];
for I in {ideal(x*(y^3-1)), ideal(x^2*(y^4-1), x*y^2*(y-1))} do (
    r := binomialRadical I;
    assert(ring r === R);
    assert(r == radical I));
