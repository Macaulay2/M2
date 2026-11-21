-- doc-FINDING-cyclotomicPoly-coefficients.m2 -- a finding of the independent tester.
-- Documentation of cyclotomicPoly: input "v, a ring element, a variable of a
-- polynomial ring"; output "f, a ring element, the n-th cyclotomic polynomial
-- Phi_n(v)".  Over QQ and ZZ this holds.  Over other coefficient rings:
--   (ZZ/7)[v]:   cyclotomicPoly(5, v) is v^4+v^3+v^2+v+1 but in frac((ZZ/7)[v]);
--                cyclotomicPoly(6, v) raises "expected polynomial ring";
--   (ZZ/7)[x,y]: cyclotomicPoly(6, y) raises "expected polynomial ring";
--   (cyclotomicField 3)[s], (GF 9)[r]: cyclotomicPoly(4, .) raises "expected
--                coefficient ring of the form ZZ/n, ZZ, QQ, or GF".
-- (Unsure whether the documentation means only QQ and ZZ; it does not say so.)
B = (ZZ/7)[v];
assert(ring cyclotomicPoly(5, v) === B);
assert(cyclotomicPoly(5, v) == v^4 + v^3 + v^2 + v + 1);
assert(cyclotomicPoly(6, v) == v^2 - v + 1);
B2 = (ZZ/7)[x, y];
assert(cyclotomicPoly(6, y) == y^2 - y + 1);
P = (ZZ/2)[e];
assert(ring cyclotomicPoly(3, e) === P);
C = (cyclotomicField 3)[s];
assert(cyclotomicPoly(4, s) == s^2 + 1 and ring cyclotomicPoly(4, s) === C);
K = GF 9; D = K[r];
assert(cyclotomicPoly(4, r) == r^2 + 1 and ring cyclotomicPoly(4, r) === D);
