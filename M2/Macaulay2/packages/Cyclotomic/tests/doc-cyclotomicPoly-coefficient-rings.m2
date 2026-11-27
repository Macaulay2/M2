-- doc-cyclotomicPoly-coefficient-rings.m2
-- Documentation of cyclotomicPoly: input "v, a ring element, a variable of a
-- polynomial ring"; output "f, a ring element, the n-th cyclotomic polynomial
-- Phi_n(v)".  Phi_n(v) lies in the ring of v, also over coefficient rings
-- other than QQ and ZZ: (ZZ/7)[v], (ZZ/7)[x,y], (ZZ/2)[e],
-- (cyclotomicField 3)[s] and (GF 9)[r].
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
