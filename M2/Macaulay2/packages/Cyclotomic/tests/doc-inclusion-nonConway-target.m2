-- doc-inclusion-nonConway-target.m2
-- Documentation of cyclotomicInclusion: "B, a ring, a field that contains A";
-- output "the inclusion A -> B that is compatible with the roots of unity:
-- e(q) goes to e(q)".  cyclotomicRoot is documented for every Galois field,
-- also for one not defined by its Conway polynomial.  The inclusion exists
-- also when B is such a Galois field and A is neither ZZ/p nor B: GF 9 into
-- GF(ZZ/3[c]/(c^2+1)), GF 4 into GF(ZZ/2[d]/(d^4+d^3+d^2+d+1)).
K9 = GF 9;
N9 = GF(ZZ/3[c]/(c^2+1));
phi = cyclotomicInclusion(K9, N9);
for k from 0 to 7 do assert(phi cyclotomicRoot(K9, k/8) == cyclotomicRoot(N9, k/8));
K4 = GF 4;
N16 = GF(ZZ/2[d]/(d^4+d^3+d^2+d+1));
psi = cyclotomicInclusion(K4, N16);
assert(psi cyclotomicRoot(K4, 1/3) == cyclotomicRoot(N16, 1/3));
