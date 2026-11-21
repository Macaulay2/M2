-- Positive characteristic: coefficient fields.  Galois fields as input,
-- the rings in which results live, and the errors for fields and
-- coefficients that are not supported.
-- A part of pchar-fields-full.m2, which check skips: of the oracle checks on
-- single ideals, only the first of each section is kept here.
load (Binomials#"auxiliary files" | "tests/helpers/pchar-oracles.m2");
needsPackage "ConwayPolynomials";

-- 1. no extension needed: the answer is in the ring of the input
R = ZZ/5[x,y,z];
L = binomialPrimaryDecomposition ideal(x^4 - y^4);
assert(#L == 4 and ring first L === R);
assert(ring binomialRadical ideal(x^10 - y^10) === R);
assert(ring first binomialMinimalPrimes ideal(x^2 - y^2, z^5) === R);
-- 2. an extension: one ring per input ring and degree, shared by all functions
L = binomialPrimaryDecomposition ideal(x^3 - y^3);
T = ring first L;
F = coefficientRing T;
assert(instance(F, GaloisField) and F.order == 25 and char T == 5);
assert((options T).MonomialOrder === (options R).MonomialOrder and degrees T == degrees R);
assert(apply(gens T, baseName) == apply(gens R, baseName));
assert(ring first binomialMinimalPrimes ideal(x^3 - y^3) === T);
assert(ring first binomialAssociatedPrimes ideal(x^8 - y^8, z) === T);
assert(ring first binomialPrimaryDecomposition ideal(x^24 - y^24) === T);
assert(ring first first binomialSolve ideal(x^3 - 1, y, z) === F);
-- the generator of the field is called ww_(order - 1) (provisional)
assert(toString F_0 == "ww_24");
-- 3. results over different fields are moved to one ring
L = binomialPrimaryDecomposition ideal(x^3 - y^3, z^7 - 1);    -- 3 | 25 - 1, 7 | 5^6 - 1
assert(#L == 21 and (coefficientRing ring first L).order == 5^6);
I = ideal(z*(x^3 - y^3), z^2 - z, x^7 - y^7);                   -- components over GF(25) and GF(5^6)
L = binomialPrimaryDecomposition I;
S = ring first L;
assert((coefficientRing S).order == 5^6 and intersect L == pchExtend(I, S));
assert pchCheckPrimaryDecomposition(I, L);
assert pchCheckMinimalPrimes(I, binomialMinimalPrimes I);
assert pchCheckAssociatedPrimes(I, binomialAssociatedPrimes I);
-- 4. input over a Galois field
K = GF(2, 2, Variable => a);
U = K[x,y,z];
I = ideal(x^3 - y^3);                                            -- splits over GF(4)
L = binomialPrimaryDecomposition I;
assert(#L == 3 and ring first L === U and intersect L == I);
assert pchCheckAll I;
I = ideal(x^5 - y^5, z^2);                                       -- needs GF(16)
L = binomialPrimaryDecomposition I;
T2 = ring first L;
assert(#L == 5 and (coefficientRing T2).order == 16 and intersect L == pchExtend(I, T2));
assert pchCheckAll I;
assert pchCheckAll ideal(x^6 - y^6, z*(x^2 - y^2), z^2);
-- a = e(1/3) has a square root in GF(4) (a^2 = e(2/3)) and a cube root in GF(64)
use U;                                                           -- T2 = ... rebound x, y, z to T2
assert(binomialPrimaryDecomposition ideal(x^2 - a*y^2) == {ideal(x^2 - a*y^2)});   -- (x - a^2 y)^2
assert(binomialRadical ideal(x^2 - a*y^2) == ideal(x - a^2*y));
L = binomialPrimaryDecomposition ideal(x^3 - a*y^3);
assert(#L == 3 and (coefficientRing ring first L).order == 64);
K9 = GF(3, 2, Variable => b);
V = K9[x,y,z];
-- not unital: the radical needs no root (index 2, prime to 3), the primes do
assert(cellularBinomialRadical ideal(x^2 - b^2*y^2, z^3) == ideal(x^2 - b^2*y^2, z));
assert(binomialPrimaryDecomposition ideal(x^2 - b^2*y^2) == {ideal(x - b*y), ideal(x + b*y)});
-- a Galois field of degree 1
K7 = GF 7;
W = K7[x,y];
L = binomialPrimaryDecomposition ideal(x^4 - y^4);
assert(#L == 4 and (coefficientRing ring first L).order == 49);
-- 5. coefficients: 1 and -1, and every other nonzero element of the field
R3 = ZZ/3[x,y,z];
I = ideal(x^2 + y^2);                                            -- -1 = e(1/2): 4th roots of unity, GF(9)
L = binomialPrimaryDecomposition I;
assert(#L == 2 and (coefficientRing ring first L).order == 9);
assert pchCheckAll I;
assert pchCheckAll ideal(x^3 + y^3);                             -- (x + y)^3: primary
assert(binomialRadical ideal(x^3 + y^3) == ideal(x + y));
L = binomialMinimalPrimes ideal(x^2 + 1, y - 1, z);                -- x^2 + 1 is not unital: no binomialSolve
assert(#L == 2 and (coefficientRing ring first L).order == 9);
I0 = ideal(R3_0^2 + 1, R3_1 - 1, R3_2);
assert(all(L, P -> isSubset(pchExtend(I0, ring P), P) and dim P == 0 and degree P == 1));
R5 = ZZ/5[x,y];
-- the unique 5th root of 2 is 2 itself: x^5 - 2 y^5 = (x - 2y)^5
assert(binomialPrimaryDecomposition ideal(x^5 - 2*y^5) == {ideal(x^5 - 2*y^5)});
assert(binomialRadical ideal(x^5 - 2*y^5) == ideal(x - 2*y));
assert(binomialMinimalPrimes ideal(x^5 - 2*y^5) == {ideal(x - 2*y)});
assert(binomialIsPrimary ideal(x^5 - 2*y^5) and not binomialIsPrime ideal(x^5 - 2*y^5));
assert(not binomialIsPrimary ideal(x^3 - 2*y^3));                 -- decided without roots (index 3 is prime to 5)
-- 2 = 3^3 is a cube in ZZ/5, and the cube roots of unity lie in GF(25)
L = binomialMinimalPrimes ideal(x^3 - 2*y^3);
assert(#L == 3 and (coefficientRing ring first L).order == 25 and member(pchExtend(ideal(x - 3*y), ring first L), L));
-- ... and no roots needed: fine
assert(binomialPrimaryDecomposition ideal(x - 2*y) == {ideal(x - 2*y)});
assert(binomialRadical ideal(x^3 - 2*y^3) == ideal(x^3 - 2*y^3));           -- index 3 prime to 5: radical without roots
assert(cellularBinomialRadical ideal(x^3 - 2*y^3) == ideal(x^3 - 2*y^3));
assert(binomialIsPrime ideal(x - 2*y) and not binomialIsPrime ideal(x^3 - 2*y^3));
-- 6. fields that are not supported: not perfect, so roots may not exist in any separable extension
Q = frac(ZZ/3[t, Degrees => {0}]);      -- with t of degree 1 Macaulay2's own saturate and radical fail
X = Q[x,y];
assert(binomialCellularDecomposition ideal(x^2 - t*y^2) == {ideal(x^2 - t*y^2)});
assert(binomialRadical ideal(x^2 - t*y^2) == ideal(x^2 - t*y^2));                    -- index 2, prime to 3
assert(binomialRadical ideal(x^3 - y^3) == ideal(x - y));                            -- the 3-saturation needs no root
assert(binomialPrimaryDecomposition ideal(x^2 - y^2) == {ideal(x - y), ideal(x + y)});   -- no extension needed
-- field extensions and roots are refused, with the reason
assert(try (binomialMinimalPrimes ideal(x^4 - y^4); false) else true);
assert(binomialMinimalPrimes ideal(x - y) == {ideal(x - y)});
assert(try (binomialRadical ideal(x^3 - t*y^3); false) else true);                   -- the cube root of t is inseparable
assert(try (binomialPrimaryDecomposition ideal(x^2 - t*y^2); false) else true);      -- a square root of t
-- a Galois field that is not defined by its Conway polynomial works, see pchar-nonconway.m2
A = ZZ/3[c]/(c^2 + 1);
Kn = GF A;
Y = Kn[x,y];
assert(binomialPrimaryDecomposition ideal(x^2 - y^2) == {ideal(x - y), ideal(x + y)});   -- no extension needed
L = binomialPrimaryDecomposition ideal(x^5 - y^5);                                       -- extension needed
assert(#L == 5 and (coefficientRing ring first L).order == 81);
