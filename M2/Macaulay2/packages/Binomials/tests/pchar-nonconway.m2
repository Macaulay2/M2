-- Positive characteristic: Galois fields that are not given by their Conway
-- polynomial, as made by GF(ZZ/p[c]/(f)).  The package maps such a field
-- isomorphically onto the Conway field of the same size (a root of the
-- Conway polynomial in the field is the generator), and results that need
-- an extension live over GF(p, m) made by GF, with the input field embedded
-- by sending its generator to a root of its minimal polynomial.  The
-- descent oracle is told that root (pchGeneratorImage) and checks that it is
-- one; the decompositions are then compared with Macaulay2's over the input
-- field.
-- A part of pchar-nonconway-full.m2, which check skips: of the oracle checks on
-- single ideals, only the first of each section is kept here.
load (Binomials#"auxiliary files" | "tests/helpers/pchar-oracles.m2");
needsPackage "Cyclotomic";   -- discreteLog
debug Binomials;      -- galoisRing, to read off the root the package chose

-- register the inclusions of the coefficient field of R into GF(p^m) for the degrees m
register = (R, degs) -> scan(degs, m -> (
        (T, phi) := galoisRing(R, m);
        if T === R then return;
        K := coefficientRing R;
        F := coefficientRing T;
        pchGeneratorImage#(K, F) = lift(phi promote(K_0, R), F)));

-- 1. GF(9) given by c^2 + 1 (the Conway polynomial is c^2 - c - 1; c has order 4)
Kn = GF(ZZ/3[c]/(c^2 + 1));
Y = Kn[x,y,z];
register(Y, {4, 6, 8});
assert pchCheckAll ideal(x^2 - y^2);                                    -- no extension
I = ideal(x^5 - y^5, z^2);
L = binomialPrimaryDecomposition I;
T = ring first L;
assert(#L == 5 and (coefficientRing T).order == 81 and T === first galoisRing(Y, 4));
assert pchCheckAll I;
use Y;                                                                  -- T = ... rebound x, y, z to T
sols = binomialSolve ideal(x^4 - c^2, y - 1, z - c);                    -- x^4 = -1: four points in GF(9) itself
assert(#sols == 4 and ring first first sols === Kn and all(sols, s -> s#0^4 == -1 and s#1 == 1 and s#2 == Kn_0));
-- the same answers as for the Conway field GF(9), transported by an isomorphism
K9 = GF(3, 2, Variable => b);
S9 = K9[x,y,z];
(p, n, g) = finiteFieldData Kn;                                         -- g is a root of the Conway polynomial in Kn
iso = map(S9, Y, gens S9 | {lift((map(K9, Kn, {(K9_0)^(discreteLog Kn_0)})) Kn_0, K9)});   -- g -> b
assert(iso (Y_0 - g * Y_1) == S9_0 - b * S9_1);
-- (Macaulay2's own minimalPrimes fails with "not implemented yet" over Kn
-- on the last two ideals, so the oracle cannot be used there)
-- the lists L1 (over Y or an extension GF(3, m)[x,y,z] of it) and L2 (the
-- same for S9) agree after moving L1 by iso, or by the identity on the
-- common Conway field
sameAfterIso = (L1, L2) -> (
    T1 := ring first L1; T2 := ring first L2;
    assert(#L1 == #L2 and (coefficientRing T1).order == (coefficientRing T2).order);
    move := if T1 === Y then iso else map(T2, T1, gens T2 | {(coefficientRing T2)_0});
    pchSameSets(apply(L1, J -> move J), L2));
scan({ideal(Y_0^4 - Kn_0*Y_1^4, Y_2^3 - Kn_0*Y_2^2), ideal(Y_0*Y_1 - Kn_0*Y_2^2, Y_0^2 - (Kn_0 + 1)*Y_1^2), ideal(Y_0^8 - Y_1^8, Y_1^4 - Kn_0*Y_2^4)}, I -> (
        assert sameAfterIso(binomialPrimaryDecomposition I, binomialPrimaryDecomposition iso I);
        assert sameAfterIso(binomialMinimalPrimes I, binomialMinimalPrimes iso I);
        assert sameAfterIso(binomialAssociatedPrimes I, binomialAssociatedPrimes iso I);
        assert(iso binomialRadical I == binomialRadical iso I)));
-- 2. GF(16) given by c^4 + c^3 + 1 (the Conway polynomial is c^4 + c + 1)
K16 = GF(ZZ/2[c]/(c^4 + c^3 + 1));
Z = K16[x,y,z];
register(Z, {8, 12, 20});
assert pchCheckAll ideal(x^3 - c*y^3);                                  -- GF(2^12)
-- 3. GF(25) given by c^2 - 2 (c has order 8)
K25 = GF(ZZ/5[c]/(c^2 - 2));
W = K25[x,y];
register(W, {4, 6});
assert pchCheckAll ideal(x^2 - c*y^2);                                  -- GF(5^4)
L = binomialPrimaryDecomposition ideal(x^10 - 2*y^10);
assert(ring first L === W and pchSameSets(L, {ideal(x^5 - c*y^5), ideal(x^5 + c*y^5)}));
-- 4. GF(27) given by c^3 - c + 1
K27 = GF(ZZ/3[c]/(c^3 - c + 1));
V = K27[x,y];
register(V, {6, 12});
assert pchCheckAll ideal(x^2 - c*y^2);
