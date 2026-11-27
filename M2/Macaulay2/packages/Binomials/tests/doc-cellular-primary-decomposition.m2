-- doc-cellular-primary-decomposition.m2
-- cellularBinomialPrimaryDecomposition with and without CellVariables and
-- with Verbose; documented: primary binomial ideals whose intersection is I,
-- one for each associated prime of I, for cellular I; generators not
-- minimal (Caveat).
-- Oracles (over QQ[x,ww]/(Phi)): intersection equals I, each component
-- primary and binomial, every associated prime of I is the radical of a
-- component, all components share the same nilpotent variables as I (they
-- contain the monomial part).  Embedded components that need roots of
-- unity: doc-cellular-primary-decomposition-roots-of-unity.m2.

-- ---------------------------------------------------------------------------
-- Oracle helpers (independent of the package; only Macaulay2 core is used).
-- Results may live in K[x] with K = QQ[ww]/(Phi) a cyclotomic field.  Since
-- K[x] = QQ[x,ww]/(Phi), ideals of K[x] correspond to ideals of QQ[x,ww]
-- containing Phi, and primality, primariness, associated primes, minimal
-- primes, intersections and containments are preserved under this
-- correspondence.  liftData S returns (phi, Phi) with phi: S -> QQ[x,ww]
-- sending a normal form to a representative; liftIdeal adds Phi back.
liftData = S -> (
    g := generators(S, CoefficientRing => QQ);
    T := QQ[Variables => #g];
    phi := map(T, S, gens T);
    rel := ideal(0_T);
    R0 := S;
    while R0 =!= QQ do (
        if instance(R0, QuotientRing) then (
            A0 := ambient R0;
            gA := generators(A0, CoefficientRing => QQ);
            rel = rel + (map(T, A0, take(gens T, -#gA))) ideal R0;
            R0 = A0)
        else R0 = coefficientRing R0);
    (phi, rel))
liftIdeal = (J, D) -> (D#0) J + D#1
-- equality of two lists of ideals as sets
setEq = (A, B) -> all(A, a -> any(B, b -> a == b)) and all(B, b -> any(A, a -> a == b))
-- cellular variables / nilpotent variables by definition
nzdVars = I -> select(gens ring I, v -> saturate(I, v) == I)
nilVars = I -> (r := radical I; select(gens ring I, v -> isSubset(ideal v, r)))
isCellularOracle = I -> (#nzdVars I + #nilVars I == numgens ring I)
-- ---------------------------------------------------------------------------

checkCPD = I -> (
    assert(isCellularOracle I);
    cv := isCellular(I, ReturnCellVars => true);
    nil := nilVars I;
    for P in {cellularBinomialPrimaryDecomposition I,
              cellularBinomialPrimaryDecomposition(I, CellVariables => cv),
              cellularBinomialPrimaryDecomposition(I, CellVariables => cv, Verbose => false)} do (
        S := ring first P; D := liftData S;
        lP := apply(P, p -> liftIdeal(p, D));
        lI := liftIdeal(sub(I, S), D);
        assert(intersect lP == lI);
        assert(all(lP, isPrimary));
        assert(all(P, isBinomial));
        rads := apply(lP, radical);
        assert(all(associatedPrimes lI, Q -> any(rads, r -> r == Q)));
        assert(all(rads, r -> all(nil, v -> (D#0)(sub(v, S)) % r == 0)))));
R = QQ[x,y,z];
scan({ideal(x^3-1, y-x), ideal(x^2-1, y-x), ideal(x^4-y^4), ideal(x^2-y^2, z^3), ideal(x^3-y^3, z^2),
      ideal(y^2, y*(x-1)), ideal(y^2, y*(x^2-1)), ideal(z^2, y^2, y*z*(x^2-1)), ideal(x-z^2, y^4),
      ideal(x*y-z^2, x^2-y*z, y^2-x*z), ideal(x^6-1, y-x^2), ideal(x^2*y-z^3), ideal(0_R), ideal(x,y,z),
      ideal(x^2-y^2, x*z-y*z, z^2), ideal(y^3, y^2*z^2-x^3, x*y^2*z, x^3*z-x*y)}, checkCPD);
-- documented example: three linear components over QQ(zeta_3)
P = cellularBinomialPrimaryDecomposition(ideal(x^3-1, y-x), CellVariables => {x,y,z});
assert(#P == 3);
