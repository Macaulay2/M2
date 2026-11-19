-- doc-FINDING-cellular-pd-not-primary.m2
-- FINDING: cellularBinomialPrimaryDecomposition returns a non-primary
-- component for a cellular ideal whose embedded primes need roots of unity.
-- Documentation: "a binomial primary decomposition of I" (I cellular).
-- Reproduction: R = QQ[x,y,z]; J = ideal(y^2, y*(x^3-1)) (cellular, cell
-- variables {x,z}).
--   Observed: {ideal(y^2,x^3*y-y), ideal(x-1,y^2), ideal(x-ww_3,y^2),
--              ideal(x+ww_3+1,y^2)} -- the first component is J itself,
--   whose associated primes are (y), (y,x-1), (y,x-ww_3), (y,x+ww_3+1).
--   Expected: the first component should be (y)-primary, e.g. ideal(y).
-- Same for ideal(z^2, y^3, y*z*(x^3-1)) (component equal to the input).

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

R = QQ[x,y,z];
for J in {ideal(y^2, y*(x^3-1)), ideal(z^2, y^3, y*z*(x^3-1))} do (
    assert(isCellularOracle J);
    P := cellularBinomialPrimaryDecomposition J;
    S := ring first P; D := liftData S;
    lP := apply(P, p -> liftIdeal(p, D));
    assert(intersect lP == liftIdeal(sub(J, S), D));
    assert(all(lP, isPrimary)));
