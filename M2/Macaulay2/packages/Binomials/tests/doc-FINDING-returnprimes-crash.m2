-- doc-FINDING-returnprimes-crash.m2
-- FINDING: cellularBinomialIsPrimary(I, ReturnPrimes=>true) crashes on
-- non-primary cellular ideals with nilpotent variables and embedded primes.
-- Reproduction: R = QQ[x,y,z]; I = ideal(x^2, x*(y-1)) (cellular, cell
-- variables {y,z}; Ass = {(x), (x,y-1)}).
-- Observed: error: no method found for applying idealFromCharacter to:
--     argument 1 :  QQ[y..z] (of class PolynomialRing)
--     argument 2 :  {PartialCharacter{...}} (of class List)
-- Also crashes for ideal(x^2, y^2, x*y*(z^2-1)) and ideal(y^2, y*(x^3-1)).
-- (Without ReturnPrimes the answer false is correct.)
-- Expected: two distinct associated primes, e.g. (x) and (x,y-1).

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
for I in {ideal(x^2, x*(y-1)), ideal(x^2, y^2, x*y*(z^2-1)), ideal(y^2, y*(x^3-1))} do (
    assert(isCellularOracle I and not isPrimary I);
    assert(cellularBinomialIsPrimary I === false);
    pr := try cellularBinomialIsPrimary(I, ReturnPrimes => true) else "error";
    assert(instance(pr, List) and #pr == 2);
    S := ring first pr; D := liftData S;
    assP := associatedPrimes liftIdeal(sub(I, S), D);
    assert(all(pr, P -> any(assP, Q -> Q == liftIdeal(P, D)))));
