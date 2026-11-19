-- doc-returnpchars-type.m2
-- cellularBinomialIsPrimary(I, ReturnPChars=>true) returns partial
-- characters as objects of type PartialCharacter.
-- Documented (ReturnPChars): "The answer is a list of objects of type
-- PartialCharacter".
-- Cases: R = QQ[x], ideal(x^2-1); R = QQ[x,y,z], ideal(x^2,x*(y-1)).
-- Expected: two objects of class PartialCharacter (usable with idealFromCharacter).

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

R = QQ[x];
pcs = cellularBinomialIsPrimary(ideal(x^2-1), ReturnPChars => true);
assert(#pcs == 2);
assert(all(pcs, p -> instance(p, PartialCharacter)));
R = QQ[x,y,z];
pcs = cellularBinomialIsPrimary(ideal(x^2, x*(y-1)), ReturnPChars => true);
assert(#pcs == 2);
assert(all(pcs, p -> instance(p, PartialCharacter)));
