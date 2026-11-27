-- doc-associated-primes-roots-of-unity.m2
-- binomialAssociatedPrimes finds the embedded primes that need roots of
-- unity.  Documented: the associated primes of I over the algebraic closure
-- of the coefficient field.
-- R = QQ[x,y]; I = ideal(y-x^3*y, y^2-x*y^2):
--   associatedPrimes I (core, over QQ) = {ideal y, ideal(x-1), ideal(y,x^2+x+1)}
--   i.e. over QQ(zeta_3): (y), (x-1), (y,x-zeta), (y,x-zeta^2).
-- QQ[x,y,z]: the associated primes of I = ideal(x^2*y*z^2-x*y^3,
--   x^3*z^2-x*y*z^2, -x^3*y*z+y*z) include (y,z,x-1), (y,z,x-zeta),
--   (y,z,x-zeta^2).

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

assOK = I -> (
    A := binomialAssociatedPrimes I;
    S := ring first A; D := liftData S;
    setEq(apply(A, a -> liftIdeal(a, D)), associatedPrimes liftIdeal(sub(I, S), D)));
R = QQ[x,y];
assert(assOK ideal(y-x^3*y, y^2-x*y^2));
R = QQ[x,y,z];
assert(assOK ideal(x^2*y*z^2-x*y^3, x^3*z^2-x*y*z^2, -x^3*y*z+y*z));
