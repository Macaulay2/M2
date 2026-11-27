-- doc-minimal-primes.m2
-- binomialMinimalPrimes (option Verbose).  Documented: the minimal primes of
-- I over the algebraic closure of the coefficient field; binomial minimal
-- primes may need a (cyclotomic) extension field.
-- Oracle: with K the coefficient field of the output ring, the output lifted to
-- QQ[x,ww] (+ cyclotomic polynomial) must equal, as a set, minimalPrimes of the
-- lifted input.  Also: every output ideal is binomial, and the output has no
-- repetitions.

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

checkMin = I -> (
    M := binomialMinimalPrimes I;
    assert(instance(M, List) and all(M, m -> instance(m, Ideal)));
    if I == 1 then (assert(M == {}); return);
    S := ring first M;
    assert(all(M, m -> ring m === S));
    D := liftData S;
    lM := apply(M, m -> liftIdeal(m, D));
    assert(setEq(lM, minimalPrimes liftIdeal(sub(I, S), D)));
    assert(#unique lM == #M);
    assert(all(M, isBinomial));
    M2 := binomialMinimalPrimes(I, Verbose => false);
    assert(#M2 == #M);
    );

R = QQ[x,y,z];
scan({ideal(y^3, y^2*z^2-x^3, x*y^2*z, x^3*z-x*y), ideal(x*y-z, x*z-y^2), ideal(x^2-y, y^2-x),
      ideal(x^2, x*y), ideal(x*(y^3-1)), ideal(x^4-y^4), ideal(x^6-y^6, z), ideal(x*y*z),
      ideal(x^2-y^2, z^3), ideal(x*y-z^2), ideal(0_R), ideal(1_R), ideal(x,y,z),
      ideal(x^2*(y-1), z^2*(x^2-1)), ideal(x^3-1, y^3-1), ideal(x-y+z, z),
      ideal(x^2, x*y, y^2, x*(z^3-1), y*(z^2-1)), ideal(y^2, y*(x^3-1)),
      ideal(x^12-1, y-1, z-1), ideal(x^5-y^5)}, checkMin);
-- documented example: a single minimal prime (x,y)
M = binomialMinimalPrimes ideal(y^3, y^2*z^2-x^3, x*y^2*z, x^3*z-x*y);
assert(#M == 1);
assert(sub(first M, R) == ideal(x,y));
-- counts that need roots of unity
assert(#binomialMinimalPrimes ideal(x^4-y^4) == 4);
assert(#binomialMinimalPrimes ideal(x*(y^3-1)) == 4);
assert(#binomialMinimalPrimes ideal(x^3-1, y^3-1) == 9);
