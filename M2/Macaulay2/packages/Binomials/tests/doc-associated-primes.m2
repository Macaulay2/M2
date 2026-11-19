-- doc-associated-primes.m2
-- binomialAssociatedPrimes and cellularBinomialAssociatedPrimes (options
-- CellVariables, Verbose).  Documented: "the list of associated primes of I".
-- Oracle: the output, lifted to QQ[x,ww] + (cyclotomic polynomial), equals as a
-- set associatedPrimes of the lifted input.  Every output ideal is binomial.
-- Inputs include embedded primes and embedded primes that need roots of unity.
-- Known counterexamples are in doc-FINDING-bpd-not-primary.m2 and
-- doc-FINDING-unit-ideal-associated-primes.m2.

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

assEq = (A, I) -> (
    S := ring first A;
    assert(all(A, a -> ring a === S));
    D := liftData S;
    setEq(apply(A, a -> liftIdeal(a, D)), associatedPrimes liftIdeal(sub(I, S), D)));
checkAss = I -> (
    A := binomialAssociatedPrimes I;
    assert(instance(A, List) and all(A, a -> instance(a, Ideal)));
    assert(assEq(A, I));
    assert(all(A, isBinomial));
    if isCellularOracle I then (
        cv := isCellular(I, ReturnCellVars => true);
        for B in {cellularBinomialAssociatedPrimes I, cellularBinomialAssociatedPrimes(I, CellVariables => cv),
                  cellularBinomialAssociatedPrimes(I, CellVariables => cv, Verbose => false)} do (
            assert(assEq(B, I));
            assert(all(B, isBinomial)))));

R = QQ[x,y,z];
scan({ideal(x^2-y, y^2-x), ideal(x^2, x*y), ideal(x*y-z, x*z-y^2), ideal(y^2, y*(x-1)),
      ideal(y^2, y*(x^2-1)), ideal(y^2, y*(x^3-1)), ideal(z^2, y^3, y*z*(x^3-1)), ideal(x*(y^3-1), x^2),
      ideal(x^2, x*y, y^2, x*(z^3-1), y*(z^2-1)), ideal(y^3, y^2*z^2-x^3, x*y^2*z, x^3*z-x*y),
      ideal(x^3-y^2), ideal(x^4-y^4), ideal(x^2-1, y-x), ideal(x^3-1, y-x), ideal(0_R), ideal(x,y,z),
      ideal(x^2, y^2, z^2), ideal(x^3, x*y, y^2), ideal(x-z^2, y^4), ideal(x^2*(y-1), x^3),
      ideal(x*y, x*z), ideal(x^2*y - z^3, x*z - y^2), ideal(x^2-y^2, z^3), ideal(x^6-1, y^2)}, checkAss);

-- documented examples
R = QQ[x,y];
assert(#binomialAssociatedPrimes ideal(x^2-y, y^2-x) == 4);
cap = cellularBinomialAssociatedPrimes(ideal(x^2-1, y-x), CellVariables => {x,y});
assert(setEq(apply(cap, P -> sub(P, R)), {ideal(x-1, y-1), ideal(x+1, y+1)}));
-- embedded primes that need zeta_3: (y), (y, x-1), (y, x-zeta), (y, x-zeta^2)
assert(#binomialAssociatedPrimes ideal(y^2, y*(x^3-1)) == 4);
assert(#cellularBinomialAssociatedPrimes ideal(y^2, y*(x^3-1)) == 4);
