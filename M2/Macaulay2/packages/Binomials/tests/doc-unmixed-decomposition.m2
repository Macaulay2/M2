-- doc-unmixed-decomposition.m2
-- binomialUnmixedDecomposition, its synonym BUD, Verbose, and
-- cellularBinomialUnmixedDecomposition (with and without CellVariables).
-- Documented: a list of unmixed binomial ideals whose intersection is I.
-- Checked here: intersection equals I; every component is binomial; every
-- component is equidimensional (all minimal primes have the same dimension),
-- which is a necessary condition for unmixedness.  The full unmixedness
-- (no embedded primes) is tested in doc-unmixed-components.m2.

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

equidim = J -> #unique apply(minimalPrimes J, dim) <= 1;
checkBUD = I -> (
    U := binomialUnmixedDecomposition I;
    assert(instance(U, List) and all(U, u -> instance(u, Ideal) and ring u === ring I));
    if I == 1 then (assert(U == {}); return);
    assert(intersect U == I);
    assert(all(U, isBinomial));
    assert(all(U, equidim));
    assert(setEq(BUD I, U));
    assert(setEq(binomialUnmixedDecomposition(I, Verbose => false), U));
    );
checkCellUD = I -> (
    assert(isCellularOracle I);
    cv := isCellular(I, ReturnCellVars => true);
    for U in {cellularBinomialUnmixedDecomposition I, cellularBinomialUnmixedDecomposition(I, CellVariables => cv),
              cellularBinomialUnmixedDecomposition(I, CellVariables => cv, Verbose => false)} do (
        assert(intersect U == I);
        assert(all(U, isBinomial));
        assert(all(U, equidim))));

R = QQ[x,y,z];
scan({ideal(x^2, x*y, y^2, x*(z^3-1), y*(z^2-1)), ideal(x*y-z, x*z-y^2), ideal(x^2, x*y),
      ideal(x*(y-1)), ideal(x*(y^3-1), x^2), ideal(x^3-y^2), ideal(0_R), ideal(1_R), ideal(x,y,z),
      ideal(x^2-2*y^2, x*z), ideal(2*x-3*y), ideal(y^2, y*(x^3-1)), ideal(x^2*y - z^3, x*z - y^2),
      ideal(y^3, y^2*z^2-x^3, x*y^2*z, x^3*z-x*y), ideal(x*y, y*z, x*z)}, checkBUD);

scan({ideal(x*(y^3-1), x^2), ideal(x^2, x*y, y^2, x*(z^3-1), y*(z^2-1)), ideal(x^3-y^2),
      ideal(y^2, y*(x-1)), ideal(x^2-2*y^2, z^2), ideal(y^3, y^2*z^2-x^3, x*y^2*z, x^3*z-x*y),
      ideal(0_R), ideal(x,y,z)}, checkCellUD);

-- an unmixed (here prime) input stays one component
U = BUD ideal(x*y-z^2);
assert(#U == 1 and first U == ideal(x*y-z^2));
-- two components of different dimension must be separated
U = BUD ideal(x*z, y*z);   -- (z) cap (x,y)
assert(#U >= 2);
assert(intersect U == ideal(x*z, y*z));
