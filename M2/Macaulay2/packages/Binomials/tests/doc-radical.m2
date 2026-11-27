-- doc-radical.m2
-- binomialRadical and cellularBinomialRadical (with and without
-- CellVariables).  Oracle: Macaulay2's radical.  The comparison is done in
-- the ring of the answer, using that in characteristic 0 radicals commute
-- with field extension; that the answer lies in the ring of I is tested in
-- doc-radical-ring.m2.
-- Non-unital inputs are tested here only for cellular ideals.

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

checkRad = I -> (
    r := binomialRadical I;
    assert(instance(r, Ideal));
    assert(r == sub(radical I, ring r));
    if isCellularOracle I then (
        assert(ring r === ring I);
        cv := isCellular(I, ReturnCellVars => true);
        r1 := cellularBinomialRadical I;
        r2 := cellularBinomialRadical(I, CellVariables => cv);
        assert(r1 == radical I);
        assert(r2 == radical I)));

R = QQ[x,y,z];
scan({ideal(y^2, x*y-y, x^2-1), ideal(y^3, y^2*z^2-x^3, x*y^2*z, x^3*z-x*y), ideal(x*y-z, x*z-y^2),
      ideal(x^2, x*y), ideal(x^3, y^2, z^4-x*y), ideal(x^2-y^2, z^3), ideal(x^4-y^4, z^2*(x-y), z^3),
      ideal(x*(y-1)), ideal(x^2*(y-1)), ideal(x^2-y, y^2-x), ideal(x^6-1, y^3), ideal(0_R), ideal(x,y,z),
      ideal(x^2, y^2, z^2), ideal(x-y+z, z), ideal(x^2-y^2, x^2+y^2-2*z^2), ideal(x^2*(y^2-1), z^2*(x-1)),
      ideal(x-z^2, y^4), ideal(x^3-y^2), ideal(x^2*y-z^2, x*z^3)}, checkRad);
-- cellular non-unital
scan({ideal(x^2-2*y^2, z^3), ideal(2*x-3*y), ideal((2*x-3*y)^1, z^2), ideal(x^2-2*y^2)}, checkRad);
-- unit ideal
r = binomialRadical ideal(1_R);
assert(r == 1);
-- documented example
R = QQ[x,y];
r = binomialRadical ideal(y^2, x*y-y, x^2-1);
assert(r == sub(ideal(y, x^2-1), ring r));
-- radical of a non-reduced cellular ideal with roots of unity: (x^3-1)^... style
R = QQ[x,y,z];
I = ideal(z^3, x^3*z - z, y^2 - x^2);
assert(isCellularOracle I);
assert(cellularBinomialRadical I == radical I);
