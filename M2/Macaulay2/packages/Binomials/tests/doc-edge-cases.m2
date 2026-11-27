-- doc-edge-cases.m2
-- Edge cases the documentation implies should work, each run through all
-- decomposition / prime / radical / predicate functions and compared with
-- core oracles (checkEverything): the zero ideal, monomial ideals, principal
-- ideals, ideals in one variable, prime / primary / cellular inputs, ideals
-- with embedded components, binomial ideals given by non-binomial or
-- redundant generators, rings with many variables of which few are used.
-- The unit ideal ideal(1_R) is checked separately for the outputs that are
-- unambiguous (empty decompositions).  Other unit-ideal behaviour is in
-- doc-unit-ideal-*.m2.

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

-- checkEverything I: all decomposition / prime / radical functions on a unital
-- binomial ideal I (not the unit ideal), each against a core oracle.
checkEverything = I -> (
    R := ring I;
    -- cellular decomposition
    C := binomialCellularDecomposition I;
    assert(intersect C == I);
    assert(all(C, isCellularOracle));
    CV := binomialCellularDecomposition(I, ReturnCellVars => true);
    assert(all(CV, p -> set p#1 === set nzdVars p#0));
    -- unmixed decomposition
    U := binomialUnmixedDecomposition I;
    assert(intersect U == I);
    -- radical
    r := binomialRadical I;
    assert(r == sub(radical I, ring r));
    -- primary decomposition
    P := binomialPrimaryDecomposition I;
    S := ring first P; D := liftData S;
    lI := liftIdeal(sub(I, S), D);
    lP := apply(P, p -> liftIdeal(p, D));
    assert(intersect lP == lI);
    assert(all(lP, isPrimary));
    ass := associatedPrimes lI;
    assert(all(ass, Q -> any(lP, p -> radical p == Q)));
    -- minimal and associated primes
    M := binomialMinimalPrimes I;
    DM := liftData ring first M;
    assert(setEq(apply(M, m -> liftIdeal(m, DM)), minimalPrimes liftIdeal(sub(I, ring first M), DM)));
    A := binomialAssociatedPrimes I;
    DA := liftData ring first A;
    assert(setEq(apply(A, a -> liftIdeal(a, DA)), associatedPrimes liftIdeal(sub(I, ring first A), DA)));
    -- predicates
    assert(binomialIsPrime I === isPrime I);
    assert(binomialIsPrimary I === isPrimary I);
    assert(isCellular I === isCellularOracle I);
    if isCellularOracle I then (
        assert(cellularBinomialIsPrimary I === isPrimary I);
        assert(cellularBinomialRadical I == radical I);
        B := cellularBinomialAssociatedPrimes I;
        DB := liftData ring first B;
        assert(setEq(apply(B, b -> liftIdeal(b, DB)), associatedPrimes liftIdeal(sub(I, ring first B), DB)));
        Q := cellularBinomialPrimaryDecomposition I;
        DQ := liftData ring first Q;
        lQ := apply(Q, q -> liftIdeal(q, DQ));
        assert(intersect lQ == liftIdeal(sub(I, ring first Q), DQ));
        assert(all(lQ, isPrimary));
        assert(intersect cellularBinomialUnmixedDecomposition I == I));
    );

R = QQ[x,y,z];
scan({ideal(0_R), ideal(x), ideal(x,y,z), ideal(x^2, y^3), ideal(x^2, x*y), ideal(x*y*z), ideal(x^2*y, y^2*z, z^2*x),
      ideal(x^3-y^2), ideal(x*y-1), ideal(x^2-y^2), ideal(x^2*y^3 - z^5), ideal(x-y, z^3),
      ideal(x^2-y^2, x^2+y^2-2*z^2), ideal(x-y+z^2, z^2), ideal(x*y-z, x*z-y^2, x*y-z+x*z-y^2),
      ideal(x*y-1, x^2-y, x^3-1, x*y*z-z), ideal(x^2, x*(y-1)), ideal(x^2*(y-1), x^3)}, checkEverything);
-- unit ideal: empty decompositions
R = QQ[x,y,z];
U = ideal(1_R);
assert(binomialPrimaryDecomposition U == {});
assert(binomialCellularDecomposition U == {});
assert(binomialUnmixedDecomposition U == {});
assert(binomialMinimalPrimes U == {});
assert(binomialRadical U == U);
assert(binomialIsPrimary U === false);
assert(binomialSolve U == {});
-- zero ideal: shapes
Z = ideal(0_R);
assert(#binomialPrimaryDecomposition Z == 1 and first binomialPrimaryDecomposition Z == 0);
assert(set isCellular(Z, ReturnCellVars => true) === set gens R);
assert(binomialIsPrime Z === true);
-- the unit ideal given by other generators
for U in {ideal(x-1, x^2), ideal(y^2, 1-y^3), ideal(x^3-1, -x*y^2+x^2, x^3*y^3)} do (
    assert(U == 1);
    assert(binomialCellularDecomposition U == {});
    assert(binomialPrimaryDecomposition U == {});
    assert(binomialUnmixedDecomposition U == {});
    assert(binomialMinimalPrimes U == {});
    assert(binomialRadical U == 1);
    assert(binomialIsPrimary U === false);
    assert(binomialSolve U == {}));
