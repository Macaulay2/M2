-- doc-orders-gradings.m2
-- The documentation does not restrict the monomial order or grading of the
-- ring.  All functions are run (checkEverything, core oracles) in rings with
-- Lex order, a weight vector, GRevLex refined by weights, non-standard degrees
-- and a multigrading, plus binomialSolve.

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

satisfies = (I, p) -> (K := ring first p; f := map(K, ring I, p); all(flatten entries gens I, g -> f g == 0));
nPoints = I -> (J := radical I; numcols basis((ring J)/J));
for R in {QQ[x,y,z, MonomialOrder=>Lex], QQ[x,y,z, Weights=>{1,2,3}], QQ[x,y,z, Degrees=>{1,2,3}],
          QQ[x,y,z, MonomialOrder=>{Weights=>{3,1,2}, GRevLex}], QQ[x,y,z, Degrees=>{{1,0},{0,1},{1,1}}]} do (
    x := R_0; y := R_1; z := R_2;
    checkEverything ideal(x^3*z - y*z, y^2*z^2 - z^2*x);
    checkEverything ideal(x*y-z, x*z-y^2);
    I := ideal(x^3-1, y^2-x, z-y);
    sols := binomialSolve I;
    assert(all(sols, p -> satisfies(I, p)));
    assert(#unique sols == nPoints I));
