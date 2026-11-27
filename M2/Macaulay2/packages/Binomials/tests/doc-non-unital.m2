-- doc-non-unital.m2
-- Non-unital binomial ideals (coefficients other than +-1, e.g. x^2-2*y^2,
-- 2*x-3*y).  The documentation lists "a binomial ideal" as input for the
-- cellular decomposition, unmixed decomposition, radical, isCellular and the
-- cellular functions, and restricts binomialSolve to unital ideals.
-- Checked: isBinomial/isUnital, isCellular + cell variables, BCD, BUD,
-- cellularBinomialRadical and binomialRadical on cellular input,
-- cellularBinomialUnmixedDecomposition, partialCharacter/idealFromCharacter,
-- against core oracles.  (binomialMinimalPrimes, binomialAssociatedPrimes,
-- binomialIsPrimary and binomialPrimaryDecomposition on such input:
-- non-unital.m2.)

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
nonUnital = {ideal(x^2-2*y^2), ideal(2*x-3*y), ideal(x*z-3*y*z), ideal(x^2-2*y^2, x*z), ideal(x*(y^2-2*z^2)),
             ideal(x^2-2*y^2, z^3), ideal(2*x-3*y, z^2), ideal(x^3-5*y*z, z^2), ideal(x*y-7, z), ideal(x^2*y-2*z^3),
             ideal(x^2+y^2), ideal(x^2+1, y-2)};
for I in nonUnital do (
    assert(isBinomial I === true);
    assert(isUnital I === false);
    assert(isCellular I === isCellularOracle I);
    C := binomialCellularDecomposition I;
    assert(intersect C == I);
    assert(all(C, isCellularOracle));
    CV := binomialCellularDecomposition(I, ReturnCellVars => true);
    assert(all(CV, p -> set p#1 === set nzdVars p#0));
    U := binomialUnmixedDecomposition I;
    assert(intersect U == I);
    if isCellularOracle I then (
        cv := isCellular(I, ReturnCellVars => true);
        assert(set cv === set nzdVars I);
        assert(cellularBinomialRadical I == radical I);
        assert(cellularBinomialRadical(I, CellVariables => cv) == radical I);
        r := binomialRadical I;
        assert(r == sub(radical I, ring r));
        assert(intersect cellularBinomialUnmixedDecomposition(I, CellVariables => cv) == I);
        pc := partialCharacter(I, CellVariables => cv);
        assert(set pc#"J" === set cv);
        -- cell variables are x,y,(z) here, i.e. an initial segment of the variables
        if pc#"J" == take(gens R, #(pc#"J")) then (
            nc := toList(set gens R - set cv);
            E := if #nc == 0 then I else eliminate(nc, I);
            assert(idealFromCharacter(R, pc) == E))));
-- explicit character values
pc = partialCharacter ideal(2*x-3*y);
assert(idealFromCharacter(R, pc) == ideal(2*x-3*y));
pc = partialCharacter ideal(x^2*y-2*z^3);
assert(idealFromCharacter(R, pc) == ideal(x^2*y-2*z^3));
