-* no-check-flag *-
-- The full version of oracle-random.m2, skipped by check because it takes several
-- seconds; oracle-random.m2 runs a part of it.  Run it with
--   load (Binomials#"auxiliary files" | "tests/oracle-random-full.m2")
-- Every decomposition function on random binomial ideals, checked against
-- Macaulay2's general algorithms (associatedPrimes, minimalPrimes, radical,
-- isPrime, isPrimary) through the oracles in helpers/oracles.m2.
load (Binomials#"auxiliary files" | "tests/helpers/oracles.m2");
checkAll = I -> (
    assert orcCheckCellularDecomposition(I, binomialCellularDecomposition I);
    assert orcCheckPrimaryDecomposition(I, binomialPrimaryDecomposition I);
    assert orcCheckAssociatedPrimes(I, binomialAssociatedPrimes I);
    assert orcCheckMinimalPrimes(I, binomialMinimalPrimes I);
    assert orcCheckUnmixedDecomposition(I, binomialUnmixedDecomposition I);
    r := binomialRadical I;
    assert(ring r === ring I and r == radical I);
    assert(isCellular I === orcIsCellular I);
    if isUnital I then (
        assert(binomialIsPrime I === isPrime I);
        assert(binomialIsPrimary I === isPrimary I)));
R = QQ[x,y,z];
-- unital ideals
for seed from 1 to 12 do (
    setRandomSeed seed;
    I := orcRandomBinomialIdeal(R, 3, 3, 3, {1}, 2);
    if I != 0 and I != 1 then checkAll I);
-- coefficients 1 and -1: these need roots of unity
for seed from 1 to 8 do (
    setRandomSeed seed;
    I := orcRandomBinomialIdeal(R, 3, 3, 3, {1, -1}, 2);
    if I != 0 and I != 1 then checkAll I);
