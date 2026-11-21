-- doc-FINDING-unit-ideal-cellular-crash.m2
-- FINDING: isCellular says the unit ideal is cellular (isCellular ideal(1_R)
-- -> true), but cellularBinomialPrimaryDecomposition and
-- cellularBinomialUnmixedDecomposition crash on it:
--   cellularBinomialPrimaryDecomposition ideal(1_R)
--     -> error: array index 0 out of bounds 0 .. -1
--   cellularBinomialUnmixedDecomposition ideal(1_R) -> error
--   cellularBinomialPrimaryDecomposition ideal(x-1,x^2) -> error
-- (binomialPrimaryDecomposition, binomialCellularDecomposition,
-- binomialUnmixedDecomposition and binomialMinimalPrimes return {} on the
-- same inputs.)  Expected: {} (or, if the unit ideal is not meant to be
-- cellular, isCellular should return false; the documentation is silent).

R = QQ[x,y,z];
for U in {ideal(1_R), ideal(x-1, x^2)} do (
    assert(isCellular U === true);
    assert((try cellularBinomialPrimaryDecomposition U else "error") === {});
    assert((try cellularBinomialUnmixedDecomposition U else "error") === {}));
