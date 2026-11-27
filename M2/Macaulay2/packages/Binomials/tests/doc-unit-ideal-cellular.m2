-- doc-unit-ideal-cellular.m2
-- The unit ideal is cellular (documented: "The unit ideal counts as
-- cellular, with no cell variables"), and cellularBinomialPrimaryDecomposition
-- and cellularBinomialUnmixedDecomposition return {} on it, as
-- binomialPrimaryDecomposition, binomialCellularDecomposition,
-- binomialUnmixedDecomposition and binomialMinimalPrimes do.
-- Inputs: ideal(1_R) and ideal(x-1,x^2).

R = QQ[x,y,z];
for U in {ideal(1_R), ideal(x-1, x^2)} do (
    assert(isCellular U === true);
    assert((try cellularBinomialPrimaryDecomposition U else "error") === {});
    assert((try cellularBinomialUnmixedDecomposition U else "error") === {}));
