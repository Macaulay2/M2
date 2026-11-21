-- The exported interface of Binomials, pinned from version 1.2.1.
-- Names, option names and option defaults must not change, and nothing may
-- be removed.  Additions need the consent of Thomas and Samuel; when one is
-- agreed, extend the lists here.
pinnedExports = {
    "BCD", "BPD", "BUD", "CellVariables", "PartialCharacter", "ReturnCellVars",
    "ReturnPChars", "ReturnPrimes", "binomialAssociatedPrimes",
    "binomialCellularDecomposition", "binomialIsPrimary", "binomialIsPrime",
    "binomialMinimalPrimes", "binomialPrimaryDecomposition", "binomialRadical",
    "binomialSolve", "binomialUnmixedDecomposition",
    "cellularBinomialAssociatedPrimes", "cellularBinomialIsPrimary",
    "cellularBinomialPrimaryDecomposition", "cellularBinomialRadical",
    "cellularBinomialUnmixedDecomposition", "extractInclusionMinimalIdeals",
    "idealFromCharacter", "isBinomial", "isCellular", "isUnital",
    "latticeBasisIdeal", "makeBinomial", "partialCharacter", "randomBinomialIdeal",
    -- added in 2.0 with Thomas's consent (2026-11-02), from the unreleased 1.3
    "binomialBasis", "GroebnerFree"};
exported = toString \ (Binomials#"exported symbols" | Binomials#"exported mutable symbols");
-- nothing was removed
assert(isSubset(set pinnedExports, set exported));
-- nothing was added without updating this file
assert(set exported === set pinnedExports);

-- option names and defaults
pinnedOptions = {
    (binomialPrimaryDecomposition, {Verbose => false}),
    (binomialCellularDecomposition, {ReturnCellVars => false, Verbose => false}),
    (binomialUnmixedDecomposition, {Verbose => false}),
    (binomialMinimalPrimes, {Verbose => false}),
    (binomialIsPrime, {CellVariables => null}),
    (cellularBinomialIsPrimary, {CellVariables => null, ReturnPrimes => false, ReturnPChars => false}),
    (cellularBinomialAssociatedPrimes, {CellVariables => null, Verbose => false}),
    (cellularBinomialPrimaryDecomposition, {CellVariables => null, Verbose => false}),
    (cellularBinomialUnmixedDecomposition, {CellVariables => null, Verbose => false}),
    (cellularBinomialRadical, {CellVariables => null}),
    (isCellular, {ReturnCellVars => false}),
    (partialCharacter, {CellVariables => null}),
    (extractInclusionMinimalIdeals, {Verbose => false}),
    (isBinomial, {GroebnerFree => false})};
scan(pinnedOptions, p -> (
    o := options p#0;
    scan(p#1, kv -> assert(o#?(kv#0) and o#(kv#0) === kv#1))));

-- every function is callable with the documented argument types
R = QQ[x,y,z];
I = ideal(x*y-z, x*z-y^2);
assert instance(binomialPrimaryDecomposition I, List);
assert instance(binomialCellularDecomposition I, List);
assert instance(binomialUnmixedDecomposition I, List);
assert instance(binomialMinimalPrimes I, List);
assert instance(binomialAssociatedPrimes ideal(x^2-y, y^2-x), List);
assert instance(binomialRadical ideal(y^2, x*y-y, x^2-1), Ideal);
assert instance(binomialSolve ideal(x^2-1, y-x, z-1), List);
assert instance(binomialIsPrime I, Boolean);
assert instance(binomialIsPrimary I, Boolean);
assert instance(isCellular I, Boolean);
assert instance(isBinomial I, Boolean);
assert instance(isUnital I, Boolean);
assert instance(makeBinomial(R, {1,-1,0}, 1), R);
assert instance(latticeBasisIdeal(R, matrix{{1},{-1},{0}}), Ideal);
assert instance(randomBinomialIdeal(R, 2, 2, 2, true), Ideal);
assert instance(extractInclusionMinimalIdeals {ideal x, ideal x^2}, List);
J = ideal(x^3-1, y-x, z-1);
cv = isCellular(J, ReturnCellVars => true);
assert instance(cv, List);
pc = partialCharacter(J, CellVariables => cv);
assert instance(pc, PartialCharacter);
assert(set keys pc === set {"J", "L", "c"});
assert instance(idealFromCharacter(R, pc), Ideal);
assert instance(cellularBinomialRadical(J, CellVariables => cv), Ideal);
assert instance(cellularBinomialAssociatedPrimes(J, CellVariables => cv), List);
assert instance(cellularBinomialPrimaryDecomposition(J, CellVariables => cv), List);
assert instance(cellularBinomialUnmixedDecomposition(J, CellVariables => cv), List);
assert instance(cellularBinomialIsPrimary(J, CellVariables => cv), Boolean);
-- the synonyms forward options
assert(BPD I == binomialPrimaryDecomposition I);
assert(BCD I == binomialCellularDecomposition I);
assert(BUD I == binomialUnmixedDecomposition I);
assert instance(BPD (I, Verbose => false), List);
assert instance(BCD (I, ReturnCellVars => true), List);
assert instance(BUD (I, Verbose => false), List);
-- PartialCharacter is a type of hash table
assert(parent PartialCharacter === HashTable);
