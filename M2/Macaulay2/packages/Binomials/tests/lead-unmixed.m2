-- Unmixed decompositions: the components intersect to the ideal and all
-- associated primes of a component have the same dimension.  In version
-- 1.2.1 the cellular function returned its input unchanged.
load (Binomials#"auxiliary files" | "tests/helpers/oracles.m2");
R = QQ[x,y,z];
-- the two documented examples
I = ideal(x^2, x*y, y^2, x*(z^3 - 1), y*(z^2 - 1));
ud = binomialUnmixedDecomposition I;
assert orcCheckUnmixedDecomposition(I, ud);
assert(#ud == 3 and all(ud, C -> ring C === R));          -- no field extension
J = ideal(x*(y^3 - 1), x^2);
cv = isCellular(J, ReturnCellVars => true);
cud = cellularBinomialUnmixedDecomposition(J, CellVariables => cv);
assert(#cud == 2 and orcCheckUnmixedDecomposition(J, cud));
-- an unmixed cellular ideal is returned as it is
K = ideal(x^2 - 1, y^2, z);
assert(cellularBinomialUnmixedDecomposition K === {K});
-- embedded components of two different dimensions
I = ideal(x^3, x^2*(y - 1), x*(z^2 - 1));
ud = binomialUnmixedDecomposition I;
assert orcCheckUnmixedDecomposition(I, ud);
assert(sort unique flatten apply(ud, C -> apply(associatedPrimes C, dim)) == {0, 1, 2});
-- characters that are not trivial
I = ideal(x^2, x*(y^2 + 1), x*(z - 2));
assert orcCheckUnmixedDecomposition(I, binomialUnmixedDecomposition I);
-- random ideals
for seed from 1 to 15 do (
    setRandomSeed seed;
    I := orcRandomBinomialIdeal(R, 3, 3, 3, {1}, 3);
    if I != 0 and I != 1 then assert orcCheckUnmixedDecomposition(I, binomialUnmixedDecomposition I));
