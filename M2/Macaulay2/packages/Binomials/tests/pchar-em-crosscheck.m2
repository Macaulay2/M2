-- Cross-check of the primary decomposition formula of Eser and Matusevich
-- (Theorem 2.11 of [EM16]), which the package uses in positive
-- characteristic, in characteristic zero: there Sat'_0 = Sat and the
-- formula must give a primary decomposition with the same associated
-- primes as the formula of the characteristic zero code path.  Checked with
-- Macaulay2's algorithms through the oracles of helpers/oracles.m2.
debug Binomials;
load (Binomials#"auxiliary files" | "tests/helpers/oracles.m2");

crossCheck = I -> (
    cellIdx := cellIndices I;
    R := ring I;
    L := cellularPrimaryDecompositionEM(I, cellIdx, false);
    assert orcCheckPrimaryDecomposition(I, L);
    L0 := cellularBinomialPrimaryDecomposition(I, CellVariables => R_* _ cellIdx);
    S := orcCommonRing L;
    assert(S === orcCommonRing L0);
    assert(#L == #L0);
    -- the same primes in the same order
    assert(apply(L, Q -> radical orcDescend Q) == apply(L0, Q -> radical orcDescend Q));
    true);

R = QQ[x,y,z,w];
-- lattice ideals: components are primes
assert crossCheck ideal(x^2 - y^2);
assert crossCheck ideal(x^3 - y^3, z^2 - w^2);
-- cellular ideals with embedded primes, including Example 5.4 of [EM16]
-- (there the formula is redundant)
assert crossCheck ideal(y*(x^2 - 1), z*(x^3 - 1), y^2, z^2, y*z);
assert crossCheck ideal(x^2 - 1, y*(x - 1), y^2);
assert crossCheck ideal(x^2 - y^2, z*(x - y), z^2);
assert crossCheck ideal(x^4 - y^4, z*(x^2 - y^2), z^2);
assert crossCheck ideal(x^6 - y^6, z*(x^2 - y^2), w*(x^3 - y^3), z^2, w^2, z*w);
assert crossCheck ideal(z^2, z*(x^3 - 1), w^3, w*(y^2 - 1), x*y - 1);
-- Example 5.2 of [EM16] (O'Neill): two embedded lattices
S = QQ[x1,x2,x3,x4,x5,x6];
assert crossCheck ideal(x5*(x1 - x2), x6*(x3 - x4), x5^2, x6^2, x5*x6);
-- Example 5.3 of [EM16]
T = QQ[x1,x2,x3];
assert crossCheck ideal(x3^2*(x1^2 - x2^2), x3*(x1^4 - x2^4), x3^3);
-- from the package tests
U = QQ[x,y,z];
assert crossCheck ideal(x^2, y^2, x*y, x*(z^3 - 1), y*(z^2 - 1));
scan(binomialCellularDecomposition(ideal(x^2*y - z^2, x^2 - z^3, y^4 - 1), ReturnCellVars => true), p -> assert crossCheck p#0);   -- not cellular itself
-- a cellular component of a birth-death ideal
V = QQ[R00,U00,R01,D01,U01,R02,D02,L10,U10,L11,D11,U11,L12,D12];
I = ideal(U00*R01-R00*U10, R01*D11-D01*R00, D11*L10-L11*D01, L10*U00-U10*L11, U01*R02-R01*U11, R02*D12-D02*R01, D12*L11-L12*D02, L11*U01-U11*L12);
scan(binomialCellularDecomposition(I, ReturnCellVars => true), p -> assert crossCheck p#0);
