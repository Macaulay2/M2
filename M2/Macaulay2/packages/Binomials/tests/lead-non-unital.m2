-- Binomial ideals with coefficients other than 1.  Version 1.2.1 stopped with
-- an error as soon as a character had to be saturated.
load (Binomials#"auxiliary files" | "tests/helpers/oracles.m2");
R = QQ[x,y];
-- roots of unity as values: x^2 + 1 = (x - i)(x + i)
L = binomialPrimaryDecomposition ideal(x^2 + 1);
assert(#L == 2 and orcCheckPrimaryDecomposition(ideal(x^2 + 1), L));
assert orcCheckMinimalPrimes(ideal(x^2 + y^2), binomialMinimalPrimes ideal(x^2 + y^2));
assert orcCheckAssociatedPrimes(ideal(x^3 + y^3, x*y^2), binomialAssociatedPrimes ideal(x^3 + y^3, x*y^2));
-- prime over QQ but not over the algebraic closure
assert(isPrime ideal(x^2 + 1) and not binomialIsPrime ideal(x^2 + 1));
assert(not binomialIsPrimary ideal(x^2 + 1));
-- rational roots: x^2 - 4, x^2 - 4*y^2, x^3 - 8, 9*x^2 - 4*y^2, x^3 + 27*y^3
scan({ideal(x^2 - 4), ideal(x^2 - 4*y^2), ideal(x^3 - 8), ideal(9*x^2 - 4*y^2), ideal(x^3 + 27*y^3), ideal(x^2 - 4, y^2 - 9), ideal(x^2*y - 4*y, y^2)},
    I -> (
        assert orcCheckPrimaryDecomposition(I, binomialPrimaryDecomposition I);
        assert orcCheckMinimalPrimes(I, binomialMinimalPrimes I);
        assert orcCheckAssociatedPrimes(I, binomialAssociatedPrimes I);
        assert(binomialRadical I == radical I)));
-- saturated lattices never needed roots; any coefficients work
scan({ideal(x - 2*y), ideal(2*x - 3*y, y^2), ideal(x*y - 5), ideal(x^2*y - 3*x)},
    I -> assert orcCheckPrimaryDecomposition(I, binomialPrimaryDecomposition I));
assert(binomialIsPrime ideal(x - 2*y) and binomialIsPrimary ideal(2*x - 3*y, y^2));
-- answers that need no root are given without one
assert(binomialIsPrimary ideal(x^2 - 2) === false);
assert(binomialIsPrime ideal(x^2 - 2*y^2) === false);
assert(binomialRadical ideal(x^2 - 2*y^2, x*y^3) == radical ideal(x^2 - 2*y^2, x*y^3));
-- a square root of 2 is not found (it lies in the cyclotomic field of order 8,
-- but the package does not look for it): an error, not a wrong answer
assert(try (binomialPrimaryDecomposition ideal(x^2 - 2*y^2); false) else true);
assert(try (binomialMinimalPrimes ideal(x^2 - 2); false) else true);
