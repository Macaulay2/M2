-- Families of binomial ideals from the literature, checked against
-- Macaulay2's general algorithms.
load (Binomials#"auxiliary files" | "tests/helpers/oracles.m2");
adjacentMinors = (m, n) -> (
    R := QQ(monoid [Variables => m * n]);
    X := genericMatrix(R, n, m);
    ideal flatten for i from 0 to m-2 list for j from 0 to n-2 list
        X_(j,i)*X_(j+1,i+1) - X_(j+1,i)*X_(j,i+1));
-- Adjacent 2-minors of a 3x4 matrix.  The cellular components have 11
-- associated primes together, but only 9 of them are associated to the ideal.
-- Version 1.2.1 returned all 11.
I = adjacentMinors(3, 4);
ap = binomialAssociatedPrimes I;
assert(#ap == 9);
assert orcCheckAssociatedPrimes(I, ap);
assert orcCheckMinimalPrimes(I, binomialMinimalPrimes I);
bpd = binomialPrimaryDecomposition I;
assert(intersect bpd == I and all(bpd, isPrimary));
-- 3x3: four associated primes, three of them minimal
I = adjacentMinors(3, 3);
assert(#binomialAssociatedPrimes I == 4 and #binomialMinimalPrimes I == 3);
assert orcCheckPrimaryDecomposition(I, binomialPrimaryDecomposition I);
-- 2xn: the ideal is radical with Fibonacci many minimal primes
I = adjacentMinors(2, 6);
assert(binomialRadical I == I);
assert(#binomialMinimalPrimes I == 8);
assert orcCheckAssociatedPrimes(I, binomialAssociatedPrimes I);
-- the binomial edge ideal of the 5-cycle
R = QQ[x_1..x_5, y_1..y_5];
I = ideal apply(5, i -> x_(i+1) * y_((i+1) % 5 + 1) - x_((i+1) % 5 + 1) * y_(i+1));
assert orcCheckMinimalPrimes(I, binomialMinimalPrimes I);
assert orcCheckPrimaryDecomposition(I, binomialPrimaryDecomposition I);
-- 2x2 permanents of a 3x3 matrix: coefficients +1, so the ideal is not unital
R = QQ[a..i];
X = genericMatrix(R, 3, 3);
I = ideal flatten flatten flatten for r1 from 0 to 1 list for r2 from r1+1 to 2 list for c1 from 0 to 1 list for c2 from c1+1 to 2 list
    X_(r1,c1)*X_(r2,c2) + X_(r1,c2)*X_(r2,c1);
assert(not isUnital I);
assert orcCheckMinimalPrimes(I, binomialMinimalPrimes I);
bpd = binomialPrimaryDecomposition I;
assert(intersect bpd == I and all(bpd, isPrimary));
