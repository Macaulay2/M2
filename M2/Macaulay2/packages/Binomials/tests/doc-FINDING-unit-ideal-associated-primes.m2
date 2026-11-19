-- doc-FINDING-unit-ideal-associated-primes.m2
-- FINDING: the associated primes of the unit ideal are reported as {ideal 1}.
-- R/(1) = 0 has no associated primes (Macaulay2: associatedPrimes ideal 1_R
-- == {}), and ideal 1 is not a prime.
-- Observed: binomialAssociatedPrimes ideal(1_R) -> {ideal 1};
--           cellularBinomialAssociatedPrimes ideal(1_R) -> {ideal 1};
--           binomialAssociatedPrimes ideal(x-1,x^2) -> {ideal(1,z,y,x)}.
-- Expected: {}.

R = QQ[x,y,z];
assert(associatedPrimes ideal(1_R) == {});
assert(binomialAssociatedPrimes ideal(1_R) == {});
assert(cellularBinomialAssociatedPrimes ideal(1_R) == {});
assert(binomialAssociatedPrimes ideal(x-1, x^2) == {});
