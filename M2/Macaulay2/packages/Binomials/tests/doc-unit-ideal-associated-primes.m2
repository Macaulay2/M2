-- doc-unit-ideal-associated-primes.m2
-- The unit ideal has no associated primes: R/(1) = 0 (Macaulay2:
-- associatedPrimes ideal 1_R == {}), and ideal 1 is not a prime.
-- Checked: binomialAssociatedPrimes and cellularBinomialAssociatedPrimes of
-- ideal(1_R), and binomialAssociatedPrimes ideal(x-1,x^2).
-- Expected: {}.

R = QQ[x,y,z];
assert(associatedPrimes ideal(1_R) == {});
assert(binomialAssociatedPrimes ideal(1_R) == {});
assert(cellularBinomialAssociatedPrimes ideal(1_R) == {});
assert(binomialAssociatedPrimes ideal(x-1, x^2) == {});
