-- doc-FINDING-unit-ideal-isprime.m2
-- FINDING: binomialIsPrime(ideal 1_R) returns true.  The unit ideal is not
-- prime (Macaulay2: isPrime ideal(1_R) == false); binomialIsPrimary returns
-- false on it, consistently with isPrimary.
-- Observed: binomialIsPrime ideal(1_R) -> true; also for ideal(x-1,x^2) and
-- ideal(y^2,1-y^3) (the unit ideal with other generators).  Expected: false.

R = QQ[x,y,z];
assert(isPrime ideal(1_R) === false);
assert(binomialIsPrime ideal(1_R) === false);
assert(binomialIsPrime ideal(x-1, x^2) === false);
assert(binomialIsPrime ideal(y^2, 1-y^3) === false);
