-- doc-unit-ideal-isprime.m2
-- binomialIsPrime(ideal 1_R) is false: the unit ideal is not prime
-- (Macaulay2: isPrime ideal(1_R) == false), as binomialIsPrimary and
-- isPrimary say.
-- Inputs: ideal(1_R), ideal(x-1,x^2) and ideal(y^2,1-y^3) (the unit ideal
-- with other generators).  Expected: false.

R = QQ[x,y,z];
assert(isPrime ideal(1_R) === false);
assert(binomialIsPrime ideal(1_R) === false);
assert(binomialIsPrime ideal(x-1, x^2) === false);
assert(binomialIsPrime ideal(y^2, 1-y^3) === false);
