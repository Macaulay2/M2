-- Positive characteristic: coefficients other than 1 and -1.  Every nonzero
-- element of a finite field is a root of unity, and the package takes roots
-- of it through its discrete logarithm.  Everything is checked against
-- Macaulay2's own decompositions over the input field by Galois descent
-- (helpers/pchar-oracles.m2).  A part of pchar-coefficients-full.m2, which
-- check skips: of the oracle checks on single ideals, only the first for each
-- field is kept here.
load (Binomials#"auxiliary files" | "tests/helpers/pchar-oracles.m2");

-- 1. ZZ/5: the unique 5th root, and lattices whose index has a p-part and a
-- prime-to-p part
R = ZZ/5[x,y,z];
I = ideal(x^5 - 2*y^5);                                    -- (x - 2y)^5: 2 is its own 5th root
assert(binomialPrimaryDecomposition I == {I});
assert(binomialRadical I == ideal(x - 2*y));
assert(binomialMinimalPrimes I == {ideal(x - 2*y)});
assert(binomialAssociatedPrimes I == {ideal(x - 2*y)});
assert(binomialIsPrimary I and not binomialIsPrime I);
assert(cellularBinomialIsPrimary(I, ReturnPrimes => true) == {ideal(x - 2*y)});
assert pchCheckAll I;
assert pchCheckAll ideal(x^25 - 3*y^25, z^2);              -- (x - 3^(1/25) y)^25 = (x - 3y)^25
I = ideal(x^10 - 2*y^10);                                  -- index 10 = 5 * 2: two components x^5 - s y^5, s^2 = 2
L = binomialPrimaryDecomposition I;
T = ring first L;
assert(#L == 2 and (coefficientRing T).order == 25);
assert all(L, Q -> numgens Q == 1 and (s := - coefficient(T_1^5, Q_0) / coefficient(T_0^5, Q_0); s^2 == 2));
P = binomialMinimalPrimes I;                               -- the primes x - s^(1/5) y = x + s y
assert(#P == 2 and all(P, Q -> numgens Q == 1 and (w := - coefficient(T_1, Q_0) / coefficient(T_0, Q_0); w^2 == 2)));
use R;                                                     -- T = ... rebound x, y, z to T
assert(binomialRadical I == ideal(x^2 - 2*y^2));
assert pchCheckAll I;
-- zero-dimensional: binomialSolve takes every binomial ideal in characteristic p
I = ideal(x^5 - 2, y^2 - 2, z - 3);                        -- x = 2 with multiplicity 5, y = plus or minus the square root of 2
sols = binomialSolve I;
assert(#sols == 10 and all(sols, s -> s#0 == 2 and s#2 == 3 and s#1^2 == 2));
assert(#unique sols == 2 and (ring first first sols).order == 25);
assert pchCheckAll I;
I = ideal(x^4 - 2, y - x^2, z^3 - 1);                      -- 2 = e(1/4): 4th roots of order 16, in GF(5^4)
sols = binomialSolve I;
assert(#sols == 12 and (ring first first sols).order == 625 and all(sols, s -> s#0^4 == 2 and s#1 == s#0^2 and s#2^3 == 1));
assert pchCheckAll I;

-- 2. ZZ/7, with its least primitive root 3
R7 = ZZ/7[x,y,z];
assert pchCheckAll ideal(x^2 - 3*y^2);                     -- 3 = e(1/6): square roots in GF(49)

-- 3. Galois fields with their generators as coefficients
K4 = GF(2, 2, Variable => a);
S4 = K4[x,y,z];
assert pchCheckAll ideal(x^2 - a*y^2);                     -- (x - a^2 y)^2
assert(binomialSolve ideal(x^4 - a, y - a*x, z^2 - a^2) == toList(8 : {a, a^2, a}));
K9 = GF(3, 2, Variable => b);
S9 = K9[x,y,z];
assert pchCheckAll ideal(x^2 - b*y^2);                     -- GF(81)
assert pchCheckAll ideal(x*y - b*z^2, x^3 - b^2*y^3);
K25 = GF(5, 2, Variable => d);
S25 = K25[x,y,z];

-- 4. a large prime field: discrete logarithms modulo 32002 = 2 * 16001
B = ZZ/32003[x,y,z];
assert pchCheckAll ideal(x^2 - 2*y^2);                     -- 2 is not a square modulo 32003: GF(32003^2)
L = binomialPrimaryDecomposition ideal(x^2 - 2*y^2, z^3 - 11*z^2);
assert(#L == 4 and (coefficientRing ring first L).order == 32003^2);
