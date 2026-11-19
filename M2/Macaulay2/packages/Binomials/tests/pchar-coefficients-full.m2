-* no-check-flag *-
-- The full version of pchar-coefficients.m2, skipped by check because it takes several
-- seconds; pchar-coefficients.m2 runs a part of it.  Run it with
--   load (Binomials#"auxiliary files" | "tests/pchar-coefficients-full.m2")
-- Positive characteristic: coefficients other than 1 and -1.  Every nonzero
-- element of a finite field is a root of unity, and the package takes roots
-- of it through its discrete logarithm.  Everything is checked against
-- Macaulay2's own decompositions over the input field by Galois descent
-- (helpers/pchar-oracles.m2).
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
assert pchCheckAll ideal(x^15 - 3*y^15);                   -- index 15 = 5 * 3
assert pchCheckAll ideal(x^10 - 2*y^10, z^5 - 3*x^5);      -- a lattice of rank 2
assert pchCheckAll ideal(x^5 - 2*y^5, z*(x - 3*y), z^2);   -- an embedded component with coefficient 3
assert pchCheckAll ideal(x^10 - 2*y^10, z*(x^2 - 3*y^2), z^3);
assert pchCheckAll ideal(x^2 - 2*y^2, x*y - 3*z^2);        -- not cellular
assert pchCheckAll ideal(x*y - 2*z^2, y^2 - 3*x*z, x^5 - 4*z^5);
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
assert pchCheckAll ideal(x^2 - 2*y, y^2 - 3*x*z, z^5 - 4);

-- 2. ZZ/7, with its least primitive root 3
R7 = ZZ/7[x,y,z];
assert pchCheckAll ideal(x^2 - 3*y^2);                     -- 3 = e(1/6): square roots in GF(49)
assert pchCheckAll ideal(x^3 - 2*y^3);                     -- 2 = e(1/3): cube roots e(1/9) in GF(7^3)
assert pchCheckAll ideal(x^7 - 3*y^7, z^2);                -- (x - 3y)^7
assert pchCheckAll ideal(x^14 - 5*y^14);                   -- 14 = 7 * 2
assert pchCheckAll ideal(x^14 - 5*y^14, z*(x^7 - 3*y^7), z^2);
assert pchCheckAll ideal(x^3 - 3, y^2 - 5*z^2, z - 2);     -- zero-dimensional, points in GF(7^6)
assert pchCheckAll ideal(x*y - 3*z^2, x^2 - 2*y^2);

-- 3. Galois fields with their generators as coefficients
K4 = GF(2, 2, Variable => a);
S4 = K4[x,y,z];
assert pchCheckAll ideal(x^2 - a*y^2);                     -- (x - a^2 y)^2
assert pchCheckAll ideal(x^3 - a*y^3);                     -- GF(64)
assert pchCheckAll ideal(x^6 - a*y^6, z^2 - a*z);
assert pchCheckAll ideal(x^4 - a, y - a*x, z^2 - a^2);     -- zero-dimensional: x = a (4 times), y = a^2, z = a (twice)
assert(binomialSolve ideal(x^4 - a, y - a*x, z^2 - a^2) == toList(8 : {a, a^2, a}));
K9 = GF(3, 2, Variable => b);
S9 = K9[x,y,z];
assert pchCheckAll ideal(x^2 - b*y^2);                     -- GF(81)
assert pchCheckAll ideal(x^3 - b*y^3);                     -- (x - b^3 y)^3
assert pchCheckAll ideal(x^6 - b^3*y^6, z^3 - b*z^2);      -- 6 = 3 * 2
assert pchCheckAll ideal(x^4 - b^2, y^3 - b, z - b^5);     -- zero-dimensional, x in GF(81), y = b^3 three times
assert pchCheckAll ideal(x*y - b*z^2, x^3 - b^2*y^3);
K25 = GF(5, 2, Variable => d);
S25 = K25[x,y,z];
assert pchCheckAll ideal(x^2 - d*y^2);                     -- d = e(1/24): square roots in GF(5^4)
assert pchCheckAll ideal(x^5 - d*y^5, z^2);                -- (x - d^5 y)^5
assert pchCheckAll ideal(x^3 - d^2*y^3);                   -- cube roots of e(1/12) in GF(5^6)
assert pchCheckAll ideal(x^10 - d^3*y^10, z*(x - d*y), z^2);
assert pchCheckAll ideal(x^2 - d, y^5 - d^7, z - d*x);     -- zero-dimensional

-- 4. a large prime field: discrete logarithms modulo 32002 = 2 * 16001
B = ZZ/32003[x,y,z];
assert pchCheckAll ideal(x^2 - 2*y^2);                     -- 2 is not a square modulo 32003: GF(32003^2)
assert pchCheckAll ideal(x^3 - 7*y^3, z^2);                -- cube roots of unity in GF(32003^2)
assert pchCheckAll ideal(x^2 - 5*y^2, y^2 - 7*z^2);
assert pchCheckAll ideal(x^2 - 2, y^2 - 3*x, z - 5);       -- zero-dimensional
L = binomialPrimaryDecomposition ideal(x^2 - 2*y^2, z^3 - 11*z^2);
assert(#L == 4 and (coefficientRing ring first L).order == 32003^2);
