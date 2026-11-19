-* no-check-flag *-
-- The full version of pchar-lattice.m2, skipped by check because it takes several
-- seconds; pchar-lattice.m2 runs a part of it.  Run it with
--   load (Binomials#"auxiliary files" | "tests/pchar-lattice-full.m2")
-- Positive characteristic: lattice ideals and small cellular
-- ideals over ZZ/2, ZZ/3 and ZZ/5, against Macaulay2's algorithms through
-- Galois descent (helpers/pchar-oracles.m2) and by hand.
load (Binomials#"auxiliary files" | "tests/helpers/pchar-oracles.m2");

-- 1. x^2 - y^2 in characteristic 2 is primary and not prime; its radical is (x - y)
R = ZZ/2[x,y];
I = ideal(x^2 - y^2);
assert(binomialIsPrimary I and not binomialIsPrime I);
assert(binomialRadical I == ideal(x - y) and cellularBinomialRadical I == ideal(x - y));
assert(binomialPrimaryDecomposition I == {I});
assert(binomialMinimalPrimes I == {ideal(x - y)} and binomialAssociatedPrimes I == {ideal(x - y)});
assert pchCheckAll I;
assert(cellularBinomialIsPrimary(I, ReturnPrimes => true) == {ideal(x - y)});
-- the character returned when the ideal is primary is the partial character of I itself
pc = first cellularBinomialIsPrimary(I, ReturnPChars => true);
assert(instance(pc, PartialCharacter) and pc#"L" == transpose matrix {{2, -2}});
-- the same ideal in characteristic 3 splits
S = ZZ/3[x,y];
J = ideal(x^2 - y^2);
assert(not binomialIsPrimary J and not binomialIsPrime J);
assert(binomialRadical J == J);
assert(binomialPrimaryDecomposition J == {ideal(x - y), ideal(x + y)});
assert pchCheckAll J;

-- 2. lattices of index p^a, p^a g and g in characteristic p
R2 = ZZ/2[x,y,z];
scan({ideal(x^4 - y^4), ideal(x^8 - y^8, z - 1), ideal(x^2 - y^2, y^2 - z^2), ideal(x^4 - y^4, x^2*y^2 - z^4)}, I -> (
        assert(binomialIsPrimary I);
        assert pchCheckAll I));
-- index 6 = 2 * 3: three primary components over GF(4), each of multiplicity 2
I = ideal(x^6 - y^6);
L = binomialPrimaryDecomposition I;
assert(#L == 3 and (coefficientRing ring first L).order == 4);
assert all(L, Q -> degree Q == 2);
assert pchCheckAll I;
assert(binomialRadical I == ideal(x^3 - y^3));
S3 = ZZ/3[x,y,z];
scan({ideal(x^3 - y^3), ideal(x^9 - y^9), ideal(x^3 - y^3, y^3 - z^3), ideal(x^6 - y^6), ideal(x^12 - y^12, z^2 - 1)}, I -> assert pchCheckAll I);
assert(binomialIsPrimary ideal(x^9 - y^9) and not binomialIsPrimary ideal(x^6 - y^6));
assert(binomialRadical ideal(x^6 - y^6) == ideal(x^2 - y^2));
assert(binomialRadical ideal(x^12 - y^12, z^2 - 1) == ideal(x^4 - y^4, z^2 - 1));
S5 = ZZ/5[x,y,z];
scan({ideal(x^5 - y^5), ideal(x^10 - y^10), ideal(x^4 - y^4), ideal(x^25 - y^25, z^3 - 1)}, I -> assert pchCheckAll I);

-- 3. the p-saturation of a lattice that is not principal: index 4 in Z^2 with
--    two elementary divisors
use R2;
I = ideal(x^2 - y^2, y^2 - z^2);              -- lattice {(2,-2,0), (0,2,-2)}, index 4 = 2^2
assert(binomialRadical I == ideal(x - y, y - z));
assert pchCheckAll I;
I = ideal(x^2*y^2 - z^4, x^4 - y^4);
assert pchCheckAll I;

-- 4. cellular ideals with nilpotent variables: embedded components whose
--    lattices have index divisible by p
use R2;
I = ideal(x^2 - 1, z*(x - 1), z^2);          -- {x}-cellular, index 2 at the bottom
assert(binomialIsPrimary I);
assert(binomialRadical I == ideal(x - 1, z) and cellularBinomialRadical I == ideal(x - 1, z));
assert pchCheckAll I;
I = ideal(x^2 - 1, z*(x*y - 1), z^2, y^2 - 1);
assert pchCheckAll I;
I = ideal(x^4 - y^4, z*(x^2 - y^2), z^2);    -- bottom index 4, embedded index 2
assert pchCheckAll I;
assert(#binomialAssociatedPrimes I == 1);
I = ideal(x^6 - y^6, z*(x^2 - y^2), z^2);    -- bottom index 6, embedded index 2: 3 associated primes
assert pchCheckAll I;
assert(#binomialAssociatedPrimes I == 3);
use S3;
I = ideal(x^6 - y^6, z*(x^3 - y^3), z^2);    -- characteristic 3: bottom index 6, embedded index 3
assert pchCheckAll I;
assert(#binomialAssociatedPrimes I == 2);
I = ideal(x^3 - y^3, z*(x - y), z^3);        -- primary, embedded lattice saturated
assert(binomialIsPrimary I and #binomialPrimaryDecomposition I == 1);
assert pchCheckAll I;
I = ideal(x^2 - y^2, z*(x - y), z^2);        -- two minimal primes, one with an embedded contribution
assert pchCheckAll I;

-- 5. ideals that are not cellular
use R2;
I = ideal(x^2 - y^2, x*y^2 - x, z*(x - y));
assert pchCheckAll I;
I = ideal(x^2*y - y^3, z^2 - 1, x*z - x);
assert pchCheckAll I;
use S3;
I = ideal(x^3*y - y^4, z^2*(x - y), z^3 - z);
assert pchCheckAll I;
use S5;
I = ideal(x^5*y - y^6, z^2*(x - y), z^3 - z, x^4 - 1);
assert pchCheckAll I;

-- 6. binomialSolve: multiplicities are the orders of Sat_p(L)/L
use R2;
sols = binomialSolve ideal(x^2 - 1, y^4 - 1, z - 1);
assert(#sols == 8 and unique sols == {{1_(ZZ/2), 1_(ZZ/2), 1_(ZZ/2)}});
assert pchCheckSolve(ideal(x^2 - 1, y^4 - 1, z - 1), sols);
sols = binomialSolve ideal(x^3 - 1, y^2 - 1, z^2);
assert(#sols == 12 and #unique sols == 3 and (ring first first sols).order == 4);
assert pchCheckSolve(ideal(x^3 - 1, y^2 - 1, z^2), sols);
use S3;
sols = binomialSolve ideal(x^3 - 1, y^4 - 1, z);
assert(#sols == 12 and #unique sols == 4 and (ring first first sols).order == 9);
assert pchCheckSolve(ideal(x^3 - 1, y^4 - 1, z), sols);
sols = binomialSolve ideal(x^2 - 1, y*x - y, y^2, z);
assert(#sols == 3 and sols == {{1_(ZZ/3), 0_(ZZ/3), 0_(ZZ/3)}, {1_(ZZ/3), 0_(ZZ/3), 0_(ZZ/3)}, {-1_(ZZ/3), 0_(ZZ/3), 0_(ZZ/3)}});
assert pchCheckSolve(ideal(x^2 - 1, y*x - y, y^2, z), sols);
-- the unit ideal and the zero-dimensional monomial ideal
assert(binomialSolve ideal(1_S3) == {} and binomialSolve ideal(x, y^2, z) == {{0_(ZZ/3), 0_(ZZ/3), 0_(ZZ/3)}, {0_(ZZ/3), 0_(ZZ/3), 0_(ZZ/3)}});
