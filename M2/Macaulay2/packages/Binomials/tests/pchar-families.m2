-- Positive characteristic: families of binomial ideals (parity binomial edge
-- ideals, birth-death ideals, adjacent minors) over ZZ/2, ZZ/3 and ZZ/5,
-- checked against Macaulay2's algorithms by Galois descent and against the
-- theorems of Kahle, Sarmiento and Windisch (2016) on parity binomial edge
-- ideals.  A part of pchar-families-full.m2, which check skips.
load (Binomials#"auxiliary files" | "tests/helpers/pchar-oracles.m2");
load (Binomials#"auxiliary files" | "tests/helpers/pchar-families.m2");

-- 1. parity binomial edge ideals.  [KSW16]: radical if and only if the graph
--    is bipartite or the characteristic is not 2 (Theorem 5.5 and the
--    introduction); in characteristic 2 the decomposition (5.1) is primary.
graphs = {
    ("triangle", 3, pchCycleEdges 3),
    ("4-cycle", 4, pchCycleEdges 4)};
scan({2, 3}, p -> scan(graphs, g -> (
            (name, n, E) := g;
            I := pchParityEdgeIdeal(ZZ/p, n, E);
            r := binomialRadical I;
            assert((r == I) === (pchIsBipartite(n, E) or p != 2));
            assert pchCheckAll I)));
-- the triangle in detail: in characteristic 2 the two sign primes
-- (x_i - y_i) and (x_i + y_i) of characteristic 3 become one component
-- (x_i + y_i)-primary that is not prime, next to the three primes with a
-- vertex removed; all four associated primes are minimal
I = pchParityEdgeIdeal(ZZ/2, 3, pchCycleEdges 3);
R = ring I;
assert(#binomialMinimalPrimes I == 4 and #binomialAssociatedPrimes I == 4);
assert(not binomialIsPrimary I);
L = binomialPrimaryDecomposition I;
assert(#L == 4 and ring first L === R and intersect L == I);
assert(#select(L, Q -> binomialIsPrime Q) == 3);
I3 = pchParityEdgeIdeal(ZZ/3, 3, pchCycleEdges 3);
L3 = binomialPrimaryDecomposition I3;
assert(#L3 == 5 and ring first L3 === ring I3 and all(L3, binomialIsPrime));

-- 2. birth-death ideals (Evans, Sturmfels, Uhler): in pchar-families-full.m2

-- 3. adjacent minors
scan({2, 3}, p -> assert pchCheckAll pchAdjacentMinors(ZZ/p, 2, 4));

-- 4. binomial edge ideals are radical in every characteristic (Herzog et al.)
scan({2}, p -> (
        I := pchBinomialEdgeIdeal(ZZ/p, 5, pchCycleEdges 5);
        assert(binomialRadical I == I);
        assert pchCheckMinimalPrimes(I, binomialMinimalPrimes I)));
