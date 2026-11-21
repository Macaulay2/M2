-- Positive characteristic: random binomial ideals with arbitrary nonzero
-- coefficients over ZZ/2, ZZ/3, ZZ/5, ZZ/7, GF(4), GF(9), GF(25) and
-- ZZ/32003, every decomposition function checked against Macaulay2's
-- algorithms by Galois descent (helpers/pchar-oracles.m2).  The seeds are
-- fixed so that the run is reproducible.  A part of
-- pchar-coefficients-random-full.m2 (more seeds), which check skips.
load (Binomials#"auxiliary files" | "tests/helpers/oracles.m2");        -- orcRandomBinomialIdeal
load (Binomials#"auxiliary files" | "tests/helpers/pchar-oracles.m2");

-- all nonzero elements of a finite field K = ZZ/p or GF(p, n)
nonzeroElements = K -> if instance(K, GaloisField) then apply(K.order - 1, i -> K_0^i) else apply(char K - 1, i -> (i + 1)_K);

randomCheck = (K, seeds, n, d, w) -> (
    R := K[x,y,z];
    cs := nonzeroElements K;
    count := 0;
    scan(seeds, seed -> (
            setRandomSeed seed;
            I := orcRandomBinomialIdeal(R, n, d, w, cs, 2);
            if I == 0 or I == 1 then return;
            -- a zero-dimensional ideal of large degree has its points in a
            -- large Galois field, where the descent oracle takes long
            -- (the package needs 0.6 s for 72 points in GF(5^12), the
            -- oracle 30 s); the package is still run on it
            if dim I == 0 and degree I > 24 then (binomialPrimaryDecomposition I; return);
            count = count + 1;
            assert pchCheckAll I));
    count);
assert(randomCheck(ZZ/3, toList(21..22), 3, 3, 2) > 0);
assert(randomCheck(ZZ/7, toList(1..2), 3, 3, 2) > 0);
assert(randomCheck(GF(2, 2), toList(1..2), 3, 3, 2) > 0);
assert(randomCheck(GF(5, 2), toList(1..2), 3, 3, 2) > 0);
assert(randomCheck(ZZ/32003, toList(1..2), 3, 3, 2) > 0);
-- zero-dimensional ideals with arbitrary coefficients: binomialSolve
R5 = ZZ/5[x,y,z];
setRandomSeed 11;
scan(2, i -> (
        cs := {1_(ZZ/5), 2_(ZZ/5), 3_(ZZ/5), 4_(ZZ/5)};
        I := ideal(x^(random(1, 4)) - cs#(random 4), y^(random(1, 3)) - cs#(random 4) * x^(random(0, 2)), z^(random(1, 3)) - cs#(random 4) * y);
        if degree I <= 24 then assert pchCheckAll I));
