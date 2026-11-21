-- Positive characteristic: random binomial ideals with coefficients 1 and
-- -1 over ZZ/2, ZZ/3, ZZ/5 and GF(4), every decomposition function checked
-- against Macaulay2's algorithms by Galois descent (helpers/pchar-oracles.m2).
-- The seeds are fixed so that the run is reproducible.  A part of
-- pchar-random-full.m2 (more seeds), which check skips.
load (Binomials#"auxiliary files" | "tests/helpers/oracles.m2");        -- orcRandomBinomialIdeal
load (Binomials#"auxiliary files" | "tests/helpers/pchar-oracles.m2");

randomCheck = (K, seeds, n, d, w) -> (
    R := K[x,y,z];
    cs := {1_K, -1_K};
    count := 0;
    scan(seeds, seed -> (
            setRandomSeed seed;
            I := orcRandomBinomialIdeal(R, n, d, w, cs, 2);
            if I == 0 or I == 1 then return;
            count = count + 1;
            assert pchCheckAll I));
    count);
assert(randomCheck(ZZ/2, toList(1..4), 3, 3, 2) > 1);
assert(randomCheck(ZZ/3, toList(1..3), 3, 3, 2) > 1);
assert(randomCheck(ZZ/5, toList(1..2), 3, 3, 2) > 0);
assert(randomCheck(GF(2, 2), toList(1..2), 3, 3, 2) > 0);
-- unital ideals made by the package's own generator, in characteristic 2
R = ZZ/2[x,y,z];
setRandomSeed 7;
scan(2, i -> (
        I := randomBinomialIdeal(R, 3, 3, 2, false);
        if I != 0 and I != 1 then assert pchCheckAll I));
