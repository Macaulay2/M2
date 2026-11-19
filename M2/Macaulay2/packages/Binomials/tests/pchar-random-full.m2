-* no-check-flag *-
-- The full version of pchar-random.m2, skipped by check because it takes several
-- seconds; pchar-random.m2 runs a part of it.  Run it with
--   load (Binomials#"auxiliary files" | "tests/pchar-random-full.m2")
-- Positive characteristic: random binomial ideals with coefficients 1 and
-- -1 over ZZ/2, ZZ/3, ZZ/5 and GF(4), every decomposition function checked
-- against Macaulay2's algorithms by Galois descent (helpers/pchar-oracles.m2).
-- The seeds are fixed so that the run is reproducible.
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
assert(randomCheck(ZZ/2, toList(1..14), 3, 3, 2) > 8);
assert(randomCheck(ZZ/3, toList(1..14), 3, 3, 2) > 8);
assert(randomCheck(ZZ/5, toList(1..10), 3, 3, 2) > 6);
assert(randomCheck(GF(2, 2), toList(1..8), 3, 3, 2) > 4);
-- unital ideals made by the package's own generator, in characteristic 2
R = ZZ/2[x,y,z];
setRandomSeed 7;
scan(6, i -> (
        I := randomBinomialIdeal(R, 3, 3, 2, false);
        if I != 0 and I != 1 then assert pchCheckAll I));
