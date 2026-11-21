-- doc-FINDING-randomBinomialIdeal-count.m2
-- FINDING: randomBinomialIdeal(R, n, d, w, h) returns n+1 generators.
-- Documentation: "n -- number of generators of the output"; Caveat: "Minimal
-- generators are produced. These can be less than n".
-- Observed on the original package (setRandomSeed 17, R = QQ[a..x]):
--   (6,2,4,true) -> 7 generators, (3,4,10,false) -> 4, (1,1,2,true) -> 2
--   (ideal(t*u-1,c*h-1)), (5,3,3,false) -> 6, (10,1,24,true) -> 11.
-- The documented example randomBinomialIdeal(R,6,2,4,true) itself shows 7
-- generators.  Expected: at most n generators.

R = QQ[a..x];
for args in {(6,2,4,true), (3,4,10,false), (1,1,2,true), (5,3,3,false), (10,1,24,true)} do (
    (n, d, w, h) := args;
    setRandomSeed 17;
    I := randomBinomialIdeal(R, n, d, w, h);
    assert(numgens I <= n));
