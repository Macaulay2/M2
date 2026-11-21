-- doc-FINDING-silent-output.m2
-- FINDING: several functions print diagnostic text unconditionally, although
-- the documentation of the option Verbose says "If this option is set,
-- functions will generate additional output. Defaults to false".
-- binomialAssociatedPrimes even announces that its documented algorithm
-- ("First a cellular decomposition is run, then the associated primes of each
-- cellular component are determined") is not implemented.
-- Observed (captured with the core function capture):
--   binomialRadical ideal(x^2,x*y)
--       prints "Input not cellular, computing minimal primes ..."
--   binomialAssociatedPrimes ideal(x^2,x*y)
--       prints "Not yet implemented" / "I will compute a primary decomposition
--       and take radicals!" / "Primary Decomposition found, taking radicals now:"
--   cellularBinomialIsPrimary ideal(x^2-1)
--       prints "The radical is not prime, as the character is not saturated"
-- Expected: no output other than the result.

R = QQ[x,y,z];
-- the transcript produced by capture has only "i1 : ...", "o1 = ...", "o1 : ..."
-- and empty lines when the function prints nothing itself
extraLines = s -> (
    t := last capture s;
    select(lines t, l -> (l = replace("^ +", "", l); l != "" and not match("^(i|o)[0-9]+ ", l) and not match("^[-=|{}\\[\\]0-9 a-z^*+]*$", l))));
-- sanity: a core function produces no extra lines
assert(extraLines "radical ideal(x^2,x*y)" == {});
assert(extraLines "binomialRadical ideal(x^2,x*y)" == {});
assert(extraLines "binomialAssociatedPrimes ideal(x^2,x*y)" == {});
assert(extraLines "cellularBinomialIsPrimary ideal(x^2-1)" == {});
