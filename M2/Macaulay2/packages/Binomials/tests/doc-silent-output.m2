-- doc-silent-output.m2
-- Functions print nothing but their result unless the option Verbose is set
-- (its default is false).  Checked with the core function capture on
-- binomialRadical ideal(x^2,x*y), binomialAssociatedPrimes ideal(x^2,x*y)
-- and cellularBinomialIsPrimary ideal(x^2-1).
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
