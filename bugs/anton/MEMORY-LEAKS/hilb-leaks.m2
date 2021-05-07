restart
needs "test-mem-leaks.m2"
getRSS()

f = () -> (
    R := ZZ/101[a..d];
    I := monomialIdeal(a^2, a*b, b^5, a*d^4, b^2*c^5, d^10);
    poincare I;
    )

g = () -> (
    R := QQ[a..d];
    I := monomialIdeal(a^2, a*b, b^5, a*d^4, b^2*c^5, d^10);
    poincare I;
    )

testF(10000,f)
testF(100000,f)
testF(100000,f) -- doesn't seem to leak at all. About .45 ms per call too.
testF(100000,g) -- doesn't seem to leak at all. About .42 ms per call too.

