needsPackage "SchurRings"
A = schurRing(symbol s,4)
f = s_{1}
assert( f*f == s_{2} + s_{1,1} )
assert( dim s_{1,1} == 6 )
assert( dim s_{2} == 10 )

B = schurRing(s, 7)
f = s_{1}
assert( f*f == s_{2} + s_{1,1} )

g = f^3
expression g
terms g -- want the representations
listForm g
g1 = sum apply(listForm g, a -> a#1 * s_(a#0))
assert(g == g1)

needsPackage "SchurRings"
debug SchurRings
debug Core
B = schurRing(s, 7)
f = s_{3,3,2,1,0}
s1 = rawmonom2schur rawLeadMonomialR f
m1 = schur2monom(s1, generators monoid B)
assert(f == 1_ZZ * m1)
g = f^3
expression g
exponents g;
oo/print;
size g
(dim f)^3 == dim g
f = s_{2,1} + s_{1}
g = f^2
listForm g
g1 = sum apply(listForm g, a -> a#1 * s_(a#0))
assert(g1 == g)
g1-g
assert(dim (g1-g) == 0)
assert(dim g == (dim f)^2)
exponents f
3*f
leadTerm (3*f)
terms f^2
coefficients f
h = s_{231223123,4534324,21312}
dim h
assert(s_(first exponents h) == h)
someTerms(f^2,2,3)

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test schur.out"
-- End:
