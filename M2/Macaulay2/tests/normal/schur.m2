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
listForm g
g1 = sum apply(listForm g, a -> a#1 * s_(a#0))
assert(g == g1)

needsPackage "SchurRings"
debug SchurRings
debug Core
B = schurRing(s, 7)
f = s_{3,3,2,1,0}
g = f^3
expression g
exponents g;
oo/print;
size g
assert((dim f)^3 == dim g)
f = s_{2,1} + s_{1}
g = f^2
assert(listForm g == {({4, 2}, 1), ({4, 1, 1}, 1), ({3, 3}, 1), ({3, 2, 1}, 2), ({3, 1, 1, 1}, 1), ({3, 1}, 2), ({2, 2, 2}, 1), ({2, 2, 1, 1}, 1), ({2, 2}, 2), ({2, 1, 1}, 2), ({2}, 1), ({1, 1}, 1)})
g1 = sum apply(listForm g, a -> a#1 * s_(a#0))
assert(g1 == g)
g1-g
assert(dim (g1-g) == 0)
assert(dim g == (dim f)^2)
exponents f
3*f
-- The following do not work, as the ring is not considered as a polynomial ring
assert try leadTerm (3*f) else true
assert try terms f^2 else true
assert try coefficients f else true
assert try someTerms(f^2,2,3) else true
h = s_{231223123,4534324,21312}
-- The following is too large for the current algorithm
-- dim h
assert(s_(first exponents h) == h)


end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test schur.out"
-- End:
