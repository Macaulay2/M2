needsPackage "SchurRings"
A = Schur 4
f = A_{1}
assert( f*f == A_{2} + A_{1,1} )
assert( dim A_{1,1} == 6 )
assert( dim A_{2} == 10 )

A = Schur 7
f = A_{1}
assert( f*f == A_{2} + A_{1,1} )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test schur.out"
-- End:
