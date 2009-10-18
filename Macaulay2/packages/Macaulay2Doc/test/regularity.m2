-- test regularity

R=ZZ/101[x,y,z]
assert( 0 === regularity cokernel vars R )
assert( 1 === regularity cokernel symmetricPower(2,vars R) )
assert( 0 === regularity resolution cokernel vars R )
assert( 1 === regularity resolution cokernel symmetricPower(2,vars R) )

S=ZZ/32003[x,y]
d=5
e=3
i=ideal(x^d,y^d,x^(d-e+1)*y^e)
m = regularity module (i^5)
n = regularity (i^5)
assert( m == n )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test regularity.out"
-- End:
