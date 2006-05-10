-- test regularity

R=ZZ/101[x,y,z]
assert( 0 === regularity cokernel vars R )
assert( 1 === regularity cokernel symmetricPower(2,vars R) )
assert( 0 === regularity resolution cokernel vars R )
assert( 1 === regularity resolution cokernel symmetricPower(2,vars R) )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test regularity.out"
-- End:
