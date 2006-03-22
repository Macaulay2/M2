-- test methods

-- test ==
assert( { 1. } == { 1 } )
assert( (1,2) == (1.,2.) )


-- test unSingleton
-- assert( unSingleton (1,2,3) === (1,2,3) )
-- assert( unSingleton 1 === 1 )
-- assert( unSingleton (1:1) === 1 )
-- assert( unSingleton () === () )
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test B44.out"
-- End:
