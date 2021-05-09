-- test methods

-- test ==
assert( { 1. } == { 1 } )
assert( (1,2) == (1.,2.) )


-- test unsequence
assert( unsequence (1,2,3) === (1,2,3) )
assert( unsequence 1 === 1 )
assert( unsequence (1:1) === 1 )
assert( unsequence () === () )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test B44.out"
-- End:
