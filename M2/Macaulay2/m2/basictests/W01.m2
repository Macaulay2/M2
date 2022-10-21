-- test the engine directly
assert = x -> if not x then error "assertion failed "
randomSeed = 0
assert ( set keys tally apply(100, i -> rawRandomZZ 10) === set {0, 1, 2, 3, 4, 5, 6, 7, 8, 9} )

-- if this test fails in eval(c:Code) in evaluate.d (build a debug version with
-- type tag testing to see it stop there), it can mean that the engine is not
-- inserting correct type tag codes into RawRing objects:
assert ( class rawZZ() === RawRing )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/basictests W01.okay"
-- End:
