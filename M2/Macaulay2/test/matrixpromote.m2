R = ZZ[x]

assert( promote(matrix{{2}}, QQ) === matrix {{2/1}} )
assert( lift(matrix{{2/1}}, ZZ) === matrix {{2}} )
assert liftable(matrix{{2/1}}, ZZ)
assert not liftable(matrix{{2/3}}, ZZ)

assert( promote(matrix{{2}}, R) === matrix {{2_R}} )
assert( lift(matrix{{2/1}}, ZZ) === matrix {{2}} )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test matrixpromote.out"
-- End:
