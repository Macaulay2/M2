errorDepth = 0
R = ZZ[x]
promote'(matrix{{2}}, QQ)
  assert( oo === matrix {{2/1}} )
lift'(matrix{{2/1}}, ZZ)
  assert( oo === matrix {{2}} )
liftable'(matrix{{2/1}}, ZZ)
  assert oo
liftable'(matrix{{2/3}}, ZZ)
  assert not oo
promote'(matrix{{2}}, R)
  assert( oo === matrix {{2_R}} )
lift'(matrix{{2/1}}, ZZ)
  assert( oo === matrix {{2}} )
end
-- Local Variables:
-- M2-send-to-buffer: "*gud*"
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test matrixpromote'.out"
-- End:
