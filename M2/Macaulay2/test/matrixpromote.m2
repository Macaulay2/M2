debugLevel = 11
errorDepth = 0
R = ZZ[t,u]
promote'(matrix{{2}}, QQ)
  assert( oo === matrix {{2/1}} )
lift'(matrix{{2/1}}, ZZ)
  assert( oo === matrix {{2}} )
promote'(matrix{{2}}, R)
  assert( oo === matrix {{2_R}} )
lift'(matrix{{2/1}}, ZZ)
  assert( oo === matrix {{2}} )

S = QQ[x,y,z]
T = S/y^6
U = T[a,b,c]
V = U/b^7

chain = (ZZ,QQ,S,T,U,V)
for i from 0 to #chain-1 do for j from i to #chain-1 do (
     A = chain#i;
     B = chain#j;
     f = id_(A^3);
     stderr << "trying matrix promotion from " << A << " to " << B << endl;
     g = promote'(f,B);
     assert(g == id_(B^3));
     stderr << "trying matrix lift from " << A << " to " << B << endl;
     h = lift'(g,A);
     assert(f == h);
     )

end

liftable'(matrix{{2/1}}, ZZ)
  assert oo
liftable'(matrix{{2/3}}, ZZ)
  assert not oo


-- M2-send-to-buffer: "*gud*"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test matrixpromote.out"
-- End:
