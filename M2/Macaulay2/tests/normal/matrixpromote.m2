debugLevel = 11
errorDepth = 0
R = ZZ[t,u]
promote(matrix{{2}}, QQ)
  assert( oo === matrix {{2/1}} )
lift(matrix{{2/1}}, ZZ)
  assert( oo === matrix {{2}} )
promote(matrix{{2}}, R)
  assert( oo === matrix {{2_R}} )
lift(matrix{{2/1}}, ZZ)
  assert( oo === matrix {{2}} )

S = QQ[x,y,z]
T = S/y^6
U = T[a,b,c,Join=>false,DegreeMap=>x->{0},DegreeLift=>x->{0}]
V = U/b^7

chain = (ZZ,QQ,S,T,U,V)
for i from 0 to #chain-1 do for j from i to #chain-1 do (
     A = chain#i;
     B = chain#j;
     a := if numgens A > 0 then A_0^2+1 else 3_A;
     b := promote(a,B);
     assert( b == a_B );
     assert( a == lift(b,A) );
     f = a * id_(A^3);
     g = promote(f,B);
     assert(g == b * id_(B^3));
     h = lift(g,A);
     assert(f == h);
     )

assert( degrees promote(vars S, T) === degrees vars S )
assert( degrees promote(vars S, U) === {{{0}}, {{0}, {0}, {0}}} )
assert( degrees promote(vars S, V) === {{{0}}, {{0}, {0}, {0}}} )

end


-- M2-send-to-buffer: "*gud*"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test matrixpromote.out"
-- End:
