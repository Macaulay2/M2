-- test res over noncommutative rings

R = ZZ/101[a..d,SkewCommutative=>true]
C = time res ( coker vars R, LengthLimit => 15 )
scan(length C + 1, d -> assert( rank C_d == binomial(d+3,3) ))
scan(length C + 1 , i -> assert ( unique degrees C_i == {{i}} ))

S = R/(a*b,c*d)
D = time res ( coker vars S, LengthLimit => 4 )
assert ( D.dd^2 == 0 )
assert ( D.dd != 0 )
assert ( rank D_2 == 12 )
scan ( length D + 1 , i -> assert ( unique degrees D_i == {{i}} ))

use R
S = R/(a*b,c*d)
D = time res ( coker vars S, LengthLimit => 4, Strategy => 3 )
assert ( D.dd^2 == 0 )
assert ( D.dd != 0 )
assert ( rank D_2 == 12 )
scan ( length D + 1 , i -> assert ( unique degrees D_i == {{i}} ))

use R
S = R/(a*b,c*d)
D = time res ( coker vars S, LengthLimit => 4, Strategy => 2 )
assert ( D.dd^2 == 0 )
assert ( D.dd != 0 )
assert ( rank D_2 == 12 )
scan ( length D + 1 , i -> assert ( unique degrees D_i == {{i}} ))

use R
S = R/(a*b,c*d)
assert try(D = time res ( coker vars S, LengthLimit => 4, Strategy => 1 ); 
     error "allowed Strategy=>1 to be used with skew commuting variables") else true

use R
S = R/(a*b,c*d)
assert try(D = time res ( coker vars S, LengthLimit => 4, Strategy => 0 ); 
     error "allowed Strategy=>0 to be used with skew commuting variables") else true

T = ZZ/101[x,dx,y,dy,WeylAlgebra => {x=>dx, y=>dy}]
assert( dx * x == x*dx + 1 )


end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test res5.out"
-- End:
