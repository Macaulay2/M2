TEST' := teststr -> assert not first capture(teststr, UserMode => false)

-- these tests should be rewritten to use coimage, eventually
TEST' ///
-- Sheaves on P1 x P1
R = ZZ/32003[a..d];
S = ZZ/32003[z_(1,1)..z_(2,2)]
F = map(R,S,matrix"a,b" ** matrix"c,d")
I = ideal(a,c)
J = ideal(a,b)
K = ideal"ac2+bd2"
-- the ideal of a point:
-- was: pushForward1(F,comodule I)
coimage map(comodule I,F)
assert( 1 == degree oo and 1 == dim oo)
-- the ideal of the empty set:
coimage map (comodule J,F)
assert( 1 == degree oo and 0 == dim oo)
-- the ideal of the twisted cubic:
CO = coimage map (comodule K,F)
assert ( degree CO == 3 )
assert ( dim CO == 2 )
res CO
betti oo
assert( oo === new BettiTally from {(0,{0},0) => 1, (2,{3},3) => 2, (1,{2},2) => 3} )

-- Yau's example projected to P2xP3
a = symbol a
b = symbol b
R = ZZ/32003[a_0..a_3,b_0..b_3]
-- The ideal of the product of two cubic surfaces, cut by a hyperplane
I = ideal"a[0]3+a[1]3+a[2]3+a[3]3,
          b[0]3+b[1]3+b[2]3+b[3]3,
	  a[0]b[0]+a[1]b[1]+a[2]b[2]+a[3]b[3]"
assert( 3 == codim I )
assert( 18 == degree I )

S = ZZ/32003[a_0..a_2,b_0..b_3]
f = map(R,S)
-- was: N = ideal pushForward1(f,comodule I)
N = coimage map(comodule I,f)
assert( 2 == codim N )
assert( 18 == degree N )
betti N
assert ( oo === new BettiTally from {(1,{13},13) => 1, (1,{14},14) => 1,
	  (1,{6},6) => 1, (1,{7},7) => 1, (0,{0},0) => 1, (1,{8},8) => 2, (1,{9},9) => 1, (1,{10},10) => 1,
	  (1,{11},11) => 1, (1,{3},3) => 1})

-- A hard example
R = ZZ/32003[a..e];
I = ideal"a5+b5+c5+d5+e5-5abcde,
         ea3b+ab3c+bc3d+cd3e+de3a,
	 e2ab2+a2bc2+b2cd2+c2de2+d2ea2"
assert( 2 == codim I )
assert( 15 == degree I )
S = ZZ/32003[a..d];
use R
F = map(R,S,{a+b,2*a+c,3*a+d,4*a+e})
-- was : time pushForward1(F,comodule I)
time relations coimage map(comodule I,F)
assert( degrees oo == {{{0}}, {{15}}})
///
