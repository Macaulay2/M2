-- ideals in ZZ :
I = ideal (30,40,50)
g = gens I
raw g
entries g
assert( class g_(0,0) === ZZ )
assert( 10 == gcd flatten entries g )

-- ZZ/p
k = ZZ/101
a = 3_k
assert ( 3 === lift(a,ZZ) )
assert ( 3 == a )

-- poly rings
R = ZZ[x,y,z]
R'= raw R
2_R
-- raw (2_R)						    -- Mike's bug?
-- rawToInteger raw (2_R)

-- modules
M = monoid R
M'= raw M
F = R^3
F'= raw F
G = F**F
raw G
assert( rank G == 9 )
v = vars R
v
v_(0,0)
v_(0,1)
v_(0,2)
entries v
transpose v
w = v || v
w | w


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test/engine ring.okay"
-- End:
