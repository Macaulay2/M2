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
lift(a,ZZ)
assert ( 3 === lift(a,ZZ) )
assert ( 3 == a )

-- poly rings
R = ZZ[x,y,z]
R'= raw R
M' = raw monoid R
2_R
raw 2_R						    -- Mike's bug?
raw ZZ
rawLift(raw ZZ, raw 2_R)
rawToInteger rawLift(raw ZZ, raw 2_R)
lift(2_R, ZZ)

m = leadMonomial (x^2 * y)
m' = raw m
n = leadMonomial (x * y * z)
n' = raw n
rawCompare(M',m',n')
m ? n

-- modules
M'= raw monoid R
F = R^3
assert(raw F == raw R^3)
assert(raw F != raw R^4)
G = F**F
raw G
assert( rank G == 9 )

--matrices
v = vars R

v
v_(0,0)
v_(0,1)
v_(0,2)
entries v
v' = transpose v
w = v || v
w | w
v ++ v
v' ++ v
v ** v
v ** v'

2*v
v*2
raw x * raw v
x*v


-- remaking matrices
-- w = map(R^1,,v)						    -- Mike will make remake1

-- eventually:
-- raw v * raw x
-- v*x

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test/engine ring.okay"
-- End:
