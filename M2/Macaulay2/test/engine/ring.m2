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
assert ( degree x == {1} )
assert ( degree (x*y*z^8) == {10} )

assert( 1 === rawIndexIfVariable raw y )
assert( null === rawIndexIfVariable raw (x*y^2))
assert( null === rawIndexIfVariable raw (3*x) )
assert( null === rawIndexIfVariable raw (y*z) )
assert( null === rawIndexIfVariable raw (x*y) )
assert( null === rawIndexIfVariable raw (x*y) )

assert( rawIndices raw (x+y) == {0, 1} )
assert( rawIndices raw (x+y^2) == {0, 1} )
assert( rawIndices raw (x+y*z) == {0, 1, 2} )

size x
rawMultiDegree raw x
assert( symbol x == baseName x )
assert( symbol y == baseName (monoid R)_1 )
-- exponents x

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
rawCompareMonomial(M',m',n')
m ? n

rawSparseListFormMonomial raw m

exponents(3,raw m)
exponents(3,raw n)

-- exponents m
-- listForm m
-- standardForm m

f = (x+y+1)^3

-- listForm f
-- standardForm f

-- modules
M'= raw monoid R
F = R^3
assert(raw F == raw R^3)
assert(raw F != raw R^4)
G = F**F
raw G
assert( rank G == 9 )

H = R^{-1,-2,-3}
degrees H
assert ( degrees H == {{1}, {2}, {3}} )

--matrices

u = id_F
assert ( u == 1 )
assert ( u-u == 0 )

u0 = map(F,F,0)
assert( u0 == 0 )

v = vars R

assert try (rawMatrixEntry(raw v, 0, 11); false) else true

v
v_(0,0)
v_(0,1)
v_(0,2)
assert try (v_(0,22); false) else true

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

t = matrix {{x,y,z},{y,z,x},{z,x,y}}

assert( matrix({{v,v},{v,v}}) == v | v || v | v )
assert( matrix({{v,0},{0,v}}) == v ++ v )



toString submatrix(t,{1,2})
assert( submatrix(t,{1,2}) == matrix {{y,z},{z,x},{x,y}} )
assert( submatrix(t,{1,2},{2,0}) == matrix {{x, y}, {y, z}} )
assert( submatrix(t,,{2,0}) == matrix {{z, x}, {x, y}, {y, z}} )

-- making modules from matrices
N = coker v
L = image v
N ++ L
     
-- remaking matrices
w = map(R^1,,v)
degrees source w
assert ( degrees source w == {{1}, {1}, {1}} )
w = map(R^1,R^3,v)
degrees source w
assert( degrees source w == {{0}, {0}, {0}} )

w = map(R^1,R^{-1,-2,-3},v)
degrees source w
assert( degrees source w == {{1}, {2}, {3}} )

-- right multiplication:
raw v * raw x
v*x

-- noncommutative rings:
S = ZZ[X,Y,WeylAlgebra => {X=>Y}]

X*Y
Y*X
b = matrix {{X}} * matrix {{Y}}
c = matrix {{Y*X}}
assert( matrix {{X}} * matrix {{Y}} - matrix {{Y*X}} == 0 )

degrees target b
degrees target c

degrees source b
degrees source c

degree b
degree c

-- what to do about this?
-- assert( matrix {{X}} * matrix {{Y}} == matrix {{Y*X}} )

-- Groebner bases
f = vars R
I = image f
G = gb I
peek G
peek G.matrix

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test/engine ring.okay "
-- compile-command: "M2 --debug-M2 --stop -e 'input \"ring.m2\"' -e 'exit 0' "
-- End:
