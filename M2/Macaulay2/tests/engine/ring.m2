-- -*- coding: utf-8 -*-
--status: this old test depends on internal things and probably should be deleted



-- gbTrace = 1
oops := () -> stderr << "warning: segmentation fault commented out" << endl
chk  := () -> stderr << "warning: error commented out" << endl
cmp = method()
cmp(HashTable,HashTable) := (x,y) -> new HashTable from merge (x,y,(a,b) -> if a === b then "equal" else (a,b))
load "raw-util.m2"

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
R2 = ZZ[x,y,z,t]
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

assert( raw 0_R == 0 )

-- exponents x

R'= raw R
M' = raw monoid R
2_R
raw 2_R
raw ZZ
rawLift(raw ZZ, raw 2_R)
rawToInteger rawLift(raw ZZ, raw 2_R)
lift(2_R, ZZ)

m' = rawLeadMonomial (3, raw(x^2 * y))
n' = rawLeadMonomial (3, raw(x * y * z))
rawCompareMonomial(M',m',n')
rawSparseListFormMonomial m'

exponents(3,m')
exponents(3,n')

m = leadMonomial (x^2 * y)
n = leadMonomial (x * y * z)
m ? n

f = (x+y+1)^3

-- listForm f
-- standardForm f


-- factory

f = x^3+6*x+11*y^2+1
g = x^4+6*x+13*y
d = x^5+6*x^2+17

assert( d === gcd(f*d,g*d) )

pR = (f,g) -> rawPseudoRemainder(raw f, raw g)
assert( pR(x^2+y+11,x) == y+11 )
assert( pR(f*d,d) == 0 )

f
g
rawFactor raw (f*g)
factor(f*g)

rawCharSeries raw vars R

-- modules
M'= raw monoid R
F = R^3

assert ( degrees F == {{0}, {0}, {0}} )
assert not mutable F
assert ( F === R^3 )

assert(raw F === raw R^3)
assert(raw F =!= raw R^4)

G = F**F -- we no longer cache the results of tensor products of modules
assert( G === F**F )

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
assert( v' ++ v === v' ++ v )

v ** v
v ** v'
assert( v ** v' === v ** v' )

2*v
v*2
assert ( 2*v === v*2 )

raw x * raw v
x*v

t = matrix {{x,y,z},{y,z,x},{z,x,y}}

assert( matrix({{v,v},{v,v}}) === v | v || v | v )
assert( source matrix({{v,0},{0,v}}) === source (v ++ v) )
assert( target matrix({{v,0},{0,v}}) === target (v ++ v) )
assert( matrix({{v,0},{0,v}}) === v ++ v )

toString submatrix(t,{1,2})
assert( submatrix(t,{1,2}) === matrix {{y,z},{z,x},{x,y}} )
assert( submatrix(t,{1,2},{2,0}) === matrix {{x, y}, {y, z}} )
assert( submatrix(t,,{2,0}) === matrix {{z, x}, {x, y}, {y, z}} )

-- making modules from matrices
N = coker v
L = image v
P = N ++ L
assert( N ++ L === N ++ L )
peek P.cache
isHomogeneous P
peek P.cache

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

-- reshaping matrices

f = matrix {{1,x,x^2}}
g = matrix {{1,x^5}}
assert ( adjoint'(dual f ** g, source f, target g) === g ** f )

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

assert( matrix {{X}} * matrix {{Y}} - matrix {{Y*X}} == 0)

-- Gröbner bases
f = matrix {{x,y^2,z^3}}
I = image f
gb (I,DegreeLimit => -1)
gb (I,DegreeLimit => 0)
gb (I,DegreeLimit => 1)
gb (I,DegreeLimit => 2)
gb (I,DegreeLimit => 3)
gb (I,DegreeLimit => 4)

G = gb I
peek G
g = gens G
raw G
g = gens G
assert( f === g )
G2 = gb vars R2

assert try ( matrix {{x^2,y}} % G2; false ) else true

matrix {{x^2,y}} % G
syz f

peek f.cache

status G

-- rawGBContains

i = rawGBContains( raw gb matrix {{x,y}} , raw matrix {{0,0,x,y,0,0}} )
assert ( i == -1 )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test/engine ring.out"
-- compile-command: "echo 'input \"ring.m2\"' | M2 -q --stop"
-- End:
