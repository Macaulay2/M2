-- here we start testing the incorporation of the new raw engine 
-- types into the top level, one by one

errorDepth 0

-- monoids

monoid [x,y,z]
M = monoid [t,u]
peek t
t.RawMonomial
u.RawMonomial
M.RawMonomialOrdering
M.RawMonoid
N = degreesMonoid M
N.RawMonoid
O = degreesMonoid N
O.RawMonoid
f = t*u^6;
f.RawMonomial
f

-- the printing and sorting of monoid elements is done as though they were raw monomials

-- polynomial ring

R = ZZ[x,y,z,Degrees =>{1,2,3}]
assert( class x === R )
M = monoid R
use R
assert( class x === R )
R.RawRing

assert( raw x === x.RawRingElement )

x*y;
x+y;
x-y;
x//y;
x^3;
-x;
2*x;
1+x;
degree x
degree y
degree z
assert( x != y )
assert( x != 1 )
assert( x^2 == x^2 )
f = 3*x^2 - y^3;
f' = f.RawRingElement
toString f
print f
degree f
print rawPairs f' -- we crash here unless we comment out the previous line
assert not isHomogeneous f
assert isHomogeneous(x^2 + y)
assert( leadCoefficient x === 1 )
assert( leadMonomial x === M_0 )
assert( symbol x === baseName x )
assert( leadCoefficient (2*x^5+33*x^6) === 33 )
assert( leadMonomial (2*x^5+33*x^6) === M_0^6 )
assert( someTerms( (x+1)^5, 2, 2 ) === 10 * (x^3 + x^2) )
assert( leadTerm (x+1)^5 === x^5 )

assert( net f === (stack {"   3     2","- y  + 3x"})^1 )

T = ZZ[a..j, MonomialOrder => {3,3,4}]
T.RawRing
T = ZZ[a..j, MonomialOrder => {3,3,4}, MonomialSize => 8 ]
T.RawRing
T = ZZ[a..j, MonomialOrder => {3,GRevLex => 3,GRevLexTiny}, MonomialSize => 16 ]
T.RawRing

-- fraction field

F = frac R
X = promote(x,F)
Y = promote(y,F)
assert( class (X/Y) == F )
assert( X^2/(X*Y) == X/Y )				    -- ../m2/enginering.m2:163:77: zero divisor found
assert( X^2/(X*Y)-1 === X/Y-1 )

-- polynomial ring over polynomial ring

use R
S = R[t]
assert( class x === R )
x+t

degs = {{1,2},{3,0},{1,0}}
R = ZZ[x,y,z,Degrees =>degs]
assert( degree x == degs_0 )
assert( degree y == degs_1 )
assert( degree z == degs_2 )
assert( isHomogeneous (y + z^3) )
assert( not isHomogeneous (y + z) )

A = ZZ[x,dx,z,WeylAlgebra => {x => dx}]
assert( x*dx + 1 == dx * x )

-- free modules

R = ZZ[x,y,z]
F = R^6
assert( raw F === F.RawFreeModule )
v = 0_F
w = F_1
-- x*w
wx = w*x + F_2
net wx
<< "wx = " << wx << endl
<< "toString wx = " << toString wx << endl

(class F).AfterPrint F
F = R^{3:-3}
(class F).AfterPrint F

-- matrices over ZZ

e = {{4,5}}
g = matrix e
<< "g = " << g << endl
assert ( entries g === e )

-- matrices over QQ

h = matrix {{4/3,5},{4,5}}
assert( entries h === {{4/3,5/1},{4/1,5/1}} )
<< "h = " << h << endl
<< "toString h = " << toString h << endl
<< "2*h = " << 2*h << endl
<< "h*2 = " << h*2 << endl
<< "h+h = " << h+h << endl
<< "h-h = " << h-h << endl
<< "h*h = " << h*h << endl

-- matrices over engine rings
m = vars R
N = source m
print N
assert( degrees N == {{1}, {1}, {1}} )
<< "vars R = " << vars R << endl
<< "toString vars R = " << toString vars R << endl
f = matrix(R, {{x,y^2},{0,3}})
<< "f = " << f << endl
compactMatrixForm = false
<< "f = " << f << endl

L = coker m
print L

K = image m
print K

-- skew commutative rings

-- E = QQ[x..z,SkewCommutative => true]


-- skew commutative rings

E = Schur 4

<< "E_{2,1} = " << E_{2,1} << endl

-- promotion


Q = QQ.RawRing
i = 21 // 34_Q
assert( raw (21/34) === i )


R = ZZ[x]
assert( promote(1, R) == 1_R )

R = QQ[x]
assert( promote(1, R) == 1_R )
assert( promote(1_QQ, R) == 1_R )

-- monomial ideals

R = QQ[x,y]
I = monomialIdeal ( x^2 , x*y, y^3 )
<< "I = " << I << endl
<< "toString I = " << toString I << endl
<< "I*I = " << I*I << endl
