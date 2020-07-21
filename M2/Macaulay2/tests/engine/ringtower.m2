--status: this old test depends on internal things and probably should be deleted


-- test towers of rings at top level

load "raw-util.m2"
errorDepth = 0

A = ZZ[x]
C = A/(2*x+1)
D = C[z]
raw D
E = D/(x*z)
assert ( 2*x*z === -z )

---------
k = GF(9)
R = k[t]
presentation R -- redesign?

-------------------------
A = ZZ[s,t]
B = A/(s^2+t^2-1)
C = B[x,y, symbol z]
describe C
F = x*s*t+2*y^2*s-3
G = s*t-3
lift(G,B)
try ( lift(F,B) ; error "oops, lift should fail" )

D = C/ideal(x^2-s, y^3-t)
describe D
x
y
assert(0 ==  x^4+y^6-1)

--------------------------
A = ZZ[x]
B = A[y]
A === ring x
x1 = promote(x,B)
f = (x1+y)^3
f == (x+y)^3
assert(leadCoefficient(x1+y) == 1_A)
g = x1*y^3+(1+x1)*y
assert(2 == size g)

leadCoefficient(g) == x
leadMonomial(g)
someTerms(g,0,1) -- the lead term
someTerms(g,1,1)
someTerms(g,2,1)
listForm g
leadMonomial y
g _ (leadMonomial y) == x+1
h2 = promote((x+1)^3,B) * y^3
h1 = (x1+1)^3 * y^3
assert(h1 == h2)

A = ZZ[r,s]
B = A[x,y,z]
C = B[X,Y,Z]
ring X === C
x = promote(B_0,C)
r = promote(r,C)
ring x === C
ring r === C
(r+s+x+X)^3
-------------------------------
-- translate this is remove raw stuff
A = polyring(rawZZ(), {symbol a, symbol b, symbol c})
stderr << "warning: flattening not rewritten yet" << endl
B = polyring(A, {symbol x, symbol y, symbol z, symbol a, symbol b, symbol c})
a = rawPromote(B,a)
ring a === B
b = rawPromote(B,b)
c = rawPromote(B,c)
f = (a+b+1)*(x^2-y*z^5-1)
f^2
rawFromNumber(B,3462346246246246263287642) * c
assert(rawTermCount(3,f) === 3)
assert(rawGetTerms(3,f,1,1) === (a+b+1)*x^2)
assert((a+b+x+y)^2 === (a+b+x+y)*(a+b+x+y))
assert (rawWeightRange({1,0,0},f) == (0,2))
assert (rawWeightRange({0,0,1},f) == (0,5))
 
assert(rawWeightRange({1,0,0,3,4,5},f) == (0,6))
assert (rawMultiDegree f == {7}) -- because it is flattened, and 'polyring' gives everything degree 1.

-- The following is hard to do using raw routines, so do the same at top level. 
-- assert not rawIsHomogeneous f
-- assert rawIsHomogeneous (a^100*x^2-b*x*z-z^2)
-- rawHomogenize(a*x-y^3-1, 2, {1,1,1})
-------------------------------

-- Polynomial rings ZZ[a,b][c,d],[e,f]
a = symbol a;
b = symbol b;
c = symbol c;
d = symbol d;
e = symbol e;
f = symbol f;
A = ZZ[a,b]
B = A[c,d]
C = B[e,f]

h = 3*a^3-7*b^5
promote(h,B)
promote(h,C)
promote(h,C) == 3*(promote(a,C))^3 - 7*(promote(b,C))^5

-- Polynomial rings QQ[a,b][c,d],[e,f]
A = QQ[a,b]
B = A[c,d]
C = B[e,f]

h = 3*a^3-7*b^5
promote(h,B)
promote(h,C)
promote(h,C) == 3*(promote(a,C))^3 - 7*(promote(b,C))^5

