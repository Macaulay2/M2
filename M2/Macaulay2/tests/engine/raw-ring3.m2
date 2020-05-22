--status: this old test depends on internal things and probably should be deleted


-- Test of towers of quotient rings --

A = ZZ[a,b,c]
f = a+b+c

B = A[x,y]
f = (a+b)*x + (2*a-3*c)*y^4

assert(listForm f === {({0,4},2*a-3*c), ({1,0},a+b)})
assert(size f === 2)
assert(leadMonomial f == leadMonomial (y^4))
assert(leadCoefficient f == 2*a-3*c)
f_x == a+b
assert(someTerms(f,1,1) == (a+b)*x)
assert(a*x > b*x)
assert((x ? y) === symbol>)

C = B[t,u,v]
f = (a+b)*(x+y)*t*u
leadCoefficient f

needs "raw-util.m2"
errorDepth = 0

A = ZZ[x]
B = A[y]
use A
C = A/(2*x^3-3*x-1)

D2 = ZZ[y,x,MonomialOrder=>{1,1}]/(2*x^3-3*x-1)
D2/(x*y-x-1)

R = ZZ[x,y,z]
assert(rawAmbientRing raw R === raw R)
assert(rawDenominatorRing raw R === null)

A = R/(x^2+3*y^3-4)
describe A
toString A
toExternalString A

R2 = ZZ[x,y,z]
assert(rawAmbientRing raw A === raw R)
assert not (rawAmbientRing raw A === raw R2)
assert(rawDenominatorRing raw A === null)

B = R[a,b,c]
rawAmbientRing raw B

R = ZZ[x,y,z]
S = R[a,b,c]
ambS = ZZ[a,b,c,x,y,z,Degrees=>{1,1,1,0,0,0},MonomialOrder=>{3,3}]
--ambS = ZZ[a,b,c,x,y,z,MonomialOrder=>{3,3}]

use R
A = R/(x^2+3*y^3-4)

use A
m = ideal(3*x-4, y*z-3)
gens gb m
m1 = substitute(gens gb m, ambS)
A1 = ambS/ideal(m1)
presentation A1
describe A1
toString A1
A2 = ZZ [a, b, c, x, y, z, MonomialOrder => {3, 3}]/(3*x-4,20*z^2-243*y,y*z-3,27*y^2+x*z-8*z,x*z^2-8*z^2+81*y,x^2*z+9*y^2-4*z,x^3+y^3-x^2-x)
use A1
n = ideal(a*x+b*y+c*z)
n1 = gens gb n

n2 = substitute(n1, S)
A2 = S/ideal(n2)

-------------------
-- Test: degrees in tower of rings
A = ZZ[symbol a,b]
B = A[x,y]
raw B
C = ZZ[s,t][x,y]
raw C
f = 11 + 2*s^3 + 3*t^4 + 4*x + 5*y
monomials(Variables => 0..3,f)
coefficients(Variables => 0..3,f)
