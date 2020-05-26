-- this example is provided by Amelia Taylor

k = QQ

S = k[x,y,z]
R = S/(x*y-z^2)

use S
P = ideal(x,z)
J = P^2 + ideal R
b = set primaryDecomposition J/(J -> trim promote(J,R))

use R
P = ideal(x,z)
J = P^2 
c = set primaryDecomposition J / trim

assert( #b == #c )


clearAll
kk = QQ
A = kk[s,t]/(s^2-t^3)
B = A[x,y,z,w]/(x^2-s*y-t^2, w*z-s*x^3)
J = ideal(s*y^2-t*x^2)
(A,F) = flattenRing B
G = F^-1
B' = ring presentation A
J' = lift(F J,B')
C = primaryDecomposition J'
assert( intersect C == J')


kk = QQ
A = kk[s,t]/(s^2-t^3)
B = A[symbol x,y,z,w]/(x^2-s*y-t^2, w*z-s*x^3)
J = ideal(s*y^2-t*x^2)
time C = primaryDecomposition J
assert(intersect C == J)

R = ZZ/32003[b,s,t,u,v,w,x,y,z]
I = ideal(
    b*v+s*u,
    b*w+t*u,
    s*w+t*v,
    b*y+s*x,
    b*z+t*x,
    s*z+t*y,
    u*y+v*x,
    u*z+w*x,
    v*z+w*y)
T = R/I
time primaryDecomposition(ideal(0_T))
assert(intersect oo == 0)

-- see https://github.com/jakobkroeker/M2/issues/42
R = QQ[x,y,z]
I = ideal(4*y*z-(1/2)*z,(1/2)*x*y*z+4*z^3+2*x*z,-(5/2)*z^2-z-1)
-- used to not finish in reasonable time:
pd = primaryDecomposition( I, Strategy=>ShimoyamaYokoyama );
assert ( #pd == 1 and pd#0 == I and isPrimary I )


kk = QQ
A = kk[s,t]/(s^2-t^3)
B = A[x,y,z,w]/(x^2-s*y-t^2, w*z-s*x^3)
J = ideal(s*y^2-t*x^2)

J -- an ideal in R
R = ring J
(A,F) = flattenRing R
G = F^-1
B = ring presentation A
J' = lift(F J,B)

C = primaryDecomposition J'


A = QQ[s,z,Degrees=>{{0},{0}}]
factor(s^3*z^3)

A = QQ [x, y, z, w, s, t, Degrees => {{1}, {1}, {1}, {1}, {0}, {0}}, 
     MonomialOrder => {GRevLex => {1, 1, 1, 1}, 
	  Position => Up, GRevLex => {1, 1}}, Heft => {1}]
I = ideal (y^2*s-y*s*t-s^2,x^2-y*s-t^2,-x^3*s+z*w,-t^3+s^2)
p = primaryDecomposition I
assert( I == intersect p )



