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
(A,F,G) = flattenRing B
B' = ring presentation A
J' = lift(F J,B')
C = primaryDecomposition J'
assert( intersect C == J')

