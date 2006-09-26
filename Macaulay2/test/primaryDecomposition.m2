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
