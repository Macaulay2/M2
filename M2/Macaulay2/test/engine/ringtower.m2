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
presentation R                                              -- redesign

-------------------------
A = ZZ[s,t]
B = A/(s^2+t^2-1)
C = B[x,y,z]
describe C
F = x*s*t+2*y^2*s-3
G = s*t-3
lift(G,B)
lift(F,B)

D = C/ideal(x^2-s, y^3-t)
describe D
x
y
assert(0 ==  x^4+y^6-1)

