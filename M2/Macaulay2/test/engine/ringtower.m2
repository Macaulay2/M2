-- test towers of rings at top level

load "raw-util.m2"
errorDepth = 0

A = ZZ[x]
toExternalString A
C = A/(2*x+1,11)
toExternalString C                                          -- broken
D = C[z]                                                    -- the engine ignores the relations defining C in constructing D
toExternalString D
raw D
E = D/(x*z)
toExternalString E                                          -- broken
assert ( 2*x*z === -z )

---------
k = GF(9)
toExternalString k                                          -- broken
R = k[t]
toExternalString R
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

