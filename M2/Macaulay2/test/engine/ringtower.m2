-- test towers of rings at top level
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
k = GF(9)
toExternalString k                                          -- broken
R = k[t]
toExternalString R
presentation R                                              -- redesign
