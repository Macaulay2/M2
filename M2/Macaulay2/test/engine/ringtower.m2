-- test towers of rings at top level

A = ZZ[x]
C = A/(2*x+1)
D = C[z]                                                    -- the engine ignores the relations defining C in constructing D
raw D
assert ( 2*x*z === -z )
