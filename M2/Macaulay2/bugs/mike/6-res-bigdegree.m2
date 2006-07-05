R = ZZ/32003[x,y,z]
M = matrix{{x^1000000000,y^1000000000,z^1000000000}}
gbTrace=3
res(coker M, Strategy=>2)

-- This is also a monomial overflow situation: which it finds, perhaps not so gracefully.
syz M
syz oo
