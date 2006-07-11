R = ZZ[x,y]
f = random(R^2,R^{4:-2})
gbTrace = 3
M = coker f
C = res M
b = betti C
assert( length C <= 3 )
assert ( prune HH_1 C == R^0 )
assert ( prune HH_2 C == R^0 )
assert ( prune HH_3 C == R^0 )
assert ( isIsomorphism map(M,HH_0 C, id_(cover M)) )
