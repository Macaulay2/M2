-- pushForward bug

k = ZZ/3
Q = k[x]
R = Q/(x^3-1)
k' = R^1/(x-1)
f = map(R,Q)
M = pushForward(f,k')
assert( M != 0 )
