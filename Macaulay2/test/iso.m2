f = matrix {{3,0},{0,60}}
g = matrix {{60,0},{0,3}}
p = matrix {{0,1},{1,0}}
M = coker f
assert( presentation M == f )
N = coker g
assert( presentation N == g )
t = map(M,N,p)
assert isWellDefined t
assert isIsomorphism t
