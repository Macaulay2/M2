R = ZZ/101[a..d]
f = matrix {{a},{b}}
g = matrix {{a,b},{c,d}}
M = subquotient(f,g)
m = basis(2,M) 
betti m
