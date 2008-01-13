A = GF(9,Variable=>a)
B = A[r,s,t]
generators B
allGenerators B
C = B[x,y,z]/(x^2-a*x-r)
generators C
allGenerators C
D = A[u,v];
F = map(D, C, {0, u, v,  0, a*v+1, 1})
F (x+s*y)
