R = QQ[a..g]
f = a^3+b^2*c+3*f^10*d-1+e-e
support f
A = ZZ[a,b]; B = A[r,s,t]; C = B[x,y,z,w];
f = (a+r+z+1)^2+y
S = support f
ring S_2 === ring f
select(S, x -> index x < numgens C)
indices f
apply(support f, index)
