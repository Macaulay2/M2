A = QQ[u,v]
B = A/(u^2-v^2,u*v)
C = B[x,y,z]
I = ideal(u*x+v*y+z)
E = C/I
gbTrace=3
res(coker vars E, LengthLimit=>5)
