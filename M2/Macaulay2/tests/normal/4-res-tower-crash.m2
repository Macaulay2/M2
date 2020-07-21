A = QQ[u,v]
B = A/(u^2-v^2,u*v)
C = B[x,y,z]
I = ideal(u*x+v*y+z)
E = C/I
lim = 5
C = res(coker vars E, LengthLimit=>lim)
betti C
scan(1 .. lim-1, i -> assert( HH_i C == 0 ))
