kk = ZZ/101
A = kk[a..d]
I = ideal"a2,b2,c2,d2"
J = super basis(3,I)
B = kk[x_0..x_15]
phi = map(A,B,J)
L = trim ker phi
C = B/L
M = coker vars C
res(M, LengthLimit=>5)  -- ouch!! crash  CRASHES on gcc 4.7.2, macosx, debug or optimized
betti oo
