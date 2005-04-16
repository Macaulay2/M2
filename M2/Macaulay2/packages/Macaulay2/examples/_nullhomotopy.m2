A = ZZ/101[x,y];
M = cokernel random(A^3, A^{-2,-2})
R = cokernel matrix {{x^3,y^4}}
N = prune (M**R)
C = resolution N
d = C.dd
s = nullhomotopy (x^3 * id_C)
s*d + d*s
s^2
