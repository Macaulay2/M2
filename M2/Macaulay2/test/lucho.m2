A = ZZ/101[x,y]
f = random (A^3, A^{-2,-2})
M = cokernel f
m = x^3
n = y^4
R = cokernel matrix {{m, n}}
N = prune (M**R)
C = resolution N
d = C.dd
i = id_C
s = nullhomotopy (m*i)
t = nullhomotopy (n*i)

assert( d^2 == 0 )
assert( d*s + s*d == m )
assert( d*t + t*d == n )
assert( s^2 == 0 )
assert( t^2 == 0 )
assert( t*s + s*t == 0 )
