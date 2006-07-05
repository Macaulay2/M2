-- When some of the variables have weight 0,
-- degrees and homogenization have different behavior
A = ZZ [u, v]/(u^2-v^2,u*v)
B = A[x_0, x_1, x_2, x_3]
M = matrix{{v^2*x_2+u}}
assert not isHomogeneous M
Mh = homogenize(M, x_3)
assert isHomogeneous Mh

M = map(B^1, B^{{-4}}, {{v^2*x_2+u}})
assert not isHomogeneous M
Mh = homogenize(M, x_3)
assert isHomogeneous Mh


N = map(B^1, B^{{-4}}, {{v^2*x_2+u}})
assert not isHomogeneous N
Nh = homogenize(N, x_3)
assert isHomogeneous Nh

