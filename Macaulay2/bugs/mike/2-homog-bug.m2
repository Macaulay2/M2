-- When some of the variables have weight 0,
-- degrees and homogenization have different behavior
A = ZZ [u, v]/(u^2-v^2,u*v)
B = A[x_0, x_1, x_2, x_3]
M = map(B^1, B^{{-4}},
  {{v^2*x_2+u}})
M = matrix{{v^2*x_2+u}}

Mh = homogenize(M, x_3) -- the weight vector being sent to the engine has length 4, which gives u and v weight 0.
assert isHomogeneous Mh -- but the degrees are somehow all 1.
assert not(Mh == M)


N = map(B^1, B^{{-4}},
  {{v^2*x_2+u}})
Nh = homogenize(N, x_3)
