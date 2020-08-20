-- When some of the variables have weight 0,
-- degrees and homogenization have different behavior
A = ZZ [u, v]/(u^2-v^2,u*v)
B = A[x_0, x_1, x_2, x_3]
M = matrix{{v^2*x_2+u}}
assert not isHomogeneous M
Mh = homogenize(M, x_3)
assert isHomogeneous Mh

--status: this is a minor bug that can be fixed later
--status:  probably "homogenize" should multiply the matrix by x_3^3
--status:  to make the degrees turn out right, and give an error message if it
--status:  turns out not to be possible.
M = map(B^1, B^{{-4}}, {{v^2*x_2+u}})
assert not isHomogeneous M
Mh = homogenize(M, x_3)
assert isHomogeneous Mh
