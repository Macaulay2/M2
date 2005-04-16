R = ZZ/101[x_0 .. x_3,y_0 .. y_3]
m = matrix table (2, 2, (i,j) -> x_(i+2*j))
n = matrix table (2, 2, (i,j) -> y_(i+2*j))
f = flatten (m*n - n*m)
poincare cokernel f
