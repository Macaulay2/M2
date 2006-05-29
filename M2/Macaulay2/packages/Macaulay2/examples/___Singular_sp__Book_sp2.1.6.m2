A = QQ[x,y,z];
M = matrix{{1, x+y, z^2},
           {x, 0,   x*y*z}}
N = matrix(A, {{1,2,3},{4,5,6},{7,8,9}})
M+M
x*N
M*N
N^3
((x+y+z)*N)^3
M_(1,2)
M1 = mutableMatrix M
M1_(1,2) = 37_A
M1
matrix M1
M | M
M || N
ideal M
F = A^5
id_(A^5)
matrix{{1,2,3},{4,5,6}}
