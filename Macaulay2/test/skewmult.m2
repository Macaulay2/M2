S = (ZZ/101 [a, b, c, d, SkewCommutative => true])/(a*b,c*d)
A = matrix {{0, c, 0, b, 0, a}, {c, 0, b, 0, a, 0}, {b, a, 0, 0, 0, 0}}
B = matrix {{0}, {0}, {0}, {0}, {0}, {0}, {a}, {0}, {0}, {0}, {c}, {0}}
A * B

--A = A_{6..11}
B0 = submatrix(B,{6..11},)  -- BUG!!
B = submatrix(B,{6..11},{0})
A*B
