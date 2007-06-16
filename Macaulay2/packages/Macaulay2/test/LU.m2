permutationMatrix = (p) -> id_(ZZ^#p)^p
M = matrix {{1.0, 3.0, 4.0, 5.0},{2.0, 3.0, 0.0, 1.0}}
M = mutableMatrix(M, Dense=>true)
(P,L,U) = LU M
assert(0 == permutationMatrix P * matrix L * matrix U - matrix M)
