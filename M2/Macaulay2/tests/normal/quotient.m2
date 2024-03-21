M = matrix {{1},{0}}
N = matrix {{0},{1}}
assert(M // N == 0)

M = sub(matrix {{1},{0}}, QQ)
N = sub(matrix {{0},{1}}, QQ)
assert(M // N == 0)

M = sub(matrix {{1},{0}}, ZZ/32003)
N = sub(matrix {{0},{1}}, ZZ/32003)
assert(M // N == 0)

K = GF(32003^2)
M = sub(matrix {{1},{0}}, K)
N = sub(matrix {{0},{1}}, K)
assert(M // N == 0)
