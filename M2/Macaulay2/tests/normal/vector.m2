-- constructor methods
assert Equation(vector 1, vector matrix {{1}})
assert Equation(vector {1, 2, 3}, vector matrix {{1}, {2}, {3}})

R = QQ[x,y,z]
assert Equation(vector x, vector matrix {{x}})
assert Equation(vector {x, y, z}, vector matrix {{x}, {y}, {z}})

v = vector matrix {{1_R}}
assert Equation(vector(R, matrix {{1}}), v)
assert Equation(vector(R, {1}), v)
assert Equation(vector(R, 1), v)
assert Equation(vector(R, 1_R), v)

M = image transpose vars R
assert Equation(vector(M, 1), vector map(M,, {{1}}))

N = image vars R
v = vector(N, {1, 2, 3})
w = vector(N, {4, 5, 6})
assert Equation(v, vector map(N,, {{1}, {2}, {3}}))
assert Equation(v, matrix v)
assert Equation(matrix v, v)

assert zero vector {0, 0, 0}

-- module operations
assert Equation(+v, v)
assert Equation(v + w, vector(N, {5, 7, 9}))
assert Equation(-v, vector(N, {-1, -2, -3}))
assert Equation(v - w, vector(N, {-3, -3, -3}))
assert Equation(2 * v, vector(N, {2, 4, 6}))
assert Equation(x * v, vector map(N, R^{-1}, {{x}, {2*x}, {3*x}}))
assert Equation(v * 2, vector(N, {2, 4, 6}))
assert Equation(v * x, vector map(N, R^{-1}, {{x}, {2*x}, {3*x}}))
assert Equation(v / 2, vector(N, {1/2, 1, 3/2}))
v = vector {x, y, z}
kk = frac R
assert Equation(v / x, vector map(kk^3, kk^{-1}, {{1}, {y/x}, {z/x}}))
