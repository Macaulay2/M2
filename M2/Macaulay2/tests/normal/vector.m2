-- constructor methods
assert Equation(vector 1, vector matrix {{1}})
assert Equation(vector {1, 2, 3}, vector matrix {{1}, {2}, {3}})

R = QQ[x,y,z]
assert Equation(vector x, vector matrix {{x}})
assert Equation(vector {x, y, z}, vector matrix {{x}, {y}, {z}})

M = image transpose vars R
assert Equation(vector(M, 1), vector map(M,, {{1}}))

N = image vars R
v = vector(N, {1, 2, 3})
w = vector(N, {4, 5, 6})
assert Equation(v, vector map(N,, {{1}, {2}, {3}}))

-- module operations
assert Equation(+v, v)
assert Equation(v + w, vector(N, {5, 7, 9}))
assert Equation(-v, vector(N, {-1, -2, -3}))
assert Equation(v - w, vector(N, {-3, -3, -3}))
assert Equation(2 * v, vector(N, {2, 4, 6}))
assert Equation(x * v, vector map(N, R^{-1}, {{x}, {2*x}, {3*x}}))
assert Equation(v * 2, vector(N, {2, 4, 6}))
assert Equation(v * x, vector map(N, R^{-1}, {{x}, {2*x}, {3*x}}))
