R = QQ[x]
f = x^5 - 1
Z = roots f
assert Equation(#Z, 5)
assert all(Z, z -> abs f z < 1e-15)
Z' = roots f_(frac R) -- used to segfault
assert all(sort Z, sort Z', (x,y) -> abs(x - y) < 1e-15)
