R = QQ[x]
f = n -> image x^n
g = (m,n) -> f m / f n

h = inducedMap(f 1 / f 4, f 2 / f 5, Verify => true)
assert (target h == f 1 / f 4)
assert (source h == f 2 / f 5)
assert (coker h == f 1 / f 2)
assert isHomogeneous h

h = inducedMap(f 1 / f 11, f 2 / f 13, matrix {{x^2}}, Verify => true)
assert (coker h == f 1 / f 4)

h = inducedMap(f 1, f 2, Verify => true)
assert (target h == f 1)
assert (source h == f 2)
assert (coker h == f 1 / f 2)
assert isHomogeneous h

h = inducedMap(R^1 / f 4, R^1 / f 5, Verify => true)
assert (coker h == 0)
assert isHomogeneous h
