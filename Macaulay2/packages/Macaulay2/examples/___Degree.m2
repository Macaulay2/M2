R = ZZ/101[x]
p = map(R^1, R^1, {{x^4}})
isHomogeneous p
q = map(R^1, R^1, {{x^4}}, Degree => 4)
isHomogeneous q
