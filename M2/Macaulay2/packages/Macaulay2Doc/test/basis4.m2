R = ZZ[a,b,c]
I = ideal(2*a, b^4, c^4)
assert try(basis (R/I);false) else true
assert try(basis R;false) else true
M = R^1/ideal(a^2,b^3,c^4)

assert(numgens source basis M == 24)
assert(numgens source basis (M ++ M) == 48)
assert try(basis (M ++ R^1);false) else true

assert(basis(R^1/ideal(1_R)) == 0)

S = QQ[x]
assert(numgens source basis (S/(x^6)) == 6)
assert(numgens source basis (S^1/ideal(1_S)) == 0)
