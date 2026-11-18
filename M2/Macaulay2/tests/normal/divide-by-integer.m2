-- rational numbers map to ZZ/p when p doesn't divide the denominator
assert((1/2)_(ZZ/7) == 4_(ZZ/7))
assert(try (1/7)_(ZZ/7) then false else true)
S = ZZ/5[x]
assert(x/2 == 3*x)
assert(try x/5 then false else true)
assert(3_(ZZ/5)/2 == 4_(ZZ/5))
assert(matrix {{x, 1}}/2 == matrix {{3*x, 3}})
assert(try matrix {{x}}/5 then false else true)
F = GF 9
assert((F_0/2) * 2 == F_0)
Q = ZZ/5[y]/(y^2)
assert(y/2 == 3*y)

-- characteristic zero
Z = QQ[z]
assert(z/2 == (1/2)*z)
m = matrix {{2, 4}}/2
assert(ring m === QQ and m == matrix(QQ, {{1, 2}}))
assert(try (ZZ[w])_0/2 then false else true)
