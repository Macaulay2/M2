F = frac(QQ[x])
assert(basis F^6 == id_(F^6))
R = F[]
assert(basis R^6 == id_(R^6))

M = image random(QQ^5, QQ^3)
assert(basis M == map(M, QQ^3, id_(QQ^3)))
assert(basis M == map(M, QQ^3, id_(QQ^3)))

M = image random(ZZ^5, ZZ^3)
assert(basis M == map(M, ZZ^3, id_(ZZ^3)))
assert(basis M == map(M, ZZ^3, id_(ZZ^3)))

A = ZZ/101[x]
R = A[y]
assert(basis(1, 2, R, SourceRing => A) == map(R^1, A^2, map(R, A), matrix{{y, y^2}}))
assert(basis(2, 1, R, SourceRing => A) == map(R^1, A^0, map(R, A), {}))

R = ZZ[a, b]
assert(basis(0, 1, R) == matrix{{1, a, b}})
assert(basis(0, 2, R) == matrix{{1, a, a^2, a*b, b, b^2}})
assert(basis(0, 1, R) == matrix{{1, a, b}})

-- TODO: is this correct?
R = QQ[x, Degrees => {{}}, DegreeRank => 0]
assert try (basis_{} R; false) else true

-- https://github.com/Macaulay2/M2/issues/909
R = ZZ/101[a..d, Degrees => {4:0}]
assert try (basis R; false) else true
assert(basis(-1, R^1) == 0)
-- FIXME: this needs to be fixed in the engine
-- assert try (basis_0 R; false) else true
assert(basis(1, R^1) == 0)

-- https://github.com/Macaulay2/M2/issues/1312
R = QQ[]
assert(basis R == id_(R^1))
assert(basis_{} R == id_(R^1))
-- unfortunate that this doesn't work:
-- assert(basis_0 R == id_(R^1))

R = QQ[x]
assert try (basis R; false) else true
assert(basis_-1 R == 0)
assert(basis_0 R == id_(R^1))
assert(basis_1 R == matrix{{x}})

R = QQ[x, Degrees => {{0}}]
assert try (basis R; false) else true
assert(basis_-1 R == 0)
assert(basis_0 R == id_(R^1))
assert(basis_1 R == 0)

needsPackage "Truncations"
R = QQ[a,b,c];
m = map(R^{{-2}, {-2}, {-2}},R^{{-3}, {-3}},{{-c, 0}, {b, -b}, {0, a}})
assert(truncate({0}, m) == truncate({0}, m))

R = QQ[a,b,c, Degrees => {1,2,3}];
assert(basis(2, R) == matrix"a2,b")
assert(basis(2, R, Truncate => true) == matrix"a2,ab,ac,b,c")
assert(basis(2, R) == matrix"a2,b")
