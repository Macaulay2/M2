F = frac(QQ[x])
assert(basis F^6 == id_(F^6))
R = F[]
assert(basis R^6 == id_(R^6))

A = ZZ/101[x]
R = A[y]
assert(basis(1, 2, R, SourceRing => A) == map(R^1, A^2, map(R, A), matrix{{y, y^2}}))
assert(basis(2, 1, R, SourceRing => A) == map(R^1, A^0, map(R, A), {}))

R = ZZ[a, b]
assert(basis(0, 1, R) == matrix{{1, a, b}})
assert(basis(0, 2, R) == matrix{{1, a, a^2, a*b, b, b^2}})
assert(basis(0, 1, R) == matrix{{1, a, b}})

-- c.f. examples "part(List,Complex)"
R = QQ[a..d]
M = coker matrix {{a*b, a*c, b*c, a*d}}
N = image map(R^{4:-2}, , {{-c, -c, 0, -d}, {b, 0, -d, 0}, {0, a, 0, 0}, {0, 0, c, b}})
P = image map(R^{4:-3}, , {{d}, {0}, {-c}, {b}})
assert(basis(4, map(R^0, M, 0)) == 0)
assert(basis(4, map(M,   N, 0)) == 0)
assert(basis(4, map(N,   P, 0)) == 0)
assert(basis(4, map(P, R^0, 0)) == 0)

-- partial multidegrees
-- see https://github.com/Macaulay2/M2/pull/2056
assert(basis({3}, A = ZZ/101[a..d, Degrees=>{2:{1,2},2:{0,1}}]) == matrix"a3,a2b,ab2,b3")
assert(basis({3}, B = ZZ/101[a..d, Degrees=>{2:{1,2},2:{0,1}}]/ideal(c^2,d^2)) == matrix"a3,a2b,ab2,b3")
--
assert(basis({3},    R = QQ[x,y,Degrees=>{{1,0},{0,1}}]/ideal(y^2)) == matrix"x3")
assert(basis({3},    R = QQ[x,y,Degrees=>{1,0}]/ideal(y^2))         == matrix"x3,x3y")
assert(basis({3, 0}, R = QQ[x,y,Degrees=>{{1,0},{0,0}}]/ideal(y^2)) == matrix"x3,x3y")
-- can this output be replicated with Variables?
assert(basis({3},    R = QQ[x,y,Degrees=>{{1,1},{1,2}}]) == matrix"x3,x2y,xy2,y3")

-- Variables
assert(basis({3, 0}, R = QQ[x,y,Degrees=>{{1,0},{0,1}}]/ideal(y^2), Variables => {0}) == matrix"x3")
assert(basis({0, 3}, R = QQ[x,y,Degrees=>{{1,0},{0,1}}]/ideal(x^2), Variables => {1}) == matrix"y3")

-- TODO: is this correct?
R = QQ[x, Degrees => {{}}, DegreeRank => 0]
assert try (basis_{} R; false) else true

-- https://github.com/Macaulay2/M2/issues/909
R = ZZ/101[a..d, Degrees => {4:0}]
assert try (basis R; false) else true
assert(basis(-1, R) == 0)
assert(basis(0, R) == gens R^1) -- ignores degree 0 vars
assert try (basis(0, R, Variables => {0}); false) else true
assert(basis(1, R) == 0)

R = ZZ/101[a..d, Degrees => {4:{2:0}}]
assert try (basis R; false) else true
assert(basis(-1, R) == 0)
assert(basis(0, R) == gens R^1) -- ignores degree 0 vars
assert try (basis(0, R, Variables => {0}); false) else true
assert(basis(1, R) == 0)

S = ZZ/101[s,t]
R = S[a..d]
assert(basis({0,1}, R) == sub(vars S, R))
assert(basis({1,0}, R) == vars R)

-- FIXME: these are also broken
R = ZZ/101[a,b, Degrees => {0,1}]
basis(1, R)
-- basis(1, R, Variables => {a,b})
basis(1, R, SourceRing => coefficientRing R)

-- this would be non-finite, so we ignore degree 0 vars
assert(basis(0, A = ZZ/101[a, Degrees => {0}]) == gens A^1)
-- FIXME: assert(basis A == gens A^1)
-- this is finite over the field, so we include all vars
assert(basis(0, A = ZZ/101[a, Degrees => {0}]/ideal(a^3)) == matrix"1,a,a2")
assert(basis A == matrix"1,a,a2")
assert(basis id_(A^1) == id_(image basis A))
assert(basis map(A,A) == id_(image basis A))

A = quotient ideal conwayPolynomial(2, 3)
B = image basis A
assert(basis map(A, A, {a^2}) == map(B, B, {{1, 0, 0}, {0, 0, 1}, {0, 1, 1}}))

-- https://github.com/Macaulay2/M2/issues/1312
R = QQ[]
assert(basis R == id_(R^1))
assert(basis_{} R == id_(R^1))
-- a temporary workaround was added to make these work
-- the ideal solution would be in the engine
assert(basis_-1 R == 0)
assert(basis_0 R == id_(R^1))
assert(basis_1 R == 0)
-- the difference between QQ and QQ[] is that QQ[] can be twisted
M = R^{-3,2,2,5}
assert(basis M == id_(cover M))
assert(basis_0 M == 0)
assert(basis_-2 M == map(M, R^{2,2}, {{0, 0}, {1, 0}, {0, 1}, {0, 0}}))
assert(basis(0, 3, M) == map(M, R^{-3}, {{1}, {0}, {0}, {0}}))
-- https://github.com/Macaulay2/M2/issues/2003
S = QQ[x,y,z]
M = S^1/ideal vars S
assert(degrees Ext(M, M) == splice {{0, 0}, 3:{-1, -1}, 3:{-2, -2}, {-3, -3}})

-- FIXME: arbitrary SourceRings don't behave well
S = QQ[x,y,z]
R = S[]
basis({0,1}, R, SourceRing => QQ)
basis({0,1}, R, SourceRing => S)
basis({0,1}, R, SourceRing => R)

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
-- this is repeated to ensure that caching works fine
assert(truncate({0}, m) == truncate({0}, m))

R = QQ[a,b,c, Degrees => {1,2,3}];
assert(basis(2, R) == matrix"a2,b")
-- FIXME: assert(basis(2, R, Truncate => true) == matrix "b,a2,c")
assert(basis(2, R) == matrix"a2,b")

-- tests for basis of ring maps
R = ZZ/32003[s,t][a..d]
f = map(A := coefficientRing R, R, DegreeMap => d -> take(d, - degreeLength A))
assert(entries basis(({2}, {0,2}), f) == entries id_(ZZ^3))

f = map(QQ[s,t], QQ[w,x,y,z], matrix"s3,s2t,st2,t3")
b = basis((6, 2), f)
-- FIXME: this is very non-minimal, but ker b doesn't work if f is homogeneous ...
K = image map(source b, , gens ker b)
-- FIXME: why isn't ker b a submodule of source b already?
-- assert isSubset(ker b, source b)
assert isSubset(K, source b)
assert(ker f == K)
