R = QQ
M = mutableMatrix(QQ,3,5)
M_(0,0) = 1_QQ
M_(2,4) = 3/4
M_(-1,-2) = 7
assert(M == mutableMatrix {{1,0,0,0,0}, {0,0,0,0,0}, {0,0,0,7,3/4}})
debug Core
rawSubmatrix(raw M,(0,1),(0,1))

R = QQ[x,y,z]
f = vars R
f = f ++ f
m = mutableMatrix f;
m == mutableMatrix f
entries m
toString m
net m
m
assert(m_(0,2) == z)
m_(1,2) = x+y
assert(m_(1,2) == x+y)
assert(m_(-1,-1) == z)
assert(not (m == mutableMatrix f))
m
rowSwap(m,0,1)
assert (entries m == {{0, 0, x + y, x, y, z}, {x, y, z, 0, 0, 0}})
columnSwap(m,4,3)
assert(entries m == {{0, 0, x + y, y, x, z}, {x, y, z, 0, 0, 0}})

m
m = mutableMatrix({{0, 0, x + y, y, x, z}, {x, y, z, 0, 0, 0}}, Dense=>true)
numcols m
numrows m
rowMult
columnMult
rowAdd
columnAdd
-- 2by2's?
-- 
rowPermute
columnPermute

-- what else?
mutableIdentity
mutableMatrix
randomMutableMatrix

-- determinant, rank, inverse

R = QQ[a,b,c,d]
M = mutableMatrix {{a, b}, {c, d}}
assert(det M == a*d - b*c)
assert(rank M == 2)

-- LUdecomposition, solve, eigenvalues, eigenvectors, SVD, leastSquares, LLL

R = GF(25)
M = mutableMatrix {{a, 1}, {0, 1}}
(P, L, U) = LUdecomposition M
Q = mutableMatrix id_(R^2)_P
assert(L_(0, 1) == 0)
assert(U_(1, 0) == 0)
assert(Q * L * U == M)

-- issue #3709 (these used to crash M2)
assert try ( mutableMatrix(ZZ, -1, -1); false ) else true
assert try ( mutableMatrix(ZZ, -1,  1); false ) else true
assert try ( mutableMatrix(ZZ,  1, -1); false ) else true
assert try ( mutableIdentity(ZZ, -1);   false ) else true

assert(ring mutableMatrix(ZZ/101, {{1,2,3}}) === ZZ/101)

-- target and source
M = mutableMatrix {{1, 2, 3}, {4, 5, 6}}
assert Equation(target M, ZZ^2)
assert Equation(source M, ZZ^3)

-- submatrices
assert Equation(M_{0, -1}, mutableMatrix {{1, 3}, {4, 6}})
assert Equation(M^{-1}, mutableMatrix {{4, 5, 6}})

-- issue #997
A = mutableMatrix {{10}}
B = submatrix(A, {0}, {0})
assert(hash A != hash B)
