-- The purpose of this tutorial is to describe the
-- way Macaulay 2 handles modules, and also to
-- describe some of the operations on modules.

-- Modules in Macaulay 2 are always finitely
-- generated modules over a ring R.

R = ZZ/101[a..d]

-- The simplest modules are the free modules:
F1 = R^3

-- Degrees can be given to each of the generators
-- of the free module (at least if the ring is graded,
-- which is the default for polynomial rings):
F2 = R^{1,2,3}
degrees F2

-- In usual mathematical notation, 
-- F2 == R(1) ++ R(2) ++ R(3), where ++ is direct sum.
-- Macaulay2 uses an alternate notation, where each
-- degree shift is placed in a list, as in the definition
-- of F2.
-- Notice that the
-- degree of the first generator of F2 is -1, NOT 1.
-- This follows current usage in algebraic geometry.

--------------------------
-- Constructing modules --
--------------------------

-- Every module can be written as M = image f / image g,
-- where f : G --> F, g : H --> F are matrices between
-- (graded) free modules.

-- If g is the zero matrix, then M = image f
-- is an 'image module'.
M1 = image matrix{{a},{b}}

-- If f : F --> F is the identity matrix, then M = coker g
-- is a 'cokernel module'.

M2 = coker matrix{{a,b},{c,d}}

-- For the general case, use the 'subquotient' routine
f = matrix{{a},{b}}
g = matrix{{a,b},{c,d}}
M3 = subquotient(f,g)


-- There are a few useful basic operations on modules:
ring F2
generators F2
mingens F2
trim F2
prune F2
cover F2
super F2

F2_{0,1}
F2/(a*F2)

-- Problem: Given a homomorphism (matrix) 
-- m : N <-- M,
-- where M and N are subquotient modules,
-- let M1 and N1 be the cokernel modules of M,N.
-- How does one obtain m1 : N1 <-- M1 ?
-- Answer: since M1 has the same generators as M, and same
-- for N, and since 'matrix m' is the matrix (i.e. between
-- free modules) corresponding to these generators, the
-- answer is
--   map(N1,M1,matrix m)
--
-- Problem: Given a homomorphism (matrix) 
-- m : N <-- M,
-- where M and N are subquotient modules,
-- let M1 and N1 be the corresponding prune'd modules.
-- How does one obtain m1 : N1 <-- M1 ?
--
-- Answer: By the previous problem, we may assume that
-- M and N are cokernel modules.

-- Problem: Given a homomorphism f : N <-- M, determine
-- whether f is an isomorphism, and if so, find the
-- inverse.
--
-- For example, consider the following situation:
R = ZZ/101[a..d]
M = ideal(a,b)/ideal(a^2,b^2)
N = ideal(a,b,a-b)/ideal(a^2,b^2)
prune M
prune N
f = N.fromPrune
g = f^(-1)
h = map(M,N,matrix(R,{{1,0,1},{0,1,-1}}))
source g == target f
target g == source f
o8 * o9
o9 * o8
