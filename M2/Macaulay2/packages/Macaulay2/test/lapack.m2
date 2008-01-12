diag = method()
diag(ZZ,ZZ,List) := (a,b,L) -> (
     R := ring L#0;
     M := mutableMatrix(R,a,b);
     scan(#L, i -> M_(i,i) = L#i);
     matrix M)
diag(List) := (L) -> (
     R := ring L#0;
     M := mutableMatrix(R,#L,#L);
     scan(#L, i -> M_(i,i) = L#i);
     matrix M)

v = solve(matrix {{1.0, 2.01}, {3., 4.}, {5., 8.}},matrix {{13.01}, {29.01}, {55.01}},ClosestFit=>true,MaximalRank=>true)
assert (norm(v - matrix {{3.}, {5.}}) < .1)

checkLU = method()
checkLU(List,Matrix,Matrix) := (P,L,U) -> (
     R := ring L;
     Q = id_(R^#P) _ P;
     --Q = mutableMatrix(R, numrows L, numrows L);
     --for i from 0 to numrows L - 1 do Q_(i,P_i) = 1_R;
     --Q = matrix Q;
     Q*L*U)
checkLU Matrix := (M) -> norm (checkLU time LU M - M)

checkSVD = method()
checkSVD(Matrix) := (M) -> (
     -- Compute the SVD, check assertions
     -- return (S,U,Vt)
     (S,U,Vt) := SVD M;
     (S1,U1,Vt1) := SVD(M, DivideConquer=>true);
     nm1 := norm(M - U * diag(numgens target M, numgens source M, flatten entries S) * Vt);
     nm2 := norm(transpose U - U^-1);
     nm3 := norm(transpose Vt - Vt^-1);
     nm4 := norm(S-S1);
     nm5 := norm(U-U1);
     nm6 := norm(Vt-Vt1);
     (max(nm1,nm2,nm3), max(nm4,nm5,nm6))
     )

checkSolve = method()
checkSolve(Matrix,Matrix) := (M,b) -> (
     x := solve(M,b);
     norm (M*x-b))

checkClosestFit = method()
checkClosestFit(Matrix,Matrix) := (M,b) -> (
     x := solve(M,b,ClosestFit=>true,MaximalRank=>true);
     norm (M*x-b))

checkEigenvectors = method()
checkEigenvectors Matrix := (M) -> (
     -- compute eigenvectors, and eigenvalues
     E2 := eigenvalues M;
     (E,V) := eigenvectors M;
     n1 := norm(E-E2); -- make sure these are the same
     -- for each eigenvalue and eigenvector, compute Mv-ev
     (E,V,n1);
     M*V - diagonalMatrix first entries E -- should be small...
     )

----------------
-- Test LU -----
----------------
A = RR
M = mutableMatrix random(A^4,A^2)

M = matrix{{1.5,2.0},{1.3,1.7},{1.6,.5}}
assert(checkLU M == 0)
M = transpose M
assert(checkLU M)
M = matrix{{1.5,2.0},{1.3,1.7}}
assert(checkLU M)

A = mutableMatrix(CC,5,4, Dense=>true)
fillMatrix(A,8);
A
checkLU matrix A

A = mutableMatrix(CC,1000,900, Dense=>true)
fillMatrix(A,10000);
time checkLU matrix A

A = mutableMatrix(CC,40,100, Dense=>true)
fillMatrix(A,300);
time checkLU matrix A

A = mutableMatrix(CC,100,40, Dense=>true)
fillMatrix(A,300);
time checkLU matrix A

A = mutableMatrix(CC,40,100, Dense=>true)
fillMatrix(A,300);
time checkLU matrix A

kk = RR
A = random(kk^100,kk^100);
det A


kk = ZZ/101
A = random(kk^1000,kk^1000);
time LU A;
time det A
rank A
----------------
-- Test solve --
----------------
-- Simple test for solve
checkSolve(matrix{{3.4}},matrix{{1.2}})
checkSolve(matrix{{3.4}},matrix{{1.2, 71.457348957438573}})

M1 = matrix{{1.,2.,3.4},{.5,9.87,3.},{-3.,-5.5,-7.}}
b1 = transpose matrix{{1.,2.,3.}}

assert (checkSolve(M1,b1) < 1e-15)
assert (checkSolve(M1^4,b1) < 1e-10)
assert (checkSolve(M1^3,b1) < 1e-10)

A = CC_200
M = random(A^4,A^4,Density=>.8)
b = random(A^4,A^3)
checkSolve(M,b) < 1e-15

A = CC_53
time M = random(A^1000,A^1000,Density=>.1);
b = random(A^1000,A^3);
time solve(M,b);
time M*oo-b;
norm(oo)
checkSolve(M,b)

--------------------------
-- check least squares ---
--------------------------
A = RR_53
M = random(A^4,A^2)
b = random(A^4,A^10)
checkClosestFit(M,b)

A = CC_53
M = random(A^4,A^2)
b = random(A^4,A^10)
checkClosestFit(M,b)

-- this one fails:
A = RR_53
M = random(A^4,A^5)
b = random(A^4,A^1)
checkClosestFit(M,b)

---------------------------------------
-- Test eigenvalues and eigenvectors --
---------------------------------------
checkEigenvectors M1
oo_0-oo_1
checkSVD(M1)

E2 = eigenvalues M1
(E,V) = eigenvectors M1
E-E2
norm oo
entries oo

--------------
-- Test SVD --
--------------
M = matrix{{1.,2.,3.4},{.5,9.87,3.},{-3.,-5.5,-7.},{.00000001,1e13,1e-13}}
checkSVD(M)
SVD M

checkSVD random(RR^4,RR^8)
checkSVD random(RR^8,RR^4)
checkSVD random(RR^8,RR^8)
checkSVD random(CC^4,CC^8)
checkSVD random(CC^8,CC^4)
checkSVD random(CC^8,CC^8)

M = map(RR_53^10, RR_53^10, (i,j) -> 1.0/(i+j+1))
M = map(RR_200^10, RR_200^10, (i,j) -> 1.0P200/(i+j+1))
SVD M
checkSVD M
det M
det M
nearby(M - U * diag(numgens target M, numgens source M, flatten entries S) * Vt)
assert (clean(transpose U - U^-1, 1e-15) == 0)
clean(U * transpose U, 1e-15)
flatten entries oo
max oo
assert(transpose U == U^-1)
transpose Vt - Vt^-1

M = mutableMatrix matrix{{1.,2.,3.4},{.5,9.87,3.},{-3.,-5.5,-7.}}
M
eigenvalues M
matrix oo
oo_(0,0)

M = matrix{{1,2},{3,4}}
M = M ** RR
M = mutableMatrix M
E = matrix eigenvalues M
(E,V) = eigenvectors M
E = matrix E
V = matrix V
A = ring M
E_(0,0)
