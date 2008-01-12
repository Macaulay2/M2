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
     (S,U,Vt) = SVD M;
     (S1,U1,Vt1) = SVD(M, DivideConquer=>true);
     nm1 := norm(M - U * diag(numgens target M, numgens source M, flatten entries S) * Vt);
     nm2 := norm(M - U1 * diag(numgens target M, numgens source M, flatten entries S1) * Vt1);     
     nm3 := norm(transpose U - U^-1);
     nm4 := norm(transpose Vt - Vt^-1);
     nm5 := norm(transpose U1 - U1^-1);
     nm6 := norm(transpose Vt1 - Vt1^-1);
     nm0 := norm(S-S1);
     (nm0,nm1,nm2,max(nm3,nm4,nm5,nm6))
     )

checkSolve = method()
checkSolve(Matrix,Matrix) := (M,b) -> (
     x := solve(M,b);
     norm (M*x-b))

checkClosestFit = method()
checkClosestFit(Matrix,Matrix) := (M,b) -> (
     x := solve(M,b,ClosestFit=>true,MaximalRank=>true);
     norm (M*x-b))
checkClosestFit(Matrix,Matrix,Symbol) := (M,b,deficient) -> (
     x := solve(M,b,ClosestFit=>true);
     norm (M*x-b))

checkEigenvectors = method()
checkEigenvectors Matrix := (M) -> (
     -- compute eigenvectors, and eigenvalues
     E2 := eigenvalues M;
     (E,V) := eigenvectors M;
     n1 := norm(E-E2); -- make sure these are the same
     -- for each eigenvalue and eigenvector, compute Mv-ev
     (E,V,n1);
     norm(M*V - V * diagonalMatrix flatten entries E)
     )

----------------
-- Test LU -----
----------------
A = RR
M = mutableMatrix random(A^4,A^2)

M = matrix{{1.5,2.0},{1.3,1.7},{1.6,.5}}
assert(checkLU M == 0)
M = transpose M
assert(checkLU M == 0)
M = matrix{{1.5,2.0},{1.3,1.7}}
assert(checkLU M == 0)

A = mutableMatrix(CC,5,4, Dense=>true)
fillMatrix(A,8);
assert(checkLU matrix A < 1e-15)

A = mutableMatrix(CC,40,100, Dense=>true)
fillMatrix(A,300);
assert(time checkLU matrix A < 1e-15)

A = mutableMatrix(CC,100,40, Dense=>true)
fillMatrix(A,300);
assert(time checkLU matrix A < 1e-15)

A = mutableMatrix(CC,40,100, Dense=>true)
fillMatrix(A,300);
assert(time checkLU matrix A < 1e-15)

kk = RR
A = random(kk^100,kk^100);
norm(A*A^-1-id_(kk^100))

kk = CC
A = random(kk^100,kk^100);
time A^-1;

kk = ZZ/101
A = random(kk^1000,kk^1000);
time LU A;

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
assert(checkSolve(M,b) < 1e-14)

A = CC_53
time M = random(A^1000,A^1000,Density=>.1);
b = random(A^1000,A^3);
assert(checkSolve(M,b) < 1e-10)

---------------------------------------
-- Test eigenvalues and eigenvectors --
---------------------------------------
M = random(RR^3,RR^3)
assert(checkEigenvectors M < 1e-15)

M = random(RR^20,RR^20)
assert(checkEigenvectors M < 1e-14)

M = random(CC^20,CC^20)
assert(checkEigenvectors M < 1e-14)

M = random(CC^100,CC^100);
eigenvalues M

--------------
-- Test SVD --
--------------
M = matrix{{1.,2.,3.4},{.5,9.87,3.},{-3.,-5.5,-7.},{.00000001,1e13,1e-13}}
checkSVD(M)

checkSVD random(RR^4,RR^8)
checkSVD random(RR^8,RR^4)
checkSVD random(RR^8,RR^8)

checkSVD random(CC^4,CC^8)
checkSVD random(CC^8,CC^4)
checkSVD random(CC^8,CC^8)

M = map(RR_53^10, RR_53^10, (i,j) -> 1.0/(i+j+1))
checkSVD M
M = map(RR_200^10, RR_200^10, (i,j) -> 1.0p200/(i+j+1))
checkSVD M

M = matrix{{1+ii,2},{1,3}}
checkSVD M

--------------------------
-- check least squares ---
--------------------------
A = matrix{{1,2},{1,5}} ** RR
b = transpose matrix{{1,1}} ** RR
x = solve(A,b,ClosestFit=>true,MaximalRank=>true)
checkClosestFit(A,b)

A = matrix"1,2,3,4,5;1,4,8,10,13" ** RR
b = matrix"-1;3" ** RR
x = solve(A,b,ClosestFit=>true,MaximalRank=>true)
checkClosestFit(A,b)

A = transpose matrix"1,2,3,4,5;1,4,8,10,13" ** RR
b = matrix"-1;3;1;2;6" ** RR
x = solve(A,b,ClosestFit=>true,MaximalRank=>true)
checkClosestFit(A,b)

A = matrix{{1,2},{1,5},{1,7}} ** RR
b = .3*A_{0}+.5*A_{1}+.1*matrix{{.1},{.2},{.1}}
x0 = matrix{{.3},{.5}}
x = solve(A,b,ClosestFit=>true,MaximalRank=>true)
norm(A*x-b)
norm(A*x0-b)
checkClosestFit(A,b)

M = random(RR^4,RR^2)
b = random(RR^4,RR^10)
checkClosestFit(M,b)

A = CC_53
M = random(A^4,A^2)
b = random(A^4,A^10)
checkClosestFit(M,b)

A = RR_53
M = random(A^4,A^5)
b = random(A^4,A^1)
checkClosestFit(M,b)

A = matrix{{1,2},{1,5},{+ii,2}}
b = transpose matrix{{1+ii,1-2*ii,1}}
x1 = solve(A,b,ClosestFit=>true, MaximalRank=>true)
checkClosestFit(A,b)

A2 = matrix{{1,2},{1,5},{+ii,2}} | matrix {{.5}, {1.1}, {.4+.1*ii}}
b = transpose matrix{{1+ii,1-2*ii,1}}
x2 = solve(A2,b,ClosestFit=>true)
checkClosestFit(A2,b,Def)

apply(flatten entries (A*x1-b), abs)
apply(flatten entries (A2*x2-b), abs)

A = matrix"1,2,3,4,5;1,4,8,10,13" ** CC
b = matrix{{-ii},{1+ii}}
x = solve(A,b,ClosestFit=>true,MaximalRank=>true)
checkClosestFit(A,b)
