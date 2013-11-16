printingAccuracy=6
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

checkLU = method()
checkLU(List,Matrix,Matrix) := (P,L,U) -> (
     R := ring L;
     Q = id_(R^#P) _ P;
     --Q = mutableMatrix(R, numrows L, numrows L);
     --for i from 0 to numrows L - 1 do Q_(i,P_i) = 1_R;
     --Q = matrix Q;
     Q*L*U)
checkLU Matrix := (M) -> norm (checkLU time LUdecomposition M - M)

conjugate Matrix := (M) -> (
    L := entries M;
    map(target M, source M, L/(L1 -> L1/conjugate))
    )

checkSVD = method()
checkSVD(Matrix) := (M) -> (
     -- Compute the SVD, check assertions
     -- return (S,U,Vt)
     (S,U,Vt) = SVD M;
     (S1,U1,Vt1) = SVD(M, DivideConquer=>true);
     nm1 := norm(M - U * diag(numgens target M, numgens source M, S) * Vt);
     nm2 := norm(M - U1 * diag(numgens target M, numgens source M, S1) * Vt1);
     nm3 := norm((conjugate transpose U) * U - 1);
     nm4 := norm((conjugate transpose Vt) * Vt - 1);
     nm5 := norm((conjugate transpose U1) * U1 - 1);
     nm6 := norm((conjugate transpose Vt1) * Vt1 - 1);
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
     if numRows M >= numColumns M then (
         MtM := (conjugate transpose M) * M;
         Mtb := (conjugate transpose M) * b;
         y := solve(MtM, Mtb);
         norm(x-y)
         )
     else (
         norm(M*x-b)
         )
     )
checkClosestFit(Matrix,Matrix,Symbol) := (M,b,deficient) -> (
     x := solve(M,b,ClosestFit=>true);
     if numRows M >= numColumns M then (
         MtM := (conjugate transpose M) * M;
         Mtb := (conjugate transpose M) * b;
         y := solve(MtM, Mtb);
         norm(x-y)
         )
     else (
         norm(M*x-b)
         )
     )

checkEigenvectors = method()
checkEigenvectors Matrix := (M) -> (
     -- compute eigenvectors, and eigenvalues
     E2 = eigenvalues(M);
     (E,V) = eigenvectors M;
     n1 := norm(E-E2); -- make sure these are the same
     -- for each eigenvalue and eigenvector, compute Mv-ev
     (E,V,n1);
     norm(M*V - V * diagonalMatrix E)
     )
checkEigenvectors(Matrix,Symbol) := (M,Hermit) -> (
     -- compute eigenvectors, and eigenvalues
     E2 = eigenvalues(M,Hermitian=>true);
     (E,V) = eigenvectors(M,Hermitian=>true);
     n1 := norm(E-E2); -- make sure these are the same
     -- for each eigenvalue and eigenvector, compute Mv-ev
     (E,V,n1);
     norm(M*V - V * diagonalMatrix E)
     )

setRandomSeed 0
----------------
-- Test LUdecomposition -----
----------------
R = RR_100
M = matrix mutableMatrix random(R^4,R^2)

--M = matrix{{1.5,2.0},{1.3,1.7},{1.6,.5}}
assert(checkLU M < 1e-15)
M = transpose M
assert(checkLU M < 1e-15)
M = matrix{{1.5,2.0},{1.3,1.7}}
assert(
    checkLU M 
    < 1e-15)

A = mutableMatrix(RR,5,4, Dense=>true)
fillMatrix (A,15);
assert(
    checkLU matrix A 
    < 1e-15)

A = mutableMatrix(CC,5,4, Dense=>true)
fillMatrix(A,8);
assert(checkLU matrix A < 1e-15)

A = mutableMatrix(CC,40,100, Dense=>true)
fillMatrix(A,300);
assert(time checkLU matrix A < 1e-15)

A = mutableMatrix(CC,100,40, Dense=>true)
fillMatrix(A,300);
assert(time checkLU matrix A < 1e-14)

A = mutableMatrix(CC,40,100, Dense=>true)
fillMatrix(A,300);
assert(time checkLU matrix A < 1e-14)

kk = RR
A = random(kk^100,kk^100);
----norm(A*A^-1-id_(kk^100))

kk = CC
A = random(kk^100,kk^100);
----time A^-1;

----------------
-- Test solve --
----------------
-- Simple test for solve
checkSolve(matrix{{3.4}},matrix{{1.2}})
checkSolve(matrix{{3.4}},matrix{{1.2, 71.457348957438573}})

M1 = matrix{{1.,2.,3.4},{.5,9.87,3.},{-3.,-5.5,-7.}}
b1 = transpose matrix{{1.,2.,3.}}

assert (checkSolve(M1,b1) < 1e-14)
assert (checkSolve(M1^4,b1) < 1e-10)
assert (checkSolve(M1^3,b1) < 1e-10)

A = CC_200
M = random(A^4,A^4,Density=>.8)
b = random(A^4,A^3)
assert(checkSolve(M,b) < 1e-14)

A = RR_53
time M = random(A^100,A^100,Density=>.2);
b = random(A^100,A^3);
assert(checkSolve(M,b) < 1e-10)


A = CC_53
time M = random(A^100,A^100,Density=>.2);
b = random(A^100,A^3);
assert(checkSolve(M,b) < 1e-10)

---------------------------------------
-- Test eigenvalues and eigenvectors --
---------------------------------------
for i from 1 to 100 do (
    M = random(RR^3,RR^3);
    assert(checkEigenvectors M < 1e-13);
    )

for i from 1 to 100 do (
    M = random(RR^20,RR^20);
    time assert(checkEigenvectors M < 1e-13);
    )

M = random(CC^20,CC^20)
assert(checkEigenvectors M < 1e-13)

M = random(CC^100,CC^100);
assert(checkEigenvectors M < 1e-13)
--------------------------------------
-- Eigenvalues and eigenvectrs of   --
-- Symmetric and Hermitian matrices --
--------------------------------------
M1 = random(RR^3,RR^3)
M = M1 + transpose M1
checkEigenvectors(M, Her)
E
V
E2

M1 = random(CC^3,CC^3)
M2 = matrix apply(entries transpose M1, v -> apply(v, conjugate))
M = M1 + M2
checkEigenvectors(M, Her)

--------------
-- Test SVD --
--------------
M = matrix{{1.,2.,3.4},{.5,9.87,3.},{-3.,-5.5,-7.},{.00000001,1e13,1e-13}}
assert(max checkSVD(M) < .01) -- error is much higher

assert(max checkSVD random(RR^4,RR^8) < 1e-13)
assert(max checkSVD random(RR^8,RR^4) < 1e-13)
assert(max checkSVD random(RR^8,RR^8) < 1e-13)

assert(max checkSVD random(CC^4,CC^8) < 1e-13)
assert(max checkSVD random(CC^8,CC^4) < 1e-13)
assert(max checkSVD random(CC^8,CC^8) < 1e-13)

M = map(RR_53^10, RR_53^10, (i,j) -> 1.0/(i+j+1))
assert(max checkSVD M < 1e-13)
M = map(RR_200^10, RR_200^10, (i,j) -> 1.0p200/(i+j+1))
assert(max checkSVD M < 1e-13)

M = matrix{{1+ii,2},{1,3}}
assert(max checkSVD M < 1e-13)

--------------------------
-- check least squares ---
--------------------------
A = matrix{{1,2},{1,5}} ** RR
b = transpose matrix{{1,1}} ** RR
x = solve(A,b,ClosestFit=>true,MaximalRank=>true)
assert(checkClosestFit(A,b) < 1e-12)

A = matrix"1,2,3,4,5;1,4,8,10,13" ** RR
b = matrix"-1;3" ** RR
x = solve(A,b,ClosestFit=>true,MaximalRank=>true)
assert(checkClosestFit(A,b) < 1e-12)

A = transpose matrix"1,2,3,4,5;1,4,8,10,13" ** RR
b = matrix"-1;3;1;2;6" ** RR
x = solve(A,b,ClosestFit=>true,MaximalRank=>true)
assert(checkClosestFit(A,b) < 1e-12)

A = matrix{{1,2},{1,5},{1,7}} ** RR
b = .3*A_{0}+.5*A_{1}+.1*matrix{{.1},{.2},{.1}}
x0 = matrix{{.3},{.5}}
x = solve(A,b,ClosestFit=>true,MaximalRank=>true)
x = solve(A,b,ClosestFit=>true,MaximalRank=>false)
assert(checkClosestFit(A,b) < 1e-12)
assert(checkClosestFit(A,b,Ker) < 1e-12)

M = random(RR^4,RR^2)
b = random(RR^4,RR^10)
assert(checkClosestFit(M,b) < 1e-12)

A = CC_53
M = random(A^4,A^2)
b = random(A^4,A^10)
assert(checkClosestFit(M,b) < 1e-12)
assert(checkClosestFit(M,b, Ker) < 1e-12)

A = RR_53
M = random(A^4,A^5)
b = random(A^4,A^1)
assert(checkClosestFit(M,b) < 1e-12)
assert(checkClosestFit(M,b,Ker) < 1e-12)

-- underdetermined
A = RR_53
M = matrix{{1,1,1.0},{2,2,3}}
b = random(A^2,A^1)
assert(checkClosestFit(M,b) < 1e-12)
assert(checkClosestFit(M,b,Ker) < 1e-12)

-- underdetermined and inconsistent
A = RR_53
M = matrix{{1,1,1.0},{2,2,2}}
b = random(A^2,A^1)
checkClosestFit(M,b) < 1e-12 -- FAILS: M is not full rank
checkClosestFit(M,b,Ker) < 1e-12

A = matrix{{1,2},{1,5},{+ii,2}}
b = transpose matrix{{1+ii,1-2*ii,1}}
x1 = solve(A,b,ClosestFit=>true, MaximalRank=>true)
assert(checkClosestFit(A,b) < 1e-12)

///--------------------------------------------
-- checkClosestFit doesn't work if the matrix doesn't have full rank
-- should we make it work?
A2 = matrix{{1,2},{1,5},{+ii,2}} | matrix {{.5}, {1.1}, {.4+.1*ii}}
b = transpose matrix{{1+ii,1-2*ii,1}}
x2 = solve(A2,b,ClosestFit=>true)
A2*x2-b
--checkClosestFit(A2,b,Def)

apply(flatten entries (A*x1-b), abs)
apply(flatten entries (A2*x2-b), abs)
///-----------------------------------------------

A = matrix"1,2,3,4,5;1,4,8,10,13" ** CC
b = matrix{{-ii},{1+ii}}
x = solve(A,b,ClosestFit=>true,MaximalRank=>true)
assert(checkClosestFit(A,b) < 1e-14)

v = solve(matrix {{1.0, 2.01}, {3., 4.}, {5., 8.}},
     matrix {{13.01}, {29.01}, {55.01}},
     ClosestFit=>true,MaximalRank=>true)
assert (norm(v - matrix {{3.}, {5.}}) < .1)

---------------------------------
-- SVD smaller rank -------------
---------------------------------
M1 = random(RR^3,RR^6)
M2 = random(RR^6,RR^3)
SVD(M2*M1)
2.^-53

-------------------------------------------------
-- Trivial cases (i.e. a matrix has size zero) --
-------------------------------------------------
kk = RR
a1 = map(kk^0,kk^5,0)
a2 = map(kk^5,kk^0,0)
a0 = map(kk^0,kk^0,0)
(P,L,U) = LUdecomposition a1
assert(#P == 0 and numrows L == 0 and numcols L == 0 and numrows U == 0 and numcols U == 5)
(P,L,U) = LUdecomposition a2
assert(#P == 5 and numrows L == 5 and numcols L == 0 and numrows U == 0 and numcols U == 0)
(P,L,U) = LUdecomposition a0
assert(#P == 0 and numrows L == 0 and numcols L == 0 and numrows U == 0 and numcols U == 0)

kk = CC
a1 = map(kk^0,kk^5,0)
a2 = map(kk^5,kk^0,0)
a0 = map(kk^0,kk^0,0)
(P,L,U) = LUdecomposition a1
assert(#P == 0 and numrows L == 0 and numcols L == 0 and numrows U == 0 and numcols U == 5)
(P,L,U) = LUdecomposition a2
assert(#P == 5 and numrows L == 5 and numcols L == 0 and numrows U == 0 and numcols U == 0)
(P,L,U) = LUdecomposition a0
assert(#P == 0 and numrows L == 0 and numcols L == 0 and numrows U == 0 and numcols U == 0)

kk = ZZ/101
a1 = map(kk^0,kk^5,0)
a2 = map(kk^5,kk^0,0)
a0 = map(kk^0,kk^0,0)
(P,L,U) = LUdecomposition a1
assert(#P == 0 and numrows L == 0 and numcols L == 0 and numrows U == 0 and numcols U == 5)
(P,L,U) = LUdecomposition  a2
--status: Mike said he will fix this in the engine
--status: it's the extreme case where LUdecomposition gets an empty matrix
assert(#P == 5 and numrows L == 5 and numcols L == 0 and numrows U == 0 and numcols U == 0) -- failed
(P,L,U) = LUdecomposition a0
assert(#P == 0 and numrows L == 0 and numcols L == 0 and numrows U == 0 and numcols U == 0)

kk = RR
a0 = map(kk^0,kk^0,0)
assert try SVD map(RR^0,RR^0,0) else true
assert try SVD map(CC^0,CC^0,0) else true

assert(eigenvalues map(RR^0,RR^0,0) === VerticalList {} )
assert(eigenvalues(map(RR^0,RR^0,0), Hermitian=>true) === VerticalList {} )
assert(eigenvalues map(CC^0,CC^0,0) === VerticalList {})
assert(eigenvalues(map(CC^0,CC^0,0), Hermitian=>true) === VerticalList {})

(E,V) = eigenvectors map(RR^0,RR^0,0); assert(V == map(CC^0,CC^0,0))
(E,V) = eigenvectors(map(RR^0,RR^0,0), Hermitian=>true); assert(V == map(RR^0,RR^0,0))
(E,V) = eigenvectors map(CC^0,CC^0,0); assert(V == map(CC^0,CC^0,0))
(E,V) = eigenvectors(map(CC^0,CC^0,0), Hermitian=>true); assert(V == map(CC^0,CC^0,0))

A = map(RR^0,RR^1,0)
B = map(RR^0,RR^0,0)
assert try M = solve(A,B) else true
assert(solve(A,B,ClosestFit=>true) == map(RR^1,RR^0,0))
assert(solve(A,B,ClosestFit=>true,MaximalRank=>true) == map(RR^1,RR^0,0))

A = map(RR^0,RR^0,0)
B = map(RR^0,RR^1,0)
M = solve(A,B)
assert(numrows M == numcols A and numcols M == numcols B)
M = solve(A,B,ClosestFit=>true)
assert(numrows M == numcols A and numcols M == numcols B)
M = solve(A,B,ClosestFit=>true,MaximalRank=>true)
assert(numrows M == numcols A and numcols M == numcols B)

A = map(RR^0,RR^0,0)
B = map(RR^0,RR^0,0)
M = solve(A,B)
assert(numrows M == numcols A and numcols M == numcols B)
M = solve(A,B,ClosestFit=>true)
assert(numrows M == numcols A and numcols M == numcols B)
M = solve(A,B,ClosestFit=>true,MaximalRank=>true)
assert(numrows M == numcols A and numcols M == numcols B)

-- CC
A = map(CC^0,CC^1,0)
B = map(CC^0,CC^0,0)
assert try (M = solve(A,B)) else true
M = solve(A,B,ClosestFit=>true)
assert(numrows M == numcols A and numcols M == numcols B)
M = solve(A,B,ClosestFit=>true,MaximalRank=>true)
assert(numrows M == numcols A and numcols M == numcols B)

A = map(CC^0,CC^0,0)
B = map(CC^0,CC^1,0)
M = solve(A,B)
assert(numrows M == numcols A and numcols M == numcols B)
M = solve(A,B,ClosestFit=>true)
assert(numrows M == numcols A and numcols M == numcols B)
M = solve(A,B,ClosestFit=>true,MaximalRank=>true)
assert(numrows M == numcols A and numcols M == numcols B)

A = map(CC^0,CC^0,0)
B = map(CC^0,CC^0,0)
M = solve(A,B)
assert(numrows M == numcols A and numcols M == numcols B)
M = solve(A,B,ClosestFit=>true)
assert(numrows M == numcols A and numcols M == numcols B)
M = solve(A,B,ClosestFit=>true,MaximalRank=>true)
assert(numrows M == numcols A and numcols M == numcols B)

-- ZZ/101
kk = ZZ/101
A = map(kk^0,kk^1,0)
B = map(kk^0,kk^0,0)
M = solve(A,B)
assert(numrows M == numcols A and numcols M == numcols B)

A = map(kk^0,kk^0,0)
B = map(kk^0,kk^1,0)
M = solve(A,B)
assert(numrows M == numcols A and numcols M == numcols B)

A = map(kk^0,kk^0,0)
B = map(kk^0,kk^0,0)
M = solve(A,B)
assert(numrows M == numcols A and numcols M == numcols B)



