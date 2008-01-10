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

L1infnorm = method()
L1infnorm Matrix := (M) -> max apply(flatten entries M, e -> sqrt((realPart e)^2 + (imaginaryPart e)^2))

v = solve(matrix {{1.0, 2.01}, {3., 4.}, {5., 8.}},matrix {{13.01}, {29.01}, {55.01}},ClosestFit=>true,MaximalRank=>true)
assert (L1infnorm(v - matrix {{3.}, {5.}}) < .1)

clean = method()
clean(Matrix,RR) := (M,epsilon) -> (
     zero := 0_(ring M);
     meps := -epsilon;
     matrix applyTable(entries M, e -> if e < epsilon and e > meps then zero else e))

checkLU = method()
checkLU Matrix := (M) -> (
     R := ring M;
     (P,L,U) = LU M;
     Q = mutableMatrix(R, numrows M, numrows M);
     for i from 0 to numrows A - 1 do Q_(i,P_i) = 1_R;
     Q = matrix Q;
     (Q*L*U, M)
     )

checkSVD = method()
checkSVD(Matrix) := (M) -> (
     -- Compute the SVD, check assertions
     -- return (S,U,Vt)
     (S,U,Vt) := SVD M;
     (S1,U1,Vt1) := SVD(M, DivideConquer=>true);
     nm1 := L1infnorm(M - U * diag(numgens target M, numgens source M, flatten entries S) * Vt);
     nm2 := L1infnorm(transpose U - U^-1);
     nm3 := L1infnorm(transpose Vt - Vt^-1);
     nm4 := L1infnorm(S-S1);
     nm5 := L1infnorm(U-U1);
     nm6 := L1infnorm(Vt-Vt1);
     (max(nm1,nm2,nm3), max(nm4,nm5,nm6))
     )

checkSolve = method()
checkSolve(Matrix,Matrix) := (M,b) -> (
     x := solve(M,b);
     L1infnorm (M*x-b))

checkClosestFit = method()
checkClosestFit(Matrix,Matrix) := (M,b) -> (
     x := solve(M,b,ClosestFit=>true,MaximalRank=>true);
     L1infnorm (M*x-b))

checkEigenvectors = method()
checkEigenvectors Matrix := (M) -> (
     -- compute eigenvectors, and eigenvalues
     E2 := eigenvalues M;
     (E,V) := eigenvectors M;
     n1 := L1infnorm(E-E2); -- make sure these are the same
     -- for each eigenvalue and eigenvector, compute Mv-ev
     (E,V,n1);
     M*V - diag flatten entries E -- should be small...
     )

----------------
-- Test LU -----
----------------
A = RR_53
M = random(A^4,A^2)
checkLU M
----------------
-- Test solve --
----------------
-- Simple test for solve
checkSolve(matrix{{3.4}},matrix{{1.2}})
checkSolve(matrix{{3.4}},matrix{{1.2, 71.457348957438573}})
checkSolve(matrix{{3.4,1.2}},matrix{{1.2}})

M1 = matrix{{1.,2.,3.4},{.5,9.87,3.},{-3.,-5.5,-7.}}
b1 = transpose matrix{{1.,2.,3.}}

assert (checkSolve(M1,b1) < 1e-15)
assert (checkSolve(M1^4,b1) < 1e-10)
assert (checkSolve(M1^3,b1) < 1e-10)

A = CC_200
M = random(A^4,A^4,Density=>.1)
b = random(A^4,A^3)
checkSolve(M,b) < 1e-15

A = CC_53
time M = random(A^1000,A^1000,Density=>.1);
b = random(A^1000,A^3);
time solve(M,b);
time M*oo-b;
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
L1infnorm oo
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
