diag = method()
diag(ZZ,ZZ,List) := (a,b,L) -> (
     R := ring L#0;
     M := mutableZero(R,a,b);
     scan(#L, i -> M_(i,i) = L#i);
     matrix M)

L1norm = method()
L1norm Matrix := (M) -> max apply(flatten entries M, e -> abs e)

clean = method()
clean(Matrix,RR) := (M,epsilon) -> (
     zero := 0_(ring M);
     meps := -epsilon;
     matrix applyTable(entries M, e -> if e < epsilon and e > meps then zero else e))

checkSVD = method()
checkSVD(Matrix) := (M) -> (
     -- Compute the SVD, check assertions
     -- return (S,U,Vt)
     (S,U,Vt) := SVD M;
     (S1,U1,Vt1) := SVD(M, DivideConquer=>true);
     nm1 := L1norm(M - U * diag(numgens target M, numgens source M, flatten entries S) * Vt);
     nm2 := L1norm(transpose U - U^-1);
     nm3 := L1norm(transpose Vt - Vt^-1);
     nm4 := L1norm(S-S1);
     nm5 := L1norm(U-U1);
     nm6 := L1norm(Vt-Vt1);
     (max(nm1,nm2,nm3), max(nm4,nm5,nm6))
     )

checkSolve = method()
checkSolve(Matrix,Matrix) := (M,b) -> (
     x := solve(M,b);
     L1norm (M*x-b))

checkEigenvectors = method()
checkEigenvectors Matrix := (M) -> (
     -- compute eigenvectors, and eigenvalues
     E2 := eigenvalues M;
     (E,V) := eigenvectors M;
     n1 := L1norm(E-E2); -- make sure these are the same
     -- for each eigenvalue and eigenvector, compute Mv-ev
     (E,V,n1);
     M*V - diag flatten entries E -- should be small...
     )

-- Simple test for solve
M1 = matrix{{1.,2.,3.4},{.5,9.87,3.},{-3.,-5.5,-7.}}
b1 = transpose matrix{{1.,2.,3.}}

assert (checkSolve(M1,b1) < 1e-15)
assert (checkSolve(M1^4,b1) < 1e-10)
assert (checkSolve(M1^3,b1) < 1e-10)

checkEigenvectors M1
oo_0-oo_1
checkSVD(M1)

E2 = eigenvalues M1
(E,V) = eigenvectors M1
E-E2
L1norm oo
entries oo

M = matrix{{1.,2.,3.4},{.5,9.87,3.},{-3.,-5.5,-7.},{.00000001,1e13,1e-13}}
checkSVD(M)
SVD M

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
