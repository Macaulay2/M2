diag = method()
diag(ZZ,ZZ,List) := (a,b,L) -> (
     R := ring L#0;
     M := mutableZero(R,a,b);
     scan(#L, i -> M_(i,i) = L#i);
     matrix M)

nearby = method()
nearby Matrix := (M) -> max flatten entries M
clean = method()
clean(Matrix,RR) := (M,epsilon) -> (
     zero := 0_(ring M);
     matrix applyTable(entries M, e -> if abs(e) < epsilon then zero else e))

checkSVD = method()
checkSVD(Matrix) := (M) -> (
     -- Compute the SVD, check assertions
     -- return (S,U,Vt)
     (S,U,Vt) := SVD M;
     nm1 := nearby(M - U * diag(numgens target M, numgens source M, flatten entries S) * Vt);
     nm2 := nearby(transpose U - U^-1);
     nm3 := nearby(transpose Vt - Vt^-1);
     max(nm1,nm2,nm3)
     )

-- test SVD
M = matrix{{1.,2.,3.4},{.5,9.87,3.},{-3.,-5.5,-7.}}
checkSVD(M)

M = matrix{{1.,2.,3.4},{.5,9.87,3.},{-3.,-5.5,-7.},{.00000001,1e13,1e-13}}
checkSVD(M)
SVD M

M = map(RR_53^10, RR_53^10, (i,j) -> 1.0/(i+j+1))
M = map(RR_200^10, RR_200^10, (i,j) -> 1.0/(i+j+1))
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
