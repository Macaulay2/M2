restart

-- basic operations: *, +, -
A = rawMatrixRR(3,3)
B = rawMatrixRR(3,3)

rawSetMatrixValues(A, {(0,0),(1,0),(2,1),(2,2),(1,2),(0,2)},
     {1.1, -2.33, 0.00456, 25.7, 1.2, 5.45})
rawSetMatrixValues(B, {(0,1),(2,0),(2,1),(2,2),(1,1),(0,2)},
     {-21.12, 5.31, -1.067, 212.2, -3.77, 0.5})
assert(A+B == B+A)
assert(A-B == -(B-A))
assert(A*B != B*A)

assert(A == rawGetSubmatrix(A, {0,1,2}, {0,1,2}))

rawSetEpsilonMatrixRR(0.01)
assert(rawGetEpsilonMatrixRR() == 0.01)

C = rawMatrixCC(3,3)
D = rawMatrixCC(3,3)
rawSetMatrixValues(C, {(0,0),(1,0),(2,1),(2,2),(1,2),(0,2)},
     {M2CC(3.0,1.2), M2CC(-5.1,-7.7), M2CC(10.9,-2.4),
     M2CC(-4.0,-11.11), M2CC(10.3,-1.7), M2CC(-9.1,8.7)})
rawSetMatrixValues(D, {(1,1),(1,0),(2,1),(1,2),(1,2),(0,2)},
     {M2CC(31.1,-1.21), M2CC(2.89,17.56), M2CC(-1.92,-12.6),
     M2CC(14.2,9.222), M2CC(-11.22,6.1), M2CC(-8.12,-1.33)})
assert(D-C == -(C-D))
assert(D+C == C+D)
assert(D*C != C*D)

assert(A-C == -(C-A))
assert(A+C == C+A)
assert(A*C != C*A)

assert(C == rawGetSubmatrix(C, {0,1,2}, {0,1,2}))

rawSetEpsilonMatrixCC(0.00001)
assert(rawGetEpsilonMatrixCC() == 0.00001)

A1 = rawMatrixCC(1,1)
A2 = rawMatrixCC(1,1)
A3 = rawMatrixCC(1,1)
rawSetMatrixEntryCC(A1,0,0,M2CC(1.0,2.0))
rawSetMatrixEntryCC(A2,0,0,M2CC(1.000005, 2.000005))
rawSetMatrixEntryCC(A3,0,0,M2CC(1.00001, 2.00001))
assert(A1 == A2)
assert(A2 == A3)
assert(A1 != A3)

rawSetEpsilonMatrixCC(0.0)
assert(A1 != A2)
assert(A2 != A3)

-- solving equations
A = rawMatrixRR(3,3)
x = rawMatrixRR(3,1)
y = rawMatrixRR(3,1)
rawSetMatrixValues(A, {(0,0),(1,0),(2,1),(2,2),(1,2),(0,2)},
     {1.1, -2.33, 0.00456, 25.7, 1.2, 5.45})
rawSetMatrixValues(y, {(0,0)}, {1.0})
b = rawGetSubmatrix(A, {0,1,2}, {0})
rawSolve(A,b,x)
rawSetEpsilonMatrixRR(0.0000001)
assert(x == y)
rawSetEpsilonMatrixRR(0.0)
assert(x != y)
assert(A*y == b)

b = rawGetSubmatrix(A, {0,1,2}, {0}) - rawGetSubmatrix(A, {0,1,2}, {1}) + rawGetSubmatrix(A, {0,1,2}, {2})
rawSetMatrixValues(y, {(0,0), (1,0), (2,0)}, {1.0, -1.0, 1.0})
rawSolve(A,b,x)
rawSetEpsilonMatrixRR(0.0000001)
assert(x == y)
rawSetEpsilonMatrixRR(0.0)
assert(x != y)
assert(A*y == b)

A = rawMatrixCC(3,3)
x = rawMatrixCC(3,1)
y = rawMatrixCC(3,1)
rawSetMatrixValues(A, {(1,1),(1,0),(2,1),(1,2),(1,2),(0,2)},
     {M2CC(31.1,-1.21), M2CC(2.89,17.56), M2CC(-1.92,-12.6),
     M2CC(14.2,9.222), M2CC(-11.22,6.1), M2CC(-8.12,-1.33)})
rawSetMatrixValues(y, {(0,0)}, {M2CC(1.0, 0.0)})
b = rawGetSubmatrix(A, {0,1,2}, {0})
rawSolve(A,b,x)
rawSetEpsilonMatrixCC(0.0000001)
assert(x == y)
rawSetEpsilonMatrixCC(0.0)
assert(x != y)
assert(A*y == b)

b = rawGetSubmatrix(A, {0,1,2}, {0}) - rawGetSubmatrix(A, {0,1,2}, {1}) + rawGetSubmatrix(A, {0,1,2}, {2})
rawSetMatrixValues(y, {(0,0), (1,0), (2,0)}, {M2CC(1.0,0.0), M2CC(-1.0,0.0), M2CC(1.0,0.0)})
rawSolve(A,b,x)
rawSetEpsilonMatrixCC(0.0000001)
assert(x == y)
rawSetEpsilonMatrixRR(0.0)
assert(x != y)
assert(A*y == b)

-- LU
A = rawMatrixRR(4,5)
P = rawMatrixRR(0,0)
L = rawMatrixRR(0,0)
U = rawMatrixRR(0,0)
rawSetMatrixValues(A, {(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,1),(1,2),(1,3),(1,4),
	  (2,0),(2,1),(2,2),(2,3),(2,4),(3,0),(3,1),(3,2),(3,3),(3,4)},
     {1.1, -2.33, 0.00456, 25.7, 1.2, 5.45, 1.1, -2.33, 0.00456, 25.7, 1.2, 5.45,
	  2.3, -.0222, 4.5, -89.11, 45.25, 1.5, -34.33, 2.2})
rawLU(A,L,U,P)
rawSetEpsilonMatrixRR(0.0000001)
assert(A == P*L*U)
rawSetEpsilonMatrixRR(0.0)
assert(A != P*L*U)

A = rawMatrixCC(4,5)
P = rawMatrixRR(0,0)
L = rawMatrixCC(0,0)
U = rawMatrixCC(0,0)
rawSetMatrixValues(A, {(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,1),(1,2),(1,3),(1,4),
	  (2,0),(2,1),(2,2),(2,3),(2,4),(3,0),(3,1),(3,2),(3,3),(3,4)},
     {M2CC(1.1, -2.33), M2CC(0.00456, 25.7), M2CC(1.2, 5.45), M2CC(1.1, -2.33), M2CC(0.00456, 25.7), 
	  M2CC(1.2, 5.45), M2CC(2.3, -.0222), M2CC(4.5, -89.11), M2CC(45.25, 1.5), M2CC(-34.33, 2.2),
	  M2CC(1.1, -2.33), M2CC(0.00456, 25.7), M2CC(1.2, 5.45), M2CC(1.1, -2.33), M2CC(0.00456, 25.7), 
	  M2CC(1.2, 5.45), M2CC(2.3, -.0222), M2CC(4.5, -89.11), M2CC(45.25, 1.5), M2CC(-34.33, 2.2)})
rawLU(A,L,U,P)
rawSetEpsilonMatrixCC(0.0000001)
assert(A == P*L*U)
rawSetEpsilonMatrixCC(0.0)
assert(A != P*L*U)

rawSetMatrixValues(A, {(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,1),(1,2),(1,3),(1,4)},
     {M2CC(21.12, 5.53), M2CC(-2.46, 33.7), M2CC(11.22, 15.45), M2CC(1.21, -2.332), M2CC(0.200456, 25.7), 
	  M2CC(1.2, 5.45), M2CC(2.3, -.0222), M2CC(4.5, -89.11), M2CC(45.25, 1.5), M2CC(-354.33, 2.2)})
rawLU(A,L,U,P)
rawSetEpsilonMatrixCC(0.0000001)
assert(A == P*L*U)
rawSetEpsilonMatrixCC(0.0)
assert(A != P*L*U)

-- Eigenvalues

toDiagRR = (v) -> (
     n := rawNumberOfRowsRR(v);
     A := rawMatrixRR(n,n);
     i := 0;
     while (i < n) do (
	  x := 0.0;
	  rawGetMatrixEntryRR(v,i,0,x);
	  rawSetMatrixEntryRR(A,i,i,x);
	  i = i+1;
	  );
     A);

toDiagCC = (v) -> (
     n := rawNumberOfRowsCC(v);
     A := rawMatrixCC(n,n);
     i := 0;
     while (i < n) do (
	  z := M2CC(0.0,0.0);
	  rawGetMatrixEntryCC(v,i,0,z);
	  rawSetMatrixEntryCC(A,i,i,z);
	  i = i+1;
	  );
     A);

rawSetEpsilonMatrixRR(0.000001)
rawSetEpsilonMatrixCC(0.000001)

A = rawMatrixRR(4,4)
eigvals = rawMatrixRR(0,0)
eigvecs = rawMatrixRR(0,0)
eigenvals = rawMatrixCC(0,0)
eigenvecs = rawMatrixCC(0,0)
rawSetMatrixValues(A, {(0,0),(1,1),(2,2),(3,3)}, {1.1, -2.33, 0.00456, 25.7})

rawEigenvectorsSymmetric(A,eigvals,eigvecs)
assert(A*eigvecs == eigvecs*toDiagRR(eigvals))

rawEigenvectors(A, eigenvals, eigenvecs)
assert(A*eigenvecs == eigenvecs*toDiagCC(eigenvals))

rawSetMatrixValues(A, {(1,2),(2,1),(0,3),(3,0)}, {-21.6, -21.6, 225.7, 225.7})
rawEigenvectorsSymmetric(A,eigvals,eigvecs)
assert(A*eigvecs == eigvecs*toDiagRR(eigvals))

rawEigenvectors(A, eigenvals, eigenvecs)
assert(A*eigenvecs == eigenvecs*toDiagCC(eigenvals))

rawSetMatrixValues(A, {(1,3),(3,2)}, {11.2, -37.86})
rawEigenvectors(A, eigenvals, eigenvecs)
assert(A*eigenvecs == eigenvecs*toDiagCC(eigenvals))  -- FAILS on Mikes laptop

rawSetMatrixValues(A, {(1,3),(3,2),(3,1)}, {-1111.2, -4437.86, 555.3})
rawEigenvectors(A, eigenvals, eigenvecs)
assert(A*eigenvecs == eigenvecs*toDiagCC(eigenvals))  -- FAILS on Mikes laptop

A = rawMatrixCC(5,5)
rawSetMatrixValues(A, {(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,1),(1,2),(1,3),(1,4),
	  (2,0),(2,1),(2,2),(2,3),(2,4),(3,0),(3,1),(3,2),(3,3),(3,4),(4,0),(4,1),(4,2),(4,3),(4,4)},
     {M2CC(1.1, -2.33), M2CC(0.00456, 25.7), M2CC(1.2, 5.45), M2CC(1.1, -2.33), M2CC(0.00456, 25.7), 
	  M2CC(1.2, 5.45), M2CC(2.3, -.0222), M2CC(4.5, -89.11), M2CC(45.25, 1.5), M2CC(-34.33, 2.2),
	  M2CC(14.1, -42.33), M2CC(0.00456, 25.7), M2CC(1.2, 5.45), M2CC(1.1, -2.33), M2CC(0.00456, 25.7), 
	  M2CC(1.1, -2.33), M2CC(22.00456, -325.7), M2CC(211.2, -25.45), M2CC(1.1, -2.33), M2CC(0.00456, 25.7), 
	  M2CC(1.2, 5.45), M2CC(2.3, -.0222), M2CC(4.5, -89.11), M2CC(45.25, 1.5), M2CC(-344.33, 2.2)})
rawEigenvectors(A, eigenvals, eigenvecs)
assert(A*eigenvecs == eigenvecs*toDiagCC(eigenvals))  -- FAILS on Mikes laptop

rawEigenvectorsHermitian(A, eigvals, eigenvecs)

-- SVD

toDiagSVD = (v, m, n) -> (
     A := rawMatrixRR(m,n);
     i := 0;
     while (i < min(m,n)) do (
	  x := 0.0;
	  rawGetMatrixEntryRR(v,i,0,x);
	  rawSetMatrixEntryRR(A,i,i,x);
	  i = i+1;
	  );
     A);

rawSetEpsilonMatrixRR(0.000001)
rawSetEpsilonMatrixCC(0.000001)

A = rawMatrixRR(4,5)
Sigma = rawMatrixRR(0,0)
U = rawMatrixRR(0,0)
VT = rawMatrixRR(0,0)
rawSetMatrixValues(A, {(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,1),(1,2),(1,3),(1,4),
	  (2,0),(2,1),(2,2),(2,3),(2,4),(3,0),(3,1),(3,2),(3,3),(3,4)},
     {1.1, -2.33, 0.00456, 25.7, 1.2, 5.45, 1.1, -2.33, 0.00456, 25.7, 1.2, 5.45,
	  2.3, -.0222, 4.5, -89.11, 45.25, 1.5, -34.33, 2.2})
rawSVD(A, Sigma, U, VT)
assert(A == U*toDiagSVD(Sigma, 4,5)*VT)


A = rawMatrixCC(4,5)
Sigma = rawMatrixRR(0,0)
U = rawMatrixCC(0,0)
VT = rawMatrixCC(0,0)
rawSetMatrixValues(A, {(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,1),(1,2),(1,3),(1,4),
	  (2,0),(2,1),(2,2),(2,3),(2,4),(3,0),(3,1),(3,2),(3,3),(3,4)},
     {M2CC(1.1, -2.33), M2CC(0.00456, 25.7), M2CC(1.2, 5.45), M2CC(1.1, -2.33), M2CC(0.00456, 25.7), 
	  M2CC(1.2, 5.45), M2CC(2.3, -.0222), M2CC(4.5, -89.11), M2CC(45.25, 1.5), M2CC(-34.33, 2.2),
	  M2CC(1.1, -2.33), M2CC(0.00456, 25.7), M2CC(1.2, 5.45), M2CC(1.1, -2.33), M2CC(0.00456, 25.7), 
	  M2CC(1.2, 5.45), M2CC(2.3, -.0222), M2CC(4.5, -89.11), M2CC(45.25, 1.5), M2CC(-34.33, 2.2)})
rawSVD(A, Sigma, U, VT)
assert(A == U*toDiagSVD(Sigma, 4,5)*VT)

rawSetMatrixValues(A, {(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,1),(1,2),(1,3),(1,4)},
     {M2CC(21.12, 5.53), M2CC(-2.46, 33.7), M2CC(11.22, 15.45), M2CC(1.21, -2.332), M2CC(0.200456, 25.7), 
	  M2CC(1.2, 5.45), M2CC(2.3, -.0222), M2CC(4.5, -89.11), M2CC(45.25, 1.5), M2CC(-354.33, 2.2)})
rawSVD(A, Sigma, U, VT)
assert(A == U*toDiagSVD(Sigma, 4,5)*VT)

-- least squares

toTransposeRR = (A) -> (
     m := rawNumberOfRowsRR(A);
     n := rawNumberOfColumnsRR(A);
     B := rawMatrixRR(n,m);
     i := 0;
     while (i < m) do (
      	   j := 0;
     	   while (j < n) do (
		x := 0.0;
	  	rawGetMatrixEntryRR(A,i,j,x);
	  	rawSetMatrixEntryRR(B,j,i,x);
		j = j+1;
	  );
     	  i = i+1;
	  );
     B);

toTransposeCC = (A) -> (
     m := rawNumberOfRowsCC(A);
     n := rawNumberOfColumnsCC(A);
     B := rawMatrixCC(n,m);
     i := 0;
     while (i < m) do (
      	   j := 0;
     	   while (j < n) do (
		x := M2CC(0.0, 0.0);
	  	rawGetMatrixEntryCC(A,i,j,x);
	  	rawSetMatrixEntryCC(B,j,i,x);
		j = j+1;
	  );
     	  i = i+1;
	  );
     B);

rawSetEpsilonMatrixRR(0.000001)
rawSetEpsilonMatrixCC(0.000001)

A = rawMatrixRR(4,5)
x = rawMatrixRR(0,0)
y = rawMatrixRR(0,0)
rawSetMatrixValues(A, {(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,1),(1,2),(1,3),(1,4),
	  (2,0),(2,1),(2,2),(2,3),(2,4),(3,0),(3,1),(3,2),(3,3),(3,4)},
     {1.1, -2.33, 0.00456, 25.7, 1.2, 5.45, 1.1, -2.33, 0.00456, 25.7, 1.2, 5.45,
	  2.3, -.0222, 4.5, -89.11, 45.25, 1.5, -34.33, 2.2})
b = rawGetSubmatrix(A, {0,1,2,3}, {4,1})
rawLeastSquares(A,b,x)
assert(A*x == b)
rawLeastSquaresDeficient(A,b,y)
assert(A*y == b)
assert(x == y)

rawSetMatrixValues(b, {(0,0),(1,0),(2,0),(3,0)}, {1.2, 25.7, 4.5, 2.2})
rawSetMatrixValues(b, {(0,1),(1,1),(2,1),(3,1)}, {-200.33, 101.1, 352.45, 51.25})
rawLeastSquares(A,b,x)
assert(A*x == b)
rawLeastSquaresDeficient(A,b,y)
assert(A*y == b)
assert(x == y)

B = toTransposeRR(A)
b = rawGetSubmatrix(B, {0,1,2,3,4}, {3,2})
rawLeastSquares(B,b,x)
assert(B*x == b)
rawLeastSquaresDeficient(B,b,y)
assert(B*y == b)
assert(x == y)

rawSetMatrixValues(b, {(0,0),(1,0),(2,0),(3,0),(1,1)}, {331.1, 0.535, -1.4, -22.22, 1000.1})
rawLeastSquares(B,b,x)
assert(B*x != b)
rawLeastSquaresDeficient(B,b,y)
assert(B*y != b)
assert(x == y)

rawSetMatrixValues(B, {(0,1),(1,1),(2,1),(3,1),(4,1)}, {-89.11, 45.25, 1.5, -34.33, 2.2}) -- deficient rank
rawLeastSquares(B,b,x) 
rawLeastSquaresDeficient(B,b,y)
assert(x != y)


A = rawMatrixCC(4,5)
x = rawMatrixCC(0,0)
y = rawMatrixCC(0,0)
rawSetMatrixValues(A, {(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,1),(1,2),(1,3),(1,4),
	  (2,0),(2,1),(2,2),(2,3),(2,4),(3,0),(3,1),(3,2),(3,3),(3,4)},
     {M2CC(21.12, 5.53), M2CC(-2.46, 33.7), M2CC(11.22, 15.45), M2CC(1.21, -2.332), M2CC(0.200456, 25.7), 
	  M2CC(1.2, 5.45), M2CC(2.3, -.0222), M2CC(4.5, -89.11), M2CC(45.25, 1.5), M2CC(-354.33, 2.2),
	  M2CC(1.1, -2.33), M2CC(0.00456, 25.7), M2CC(1.2, 5.45), M2CC(1.1, -2.33), M2CC(0.00456, 25.7), 
	  M2CC(1.2, 5.45), M2CC(2.3, -.0222), M2CC(4.5, -89.11), M2CC(45.25, 1.5), M2CC(-34.33, 2.2)})
b = rawGetSubmatrix(A, {0,1,2,3,4}, {3,2})
rawLeastSquares(A,b,x)
assert(A*x == b)
rawLeastSquaresDeficient(A,b,y)
assert(A*y == b)
assert(x == y)

rawSetMatrixValues(b, {(0,0),(1,0),(2,0),(3,0)}, {M2CC(1.2, 25.7), M2CC(4.5, 2.2), M2CC(1.1,-1.1), M2CC(0.0,-3.22)})
rawSetMatrixValues(b, {(0,1),(3,1)}, {M2CC(-200.33, 101.1), M2CC(352.45, 51.25)})
rawLeastSquares(A,b,x)
assert(A*x == b)
rawLeastSquaresDeficient(A,b,y)
assert(A*y == b)
assert(x == y)

B = toTransposeCC(A)
b = rawGetSubmatrix(B, {0,1,2,3,4}, {3,2})
rawLeastSquares(B,b,x)
assert(B*x == b)
rawLeastSquaresDeficient(B,b,y)
assert(B*y == b)
assert(x == y)

rawSetMatrixValues(b, {(1,0),(3,0),(1,1)}, {M2CC(331.1, 0.535), M2CC(-1.4,-22.22), M2CC(1000.1, -9.78)})
rawLeastSquares(B,b,x)
assert(B*x != b)
rawLeastSquaresDeficient(B,b,y)
assert(B*y != b)
assert(x == y)

rawSetMatrixValues(B, {(4,1)}, {M2CC(-34.33, 2.2)}) -- deficient rank
rawLeastSquares(B,b,x)
rawLeastSquaresDeficient(B,b,y)
assert(x != y)

b = rawGetSubmatrix(B, {0,1,2,3,4}, {3,2})
rawLeastSquares(B,b,x)
assert(B*x == b)
rawLeastSquaresDeficient(B,b,y)
assert(B*y == b)
assert(x != y)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
-- End:
