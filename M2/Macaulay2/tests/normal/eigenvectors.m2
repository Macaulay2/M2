--
M = matrix{{1.0,1.0},{0.0,1.0}}
eigenvalues M
eigenvectors M

M0 = matrix{{1_RR,-1},{1,1}};
M = M0++M0;
(D, P) = eigenvectors M
assert( 1e-10 > norm(M*P - P * diagonalMatrix(D)))

M = matrix{{1.0, 2.0}, {2.0, 1.0}}
eigenvectors(M, Hermitian=>true)

M = matrix{{1.0, 2.0}, {5.0, 7.0}}
(eigvals, eigvecs) = eigenvectors M
-- here we use "norm" on vectors!
assert( 1e-10 > norm ( M * eigvecs_0 - eigvals_0 * eigvecs_0 ) )
assert( 1e-10 > norm ( M * eigvecs_1 - eigvals_1 * eigvecs_1 ) )

printingPrecision = 2

m = map(CC^10, CC^10, (i,j) -> i^2 + j^3*ii)
(eigvals, eigvecs) = eigenvectors m
max (abs \ eigvals) / min (abs \ eigvals)
scan(#eigvals, i -> assert( 1e-10 > norm ( m * eigvecs_i - eigvals_i * eigvecs_i )))

-- some ill-conditioned matrices

m = map(CC^10, CC^10, (i,j) -> (i+1)^(j+1))
(eigvals, eigvecs) = eigenvectors m
max (abs \ eigvals) / min (abs \ eigvals)
apply(#eigvals, i -> norm ( m * eigvecs_i - eigvals_i * eigvecs_i ))
scan(#eigvals, i -> assert( 1e-4 > norm ( m * eigvecs_i - eigvals_i * eigvecs_i )))

m = map(RR^10, RR^10, (i,j) -> (i+1)^(j+1))
(eigvals, eigvecs) = eigenvectors m
max (abs \ eigvals) / min (abs \ eigvals)
apply(#eigvals, i -> norm ( m * eigvecs_i - eigvals_i * eigvecs_i ))
scan(#eigvals, i -> assert( 1e-4 > norm ( m * eigvecs_i - eigvals_i * eigvecs_i )))



--
m = map(CC^10, CC^10, (i,j) -> i^2 + j^3*ii)
eigenvalues m
m = map(CC^10, CC^10, (i,j) -> (i+1)^(j+1))
eigenvalues m
m = map(RR^10, RR^10, (i,j) -> (i+1)^(j+1))
eigenvalues m


