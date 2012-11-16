--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {eigenvectors, (eigenvectors,Matrix), (eigenvectors,MutableMatrix)},
     Headline => "find eigenvectors of a matrix over RR or CC",
     Usage => "(eigvals, eigvecs) = eigenvectors M",
     Inputs => {
	  "M" => Matrix => {"or a ", TO MutableMatrix, " over ", TO RR, " or ", TO CC, ", which is
	       a square n by n matrix"}
	  },
     Outputs => {
	  "eigvals" => VerticalList => {{" a list of the eigenvalues of ", TT "M"}},
	  "eigvecs" => Matrix => {" or ", ofClass MutableMatrix, ", if ", TT "M", " is one),
	       whose columns are the corresponding eigenvectors of ", TT "M"}
	  },
     "The resulting matrix is over ", TO "CC", ", and contains the eigenvectors of ", TT "M", ".  The lapack
     library is used to compute eigenvectors of real and complex matrices.",
     PARA{},
     "Recall that if ", TT "v", " is a non-zero vector such that ", TT "Mv = av", ", for a scalar a, then
     ", TT "v", " is called an eigenvector corresponding to the eigenvalue ", TT "a", ".",
     EXAMPLE {
	  "M = matrix{{1, 2}, {5, 7}}",
	  "eigenvectors M"
	  },
     "If the matrix is symmetric (over ", TO "RR", ") or Hermitian (over ", TO "CC", "),
     this information should be provided as an optional argument ",
     TT "Hermitian=>true", ".  In this case,
     the resulting eigenvalues will be returned as real numbers, and if ", TT "M", " is real,
     the matrix of eigenvectors will be real.",
     EXAMPLE lines ///
     M = matrix {{1, 2}, {2, 1}}
     (e,v) = eigenvectors(M, Hermitian=>true)
     class \ e
     v
     ///,
     Caveat => {"The eigenvectors are approximate."},
     SeeAlso => {eigenvalues, SVD}
     }
document { 
     Key => [eigenvectors, Hermitian],
     Headline => "Hermitian=>true means assume the matrix is symmetric or Hermitian",
     Usage => "eigenvectors(M, Hermitian=>true)",
     Consequences => {
	  "The resulting matrix of eigenvalues is defined over RR, not CC, and, if the 
	  original matrix is defined over RR, the matrix of eigenvalues is too."
	  },     
     Caveat => {"The internal routine uses a different algorithm, only considering the
	  upper triangular elements.  So if the matrix is not symmetric or Hermitian,
	  the routine will give incorrect results."},
     SeeAlso => {eigenvectors}
     }
TEST ///
M = matrix{{1.0,1.0},{0.0,1.0}}
eigenvalues M
eigenvectors M

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

///
