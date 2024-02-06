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
     "The resulting matrix is over ", TO "CC", ", and contains the eigenvectors of ", TT "M", ".  The lapack and eigen
     libraries are used to compute eigenvectors of real and complex matrices.",
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
