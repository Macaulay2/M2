--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {eigenvalues, (eigenvalues,Matrix), (eigenvalues,MutableMatrix)},
     Headline => "find eigenvalues of a matrix",
     Usage => "eigenvalues M",
     Inputs => {
	  "M" => Matrix => {" or a ", TO MutableMatrix, " over ", TO RR, " or ", TO CC, ", which is
	       a square n by n matrix"}
	  },
     Outputs => { VerticalList => {" a list of the eigenvalues of ", TT "M"} },
     "In this example we compute the eigenvalues of a matrix and display their classes.",
     EXAMPLE lines ///
     M = matrix {{1,2}, {5,7}}
     eigenvalues M
     class \ oo
     ///,
     "If the matrix is symmetric (over ", TO "RR", ") or Hermitian (over ", TO "CC", "),
     this information may be provided as an optional argument ",
     TT "Hermitian=>true", ", so
     the resulting eigenvalues will be in ", TO "RR", ", not ", TO "CC", ".",
     EXAMPLE lines ///
     M = matrix {{1,2}, {2,1}}
     eigenvalues(M, Hermitian=>true)
     class \ oo
     ///,
     "The LAPACK and Eigen libraries are used to compute eigenvectors of real and complex matrices.",
     Caveat => {"The eigenvalues are approximate."},
     SeeAlso => {eigenvectors, SVD},
     Subnodes => { TO [eigenvalues, Hermitian] },
     }
document { 
     Key => [eigenvalues, Hermitian],
     Headline => "whether to assume the matrix is symmetric or Hermitian",
     Usage => "eigenvalues(M, Hermitian=>true)",
     Consequences => {
	  "The resulting list of eigenvalues is defined over RR, not CC."
	  },     
     Caveat => {"The internal routine uses a different algorithm, only considering the
	  upper triangular elements.  So if the matrix is not symmetric or Hermitian,
	  the routine will give incorrect results."},
     SeeAlso => {eigenvectors}
     }

