--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {eigenvalues, (eigenvalues,Matrix), (eigenvalues,MutableMatrix)},
     Headline => "find eigenvalues of a matrix over RR or CC",
     Usage => "eigenvalues M",
     Inputs => {
	  "M" => Matrix => {"or a ", TO MutableMatrix, " over ", TO RR, " or ", TO CC, ", which is
	       a square n by n matrix"}
	  },
     Outputs => {
	  {"An n by 1 ", TO Matrix, " or ", TO MutableMatrix, " (the same type as ", TT "M", ")"}
	  },
     "The resulting matrix is over CC, and contains the eigenvalues of ", TT "M", ".  The lapack
     library is used to compute eigenvectors of real and complex matrices.",
     EXAMPLE {
	  "M = matrix{{1.0, 2.0}, {5.0, 7.0}}",
	  "eigenvalues M"
	  },
     "If the matrix is symmetric (over RR) or Hermitian (over CC),
     this information should be provided as an optional argument ",
     TT "Hermitian=>true", ".  In this case,
     the resulting matrix of eigenvalues is defined over ", TT "RR", ", not ", TT "CC", ".",
     EXAMPLE {
	  "M = matrix{{1.0, 2.0}, {2.0, 1.0}}",
	  "eigenvalues(M, Hermitian=>true)"
	  },
     "If the matrix you wish to use is defined over ", TT "ZZ", " or ", TT "QQ", ", then first move it to ", TT "RR", ".",
     EXAMPLE {
	  "M = matrix(QQ,{{1,2/17},{2,1}})",
	  "M = substitute(M,RR)",
	  "eigenvalues M"
	  },
     Caveat => {"The eigenvalues are approximate."},
     SeeAlso => {eigenvectors, SVD, substitute}
     }
document { 
     Key => [eigenvalues, Hermitian],
     Headline => "Hermitian=>true means assume the matrix is symmetric or Hermitian",
     Usage => "eigenvalues(M, Hermitian=>true)",
     Inputs => {
	  },
     Consequences => {
	  "The resulting matrix of eigenvalues is defined over RR, not CC."
	  },     
     EXAMPLE {
	  },
     Caveat => {"The internal routine uses a different algorithm, only considering the
	  upper triangular elements.  So if the matrix is not symmetric or Hermitian,
	  the routine will give incorrect results."},
     SeeAlso => {eigenvectors}
     }
TEST ///
m = map(CC^10, CC^10, (i,j) -> i^2 + j^3*ii)
eigenvalues m
m = map(CC^10, CC^10, (i,j) -> (i+1)^(j+1))
eigenvalues m
m = map(RR^10, RR^10, (i,j) -> (i+1)^(j+1))
eigenvalues m
///
