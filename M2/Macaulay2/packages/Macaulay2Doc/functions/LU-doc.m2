document { 
     Key => {LUdecomposition,(LUdecomposition,MutableMatrix),(LUdecomposition,Matrix)},
     Headline => "LU decomposition",
     Usage => "(P,L,U) = LUdecomposition A",
     Inputs => {
	  "A" => MutableMatrix => {"or ", ofClass Matrix, ",of size ", TT "m", " by ", TT "n", ", over a 
	  a finite field ", TT "ZZ/p", ", ", TO "RR", " or ", TO "CC", ""}
	  },
     Outputs => {
	  "P" => List => {"of integers in [0, ..., m-1] forming a permutation"},
	  "L" => MutableMatrix => {"or ", ofClass Matrix, ", an m by min(m,n) lower 
	            triangular matrix, with 1's on the diagonal"},
	  "U" => MutableMatrix => {"or ", ofClass Matrix, ", a min(m,n) by n upper triangular matrix"}
	  },
     "The output matrices are mutable exactly when the input matrix is, but the matrix ", TT "A", " is not modified",
     PARA{},
     "If ", TT "Q", " is the ", TT "m", " by ", TT "m", " permutation matrix 
     such that ", TT "Q_(P_i,i) = 1", ", and all other entries are zero,
     then ", TT "A = QLU", ".  ",
     PARA{},
     "There are several restrictions.  The first is that there are only a limited number of rings
     for which this function is implemented. Second, if ", TT "A", " is a mutable matrix ",
     " defined over ", TT "RR", " or ", TT "CC", ", then ", TT "A", " must be densely encoded.  This
     restriction is not present if ", TT "A", " is ", ofClass Matrix, ".",
     PARA{},
     EXAMPLE lines ///
          kk = ZZ/101
     	  A = matrix"1,2,3,4;1,3,6,10;19,7,11,13" ** kk
	  (P,L,U) = LUdecomposition A
	  Q = id_(kk^3) _ P
	  Q * L * U == matrix A
     ///,
     "For matrices over ", TT "RR", " or ", TT "CC", ", this function calls 
     the lapack routines, which are restricted to 53 bits of precision.",
     EXAMPLE lines ///
     	  A = matrix"1,2,3,4,5,6;1,3,6,12,13,16;19,7,11,47,48,21" ** RR
	  (P,L,U) = LUdecomposition A
	  Q = id_ (RR^3) _ P
	  Q * L * U - A
	  clean(1e-15,oo)
     ///,
     "Mutable matrices can sometimes be useful for speed, and/or space. 
     If ", TT "A", " is a mutable matrix, it must be densely 
     encoded (which is the default).",
     EXAMPLE lines ///
	  A = mutableMatrix(CC,5,10, Dense=>true)
	  printingPrecision = 2
	  setRandomSeed 0
	  fillMatrix A
	  (P,L,U) = LUdecomposition A;
	  Q = id_(CC^5) _ P
	  matrix Q * matrix L * matrix U - matrix A
	  clean(1e-15,oo)
     ///,
     Caveat => {"This function is limited in scope, but is sometimes useful for very large 
	  matrices"},
     SeeAlso => {solve, SVD, MutableMatrix, fillMatrix, clean, norm}
     }
