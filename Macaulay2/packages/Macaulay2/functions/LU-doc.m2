document { 
     Key => {LU,(LU,MutableMatrix)},
     Headline => "LU decomposition",
     Usage => "(P,L,U) = LU A",
     Inputs => {
	  "A" => MutableMatrix => {"of size ", TT "m", " by ", TT "n", ", over a 
	  a finite field ", TT "ZZ/p", ", ", TO "RR", " or ", TO "CC", ""}
	  },
     Outputs => {
	  "P" => List => {"of integers in [0, ..., m-1] forming a permutation"},
	  "L" => MutableMatrix => {"an m by m lower triangular matrix, with 1's on the diagonal"},
	  "U" => MutableMatrix => {"an m by n upper triangular matrix"}
	  },
     "If ", TT "Q", " is the ", TT "m", " by ", TT "m", " permutation matrix 
     such that ", TT "Q_(i,P_i) = 1", ", and all other entries are zero,
     then ", TT "A = QLU", ".  The matrix ", TT "A", " is not modified",
     PARA{},
     "There are several restrictions.  The first is that there are only a limited number of rings
     for which this function is implemented. Second, if ", TT "A", 
     " is defined over ", TT "RR", " or ", TT "CC", ", then ", TT "A", " must be densely encoded.",
     PARA{},
     EXAMPLE lines ///
     	  kk = ZZ/101;
     	  A = matrix"1,2,3,4;1,3,6,10;19,7,11,13" ** kk
	  A = mutableMatrix A
	  (P,L,U) = LU A
	  matrix L * matrix U == matrix A
     ///,
     "Over ", TT "RR", " or ", TT "CC", ", the matrix ", TT "A", " must be densely encoded (which is the default).",
     EXAMPLE lines ///
     	  kk = RR
     	  A = matrix"1,2,3,4,5,6;1,3,6,12,13,16;19,7,11,47,48,21" ** kk
	  A = mutableMatrix(A, Dense=>true)
	  (P,L,U) = LU A
	  Q = mutableMatrix(kk, numrows A, numrows A)
	  for i from 0 to numrows A - 1 do Q_(i,P_i) = 1.0
	  matrix Q * matrix L * matrix U == matrix A
     ///,
     "For matrices over ", TT "RR", " or ", TT "CC", ", this function calls 
     the lapack routines.",
     EXAMPLE lines ///
     	  kk = CC
	  A = mutableMatrix(kk,1000,900, Dense=>true)
	  for i from 1 to 10000 do A_(random 1000, random 900) = random 1.0 + ii * random 1.0;
	  time (P,L,U) = LU A;
     ///,
     Caveat => {"This function is limited in scope, but is sometimes useful for very large 
	  matrices"},
     SeeAlso => {solve, SVD, MutableMatrix}
     }
