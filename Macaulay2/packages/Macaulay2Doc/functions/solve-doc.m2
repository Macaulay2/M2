document { 
     Key => {solve,(solve,Matrix,Matrix),
	  (solve,MutableMatrix,MutableMatrix),[solve,MaximalRank],[solve,ClosestFit]
	  },
     Headline => "solve a linear equation",
     Usage => "x = solve(A,b)",
     Inputs => {
	  "A" => {ofClass Matrix, ", or ", ofClass MutableMatrix, " of size m by n over either
	  a finite field ZZ/p, RR or CC"},
	  "b" => {"the same type of matrix, over the same ring, of size m by r"},
	  MaximalRank => ZZ => {},
	  ClosestFit => Boolean => {"whether to use the least squares method"},
	  MaximalRank => Boolean => {"whether to assume the matrix has maximal rank, in case the least squares method is used"}
	  },
     Outputs => {
	  "x" => {"the same type of matrix, over the same ring, of size n by r,
	       such that ", TT "Ax=b"}
	  },
     "There are several restrictions.  The first is that there are only a limited number of rings
     for which this function is implemented.  Second, over ", TO "RR", " or ", TO "CC", 
     ", the matrix ", TT "A", " must be a square
     non-singular matrix.  Third, if ", TT "A", " and ", TT "b", 
     " are mutable matrices over ", TO "RR", " or ", TO "CC", ", they must be dense matrices.",
     EXAMPLE lines ///
     	  kk = ZZ/101;
     	  A = matrix"1,2,3,4;1,3,6,10;19,7,11,13" ** kk
	  b = matrix"1;1;1" ** kk
	  x = solve(A,b)
	  A*x-b
     ///,
     "Over ", TO "RR", " or ", TO "CC", ", the matrix ", TT "A", " must be a non-singular square matrix.",
     EXAMPLE lines ///
     	  printingPrecision = 2;
     	  A = matrix "1,2,3;1,3,6;19,7,11" ** RR
	  b = matrix "1;1;1" ** RR
	  x = solve(A,b)
	  A*x-b
	  norm oo
     ///,
     "For large dense matrices over ", TO "RR", " or ", TO "CC", ", this function calls 
     the lapack routines.",
     EXAMPLE lines ///
     	  n = 10;
	  A = random(CC^n,CC^n)
	  b = random(CC^n,CC^2)
	  x = solve(A,b)
	  norm ( matrix A * matrix x - matrix b )
     ///,
     "This may be used to invert a matrix over ", TT "ZZ/p", ", ", TO "RR", " or ", TT "QQ", ".",
     EXAMPLE lines ///
          A = random(RR^5, RR^5)
	  I = id_(target A)
	  A' = solve(A,I)
	  norm(A*A' - I)
	  norm(A'*A - I)
     	  ///,
     "Another method, which isn't generally as fast, and isn't as stable over ", TO "RR", " or ", TO "CC", ", 
     is to lift the matrix ", TT "b", "
     along the matrix ", TT "A", " (see ", TO (symbol//,Matrix,Matrix), ").",
     EXAMPLE lines ///
          A'' = I // A
	  norm(A' - A'')
          ///,
     PARA {
	  "For division of matrices, which can also be thought of as solving a
	  system of linear equations, see ", TO (symbol //,Matrix, Matrix), "."
	  },
     Caveat => {"This function is limited in scope, but is sometimes useful for very large 
	  matrices"},
     SeeAlso => {LUdecomposition, SVD, MutableMatrix, norm, random}
     }
