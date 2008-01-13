document { 
     Key => {solve,(solve,Matrix,Matrix),
	  (solve,MutableMatrix,MutableMatrix)},
     Headline => "solve a linear equation",
     Usage => "x = solve(A,b)",
     Inputs => {
	  "A" => {ofClass Matrix, ", or ", ofClass MutableMatrix, " of size m by n over either
	  a finite field ZZ/p, RR or CC"},
	  "b" => {"the same type of matrix, over the same ring, of size m by r"}
	  },
     Outputs => {
	  "x" => {"the same type of matrix, over the same ring, of size n by r,
	       such that ", TT "Ax=b"}
	  },
     "There are several restrictions.  The first is that there are only a limited number of rings
     for which this function is implemented.  Second, over ", TO "RR", " or ", TO "CC", 
     ", the matrix ", TT "A", " must be a square
     non-singular matrix.  Third, if ", TT "A", " and ", TT "b", 
     " are mutable matrices over ", TT "RR", " or ", TT "CC", ", they must be dense matrices.",
     PARA{},
     EXAMPLE lines ///
     	  kk = ZZ/101;
     	  A = matrix"1,2,3,4;1,3,6,10;19,7,11,13" ** kk
	  b = matrix"1;1;1" ** kk
	  x = solve(A,b)
	  A*x-b
     ///,
     "Over ", TT "RR", " or ", TT "CC", ", the matrix ", TT "A", " must be a non-singular square matrix.",
     EXAMPLE lines ///
     	  kk = RR
     	  A = matrix"1,2,3;1,3,6;19,7,11" ** kk
	  b = matrix"1;1;1" ** kk
	  x = solve(A,b)
	  A*x-b
     ///,
     "For large dense matrices over ", TT "RR", " or ", TT "CC", ", this function calls 
     the lapack routines.",
     EXAMPLE lines ///
     	  kk = CC
	  A = mutableMatrix(kk,1000,1000, Dense=>true)
	  b = mutableMatrix(kk,1000,2, Dense=>true)
	  for i from 1 to 10000 do A_(random 1000, random 1000) = random 1.0 + ii * random 1.0;
	  for i from 0 to 999 do b_(i,0) = random 1.0 + ii * random 1.0;
	  for i from 0 to 999 do b_(i,1) = random 1.0 + ii * random 1.0;	  
	  time x = solve(A,b);
	  C = (matrix A)*(matrix x)-matrix b;
	  C_(123,0)
	  C_(567,1)
     ///,
     "This may be used to invert a matrix over ", TT "ZZ/p", ", ", TT "RR", " or ", TT "QQ", ".",
     EXAMPLE lines ///
          kk = RR
          A = random(kk^5, kk^5)
	  I = id_(target A)
	  x = solve(A,I)
	  A*x - I
	  x*A - I
     	  ///,
     "Another method, which isn't generally as fast, and isn't as stable over ", TT "RR", " or ", TT "CC", ", 
     is to lift the matrix ", TT "b", "
     along the matrix ", TT "A", " (see ", TO (symbol//,Matrix,Matrix), ").",
     EXAMPLE lines ///
          invA = I // A
	  x == invA
	  x - invA
          ///,
     Caveat => {"This function is limited in scope, but is sometimes useful for very large 
	  matrices"},
     SeeAlso => {LU, SVD, MutableMatrix}
     }
