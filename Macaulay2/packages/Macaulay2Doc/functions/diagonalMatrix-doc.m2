--- status: 
--- author(s): M2fest 2005
--- notes: 

document { 
     Key => diagonalMatrix,
     Headline => "make a diagonal matrix",
     SeeAlso => {"matrices"}
     }

document { 
     Key => (diagonalMatrix,Matrix),
     Headline => "make a diagonal matrix from entries of a matrix",
     Usage => "diagonalMatrix m",
     Inputs => {
	  "m" => Matrix
	  },
     Outputs => {
	  Matrix => {"a diagonal matrix whose diagonal entries are the entries of ",  "m"}
	  },
     EXAMPLE {
	  "R = QQ[a..d];",
	  "diagonalMatrix vars R"
	  },
     "If the matrix has more than one row, the first column is placed on the diagonal
     first, then the second column, etc.",
     EXAMPLE {
	  "diagonalMatrix matrix{{0,2},{4,6}}"
	  },
     SeeAlso => {"matrices"}
     }

document { 
     Key => {(diagonalMatrix,Ring,List),(diagonalMatrix,List)},
     Headline => "make a diagonal matrix from a list",
     Usage => "diagonalMatrix(R,L)\ndiagonalMatrix L",
     Inputs => {
	  "R" => Ring,
	  "L" => List => {"of elements in the ring ", TT "R"}
	  },
     Outputs => {
	  Matrix => {"a diagonal matrix whose diagonal entries are the elements of ",  TT "L"}
	  },
     "If the elements of L all have the same ring, the ring may be omitted.",
     EXAMPLE lines ///
	  R = QQ[a..d];
	  diagonalMatrix{a,b,c,3/4}
	  diagonalMatrix(R,{1,2,3})
	  ///,
     SeeAlso => {"matrices"}
     }

