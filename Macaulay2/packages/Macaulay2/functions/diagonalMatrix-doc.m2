--- status: 
--- author(s): M2fest 2005
--- notes: 

document { 
     Key => {diagonalMatrix, (diagonalMatrix,Matrix)},
     Headline => "make a diagonal matrix",
     Usage => "diagonalMatrix m",
     Inputs => {
	  "m" => Matrix => ""
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

