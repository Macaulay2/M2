--- status: 
--- author(s): M2fest 2005
--- notes: 

document { 
     Key => diagonalMatrix,
     Headline => "make a diagonal matrix",
    SeeAlso => {"matrices"},
    Subnodes => {
	TO (diagonalMatrix, Matrix),
        TO (diagonalMatrix, Ring, ZZ, ZZ, List),
        },
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
     Key => {(diagonalMatrix, Ring, ZZ, ZZ, List),(diagonalMatrix,Ring,List),(diagonalMatrix,List),
	  (diagonalMatrix, RingFamily, List),(diagonalMatrix, RingFamily, ZZ, ZZ, List),
	  (diagonalMatrix, ZZ, ZZ, List)
	  },
     Headline => "make a diagonal matrix from a list",
     Usage => "diagonalMatrix(R,L)\ndiagonalMatrix L\ndiagonalMatrix(m,n,L)\ndiagonalMatrix(R,m,n,L)",
     Inputs => {
	  "R" => Ring => "the ring of the matrix, if specified",
	  "m" => ZZ => "the number of rows, if specified",
	  "n" => ZZ => "the number of columns, if specified",
	  "L" => List => {" of elements in the ring ", TT "R", " not longer than ", TT "m", " or ", TT "n"}
	  },
     Outputs => {
	  Matrix => {"a diagonal matrix whose diagonal entries are the elements of ",  TT "L"}
	  },
     EXAMPLE lines ///
	  R = QQ[a..d];
	  diagonalMatrix{a,b,c,3/4}
	  diagonalMatrix(3,5,{a,b,3/4})
	  diagonalMatrix(R,{1,2,3})
	  ///,
     SeeAlso => {"matrices"}
     }

