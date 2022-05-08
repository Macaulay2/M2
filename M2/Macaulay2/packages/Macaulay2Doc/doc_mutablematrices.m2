document {
     Key => "row and column operations",
     "The usual row and column operations apply to 
     mutable matrices.  These are:",
     UL {
	  TO rowAdd,
	  TO rowSwap,
	  TO rowPermute,
	  TO rowMult,
	  TO columnAdd,
	  TO columnSwap,
	  TO columnPermute,
	  TO columnMult,
	  },
     }

document {
     Key => "mutable matrices",
     "To and from matrices:",
     EXAMPLE {
	  "m = matrix{{1,2,3},{4,5,6}}",
	  "n = mutableMatrix m",
	  "m2 = matrix n",
	  "m2 - m == 0"
	  },
     "Modifying and accessing entries.  Remember that the upper left
     entry is (0,0), not (1,1)!",
     EXAMPLE {
	  "n_(0,0) = 212314323",
	  "n_(0,0)",
	  "n"
	  },
     "Number of rows, columns, and the ring:",
     EXAMPLE {
	  "numrows n",
	  "numColumns n",
	  "numcols n",
	  "ring n"
	  },
     PARA{},
     TO "row and column operations",
     PARA{},
     "Some other methods for creating mutable matrices.",
     EXAMPLE {
	  "mutableIdentity(RR_100,5)",
	  "mutableMatrix(QQ,3,5)",
	  "randomMutableMatrix(4,4,.5,100)"
	  }
     }

document {
     Key => MutableMatrix,
     Headline => "the class of all mutable matrices",
     "A mutable matrix in Macaulay2 is a rectangular array of elements
     of a specific ring, whose entries can be modified.",
     PARA{},
     "A mutable matrix is different from a ", TO Matrix, " in that
     a matrix contains degree information for the target and source of
     the matrix, while a mutable matrix has no such information.  Also,
     more operations are provided for matrices.",
     PARA{},
     "For an overview of mutable matrices, see ", TO "mutable matrices", ".",
     PARA{},
     "Mutable matrices can either be encoded in a sparse manner (the matrix
     only encodes the non-zero elements), or in a dense manner (all elements
     are stored -- even zeros).  The distinction is an option to several of
     the routines that create mutable matrices (", TO mutableMatrix, 
	  ", ", 
	  TO mutableIdentity,
     ").  
     Certain operations over RR or CC are performed using the lapack library
     and require dense encoding of matrices: ",
     TO "LUdecomposition", ", ", TO "SVD", ", ", TO "solve", ", ", TO "eigenvalues", ", ", TO "eigenvectors", ".",
     
     HEADER3 "row and column operations",
     UL {
	  TO rowAdd,
	  TO rowSwap,
	  TO rowPermute,
	  TO rowMult,
	  TO columnAdd,
	  TO columnSwap,
	  TO columnPermute,
	  TO columnMult,
	  },
     HEADER3 "matrix arithmetic",
     "Many matrix arithmetic routines are only available for immutabie 
     matrices, not
     mutable matrices.  It is necessary to use ", TO matrix, " to make
     an immutable matrix first.",
     }


--- status: TODO
--- author(s): MES
--- notes: 

document { 
     Key => {rowAdd,
	  (rowAdd,MutableMatrix,ZZ,Number,ZZ),(rowAdd,MutableMatrix,ZZ,RingElement,ZZ)},
     Headline => "add a multiple of one row to another",
     Usage => "rowAdd(m,i,a,j)",
     Inputs => {
	  "m" => MutableMatrix,
	  "i" => ZZ,
	  "a" => RingElement => {"in the same ring as ", TT "m"},
	  "j" => ZZ
          },
     Consequences => {
	  {"The ", TT "i", " th row of ", TT "m", 
	  " is modified by adding ", TT "a", 
	  " times the ", TT "j", " th 
	  row of ", TT "m"}
          },     
     EXAMPLE {
	  "R = ZZ[a..f];",
	  "m = mutableMatrix genericMatrix(R,a,2,3)",
	  "rowAdd(m,0,c,1)",
	  "m"
          },
     SeeAlso => {"mutable matrices", "row and column operations"}
     }

document { 
     Key => {columnAdd,(columnAdd,MutableMatrix,ZZ,Number,ZZ),
	  (columnAdd,MutableMatrix,ZZ,RingElement,ZZ)},
     Headline => "add a multiple of one column to another",
     Usage => "columnAdd(m,i,a,j)",
     Inputs => {
	  "m" => MutableMatrix,
	  "i" => ZZ,
	  "a" => RingElement => {"in the same ring as ", TT "m"},
	  "j" => ZZ
          },
     Consequences => {
	  {"The ", TT "i", " th column of ", TT "m", 
	  " is modified by adding ", TT "a", 
	  " times the ", TT "j", " th 
	  column of ", TT "m"}
          },     
     EXAMPLE {
	  "R = ZZ[a..f];",
	  "m = mutableMatrix genericMatrix(R,a,2,3)",
	  "columnAdd(m,0,c,1)",
	  "m"
          },
     SeeAlso => {"mutable matrices", "row and column operations"}
     }

document { 
     Key => {rowSwap,
	  (rowSwap,MutableMatrix,ZZ,ZZ)},
     Headline => "interchange rows",
     Usage => "rowSwap(m,i,j)",
     Inputs => {
	  "m" => MutableMatrix,
	  "i" => ZZ,
	  "j" => ZZ
          },
     Consequences => {
	  {"Interchanges the ", TT "i", " th and ", TT "j", 
	       " th rows of ", TT "m"}
          },
     EXAMPLE {
	  "m = mutableMatrix matrix{{1,2,3},{4,5,6}}",
	  "rowSwap(m,0,1)",
	  "m"
          },
     SeeAlso => {"mutable matrices", "row and column operations"}
     }

document { 
     Key => {columnSwap,
	  (columnSwap,MutableMatrix,ZZ,ZZ)},
     Headline => "interchange columns",
     Usage => "columnSwap(m,i,j)",
     Inputs => {
	  "m" => MutableMatrix,
	  "i" => ZZ,
	  "j" => ZZ
          },
     Consequences => {
	  {"Interchanges the ", TT "i", " th and ", TT "j", 
	       " th columns of ", TT "m"}
          },
     EXAMPLE {
	  "m = mutableMatrix matrix{{1,2,3},{4,5,6}}",
	  "columnSwap(m,0,1)",
	  "m"
          },
     SeeAlso => {"mutable matrices", "row and column operations"}
     }

document { 
     Key => {rowMult,(rowMult,MutableMatrix,ZZ,Number),
	  (rowMult,MutableMatrix,ZZ,RingElement)},
     Headline => "multiply a row by a ring element",
     Usage => "rowMult(m,i,a)",
     Inputs => {
	  "m" => MutableMatrix,
	  "i" => ZZ,
	  "a" => RingElement => {"in the same ring as ", TT "m"},
          },
     Consequences => {
	  {"The ", TT "i", " th row of ", TT "m", 
	  " is modified by multiplying it by ", TT "a"}
          },     
     EXAMPLE {
	  "R = ZZ[a..f];",
	  "m = mutableMatrix genericMatrix(R,a,2,3)",
	  "rowMult(m,0,c)",
	  "m"
          },
     SeeAlso => {"mutable matrices", "row and column operations"}
     }

document { 
     Key => {columnMult,(columnMult,MutableMatrix,ZZ,Number),
	  (columnMult,MutableMatrix,ZZ,RingElement)},
     Headline => "multiply a column by a ring element",
     Usage => "columnMult(m,i,a)",
     Inputs => {
	  "m" => MutableMatrix,
	  "i" => ZZ,
	  "a" => RingElement => {"in the same ring as ", TT "m"},
          },
     Consequences => {
	  {"The ", TT "i", " th column of ", TT "m", 
	  " is modified by multiplying it by ", TT "a"}
          },     
     EXAMPLE {
	  "R = ZZ[a..f]",
	  "m = mutableMatrix genericMatrix(R,a,2,3)",
	  "columnMult(m,0,c)",
	  "m"
          },
     SeeAlso => {"mutable matrices", "row and column operations"}
     }
document { 
     Key => {rowPermute,(rowPermute,MutableMatrix,ZZ,List)},
     Headline => "permute some rows",
     Usage => "rowPermute(m,i,{...})",
     Inputs => {
	  "m" => MutableMatrix,
	  "i" => ZZ => "starting row",
	  Nothing => { "a list of integers, denoting a permutation of ", TT "0..d", ", for some number ", TT "d" }
	  },
     Consequences => {
	  {"If the permutation is ", TT "{p0,p1,...,pd}", ", then ", TT "m", " is modified so that
	  the ", TT "i+j", " row becomes the ", TT "i+pj", " row of the original matrix, for ", TT "j=0..d"}
	  },     
     EXAMPLE {
	  "m = mutableMatrix map(ZZ^5,ZZ^6, (i,j) -> 100*i+j)",
	  "rowPermute(m,1,{2,0,1})"
	  },
     SeeAlso => {"mutable matrices", "row and column operations"}
     }
document { 
     Key => {columnPermute,(columnPermute,MutableMatrix,ZZ,List)},
     Headline => "permute some columns",
     Usage => "columnPermute(m,i,{...})",
     Inputs => {
	  "m" => MutableMatrix,
	  "i" => ZZ => "starting column",
	  Nothing => { "a list of integers, denoting a permutation of ", TT "0..d", ", for some number ", TT "d" }
	  },
     Consequences => {
	  {"If the permutation is ", TT "{p0,p1,...,pd}", ", then ", TT "m", " is modified so that
	  the ", TT "i+j", " column becomes the ", TT "i+pj", " column of the original matrix, for ", TT "j=0..d"}
	  },     
     EXAMPLE {
	  "m = mutableMatrix map(ZZ^5,ZZ^6, (i,j) -> 100*i+j)",
	  "columnPermute(m,1,{2,0,1})"
	  },
     SeeAlso => {"mutable matrices", "row and column operations"} 
     }

document {
     Key => { nullSpace, (nullSpace, MutableMatrix) },
     Headline => "find the null space of a mutable matrix",
     Usage => "nullSpace m",
     Inputs => {
	  "m" => MutableMatrix => { "over ", TO "RR", " or ", TO "CC" }
	  },
     Outputs => {
	  MutableMatrix => {"a mutable matrix whose columns span the null space of ", TT "m"}
	  },
     EXAMPLE {
	  "m = mutableMatrix {{1.p500,1},{-2,-2}}",
	  "nullSpace m",
	  "precision oo"
	  }
     }

document {
     Key => { rowRankProfile, (rowRankProfile, MutableMatrix)},
     Headline => "find the row rank profile of a mutable matrix",
     Usage => "rowRankProfile m",
     Inputs => {
	  "m" => MutableMatrix => { "over ", TO "RR", " or ", TO "CC" }
	  },
     Outputs => {
	  List => {"the lexicographically smallest list of indices of linearly independent rows generating the row space of ", TT "m"}
	  },
     EXAMPLE "rowRankProfile mutableMatrix {{1,2,3}, {0,0,0.}, {3,4,5} }",
     SeeAlso => { columnRankProfile }
     }

document {
     Key => { columnRankProfile, (columnRankProfile, MutableMatrix)},
     Headline => "find the column rank profile of a mutable matrix",
     Usage => "columnRankProfile m",
     Inputs => {
	  "m" => MutableMatrix => { "over ", TO "RR", " or ", TO "CC" }
	  },
     Outputs => {
	  List => {"the lexicographically smallest list of indices of linearly independent columns generating the column space of ", TT "m"}
	  },
     EXAMPLE "columnRankProfile transpose mutableMatrix {{1,2,3}, {0,0,0.}, {3,4,5} }",
     SeeAlso => { rowRankProfile }
     }
