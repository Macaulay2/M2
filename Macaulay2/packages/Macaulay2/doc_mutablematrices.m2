document {
     Key => "row and column operations",
     "The usual row and column operations apply to 
     mutable matrices.  These are:",
     UL {
	  TO rowAdd,
	  TO rowSwap,
	  TO rowPermute,
	  TO rowMult,
	  TO "rowDivide",
	  TO columnAdd,
	  TO columnSwap,
	  TO columnPermute,
	  TO columnMult,
	  TO "columnDivide"
	  },
     PARA,
     "It is possible to record these operations (except for the row and column
     divide routines), as follows.",
     EXAMPLE {
	  -- setRowChange and getRowChange have been removed!
	  "m = mutableMatrix{{1,2,3},{5,7,2},{12,1,0}}",
	  -- "setRowChange(m, mutableIdentity(ZZ,3))",
	  "rowAdd(m,1,-5,0)",
	  "rowAdd(m,2,-12,0)",
	  "rowAdd(m,2,-8,1)",
	  "rowSwap(m,1,2)",
	  "rowAdd(m,2,3,1)",
	  -- "getRowChange m"
	  }
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
     PARA,
     TO "row and column operations",
     PARA,
     "Some other methods for creating mutable matrices.",
     EXAMPLE {
	  "mutableIdentity(RR,5)",
	  "mutableZero(QQ,3,5)",
	  "randomMutableMatrix(4,4,.5,100)"
	  }
     }

document {
     Key => MutableMatrix,
     Headline => "the class of all mutable matrices",
     "A mutable matrix in Macaulay2 is a rectangular array of elements
     of a specific ring, whose entries can be modified.",
     PARA,
     "A mutable matrix is different from a ", TO Matrix, " in that
     a matrix contains degree information for the target and source of
     the matrix, while a mutable matrix has no such information.  Also,
     more operations are provided for matrices.",
     PARA,
     "For an overview of mutable matrices, see ", TO "mutable matrices", ".",
     PARA,
     "Mutable matrices can either be encoded in a sparse manner (the matrix
     only encodes the non-zero elements), or in a dense manner (all elements
     are stored -- even zeros).  The distinction is an option to several of
     the routines which create mutable matrices (", TO mutableMatrix, 
	  ", ", 
	  TO mutableZero,
	  ", ", 
	  TO mutableIdentity,
     ").  Certain operations on (dense) matrices over RR or CC (e.g. eigenvalues,
     eigenvectors, LU decomposition, SVD decomposition, and solving) 
     are performed using the lapack library.",
     HEADER3 "row and column operations",
     UL {
	  TO rowAdd,
	  TO rowSwap,
	  TO rowPermute,
	  TO rowMult,
	  TO "rowDivide",
	  TO columnAdd,
	  TO columnSwap,
	  TO columnPermute,
	  TO columnMult,
	  TO "columnDivide"
	  },
     HEADER3 "matrix arithemtic",
     "Many matrix arithmetic routines are only available for immutabie 
     matrices, not
     mutable matrices.  It is necessary to use ", TO matrix, " to make
     an immutable matrix first.",
     PARA
     }


--- status: TODO
--- author(s): MES
--- notes: 

document { 
     Key => {rowAdd,
	  (rowAdd,MutableMatrix,ZZ,QQ,ZZ),
	  (rowAdd,MutableMatrix,ZZ,RingElement,ZZ),
	  (rowAdd,MutableMatrix,ZZ,RR,ZZ),
	  (rowAdd,MutableMatrix,ZZ,CC,ZZ),
	  (rowAdd,MutableMatrix,ZZ,ZZ,ZZ)},
     Headline => "add a multiple of one row to another",
     Usage => "rowAdd(m,i,a,j)",
     Inputs => {
	  "m" => MutableMatrix => "",
	  "i" => ZZ => "",
	  "a" => RingElement => {"in the same ring as ", TT "m"},
	  "j" => ZZ => ""
          },
     Consequences => {
	  {"The ", TT "i", " th row of ", TT "m", 
	  " is modified by adding ", TT "a", 
	  " times the ", TT "j", " th 
	  row of ", TT "m"}
          },     
     EXAMPLE {
	  "R = ZZ[a..f];",
	  "n = genericMatrix(R,a,2,3)",
	  "m = mutableMatrix n",
	  "rowAdd(m,0,c,1)",
	  "m"
          },
     SeeAlso => {"mutable matrices", columnAdd}
     }

document { 
     Key => {columnAdd,
	  (columnAdd,MutableMatrix,ZZ,QQ,ZZ),
	  (columnAdd,MutableMatrix,ZZ,RingElement,ZZ),
	  (columnAdd,MutableMatrix,ZZ,RR,ZZ),
	  (columnAdd,MutableMatrix,ZZ,CC,ZZ),
	  (columnAdd,MutableMatrix,ZZ,ZZ,ZZ)},
     Headline => "add a multiple of one column to another",
     Usage => "columnAdd(m,i,a,j)",
     Inputs => {
	  "m" => MutableMatrix => "",
	  "i" => ZZ => "",
	  "a" => RingElement => {"in the same ring as ", TT "m"},
	  "j" => ZZ => ""
          },
     Consequences => {
	  {"The ", TT "i", " th column of ", TT "m", 
	  " is modified by adding ", TT "a", 
	  " times the ", TT "j", " th 
	  column of ", TT "m"}
          },     
     EXAMPLE {
	  "R = ZZ[a..f];",
	  "n = genericMatrix(R,a,2,3)",
	  "m = mutableMatrix n",
	  "columnAdd(m,0,c,1)",
	  "m"
          },
     SeeAlso => {"mutable matrices",rowAdd}
     }

document { 
     Key => {rowSwap,
	  (rowSwap,MutableMatrix,ZZ,ZZ)},
     Headline => "interchange rows",
     Usage => "rowSwap(m,i,j)",
     Inputs => {
	  "m" => MutableMatrix => "",
	  "i" => ZZ => "",
	  "j" => ZZ => ""
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
     SeeAlso => {"mutable matrices",columnSwap}
     }

document { 
     Key => {columnSwap,
	  (columnSwap,MutableMatrix,ZZ,ZZ)},
     Headline => "interchange columns",
     Usage => "columnSwap(m,i,j)",
     Inputs => {
	  "m" => MutableMatrix => "",
	  "i" => ZZ => "",
	  "j" => ZZ => ""
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
     SeeAlso => {"mutable matrices",rowSwap,columnAdd,columnMult}
     }
document { 
     Key => {rowMult,
	  (rowMult,MutableMatrix,ZZ,QQ),
	  (rowMult,MutableMatrix,ZZ,RingElement),
	  (rowMult,MutableMatrix,ZZ,RR),
	  (rowMult,MutableMatrix,ZZ,CC),
	  (rowMult,MutableMatrix,ZZ,ZZ)},
     Headline => "multiply a row by a ring element",
     Usage => "rowMult(m,i,a)",
     Inputs => {
	  "m" => MutableMatrix => "",
	  "i" => ZZ => "",
	  "a" => RingElement => {"in the same ring as ", TT "m"},
          },
     Consequences => {
	  {"The ", TT "i", " th row of ", TT "m", 
	  " is modified by multiplying it by ", TT "a"}
          },     
     EXAMPLE {
	  "R = ZZ[a..f];",
	  "n = genericMatrix(R,a,2,3)",
	  "m = mutableMatrix n",
	  "rowMult(m,0,c)",
	  "m"
          },
     SeeAlso => {"mutable matrices",columnAdd}
     }

document { 
     Key => {columnMult,
	  (columnMult,MutableMatrix,ZZ,QQ),
	  (columnMult,MutableMatrix,ZZ,RingElement),
	  (columnMult,MutableMatrix,ZZ,RR),
	  (columnMult,MutableMatrix,ZZ,CC),
	  (columnMult,MutableMatrix,ZZ,ZZ)},
     Headline => "multiply a column by a ring element",
     Usage => "columnMult(m,i,a)",
     Inputs => {
	  "m" => MutableMatrix => "",
	  "i" => ZZ => "",
	  "a" => RingElement => {"in the same ring as ", TT "m"},
          },
     Consequences => {
	  {"The ", TT "i", " th column of ", TT "m", 
	  " is modified by multiplying it by ", TT "a"}
          },     
     EXAMPLE {
	  "R = ZZ[a..f]",
	  "n = genericMatrix(R,a,2,3)",
	  "m = mutableMatrix n",
	  "columnMult(m,0,c)",
	  "m"
          },
     SeeAlso => {"mutable matrices", columnAdd, columnSwap, rowMult}
     }
document { 
     Key => {rowPermute,(rowPermute,MutableMatrix,ZZ,List)},
     Headline => "permute some rows",
     Usage => "rowPermute(m,i,{...})",
     Inputs => {
	  "m" => MutableMatrix => "",
	  "i" => ZZ => "starting row",
	  "{...}" => List => "of integers, denoting a permutation of 0,...,d, for
	  some number d"
	  },
     Consequences => {
	  {"If the permutation is {p0,p1,...,pd}, then m is modified so that
	  the i+j row becomes the i+pj row of the original matrix, for j=0..d"}
	  },     
     EXAMPLE {
	  "n = map(ZZ^5,ZZ^6, (i,j) -> 100*i+j)",
	  "m = mutableMatrix n",
	  "rowPermute(m,1,{2,0,1})"
	  },
     SeeAlso => {"mutable matrices", columnPermute, rowAdd, rowMult, rowSwap}
     }
document { 
     Key => {columnPermute,(columnPermute,MutableMatrix,ZZ,List)},
     Headline => "permute some columns",
     Usage => "columnPermute(m,i,{...})",
     Inputs => {
	  "m" => MutableMatrix => "",
	  "i" => ZZ => "starting column",
	  "{...}" => List => "of integers, denoting a permutation of 0,...,d, for
	  some number d"
	  },
     Consequences => {
	  {"If the permutation is {p0,p1,...,pd}, then m is modified so that
	  the i+j column becomes the i+pj column of the original matrix, for j=0..d"}
	  },     
     EXAMPLE {
	  "n = map(ZZ^5,ZZ^6, (i,j) -> 100*i+j)",
	  "m = mutableMatrix n",
	  "columnPermute(m,1,{2,0,1})"
	  },
     SeeAlso => {"mutable matrices",rowPermute,columnAdd,columnSwap,columnMult}
     }

TEST ///
debug Macaulay2Core
M = mutableMatrix(map(ZZ^5, ZZ^7, (i,j) -> 3*i^3 + j^2 +3),Dense=>false)
rawInsertColumns(raw M,3,4)
M
rawDeleteColumns(raw M,8,9)
M

rawInsertRows(raw M,5,6)
M
rawDeleteRows(raw M,1,1)
M

M = mutableMatrix(map(ZZ^5, ZZ^7, (i,j) -> 3*i^3 + j^2 +3),Dense=>true)
rawInsertColumns(raw M,3,4)
M
rawDeleteColumns(raw M,8,9)
M

rawInsertRows(raw M,5,6)
M
rawDeleteRows(raw M,1,1)
M

rawDeleteColumns(raw M,3,6)
M
rawDeleteRows(raw M,4,9)
M
///
