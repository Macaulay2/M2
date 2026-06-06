document {
    Key => {
	(symbol _, Matrix, List),
	(symbol _, MutableMatrix, List),
    },
     Headline => "select columns",
     Usage => "f_cols",
     Inputs => {
	  "f" => {"a matrix between free modules"},
	  "cols" => "a list of integers denoting the choice of columns",
	  },
     Outputs => {
	  {"the submatrix of ", TT "f", " corresponding to the columns ", TT "cols", "."}
	  },
     "This is the same as calling ",
     TO2 ((submatrix,Matrix,VisibleList), TT "submatrix(f,cols)"), ".",
     EXAMPLE {
	  "R = QQ[a..f];",
	  "p = matrix {{a,b,c},{d,e,f}}",
	  "p_{1}",
	  "p_{1,2,0}"
	  },
     SeeAlso => {submatrix, (symbol ^, Matrix, List)}
     }
document {
    Key => {
	(symbol ^, Matrix, List),
	(symbol ^, MutableMatrix, List),
	(symbol ^, Vector, List),
    },
     Headline => "select rows",
     Usage => "f_rows",
     Inputs => {
	  "f" => {"a matrix between free modules"},
	  "rows" => "a list of integers denoting the choice of rows",
	  },
     Outputs => {
	  {"the submatrix of ", TT "f", " corresponding to the rows in the list ", TT "rows", "."}
	  },
     "This is the same as calling ",
     TO2 ((submatrix,Matrix,VisibleList,VisibleList), TT "submatrix(f,rows,)"), ".",
     EXAMPLE {
	  "R = QQ[a..f];",
	  "p = matrix {{a,b,c},{d,e,f}}",
	  "p^{1}",
	  "p^{1,0}"
	  },
     SeeAlso => {submatrix, (symbol _, Matrix, List)}
     }

undocumented {
    (submatrix,Matrix,Nothing,Nothing),
    (submatrix,Matrix,Nothing,VisibleList),
    (submatrix,Matrix,VisibleList,Nothing),
    (submatrix,MutableMatrix,Nothing,Nothing),
    (submatrix,MutableMatrix,Nothing,VisibleList),
    (submatrix,MutableMatrix,VisibleList,Nothing)
    }

-- FIXME: these cannot be documented because of a bug in document.m2
document {
     Key => {
	  submatrix,
--	 (submatrix, Matrix, Nothing, VisibleList),
--	 (submatrix, Matrix, VisibleList, Nothing),
	 (submatrix, Matrix, VisibleList, VisibleList),
--	 (submatrix, MutableMatrix, Nothing, VisibleList),
--	 (submatrix, MutableMatrix, VisibleList, Nothing),
	 (submatrix, MutableMatrix, VisibleList, VisibleList)},
     Headline => "select part of a matrix",
     Usage => "submatrix(f, rows, cols)\nsubmatrix(f,,cols)\nsubmatrix(f,rows,)",
     Inputs => { "f" => "a map between free modules",
	  "rows" => "a list of integers denoting the choice of rows.
	            If omitted, use all rows",
	  "cols" => "a list of integers denoting the choice of columns.
	            If omitted, use all columns"},
     Outputs => {Matrix => {"the submatrix of ", TT "f", " corresponding to the lists ", TT "rows",
	  " and ", TT "cols", ""}},
     "Yields an r by c matrix, where r is the length of the list of integers
     ", TT "rows", ", and c is the length of the list of integers ", TT "cols", ".
     The ", TT "(i,j)", "-th entry of the result is ", TT "f_(rows_i, cols_j)", ".",
     PARA{},
     "Each list of integers may contain ranges and repetitions, as in ",
     TT "{3, 5..7, 3:0}",
     PARA{},
     "If ", TT "rows", " or ", TT "cols", " is omitted, all the corresponding indices are used.",
     PARA{},
     "Both ", TT "rows", " and/or ", TT "cols", " may contain duplicate values,
     in which case the result will contain
     duplicate rows and/or columns.",
     EXAMPLE {
	  "R = ZZ/101[a .. o];",
	  "f = genericMatrix(R, a, 3, 5)",
	  "submatrix(f, {1,2,0}, {0..2, 4})",
	  },
     PARA{},
     EXAMPLE {"submatrix(f, {1,2}, )"},
     TT "submatrix(f,,cols)", " can be written as ", TT "submatrix(f,cols)", ", or ",
     TT "f_cols", ".",
     EXAMPLE {
	  "submatrix(f,,{0,1})",
	  "f_{0,1}"
	  },
     SeeAlso => {
	  submatrix',
	  (symbol_,Matrix,List),
	  (symbol^,Matrix,List),
	  (submatrix,Matrix,VisibleList),
	  (symbol_,Matrix,Array),
	  (symbol^,Matrix,Array),
	  },
     }
document {
     Key => {
	 (submatrix, Matrix, VisibleList),
	 (submatrix, MutableMatrix, VisibleList)},
     Headline => "select columns",
     Usage => "submatrix(f, cols)",
     Inputs => { "f" => "a map between free modules",
	  "cols" => "a list of integers denoting the choice of columns"},
     Outputs => {Matrix => {"the submatrix of ", TT "f", " corresponding to the columns ",
	        TT "cols", ""}},
     "Yields an r by c matrix, where r is the number of rows of ", TT "f", ", and
     c is the length of the list of integers ", TT "cols", ".
     The ", TT "(i,j)", "-th entry of the result is ", TT "f_(i, cols_j)", ".",
     PARA{},
     "The list of integers, ", TT "cols", ", may contain ranges and repetitions, as in ",
     TT "{3, 5..7, 3:0}",
     PARA{},
     "If the list of column indices is a permutation of 0 .. n-1, where n is
     the number of columns, then the result is the corresponding permutation
     of the columns of f.",
     EXAMPLE {
	  "R = ZZ/101[a .. o];",
	  "f = genericMatrix(R, a, 3, 5)",
	  "submatrix(f, {1..3,0})",
	  },
     SeeAlso => {
	  submatrix',
	  (symbol_,Matrix,List),
	  (symbol^,Matrix,List),
	  (symbol_,Matrix,Array),
	  (symbol^,Matrix,Array)
	  },
     }

document {
     Key => {submatrix',
	  (submatrix',Matrix,VisibleList,VisibleList),
	  (submatrix', Matrix, Nothing, VisibleList),
	  (submatrix', Matrix, VisibleList, Nothing),
	  (submatrix', Matrix, Nothing, Nothing),
	  (submatrix', Matrix, VisibleList)},
     Headline => "exclude rows and/or columns of a matrix",
     Usage => "submatrix'(f, rows, cols)\nsubmatrix'(f,,cols)\nsubmatrix'(f,cols)\nsubmatrix'(f,rows,)",
     Inputs => { "f" => "a map between free modules",
	  "rows" => VisibleList => "a list of integers denoting the choice of rows to exclude.
	            If omitted, exclude no rows",
	  "cols" => VisibleList => "a list of integers denoting the choice of columns to exclude.
	            If omitted, exclude no columns"},
     Outputs => {Matrix => {"the submatrix of ", TT "f", " with ", TT "rows",
	  " and ", TT "cols", " left out"}},
     PARA{},
     "Each list of integers may contain ranges and repetitions, as in ",
     TT "{3, 5..7, 3:0}", ".  Excluding a row or column more than once is allowed but has no
     extra effect.",
     PARA{},
     EXAMPLE lines ///
	  R = ZZ/101[a .. o];
	  f = genericMatrix(R, a, 3, 5)
	  submatrix'(f, {1}, {2})
	  submatrix'(f, {1}, {2,3,3,2,2})
	  ///,
     PARA{},
     EXAMPLE lines ///
          submatrix'(f, {1,3}, )
          submatrix'(f, , {1,3} )
          submatrix'(f, {1,3} )
	  ///,
     SeeAlso => {
	  submatrix
	  },
     }

doc ///
   Key
      submatrixByDegrees
     (submatrixByDegrees, Matrix, List, List)
     (submatrixByDegrees, Matrix, Sequence, Sequence)
     (submatrixByDegrees, Matrix, ZZ, ZZ)
   Headline
     submatrix consisting of rows and columns in an interval or box of degrees
   Usage
     submatrixByDegrees(m, targetBox, sourceBox)
   Inputs
     m:Matrix
       A matrix between free modules
     targetBox:Sequence
       A sequence of 2 degree vectors, {\tt (lodegree, hidegree)}.  All rows whose multi-degree is
       greater than or equal (in each component) to lodegree, and less than or equal to hidegree, are selected
     sourceBox:Sequence
       Same as targetBox, except this governs which columns are selected
   Outputs
     :Matrix
       The submatrix of {\tt m} consisting of those rows (respectively columns) whose (multi-)degree lie in
       the box {\tt targetBox}, (respectively {\tt columnBox})
   Description
    Text
      If only one degree (as integer, or list of integers) is given for {\tt targetBox} or {\tt sourceBox}, then
      only rows or columns that match that exact degree are used.
    Example
      R = QQ[a..d];
      I = ideal"a2b-c3,abc-d3,ac2-bd2-cd2,abcd-c4"
      C = res I;
      m = C.dd_2
      submatrixByDegrees(m, 3, 6)
      submatrixByDegrees(m, (3,3), (6,7))
      submatrixByDegrees(m, (4,4), (7,7))
    Text
      For multidegrees, the interval is a box.
    Example
      S = QQ[a..d, Degrees=>{2:{1,0},2:{0,1}}];
      I = ideal(a*d^4, b^3, a^2*d^2, a*b*c*d^3)
      C = res I
      m = C.dd_2
      degrees target m
      degrees source m
      submatrixByDegrees(C.dd_2, ({2,2},{2,4}), ({2,2},{5,4}))
   Caveat
     The degrees are taken from the target and source free modules,
       not from the matrix entries themselves.
   SeeAlso
     submatrix
///
