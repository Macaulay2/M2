doc ///
  Key
    "row and column operations"
  Description
    Text
      The usual row and column operations apply to mutable matrices.
      These are:
  Subnodes
    numRows
    rowAdd
    rowSwap
    rowPermute
    rowMult
    numColumns
    columnAdd
    columnSwap
    columnPermute
    columnMult
///

doc ///
  Key
    "mutable matrices"
  Description
    Text
      To and from matrices:
    Example
      m = matrix{{1,2,3},{4,5,6}}
      n = mutableMatrix m
      m2 = matrix n
      m2 - m == 0
    Text
      Modifying and accessing entries.  Remember that the upper left entry is
      (0,0), not (1,1)!
    Example
      n_(0,0) = 212314323
      n_(0,0)
      n
    Text
      Number of rows, columns, and the ring:
    Example
      numrows n
      numColumns n
      numcols n
      ring n
    Text
      @TO "row and column operations"@
    Text
      Some other methods for creating mutable matrices.
    Example
      mutableIdentity(RR_100,5)
      mutableMatrix(QQ,3,5)
      randomMutableMatrix(4,4,.5,100)
  Subnodes
    MutableMatrix
    mutableMatrix
    (mutableMatrix, Ring, ZZ, ZZ)
    mutableIdentity
    "MutableMatrix _ Sequence = Thing"
    "row and column operations"
    fillMatrix
    (randomMutableMatrix, ZZ, ZZ, RR, ZZ)
    -- TODO: move these to linear algebra node?
    nullSpace
    reducedRowEchelonForm
    rowRankProfile
    columnRankProfile
///

doc ///
  Key
    MutableMatrix
  Headline
    the class of all mutable matrices
  Description
    Text
      A mutable matrix in Macaulay2 is a rectangular array of elements of a
      specific ring, whose entries can be modified.
    Text
      A mutable matrix is different from a @TO Matrix@ in that a matrix
      contains degree information for the target and source of the matrix,
      while a mutable matrix has no such information.  Also, more operations
      are provided for matrices.
    Text
      For an overview of mutable matrices, see @TO "mutable matrices"@.
    Text
      Mutable matrices can either be encoded in a sparse manner (the matrix
      only encodes the non-zero elements), or in a dense manner (all elements
      are stored -- even zeros).  The distinction is an option to several of
      the routines that create mutable matrices (@TO mutableMatrix@,
      @TO mutableIdentity@).  Certain operations over RR or CC are performed
      using the LAPACK library and require dense encoding of matrices:
      @TO "LUdecomposition"@, @TO "SVD"@, @TO "solve"@, @TO "eigenvalues"@,
      @TO "eigenvectors"@.
    Tree
      :row and column operations
        rowAdd
        rowSwap
        rowPermute
        rowMult
        columnAdd
        columnSwap
        columnPermute
        columnMult
    Text
      @HEADER4 "matrix arithmetic"@
    Text
      Many matrix arithmetic routines are only available for immutable
      matrices, not mutable matrices.  It is necessary to use @TO matrix@ to
      make an immutable matrix first.
///


--- status: TODO
--- author(s): MES
--- notes:

doc ///
  Key
    rowAdd
    (rowAdd, MutableMatrix, ZZ, Number, ZZ)
    (rowAdd, MutableMatrix, ZZ, RingElement, ZZ)
  Headline
    add a multiple of one row to another
  Usage
    rowAdd(m,i,a,j)
  Inputs
    m:MutableMatrix
    i:ZZ
    a:RingElement
      in the same ring as @TT "m"@
    j:ZZ
  Consequences
    Item
      The @TT "i"@ th row of @TT "m"@ is modified by adding @TT "a"@ times
      the @TT "j"@ th row of @TT "m"@
  Description
    Example
      R = ZZ[a..f];
      m = mutableMatrix genericMatrix(R,a,2,3)
      rowAdd(m,0,c,1)
      m
  SeeAlso
    "mutable matrices"
    "row and column operations"
///

doc ///
  Key
    columnAdd
    (columnAdd, MutableMatrix, ZZ, Number, ZZ)
    (columnAdd, MutableMatrix, ZZ, RingElement, ZZ)
  Headline
    add a multiple of one column to another
  Usage
    columnAdd(m,i,a,j)
  Inputs
    m:MutableMatrix
    i:ZZ
    a:RingElement
      in the same ring as @TT "m"@
    j:ZZ
  Consequences
    Item
      The @TT "i"@ th column of @TT "m"@ is modified by adding @TT "a"@ times
      the @TT "j"@ th column of @TT "m"@
  Description
    Example
      R = ZZ[a..f];
      m = mutableMatrix genericMatrix(R,a,2,3)
      columnAdd(m,0,c,1)
      m
  SeeAlso
    "mutable matrices"
    "row and column operations"
///

doc ///
  Key
    rowSwap
    (rowSwap, MutableMatrix, ZZ, ZZ)
  Headline
    interchange rows
  Usage
    rowSwap(m,i,j)
  Inputs
    m:MutableMatrix
    i:ZZ
    j:ZZ
  Consequences
    Item
      Interchanges the @TT "i"@ th and @TT "j"@ th rows of @TT "m"@
  Description
    Example
      m = mutableMatrix matrix{{1,2,3},{4,5,6}}
      rowSwap(m,0,1)
      m
  SeeAlso
    "mutable matrices"
    "row and column operations"
///

doc ///
  Key
    columnSwap
    (columnSwap, MutableMatrix, ZZ, ZZ)
  Headline
    interchange columns
  Usage
    columnSwap(m,i,j)
  Inputs
    m:MutableMatrix
    i:ZZ
    j:ZZ
  Consequences
    Item
      Interchanges the @TT "i"@ th and @TT "j"@ th columns of @TT "m"@
  Description
    Example
      m = mutableMatrix matrix{{1,2,3},{4,5,6}}
      columnSwap(m,0,1)
      m
  SeeAlso
    "mutable matrices"
    "row and column operations"
///

doc ///
  Key
    rowMult
    (rowMult, MutableMatrix, ZZ, Number)
    (rowMult, MutableMatrix, ZZ, RingElement)
  Headline
    multiply a row by a ring element
  Usage
    rowMult(m,i,a)
  Inputs
    m:MutableMatrix
    i:ZZ
    a:RingElement
      in the same ring as @TT "m"@
  Consequences
    Item
      The @TT "i"@ th row of @TT "m"@ is modified by multiplying it by
      @TT "a"@
  Description
    Example
      R = ZZ[a..f];
      m = mutableMatrix genericMatrix(R,a,2,3)
      rowMult(m,0,c)
      m
  SeeAlso
    "mutable matrices"
    "row and column operations"
///

doc ///
  Key
    columnMult
    (columnMult, MutableMatrix, ZZ, Number)
    (columnMult, MutableMatrix, ZZ, RingElement)
  Headline
    multiply a column by a ring element
  Usage
    columnMult(m,i,a)
  Inputs
    m:MutableMatrix
    i:ZZ
    a:RingElement
      in the same ring as @TT "m"@
  Consequences
    Item
      The @TT "i"@ th column of @TT "m"@ is modified by multiplying it by
      @TT "a"@
  Description
    Example
      R = ZZ[a..f]
      m = mutableMatrix genericMatrix(R,a,2,3)
      columnMult(m,0,c)
      m
  SeeAlso
    "mutable matrices"
    "row and column operations"
///

doc ///
  Key
    rowPermute
    (rowPermute, MutableMatrix, ZZ, List)
  Headline
    permute some rows
  Usage
    rowPermute(m,i,{...})
  Inputs
    m:MutableMatrix
    i:ZZ
      starting row
    :List
      a list of integers, denoting a permutation of @TT "0..d"@, for some
      number @TT "d"@
  Consequences
    Item
      If the permutation is @TT "{p0,p1,...,pd}"@, then @TT "m"@ is modified
      so that the @TT "i+j"@ row becomes the @TT "i+pj"@ row of the original
      matrix, for @TT "j=0..d"@
  Description
    Example
      m = mutableMatrix map(ZZ^5,ZZ^6, (i,j) -> 100*i+j)
      rowPermute(m,1,{2,0,1})
  SeeAlso
    "mutable matrices"
    "row and column operations"
///

doc ///
  Key
    columnPermute
    (columnPermute, MutableMatrix, ZZ, List)
  Headline
    permute some columns
  Usage
    columnPermute(m,i,{...})
  Inputs
    m:MutableMatrix
    i:ZZ
      starting column
    :List
      a list of integers, denoting a permutation of @TT "0..d"@, for some
      number @TT "d"@
  Consequences
    Item
      If the permutation is @TT "{p0,p1,...,pd}"@, then @TT "m"@ is modified
      so that the @TT "i+j"@ column becomes the @TT "i+pj"@ column of the
      original matrix, for @TT "j=0..d"@
  Description
    Example
      m = mutableMatrix map(ZZ^5,ZZ^6, (i,j) -> 100*i+j)
      columnPermute(m,1,{2,0,1})
  SeeAlso
    "mutable matrices"
    "row and column operations"
///

doc ///
  Key
    nullSpace
    (nullSpace, MutableMatrix)
  Headline
    find the null space of a mutable matrix
  Usage
    nullSpace m
  Inputs
    m:MutableMatrix
      over @TO "RR"@ or @TO "CC"@
  Outputs
    :MutableMatrix
      a mutable matrix whose columns span the null space of @TT "m"@
  Description
    Example
      m = mutableMatrix {{1.p500,1},{-2,-2}}
      nullSpace m
      precision oo
///

doc ///
  Key
    rowRankProfile
    (rowRankProfile, MutableMatrix)
  Headline
    find the row rank profile of a mutable matrix
  Usage
    rowRankProfile m
  Inputs
    m:MutableMatrix
      over @TO "RR"@ or @TO "CC"@
  Outputs
    :List
      the lexicographically smallest list of indices of linearly independent
      rows generating the row space of @TT "m"@
  Description
    Example
      rowRankProfile mutableMatrix {{1,2,3}, {0,0,0.}, {3,4,5} }
  SeeAlso
    columnRankProfile
///

doc ///
  Key
    columnRankProfile
    (columnRankProfile, MutableMatrix)
  Headline
    find the column rank profile of a mutable matrix
  Usage
    columnRankProfile m
  Inputs
    m:MutableMatrix
      over @TO "RR"@ or @TO "CC"@
  Outputs
    :List
      the lexicographically smallest list of indices of linearly independent
      columns generating the column space of @TT "m"@
  Description
    Example
      columnRankProfile transpose mutableMatrix {{1,2,3}, {0,0,0.}, {3,4,5} }
  SeeAlso
    rowRankProfile
///

doc ///
  Key
    mutableMatrix
    (mutableMatrix, MutableMatrix)
    (mutableMatrix, Matrix)
    (mutableMatrix, List)
    (mutableMatrix, Ring, List)
    (mutableMatrix, RingFamily, List)
    [mutableMatrix, Dense]
  Headline
    make a mutable matrix
  Usage
    mutableMatrix m
  Inputs
    m:{Matrix, MutableMatrix, List}
    Dense => Boolean
      whether the encoding of the matrix should be dense or not: see
      @TO MutableMatrix@
  Outputs
    :MutableMatrix
      a new mutable matrix whose entries are obtained from @TT "m"@.  If
      @TT "m"@ is a list, it should be a doubly nested list (table) of ring
      elements, all from the same ring.
  Description
    Example
      f = mutableMatrix {{1,2,3,4}}
      f_(0,2)
      f_(0,2) = 33
      f
      R = QQ[a..z]
      mutableMatrix genericMatrix(R,3,3)
///

doc ///
  Key
    (mutableMatrix, Ring, ZZ, ZZ)
    (mutableMatrix, RingFamily, ZZ, ZZ)
  Headline
    make a mutable matrix filled with zeroes
  Usage
    mutableMatrix(R,nrows,ncols)
  Inputs
    R:{Ring, RingFamily}
    nrows:ZZ
    ncols:ZZ
    Dense => Boolean
      whether the encoding of the matrix should be dense or not: see
      @TO MutableMatrix@
  Outputs
    :MutableMatrix
      an @TT "nrows"@ by @TT "ncols"@ mutable matrix filled with zeroes from
      the ring @TT "R"@
  Description
    Example
      m = mutableMatrix(QQ,10,20)
      m_(5,5) = 11/13
      m
  SeeAlso
    mutableIdentity
    mutableMatrix
///

doc ///
  Key
    mutableIdentity
    (mutableIdentity, Ring, ZZ)
    (mutableIdentity, RingFamily, ZZ)
    [mutableIdentity, Dense]
  Headline
    make a mutable identity matrix
  Usage
    mutableIdentity(R,nrows)
  Inputs
    R:{Ring, RingFamily}
    nrows:ZZ
    Dense => Boolean
      whether the encoding of the matrix should be dense or not: see
      @TO MutableMatrix@
  Outputs
    :MutableMatrix
      an @TT "nrows"@ by @TT "nrows"@ mutable identity matrix filled with
      elements of the ring @TT "R"@
  Description
    Example
      m = mutableIdentity(QQ,10)
      m_(5,5) = 11/13
      m
  SeeAlso
    mutableMatrix
///
