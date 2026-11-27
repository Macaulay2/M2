doc ///
  Key
    Matrix
  Headline
    the class of all matrices
  Description
    Text
      A matrix is a homomorphism between two modules, together with an integer
      (or vector of integers) called its degree, which is used when determining
      whether the map is homogeneous. The matrix is stored in the usual way as
      a rectangular array of ring elements. When the source or target modules
      are not free, the matrix is interpreted as a linear transformation in
      terms of the generators of the modules.

      A matrix @TT "f"@ is an immutable object, so if you want to cache
      information about it, put it in the hash table @TT "f.cache"@.

      Common ways to make a matrix:

      @UL {TO "map", TO "matrix"}@

      Common ways to get information about matrices:

      @UL {TO (degree, Matrix), TO (isHomogeneous, Matrix), TO (matrix, Matrix)}@

      Common operations on matrices:

      @UL {
          TO (symbol +, Matrix, Matrix),
          TO (symbol -, Matrix, Matrix),
          TO (symbol *, RingElement, Matrix),
          TO (symbol *, Matrix, Matrix),
          TO (symbol ==, Matrix, Matrix),
          TO (symbol ++, Matrix, Matrix),
          TO (symbol **, Matrix, Matrix),
          TO (symbol %, Matrix, Matrix),
          TO (symbol //, Matrix, Matrix),
          TO (symbol |, Matrix, Matrix),
          TO (symbol ||, Matrix, Matrix),
          TO (symbol ^, Matrix, List),
          TO (symbol _, Matrix, List)
          }@

      Common ways to use a matrix:

      @UL {
          TO (cokernel, Matrix),
          TO (image, Matrix),
          TO (kernel, Matrix),
          TO (homology, Matrix, Matrix)
          }@
  SeeAlso
    "matrices"
  Subnodes
    -- TODO: merge these with the lists above
    isWellDefined
    (entries, Matrix)
    eigenvalues
    eigenvectors
    transpose
    dual
    support
    (leadTerm, Matrix)
    (leadTerm, ZZ, Matrix)
    leadComponent
    (borel, Matrix)
    (dual, Matrix)
    (modulo, Matrix, Matrix)
    (complement, Matrix)
    "Complexes :: extend(Complex,Complex,Matrix)"
    (symbol **, Matrix, RingElement)
    (ambient, Matrix)
    (contract, Matrix, Matrix)
    (cover, Matrix)
    (degrees, Matrix)
    diagonalMatrix
    (symbol ++, Matrix, Matrix)
    (markedGB, Matrix, Matrix)
    (homology, Matrix, Matrix)
    (inverse, Matrix)
    (jacobian, Matrix)
    (koszul, Matrix)
    (koszul, ZZ, Matrix)
    (map, Ring, Matrix)
    (minimalPresentation, Matrix)
    (pivots, Matrix)
    (smithNormalForm, Matrix)
    (source, Matrix)
    (syz, Matrix)
    (target, Matrix)
    (compress, Matrix)
    (tensor, RingMap, Matrix)
    (tensor, Matrix, Matrix)
    (symbol **, Matrix, Ring)
    (symbol ^, Matrix, ZZ)
    (symbol |, Matrix, Matrix)
    (symbol ||, Matrix, Matrix)
    (symbol /, Matrix, Number)
    (symbol //, Matrix, Matrix)
    (symbol %, Matrix, GroebnerBasis)
    (symbol *, Matrix, Matrix)
    (symbol _, Matrix, Sequence)
    selectInSubring
    solve
    divideByVariable
///
