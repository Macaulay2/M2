-- Overview: matrices

doc ///
  Key
    "matrices"
  Description
    Text
      @HEADER2 "An overview"@

      In Macaulay2, each matrix is defined over a ring, (see @TO "rings"@).
      Matrices are perhaps the most common data type in Macaulay2.
      -- Mike wanted this: "For an overview of matrices as homomorphisms between modules, see ", TO "homomorphisms of modules", ".  ",

      For additional common operations and a comprehensive list of all routines
      in Macaulay2 which return or use matrices, see @TO "Matrix"@.
  Subnodes
    Matrix
    matrix
    :making matrices
    "inputting a matrix"
    "projections, inclusions, and permutations"
    "random and generic matrices"
    "mutable matrices"
    :operations involving matrices
    "extracting information about a matrix"
    "basic arithmetic of matrices"
    "concatenating matrices"
    "submatrices"
    "diff and contract"
    :matrix decompositions
    LUdecomposition
    QRDecomposition
    SVD
    :determinants and related computations
    "rank of a matrix"
    "determinants and minors"
    "exterior power of a matrix"
    :display of matrices and saving matrices to a file
    "format and display of matrices in Macaulay2"
    "importing and exporting matrices"
///

doc ///
  Key
    "inputting a matrix"
  Description
    Text
      @SUBSECTION "by its entries"@

      Using the function @TO "matrix"@ is the most basic method for inputting a
      matrix. The entries are typed in by rows as a doubly nested list of ring
      elements.
    Example
      R = ZZ/5[s..z];
      M = matrix {{ x^2+y, z^3}, {y^3-z,3*z-6*x-5*y}}
    Text
      One way to construct a doubly nested list of ring elements is with the
      @TO "table"@ command.
    Example
      table(3,3,(i,j) -> R_i^j)
      p = matrix oo
      q = matrix table(3,3,(i,j) -> R_j^i)
    Text
      @SUBSECTION "by a function"@

      The function @TO map@ can be used to construct matrices.
    Example
      G = map(R^3,3,(i,j)->R_i^j)
      f = 3*s^2*v-t*u*v+s*t^2
      H = map(R^4,R^4,(i,j)->diff(R_j*R_i,f))
    Text
      @SUBSECTION "identity matrix"@

      The function @TO id@ is used to form the identity matrix as a map from a
      module to itself.
    Example
      id_(R^3)
      id_(source M)
    Text
      The first example gives a 3x3 identity matrix with entries in the ring R.
      The second gives a 2x2 identity matrix whose source and target are the
      (graded) source of the matrix @TT "M"@.
///

doc ///
  Key
    "projections, inclusions, and permutations"
    -- F_List, F^List
///

doc ///
  Key
    "random and generic matrices"
  Description
    Text
      @SUBSECTION "random matrices"@

      To construct a random m by n matrix with entries in a ring R use the
      function @TO "random"@ by typing @TT "random(R^m,R^n)"@.
    Example
      R = GF(3^2,Variable => a);
      random(R^3,R^4)
    Text
      Over a polynomial ring, this will select elements in the base ring or
      field. To obtain a matrix of (say) linear polynomials, use
    Example
      T = R[x,y];
      random(T^3,T^{4:-1})
    Text
      @SUBSECTION "matrices of variables"@

      To build an m by n matrix of variables drawn from the ring R, use
      @TO "genericMatrix"@. The syntax is @TT "genericMatrix(R,x,m,n)"@ where R
      is the ring, x is the variable where we start and m and n specify the
      size of the matrix.
    Example
      S = R[p..z];
      genericMatrix(S,t,3,2)
    Text
      Note that to use the function genericMatrix the number of variables in
      the ring R must be at least as large as @TT "m*n"@.

      @SUBSECTION "genericSymmetricMatrix"@

      To construct an n by n symmetric matrix whose entries on and above the
      diagonal are the variables of R use @TO "genericSymmetricMatrix"@. The
      syntax is @TT "genericSymmetricMatrix(R,x,n)"@ where R is the ring, x is
      the variable you want to start with and n is the size of the matrix.
    Example
      genericSymmetricMatrix(S,s,3)
    Text
      @SUBSECTION "genericSkewMatrix"@

      To construct an n by n skew symmetric matrix whose entries above the
      diagonal are the variables of R use @TO "genericSkewMatrix"@. The syntax
      is @TT "genericSkewMatrix(R,x,n)"@ where R is the ring, x is the variable
      you want to start with and n is the size of the matrix.
    Example
      genericSkewMatrix(S,u,3)
  SeeAlso
    (random, Module, Module)
  Subnodes
    genericMatrix
    genericSymmetricMatrix
    genericSkewMatrix
///

doc ///
  Key
    "extracting information about a matrix"
  Description
    Text
      Consider the ring @TT "R"@ and the matrix @TT "f"@.
    Example
      R = QQ[x,y,z];
      f = matrix{{2,x,y,x^2},{z,32,2,x}}
    Text
      @SUBSECTION "target"@

      From the above output, one sees that Macaulay2 considers @TT "f"@ as a
      linear transformation. Use the @TO "target"@ command to obtain the target
      of the linear transformation @TT "f"@.
    Example
      M = target f
    Text
      Free modules are actually graded free modules, with the same sort of
      grading that the ring comes with. The degrees of the basis vectors of the
      target are always zero.
    Example
      degree M_0
      degree M_1
    Text
      @SUBSECTION "source"@

      Likewise, to obtain the source of our linear transformation, use the
      @TO "source"@ command.
    Example
      N = source f
    Text
      If possible, the degrees of the basis vectors of the source are set so
      that the map @TT "f"@ turns out to a homogeneous map of degree zero. This
      opportunism is important because certain algorithms will run faster on
      homogeneous maps.
    Example
      degree N_0
      degree N_1
      degree N_2
      degree N_3
      isHomogeneous f
    Text
      A list of the degrees of all the basis vectors can be obtained with
      @TO "degrees"@.
    Example
      degrees N
    Text
      It may happen that the matrix can not be made homogeneous. In that case,
      the degree of a basis vector is currently set to the degree of the
      largest monomial occurring in the corresponding column of the matrix. In
      a future version of the program it might be more sensible to set the
      degrees of the basis vectors all to zero.
    Example
      g = matrix {{x,0,y*z},{y^2,x^2,0}}
      isHomogeneous g
      degrees source g
    Text
      @SUBSECTION "number of rows or columns"@

      Use @TO "numgens"@ to obtain the rank of a free module. Combining it with
      the commands @TO "target"@ or @TO "source"@ gives us a way to determine
      the number of rows or columns of a matrix @TT "f"@.
    Example
      numgens target f
      numgens source f
    Text
      @SUBSECTION "extracting an element from a matrix"@

      To extract the @TT "(i,j)"@-th element of a matrix, type @TT "f_(i,j)"@.
    Example
      f_(1,3)
    Text
      Note that the first number selects the row, starting at @TT "0"@ and the
      second number selects the column, also starting at @TT "0"@.

      @SUBSECTION "entries of a matrix"@

      To obtain the entries of a matrix in the form of a list of lists, use the
      @TO "entries"@ command.
    Example
      entries f
    Text
      Note that each inner list is a list of elements from a row of @TT "f"@.

      @SUBSECTION "ring"@

      The @TO "ring"@ command can be used to return the ring of the matrix,
      that is, the ring containing entries of the matrix.
    Example
      ring f
    Text
      Use the @TO "describe"@ command to recover how the ring of @TT "f"@ was
      constructed.
    Example
      describe ring f
///

doc ///
  Key
    "basic arithmetic of matrices"
    (symbol==, Matrix, Matrix)
  Description
    Text
      @SUBSECTION "+"@

      To add two matrices, use the @TO "+"@ operator.
    Example
      ff = matrix{{1,2,3},{4,5,6}}
      gg = matrix{{4,5,6},{1,2,3}}
      ff+gg
    Text
      The matrices in question must have the same number of rows and columns
      and also must have the same ring.

      Scalars are converted to scalar matrices when necessary.
    Example
      matrix {{1, 2}, {3, 4}} + 5
    Text
      This is also true for the other arithmetic operations discussed below.

      @SUBSECTION "-"@

      To subtract two matrices, use the @TO "-"@ operator.
    Example
      ff-gg
    Text
      The matrices in question must have the same number of rows and columns
      and also must have the same ring.

      @SUBSECTION "*"@

      To multiply two matrices use the @TO "*"@ operator.
    Example
      R = ZZ/17[a..l];
      ff = matrix {{a,b,c},{d,e,f}}
      gg = matrix {{g,h},{i,j},{k,l}}
      ff * gg
    Text
      Suppose we multiply a homogeneous polynomial by a homogeneous matrix. The
      result ought to be homogeneous, but how can we arrange that? Scalar
      multiplication should not change the source or target of a map! Instead,
      we introduce one final complication: each matrix records a degree of its
      own, which is normally zero, and is used when deciding whether the matrix
      is homogeneous.
    Example
      R = ZZ/101[x,y,z];
      degree matrix {{x^10}}
      f = matrix {{x,0,y*z},{0,y^2,x^2}};
      degree f
    Text
      Multiplying a matrix by a homogeneous polynomial adds the degree of the
      polynomial to the degree of the map.
    Example
      h = x^10 * f
      degree h
      degrees source h
      isHomogeneous h
    Text
      If you don't like this, you have an alternative. The degree of a tensor
      product of two matrices is the sum of the degrees, and its source module
      is the tensor product of the source modules.
    Example
      h = x^10 ** f
      degree h
      degrees source h
      isHomogeneous h
    Text
      @SUBSECTION "^"@

      To raise a square matrix to a power, use the @TO "^"@ operator.
    Example
      ff = matrix{{1,2,3},{4,5,6},{7,8,9}}
      ff^4
    Text
      @SUBSECTION "inverse of a matrix"@
      -- EXAMPLE {
      -- 	    -- f^-1 tends to work except no error if not working
      -- 	    },

      If a matrix @TT "f"@ is invertible, then @TT "f^-1"@ will work.

      @SUBSECTION "=="@

      To check whether two matrices are equal, one can use @TO "=="@.
    Example
      ff == gg
      ff == ff
    Text
      However, given two matrices @TT "ff"@ and @TT "gg"@, it can be the case
      that @TT "ff - gg == 0"@ returns @TO "true"@ but @TT "ff == gg"@ returns
      @TO "false"@.
    Example
      M = R^{1,2,3}
      N = R^3
      ff = id_M
      gg = id_N
      ff - gg == 0
      ff == gg
    Text
      Since the degrees attached to the matrices were different, @TO "=="@
      returned the value @TO "false"@.

      @SUBSECTION "!="@

      To check whether two matrices are not equal, one can use @TO "!="@:
    Example
      ff != gg
    Text
      From the definition above of @TT "ff"@ and @TT "gg"@ we see that
      @TO "!="@ will return a value of @TO "true"@ if the degrees attached to
      the matrices are different, even if the entries are the same.

      @SUBSECTION "**"@

      Since tensor product (also known as Kronecker product and outer product)
      is a functor of two variables, we may compute the tensor product of two
      matrices. Recalling that a matrix is a map between modules, we may write:
    Pre
      ff : K ---> L
      gg : M ---> N
      ff ** gg : K ** M  ---> L ** N
    Example
      R = ZZ/17[a..l];
      ff = matrix {{a,b,c},{d,e,f}}
      gg = matrix {{g,h},{i,j},{k,l}}
      ff ** gg
  SeeAlso
    "extracting information about a matrix"
///

doc ///
  Key
    "concatenating matrices"
  Description
    Text
      Before Macaulay2 can concatenate matrices, the matrices in question
      @EM "must"@ have entries in the same ring.

      @SUBSECTION "concatenate horizontally"@

      Use @TO "|"@ to concatenate two matrices horizontally.
    Example
      R = ZZ/32003[a..f];
      M = genericMatrix(R,a,3,2)
      N = matrix{{d^2,a*d},{b*c,b*d},{a,c}}
      M|N
    Text
      @SUBSECTION "concatenate vertically"@

      Use @TO "||"@ to concatenate two matrices vertically.
    Example
      P = matrix{{d^2,a*d,e*f},{b*c,b*d,b*e},{a,c,d}}
      transpose(M)||P
    Text
      @SUBSECTION "making block matrices"@

      The matrix function can take matrices as input as long as the sizes match
      up.
    Example
      matrix{{id_(R^3),M,P},{random(R^1,R^3),random(R^1,R^3),random(R^1,R^2)}}
    Text
      Also, the number input entries in each row must be equal. It might seem
      like we could form the same matrix with the input
      @TT "matrix{{id_(R^3),M,P},{random(R^1,R^8)}}"@ since
      @TT "random(R^1,R^8)"@ will construct a 1 by 8 matrix that has the same
      number of columns as matrix @TT "matrix{{id_(R^3),M,P}}"@ but as input
      into the matrix function that row entry must have 3 entries.

      @SUBSECTION "direct sum of matrices as maps between modules"@

      Use @TO symbol ++@ to find the direct sum of two matrices.
    Example
      R = ZZ/101[x,y,z];
      p = matrix table(3,3,(i,j) -> R_i^j)
      q = matrix table(3,3,(i,j) -> R_j^i)
      r=p++q
    Text
      The components of a direct sum can be recovered later.
    Example
      components r
///

doc ///
  Key
    "submatrices"
  Description
    Text
      Here are the ways to get a submatrix of a matrix.
  Subnodes
    submatrix'
    submatrixByDegrees
    (submatrix, Matrix, VisibleList)
    (submatrix, Matrix, VisibleList, VisibleList)
    (symbol _, Matrix, List)
    (symbol ^, Matrix, List)
    (symbol _, Matrix, Array)
    (symbol ^, Matrix, Array)
///

doc ///
  Key
    "diff and contract"
  Description
    Text
      We may use the function @TO "diff"@ to differentiate polynomials: the
      first argument is the variable to differentiate with respect to, and the
      second argument is the polynomial to be differentiated.
    Example
      R = QQ[a,b,t,x,y,z];
      f = x^7 * y^11;
      diff(x,f)
      diff(y,f)
    Text
      We indicate higher derivatives by simply multiplying the variables to
      differentiate by.
    Example
      diff(x^2,f)
      diff(x*y,f)
      diff(y^2,f)
    Text
      The first argument can also be a sum, in which case the sum of the
      answers provided by each of its terms is returned.
    Example
      diff(x+y,f)
      diff(x^2+x*y+y^2,f)
    Text
      Remark: the operation @TT "diff"@ is useful, but it's not a natural one:
      it's not invariant under linear coordinate changes; in effect, we've
      identified the a free module with its dual.

      The second argument can be a matrix, in which case each of its entries
      gets differentiated.
    Example
      m = matrix {{x^3, x^4},{x^5,x^6}}
      diff(x,m)
      diff(x^2,m)
    Text
      The first argument can also be a matrix, in which case the matrices
      obtained from each of its entries, acting upon the second argument, are
      concatenated. Thus the shape of the first matrix plays the major role.
    Example
      diff(matrix {{x,x^2,x^3,x^4}}, m)
      diff(matrix {{x,x^2},{x^3,x^4}}, m)
    Text
      Perhaps the most common usage of @TO "diff"@ is when one argument has a
      single column and the other column has a single row. For example, the
      Jacobian matrix can be computed as follows.
    Example
      diff(matrix {{x},{y}}, matrix {{x^2, x*y, y^2}})
    Text
      @HR{}@

      We can also compute the Hessian matrix of a quadratic form using
      @TO "diff"@, as follows.
    Example
      v = matrix {{x,y}}
      diff(v ** transpose v, 3*x^2 + 5*x*y + 11*y^2)
    Text
      @HR{}@

      As another example, we show how to compute the Wronskian of a polynomial
      @TT "f"@.
    Example
      f = x^3 + y^3 + z^3 - t*x*y*z
      v = matrix {{x,y,z}}
      det diff(transpose v * v, f)
    Text
      @HR{}@

      The function @TO "contract"@ is the same as @TO "diff"@, except the
      multiplication by integers that occurs during differentiation is omitted.
    Example
      contract(x,m)
      contract(x^2,m)
      contract(matrix {{x,x^2,x^3,x^4}}, m)
      contract(matrix {{x,x^2},{x^3,x^4}}, m)
    Text
      One use is for picking out coefficients of homogeneous polynomials.
    Example
      f
      v3 = symmetricPower(3,matrix{{x,y,z}})
      contract(v3, f)
    Text
      @HR{}@

      As an example, the Sylvester resultant between homogeneous polynomials
      @TT "f(x,y)"@ and @TT "g(x,y)"@ can be found in the following way.
    Example
      f = a * x^3 + b * x^2 * y + y^3
      g = b * x^3 + a * x * y^2 + y^3
    Text
      Multiply each of these by all quadrics, obtaining a set of elements in
      degree 5.
    Example
      n = matrix {{f,g}} ** symmetricPower(2,matrix {{x,y}})
    Text
      --
      --                5    2 3    3     2 2       3    4    3     2        2    3
      --       ideal(- a  - a b  - a b - a b  + 2a*b  - b  + a  - 3a b + 3a*b  - b )
      --
      Now create the matrix of coefficients by using contract against all
      monomials of degree 5 in @TT "x"@ and @TT "y"@, and compute its
      determinant.
    Example
      M = contract(transpose symmetricPower(5,matrix {{x,y}}), n)
      det M
    Text
      @HR{}@

      The function @TO "diff'"@ is the same as @TO "diff"@, except that the
      first argument is differentiated by the second; the shape of the first
      argument still plays the major role.
    Example
      diff'(m, matrix {{x,x^2,x^3,x^4}})
      diff'(m, matrix {{x,x^2},{x^3,x^4}})
    Text
      The function @TO "contract'"@ is the same as @TO "contract"@, except that
      the first argument is contracted by the second; the shape of the first
      argument still plays the major role.
    Example
      contract'(m, matrix {{x,x^2,x^3,x^4}})
      contract'(m, matrix {{x,x^2},{x^3,x^4}})
    Text
      @HR{}@

      All four of these operators are engineered so that the result is a
      homogeneous matrix if the arguments are. The operations @TO "diff"@ and
      @TO "contract"@ are essentially partially defined division operations, so
      it should come as no surprise that the source and target of
      @TT "diff(m,n)"@ are the same as those we would get from the tensor
      product @TT "transpose m^-1 ** n"@, if only @TT "m"@ were invertible.
  Subnodes
    diff
    diff'
    contract
    contract'
    jacobian
    reshape
///

doc ///
  Key
    "rank of a matrix"
  Description
    Text
      @SUBSECTION "rank"@

      @SUBSECTION "rank of a matrix, determined probabilistically"@
///

doc ///
  Key
    "determinants and minors"
  Description
    Text
      The command @TO2(determinant, "det")@ can be used to compute the
      determinant of a square matrix.
    Example
      R = ZZ[a..i];
      det matrix{{a,b},{c,d}}
    Text
      Macaulay2 can use three different algorithms to compute determinants: the
      @TO "Cofactor"@ method, which expands a determinant using the standard
      cofactor approach, and @TO "Bareiss"@ which uses a fraction-free variant
      of Gaussian elimination to compute a determinant. The @TO "Dynamic"@
      algorithm implements a version of cofactor expansion, with caching of
      intermediate results. The algorithm to use may be chosen using the
      optional @TO "Strategy"@ argument:
    Example
      m = matrix{{0,a,b},{a+b,a,d},{e,f,g}}
      det(m, Strategy => Cofactor)
      minors(2,m, Strategy => Bareiss)
      exteriorPower(2,m, Strategy => Dynamic)
    Text
      One warning is in order here: the Bareiss algorithm requires division in
      the base ring, and so can yield the INCORRECT answer if the base ring
      contains zero divisors. However, the Bareiss algorithm is often
      dramatically faster than the cofactor method, unless the matrix is
      particularly sparse. Consequently, the default strategy for rings which
      are fields or are not quotients of polynomial rings is @TO "Bareiss"@,
      while the default for quotients of polynomial rings that are not
      (declared to be) fields is @TO "Cofactor"@. The @TO "Dynamic"@ algorithm
      can sometimes also result in significant speedups compared to
      @TO "Cofactor"@ and @TO "Bareiss"@, at the cost of a slight memory
      overhead.

      The command @TO "minors"@ can be used to construct the ideal generated by
      the @TT "n"@ by @TT "n"@ minors of a matrix. Recall that the @TT "n"@ by
      @TT "n"@ minors of a matrix are the determinants of the @TT "n"@ by
      @TT "n"@ submatrices of a matrix.
    Example
      R = QQ[x,y,z];
      f = matrix{{x,y,z},{y,z,x^2}}
      I = minors(2,f)
    Text
      Sometimes finer control is needed when one is computing the ideal of
      minors of a larger matrix. Compute the ideal of some determinants using
      @TO "minors"@ with optional arguments as in
    Example
      R = ZZ[a..i];
      M = genericMatrix(R,a,3,3);
      minors(2,M,First => {{0,1},{1,2}}, Limit => 3)
    Text
      The argument to the optional argument @TO "First"@ is the list of row and
      column positions to use for the first minor. Starting at this first
      minor, we then compute three minors.
  Subnodes
    trace
    minors
    determinant
    permanents
    pfaffians
    pfaffian
    fittingIdeal
///

doc ///
  Key
    "exterior power of a matrix"
  Description
    Text
      Since the @TT "i"@-th exterior power is a functor, it applies to matrices
      as well as to modules.
    Example
      R = ZZ[vars(0..19)]
      ff = genericMatrix(R,4,5)
      exteriorPower (2,ff)
    Text
      Note that each entry of in the above matrix is a @TT "2"@ by @TT "2"@
      minor (the determinant of a @TT "2"@ by @TT "2"@ submatrix) of the matrix
      @TT "ff"@.

      The signs are chosen so that this operation commutes with multiplication:
    Example
      S = QQ[x_(1,1) .. x_(3,5), y_(1,1) .. y_(5,4)]
      M = transpose genericMatrix(S,x_(1,1),5,3)
      N = transpose genericMatrix(S,y_(1,1),4,5)
      exteriorPower(3,M*N) == exteriorPower(3,M) * exteriorPower(3,N)
  SeeAlso
    "exterior power of a module"
  Subnodes
    exteriorPower
    (exteriorPower, ZZ, Matrix)
///

doc ///
  Key
    -- something should be said about the degrees
    -- and also about line wrapping
    "format and display of matrices in Macaulay2"
  Description
    Text
      By default, Macaulay2 displays matrices in a compact form.
    Example
      QQ[x,y];
      f = matrix{{x^2, x*y},{x*y, y^2}}
      dual f
      source f
      target dual f
    Text
      Integers inside braces to the left of the matrix give the degrees of the
      basis elements of the target of the matrix; they are omitted if the
      degrees are all zero.

      Note how the exponents have been placed to the right of the variables
      rather than formatted in superscript. While this format is generally
      considered to be desirable, this can be turned off by setting the
      variable @TO "compactMatrixForm"@ to @TT "false"@.
    Example
      compactMatrixForm = false
      matrix{{x^2 + 3, x^4 + 1},{x^13 - 5, x^7 - 1}}
    Text
      To have Macaulay2 display matrices in compact form again, type:
    Example
      compactMatrixForm = true
      matrix{{x^2 + 3, x^4 + 1},{x^13 - 5, x^7 - 1}}
  Subnodes
    "blockMatrixForm"
    "compactMatrixForm"
    "printingAccuracy"
    "printingLeadLimit"
    "printingPrecision"
    "printingSeparator"
    "printingTimeLimit"
    "printingTrailLimit "
///

doc ///
  Key
    "importing and exporting matrices"
  Description
    Text
      @TO "toString"@@TO "toExternalString"@
///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
