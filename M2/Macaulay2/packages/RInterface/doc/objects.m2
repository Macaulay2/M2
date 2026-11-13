doc ///
  Key
    RObject
    (net, RObject)
    (toString, RObject)
  Headline
    R object
  Usage
    RObject x
  Inputs
    x:Thing -- a Macaulay2 object
  Outputs
    :RObject -- the R equivalent of @VAR "x"@
  Description
    Text
      An @SAMP "RObject"@ is a @SAMP "SEXP"@ (pointer) to an R object
      in memory.  Note that in R, most objects are actually vectors, and
      scalars are just vectors of length 1.

      @SAMP "RObject"@ is a @TO SelfInitializingType@, and so it acts
      as its own constructor method.  When passed a Macaulay2 object
      as input, the corresponding R object is returned.  See the
      @SAMP "converting to R objects"@ section below for the supported
      input types.

      Each @SAMP "RObject"@ is displayed by calling R's
      @SAMP "capture.output"@ function.
    Example
      RObject {2, 4, 6, 8}
      RObject pi
    Text
      It is converted to a string using R's @SAMP "toString"@ function.
    Example
      toString RObject {2, 4, 6, 8}
  Subnodes
    :converting to R objects
    (NewFromMethod, RObject, Boolean)
    (NewFromMethod, RObject, CC)
    (NewFromMethod, RObject, HashTable)
    (NewFromMethod, RObject, List)
    (NewFromMethod, RObject, Matrix)
    (NewFromMethod, RObject, Nothing)
    (NewFromMethod, RObject, RR)
    (NewFromMethod, RObject, Sequence)
    (NewFromMethod, RObject, String)
    (NewFromMethod, RObject, Symbol)
    (NewFromMethod, RObject, ZZ)
    "NA"
    :converting from R objects
    (value, RObject)
    :extracting or replacing parts
    (symbol _, RObject, Thing)
    (symbol SPACE, RObject, Array)
    :iteration
    (iterator, RObject)
    (length, RObject)
    :arithmetic operators
    (symbol +, RObject, RObject)
    (symbol -, RObject, RObject)
    (symbol *, RObject, RObject)
    (symbol /, RObject, RObject)
    (symbol ^, RObject, RObject)
    (symbol %, RObject, RObject)
    (symbol //, RObject, RObject)
    (symbol :, RObject, RObject)
    :relational operators
    (symbol ==, RObject, RObject)
    (symbol ?, RObject, RObject)
    :logical operators
    (symbol not, RObject)
    (symbol and, RObject, RObject)
    (symbol or, RObject, RObject)
    (symbol xor, RObject, RObject)
    :bitwise operators
    (symbol ~, RObject)
    (symbol &, RObject, RObject)
    (symbol |, RObject, RObject)
    (symbol ^^, RObject, RObject)
    (symbol <<, RObject, RObject)
    (symbol >>, RObject, RObject)
    :rounding
    (ceiling, RObject)
    (floor, RObject)
    (truncate, RObject)
    (round, RObject, RObject)
    :maxima, minima, sums, and products
    (max, RObject)
    (min, RObject)
    (sum, RObject)
    (product, RObject)
    :power, exponential, and logarithmic functions
    (abs, RObject)
    (sqrt, RObject)
    (exp, RObject)
    (expm1, RObject)
    (log, RObject)
    (log1p, RObject)
    :trigonometric functions
    (acos, RObject)
    (asin, RObject)
    (atan, RObject)
    (atan2, RObject, RObject)
    (cos, RObject)
    (sin, RObject)
    (tan, RObject)
    :hyperbolic functions
    (acosh, RObject)
    (asinh, RObject)
    (atanh, RObject)
    (cosh, RObject)
    (sinh, RObject)
    (tanh, RObject)
    :complex number functions
    (conjugate, RObject)
    (imaginaryPart, RObject)
    (realPart, RObject)
    :special mathematical functions
    (Beta, RObject, RObject)
    (Digamma, RObject)
    (Gamma, RObject)
    (lngamma, RObject)
    (binomial, RObject, RObject)
    (symbol !, RObject)
    :formulas
    (symbol ~, RObject, RObject)
///

doc ///
  Key
    (NewFromMethod, RObject, Nothing)
    (symbol ??, RObject)
  Headline
    create an R NULL object
  Usage
    RObject null
  Inputs
    :Nothing
  Outputs
    :RObject -- R's @SAMP "NULL"@
  Description
    Text
      Converts Macaulay2's @TO null@ to R's @SAMP "NULL"@.
    Example
      RObject null
    Text
      Note that the null coalescing operator @TO symbol ??@ treats
      R's @SAMP "NULL"@ as null.
    Example
      (?? RObject null) === null
      RObject null ?? RObject 5
  SeeAlso
    RObject
    (value, RObject)
///

doc ///
  Key
    (NewFromMethod, RObject, Boolean)
  Headline
    create an R logical vector from a Boolean
  Usage
    RObject x
  Inputs
    x:Boolean
  Outputs
    :RObject -- a logical vector of length 1
  Description
    Text
      Converts a Macaulay2 @TO Boolean@ to an R @EM "logical vector"@ of
      length 1.
    Example
      RObject true
      RObject false
    Text
      Another logical vector without a Macaulay2 equivalent is @TO "NA"@.
  SeeAlso
    "NA"
    (NewFromMethod, RObject, List)
    (symbol not, RObject)
    (symbol and, RObject, RObject)
///

doc ///
  Key
    (NewFromMethod, RObject, ZZ)
  Headline
    create an R integer vector from an integer
  Usage
    RObject n
  Inputs
    n:ZZ
  Outputs
    :RObject -- an integer vector of length 1
  Description
    Text
      Converts a Macaulay2 @TO ZZ@ to an R @EM "integer vector"@ of length 1.
    Example
      RObject 1
  SeeAlso
    (NewFromMethod, RObject, RR)
///

doc ///
  Key
    (NewFromMethod, RObject, RR)
    (NewFromMethod, RObject, Number)
  Headline
    create an R double vector from a real or rational number
  Usage
    RObject x
  Inputs
    x:RR
  Outputs
    :RObject -- a double vector of length 1
  Description
    Text
      Converts a Macaulay2 @TO RR@ (floating-point real number) to an R
      @EM "double vector"@ of length 1.
    Example
      RObject pi
    Text
      @TO QQ@ and other @TO Number@ subtypes are also converted to R double
      vectors.
    Example
      RObject (3/2)
  SeeAlso
    (NewFromMethod, RObject, ZZ)
    (NewFromMethod, RObject, CC)
///

doc ///
  Key
    (NewFromMethod, RObject, CC)
  Headline
    create an R complex vector from a complex number
  Usage
    RObject z
  Inputs
    z:CC
  Outputs
    :RObject -- a complex vector of length 1
  Description
    Text
      Converts a Macaulay2 @TO CC@ (complex number) to an R @EM "complex
      vector"@ of length 1.
    Example
      RObject ii
      RObject(3 + 2*ii)
  SeeAlso
    (NewFromMethod, RObject, RR)
    (realPart, RObject)
    (imaginaryPart, RObject)
    (conjugate, RObject)
///

doc ///
  Key
    (NewFromMethod, RObject, String)
  Headline
    create an R character vector from a string
  Usage
    RObject s
  Inputs
    s:String
  Outputs
    :RObject -- a character vector of length 1
  Description
    Text
      Converts a Macaulay2 @TO String@ to an R @EM "character vector"@ of
      length 1.
    Example
      RObject "Hello, world!"
  SeeAlso
    (NewFromMethod, RObject, Symbol)
///

doc ///
  Key
    (NewFromMethod, RObject, Symbol)
  Headline
    create an R character vector from a symbol
  Usage
    RObject s
  Inputs
    s:Symbol
  Outputs
    :RObject -- a character vector of length 1
  Description
    Text
      Converts a Macaulay2 @TO Symbol@ to an R @EM "character vector"@ of
      length 1.
    Example
      RObject foo
  SeeAlso
    (NewFromMethod, RObject, String)
///

doc ///
  Key
    (NewFromMethod, RObject, List)
  Headline
    create an R vector from a list
  Usage
    RObject L
  Inputs
    L:List
  Outputs
    :RObject -- an R vector whose type depends on the elements of @VAR "L"@
  Description
    Text
      Converts a Macaulay2 @TO List@ to an R vector using R's @SAMP "c"@
      function.  The type of the resulting vector is determined by the
      elements:
    Example
      RObject {true, false, true, false}
      RObject {2, 4, 6, 8, 10}
      RObject {0, 1/2, exp 1}
      RObject apply(3, k -> exp(2*k*pi*ii/3))
      RObject {"foo", "bar", "baz"}
    Text
      When any elements are @TO Option@ objects, the keys become names in R.
    Example
      RObject {foo => 1, bar => 2}
      RObject {foo => 1, 2, bar => 3}
  SeeAlso
    (NewFromMethod, RObject, Sequence)
///

doc ///
  Key
    (NewFromMethod, RObject, Sequence)
  Headline
    create an R pairlist from a sequence
  Usage
    RObject s
  Inputs
    s:Sequence
  Outputs
    :RObject -- a pairlist
  Description
    Text
      Converts a Macaulay2 @TO Sequence@ to an R @EM "pairlist"@, the
      linked-list type used internally by R for function argument lists.
    Example
      RObject (2, 3, 5)
    Text
      When any elements are @TO Option@ objects, the keys become names.
    Example
      RObject (baz => 3, qux => 4)
  SeeAlso
    (NewFromMethod, RObject, List)
///

doc ///
  Key
    (NewFromMethod, RObject, HashTable)
  Headline
    create an R environment from a hash table
  Usage
    RObject t
  Inputs
    t:HashTable -- with @TO String@ keys
  Outputs
    :RObject -- an R environment
  Description
    Text
      Converts a Macaulay2 @TO HashTable@ with @TO String@ keys to an R
      @EM "environment"@, with each key-value pair becoming a variable binding.
    Example
      env = RObject hashTable {"x" => 5_ZZ, "y" => 2_ZZ}
      RValue("x^y", Environment => env)
      value oo
///

doc ///
  Key
    (NewFromMethod, RObject, Matrix)
  Headline
    create an R matrix from a Macaulay2 matrix
  Usage
    RObject A
  Inputs
    A:Matrix
  Outputs
    :RObject -- an R matrix
  Description
    Text
      Converts a Macaulay2 @TO Matrix@ to an R matrix.  Since Macaulay2
      uses row-major order and R uses column-major order, the matrix is
      transposed during conversion so that the displayed values match.
    Example
      A = random(ZZ^2, ZZ^3)
      RObject A
    Text
      Note that @TO (value, RObject)@ does not undo this transposition,
      so @SAMP "value"@ and @SAMP "RObject"@ are not exact inverses when
      applied to matrices.
    Example
      value oo
      A == transpose matrix oo
  SeeAlso
    (value, RObject)
///

doc ///
  Key
    "NA"
  Headline
    missing value
  Description
    Text
      @SAMP "NA"@ is an @TO RObject@ indicating a missing value.  It is a
      logical vector of length 1.
    Example
      NA
///
