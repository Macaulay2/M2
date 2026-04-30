------------------------------
-- RInterface documentation --
------------------------------

doc ///
  Key
    RInterface
  Headline
    interface to R for statistical computing
  Description
    Text
      This package allows for the ability to interface with
      @HREF("https://www.r-project.org/", "R")@ for statistical computing
      inside Macaulay2.
    Example
      dataFrame = RFunction "data.frame"
      chisqTest = RFunction "chisq.test"
      M = dataFrame(F => {762, 327, 468}, M => {484, 239, 477},
	  "row.names" => {"Democrat", "Independent", "Republican"})
      chisqTest M
      value oo_"p.value"
  Subnodes
    RObject
    RFunction
    RSymbol
    RQuote
    "library"
///

doc ///
  Key
    RObject
    (net, RObject)
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
  Subnodes
    :converting to R objects
    (NewFromMethod, RObject, Boolean)
    (NewFromMethod, RObject, CC)
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
    RFunction
    (NewFromMethod, RFunction, RObject)
    (NewFromMethod, RFunction, String)
  Headline
    R function
  Usage
    RFunction x
  Inputs
    x:String -- specifying an R function
  Outputs
    :RFunction
  Description
    Text
      An @SAMP "RFunction"@ is a function that wraps around an R
      function specified by a string.  Its arguments are converted to
      @TO RObject@'s.
    Example
      qnorm = RFunction "qnorm"
      qnorm(0.025, "lower.tail" => false)
///

doc ///
  Key
    RSymbol
    (RSymbol, String)
    (RSymbol, Thing)
  Headline
    look up an R object by name
  Usage
    RSymbol str
  Inputs
    str:{String, Thing}
  Outputs
    :RObject -- the value of the R object named @VAR "str"@
  Description
    Text
      Look up an R object by name, returning its value.  For example,
      this can be useful for loading datasets.
    Example
      RSymbol "mtcars"
    Text
      A Macaulay2 @TO Symbol@ may also be used.
    Example
      RSymbol mtcars
    Text
      To obtain an unevaluated R symbol rather than its value, use
      @TO RQuote@ instead.  For R functions, use @TO RFunction@ instead.
  SeeAlso
    RQuote
    RFunction
///

doc ///
  Key
    RQuote
    (RQuote, String)
    (RQuote, Thing)
  Headline
    create an unevaluated R symbol
  Usage
    RQuote str
  Inputs
    str:{String, Thing}
  Outputs
    :RObject -- an R symbol (type @SAMP "symbol"@)
  Description
    Text
      Create an unevaluated R symbol.  This is analogous to R's
      @SAMP "quote"@ function, and is useful when a symbol itself
      (rather than its value) needs to be passed to an R function.

      Compare the behavior of @TO RSymbol@, which evaluates the symbol
      and returns its value:
    Example
      RSymbol "pi"
      RQuote "pi"
    Text
      A Macaulay2 @TO Symbol@ may also be used.
    Example
      RQuote pi
  SeeAlso
    RSymbol
///

doc ///
  Key
    (value, RObject)
  Headline
    convert R object to Macaulay2
  Usage
    value x
  Inputs
    x:RObject
  Outputs
    :Thing -- the Macaulay2 equivalent of @VAR "x"@
  Description
    Text
      If possible, the Macaulay2 equivalent of the given @TO RObject@
      is returned.

      Note that most R objects are vectors.  When a vector has length
      1, the corresponding Macaulay2 object is returned as a scalar.

      When the input is R's @SAMP "NULL"@, @TO null@ is returned.
    Example
      x = RObject null
      value x === null
    Text
      When the input is a logical vector, a @TO Boolean@ is returned.
    Example
      x = RObject true
      value x
    Text
      When the input is an integer vector, a @TO ZZ@ object is returned.
    Example
      x = RObject 5
      value x
    Text
      When the input is a double vector, a @TO RR@ object is returned.
    Example
      x = RObject (3/2)
      value x
    Text
      When the input is a complex vector, a @TO CC@ object is returned.
    Example
      x = RObject ii
      value x
    Text
      When the input is a character vector, a @TO String@ object is returned.
    Example
      x = RObject foo
      value x
    Text
      When the input is a vector with more than one element, a @TO List@
      object is returned.
    Example
      x = RObject {1, 3, 5, 7}
      value x
    Text
      When the input is a @EM "pairlist"@ (R's linked list type), a
      @TO Sequence@ is returned.
    Example
      x = RObject (3, 6, 9)
      value x
    Text
      There also exists a @EM "list"@ type in R, created by the function
      @SAMP "list"@, that may contain elements of of any type, much like
      Macaulay2 lists.  When the input is such a list, a @TO List@ object
      is returned.
    Example
      RList = RFunction "list"
      x = RList(2, 4, 6, 8)
      value x
    Text
      When the input is a matrix or array, a nested @TO List@ object
      is returned.  Note that R uses column-major order for
      matrices, unlike Macaulay2, which uses row-major order.  No
      attempt is made to change the order, unlike @TO (NewFromMethod,
      RObject, Matrix)@, which does do this conversion.  For the most part,
      @SAMP "value"@ and @TO RObject@ are inverses of one another, but this
      is an exception.
    Example
      A = random(ZZ^2, ZZ^3)
      x = RObject A
      value x
      A == transpose matrix oo
    Text
      When the input has any names, the elements with names are returned
      as @TO Option@ objects.
    Example
      x = RObject {foo => 1, 2, bar => 3}
      value x
///

doc ///
  Key
    "library"
  Headline
    load an R package
  Usage
    library pkg
  Inputs
    pkg:String
  Description
    Text
      This loads an R package.  It is an @TO RFunction@ that calls R's
      @SAMP "library"@ function.

      For example, suppose we want to load the @SAMP "abbey"@ dataset
      with nickel content in a Canadian syenite rock.  It is available in
      the @SAMP "MASS"@ package.
    Example
      library "MASS"
      RSymbol "abbey"
///

load "./doc/constructors.m2"
load "./doc/extract.m2"
load "./doc/iteration.m2"
load "./doc/arithmetic.m2"
load "./doc/relational.m2"
load "./doc/logical.m2"
load "./doc/bitwise.m2"
load "./doc/rounding.m2"
load "./doc/stats.m2"
load "./doc/exp-log.m2"
load "./doc/trig.m2"
load "./doc/hyperbolic.m2"
load "./doc/complex.m2"
load "./doc/special.m2"
load "./doc/formulas.m2"
