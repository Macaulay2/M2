------------------------------
-- RInterface documentation --
------------------------------

beginDocumentation()

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
///

doc ///
  Key
    RObject
    (NewFromMethod, RObject, Boolean)
    (NewFromMethod, RObject, CC)
    (NewFromMethod, RObject, List)
    (NewFromMethod, RObject, Matrix)
    (NewFromMethod, RObject, Nothing)
    (NewFromMethod, RObject, Number)
    (NewFromMethod, RObject, RR)
    (NewFromMethod, RObject, Sequence)
    (NewFromMethod, RObject, String)
    (NewFromMethod, RObject, Symbol)
    (NewFromMethod, RObject, ZZ)
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
      in memory.  An @SAMP "RObject"@ may be one of several different
      types.  Note that in R, most objects are actually vectors, and
      scalars are just vectors of length 1.

      @SAMP "RObject"@ is a @TO SelfInitializingType@, and so it acts
      as its own constructor method.  When passed a Macaulay2 object
      as input, the corresponding R object is returned.

      @TO Nothing@ inputs return R's @SAMP "NULL"@.
    Example
      RObject null
    Text
      @TO Boolean@ inputs return @EM "logical vectors"@.
    Example
      RObject true
    Text
      Another logical vector without a Macaulay2 equivalent is @TO "NA"@.
    Example
      NA
    Text
      @TO ZZ@ inputs return @EM "integer vectors"@.
    Example
      RObject 1
    Text
      @TO RR@ (and @TO QQ@) inputs return @EM "double vectors"@.
    Example
      RObject pi
    Text
      @TO CC@ inputs return @EM "complex vectors"@.
    Example
      RObject ii
    Text
      @TO String@ (and @TO Symbol@) inputs return @EM "character vectors"@.
    Example
      RObject "Hello, world!"
      RObject foo
    Text
      With a @TO List@ input, the type of the output depends on
      the elements of the list.  The R function @SAMP "c"@ is used to
      combine the elements into a vector.
    Example
      RObject {true, false, true, false}
      RObject {2, 4, 6, 8, 10}
      RObject {0, 1/2, exp 1}
      RObject apply(3, k -> exp(2*k*pi*ii/3))
      RObject {"foo", "bar", "baz"}
    Text
      When a @TO Sequence@ is given as input, a @EM "pairlist"@, a
      linked list type used by R for function arguments, is returned.
    Example
      RObject (2, 3, 5)
    Text
      When a @TO List@ or @TO Sequence@ is given as input and any of its
      elements are @TO Option@ objects, then the keys are used as names by R.
    Example
      RObject {foo => 1, bar => 2}
      RObject (baz => 3, qux => 4)
    Text
      When a @TO Matrix@ is given as input, an R matrix is returned.
    Example
      A = random(ZZ^2, ZZ^3)
      RObject A
    Text
      Each @SAMP "RObject"@ is displayed by calling R's @SAMP "capture.output"@
      function.
  SeeAlso
    "arithmetic operators on RObjects"
    "bitwise logical operations on RObjects"
    "extract or replace parts of RObjects"
    "functions on RObjects representing complex numbers"
    "hyperbolic functions on RObjects"
    "logarithmic and exponential functions on RObjects"
    "logical operators on RObjects"
    "maxima and minima of RObjects"
    "miscellaneous mathematical functions on RObjects"
    "relational operators on RObjects"
    "rounding of RObjects"
    "special mathematical functions on RObjects"
    "trigonometric functions on RObjects"
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
  Headline
    return an R object using its symbol name
  Usage
    RSymbol str
  Inputs
    str:String
  Outputs
    :RObject
  Description
    Text
      Return an R object using its symbol name.  For example, this can
      be useful for loading datasets.
    Example
      RSymbol "mtcars"
    Text
      For R functions, use @TO RFunction@ instead.
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
    "logical operators on RObjects"
    (symbol not, RObject)
    (symbol and, RObject, RObject)
    (symbol and, RObject, Thing)
    (symbol and, Thing, RObject)
    (symbol or, RObject, RObject)
    (symbol or, RObject, Thing)
    (symbol or, Thing, RObject)
    (symbol xor, RObject, RObject)
    (symbol xor, RObject, Thing)
    (symbol xor, Thing, RObject)
  Headline
    logical operators on R objects
  Description
    Text
      The logical operators @TO symbol not@, @TO symbol and@, @TO symbol or@,
      and @TO symbol xor@ are supported when working with logical vectors.
    Example
      not RObject true
      RObject true and RObject false
      RObject true or RObject false
      RObject true xor RObject false
    Text
      When a logical vector contains multiple elements, the output will also
      contain multiple elements.
    Example
      RObject {true, true, false, false} and RObject {true, false, true, false}
    Text
      For the binary operators, one of the operands may be a Macaulay2
      object.  It will be converted to an @TO RObject@ before the
      operation is performed.
    Example
      RObject {true, true, false, false} or {true, false, true, false}
      {true, true, false, false} xor RObject {true, false, true, false}
    Text
      Note that @TO symbol and@ and @TO symbol or@ use R's @SAMP "&"@ and
      @SAMP "|"@, respectively.
///

doc ///
  Key
    "arithmetic operators on RObjects"
    (symbol +, RObject)
    (symbol -, RObject)
    (symbol +, RObject, RObject)
    (symbol +, RObject, Thing)
    (symbol +, Thing, RObject)
    (symbol -, RObject, RObject)
    (symbol -, RObject, Thing)
    (symbol -, Thing, RObject)
    (symbol *, RObject, RObject)
    (symbol *, RObject, Thing)
    (symbol *, Thing, RObject)
    (symbol /, RObject, RObject)
    (symbol /, RObject, Thing)
    (symbol /, Thing, RObject)
    (symbol ^, RObject, RObject)
    (symbol ^, RObject, Thing)
    (symbol ^, Thing, RObject)
    (symbol %, RObject, RObject)
    (symbol %, RObject, Thing)
    (symbol %, Thing, RObject)
    (symbol //, RObject, RObject)
    (symbol //, RObject, Thing)
    (symbol //, Thing, RObject)
  Headline
    arithmetic operators on R objects
  Description
    Text
      The standard arithmetic operators are available when working with R
      objects, and in particular, numeric and complex vectors.

      @TO symbol +@ and @TO symbol -@ are both available as unary operators.
    Example
      x = RObject(-5)
      +x
      -x
    Text
      @TO symbol +@, @TO symbol -@, @TO symbol *@, @TO symbol /@, and
      @TO symbol ^@ are available as binary operators with the expected
      behavior.
    Example
      x = RObject 5
      y = RObject 2
      x + y
      x - y
      x * y
      x / y
      x^y
    Text
      @TO symbol //@ and @TO symbol %@ have their usual Macaulay2 behaviors
      of floor division and modulus, respectively,  wrapping around R's
      @SAMP "%/%"@ and @SAMP "%%"@.
    Example
      x // y
      x % y
    Text
      For the binary operators, one of the operands may be a Macaulay2
      object.  It will be converted to an @TO RObject@ before the
      operation is performed.
    Example
      x + 2
      5 + y
///

doc ///
  Key
    "relational operators on RObjects"
    (symbol ==, RObject, RObject)
    (symbol ==, RObject, Thing)
    (symbol ==, Thing, RObject)
    (symbol ?, RObject, RObject)
    (symbol ?, RObject, Thing)
    (symbol ?, Thing, RObject)
  Headline
    relational operators on R objects
  Description
    Text
      The relational operators @TO symbol ==@, @TO symbol !=@, @TO symbol >@,
      @TO symbol <@, @TO symbol >=@, and @TO symbol <=@ do the expected thing
      when comparing two @TO RObject@'s.
    Example
      x = RObject 5
      y = RObject 2
      x == y
      x != y
      x > y
      x < y
      x >= y
      x < y
    Text
      One of the operands may be a Macaulay2 object.  It will be
      converted to an @TO RObject@ before the
      operation is performed.
    Example
      x > 2
      5 < y
    Text
      Note that these operators return Macaulay2 @TO Boolean@ objects
      and not R logical vectors.  In particular, they are not useful
      for comparing vectors of length greater than 1.  To do this, you
      may use @TO RFunction@ to get the corresponding R operator as a
      function.
    Example
      (RFunction ">=")({1, 2, 3}, {2, 2, 2})
///

doc ///
  Key
    (symbol :, RObject, RObject)
    (symbol :, RObject, Thing)
    (symbol :, Thing, RObject)
    (symbol .., RObject, RObject)
    (symbol .., RObject, Thing)
    (symbol .., Thing, RObject)
  Headline
    R colon operator
  Description
    Text
      Generate the sequence between two numbers with step size 1 (or
      -1 if the first number is greater than the second).  This
      operator is available both as @SAMP ":"@, as in R, or @SAMP ".."@,
      its Macaulay2 equivalent.
    Example
      x = RObject 2
      y = RObject 7
      x:y
      y:x
      x..y
    Text
      One of the operands may be a Macaulay2 object.  It will be
      converted to an @TO RObject@ before the operation is performed.
      An exception is that when using the @SAMP ":"@ operator and the
      first argument given is a Macaulay2 @TO ZZ@ object,
      @TO (symbol :, ZZ, Thing)@ will be called instead.
    Example
      x:7
      2:y
      2..y
///

doc ///
  Key
    "bitwise logical operations on RObjects"
    (symbol ~, RObject)
    (symbol &, RObject, RObject)
    (symbol &, RObject, Thing)
    (symbol &, Thing, RObject)
    (symbol |, RObject, RObject)
    (symbol |, RObject, Thing)
    (symbol |, Thing, RObject)
    (symbol ^^, RObject, RObject)
    (symbol ^^, RObject, Thing)
    (symbol ^^, Thing, RObject)
    (symbol <<, RObject, RObject)
    (symbol <<, RObject, Thing)
    (symbol <<, Thing, RObject)
    (symbol >>, RObject, RObject)
    (symbol >>, RObject, Thing)
    (symbol >>, Thing, RObject)
  Headline
    bitwise logical operations on R objects
  Description
    Text
      These bitwise logical operations use syntax equivalent to their
      Macaulay2 counterparts.

      @TO symbol ~@ is bitwise @EM "not"@, calling R's @SAMP "bitwNot"@.
    Example
      x = RObject 12
      y = RObject 10
      x~
    Text
      @TO symbol &@ is bitwise @EM "and"@, calling R's @SAMP "bitwAnd"@.
    Example
      x & y
    Text
      @TO symbol |@ is bitwise @EM "or"@, calling R's @SAMP "bitwOr"@.
    Example
      x | y
    Text
      @TO symbol ^^@ is bitwise @EM "xor"@, calling R's @SAMP "bitwXor"@.
    Example
      x ^^ y
    Text
      @TO symbol <<@ and @TO symbol >>@ are the bitwise @EM "shift"@
      operators, calling R's @SAMP "bitwShiftL"@ and @SAMP "bitwShiftR"@,
      respectively.
    Example
      x << y
      oo >> y
    Text
      For the binary operators, one of the operands may be a Macaulay2
      object.  It will be converted to an @TO RObject@ before the
      operation is performed.
    Example
      x & 10
      12 | y
///

doc ///
  Key
    "miscellaneous mathematical functions on RObjects"
    (abs, RObject)
    (sqrt, RObject)
  Headline
    miscellaneous mathematical functions on R objects
  Description
    Text
      When passed an @TO RObject@, @TO abs@ and @TO sqrt@ do the
      expected thing, calling the corresponding R functions to compute
      the absolute value and square root of the given input,
      respectively.
    Example
      x = RObject(-2)
      abs x
      sqrt oo
///

doc ///
  Key
    "special mathematical functions on RObjects"
    (Beta, RObject, RObject)
    (Beta, RObject, Thing)
    (Beta, Thing, RObject)
    (Digamma, RObject)
    (Gamma, RObject)
    (lngamma, RObject)
    (binomial, RObject, RObject)
    (binomial, RObject, Thing)
    (binomial, Thing, RObject)
    (symbol !, RObject)
  Headline
    special mathematical functions on R objects
  Description
    Text
      When at least one of its arguments is an @TO RObject@, @TO Beta@
      calls R's @SAMP "beta"@ function.
    Example
      Beta(RObject 1, RObject 2)
    Text
      @TO Gamma@, @TO lngamma@, and @TO Digamma@ call R's @SAMP "gamma"@,
      @SAMP "lgamma"@, and @SAMP "digamma"@, respectively, when passed an
      @TO RObject@.
    Example
      Gamma RObject 2
      lngamma RObject 2
      Digamma RObject 2
    Text
      @TO binomial@ calls R's @SAMP "choose"@ when one of its arguments is
      an @TO RObject@, and the postfix operator @TO symbol !@ calls R's
      @SAMP "factorial"@ when passed an @TO RObject@.
    Example
      binomial(RObject 4, RObject 2)
      (RObject 3)!
///

doc ///
  Key
    "rounding of RObjects"
    (ceiling, RObject)
    (floor, RObject)
    (truncate, RObject)
    (round, RObject)
    (round, RObject, RObject)
    (round, RObject, Thing)
    (round, Thing, RObject)
  Headline
    rounding of R objects
  Description
    Text
      These functions round @TO RObject@'s representing numbers in
      specific ways.

      @TO ceiling@ and @TO floor@ do the expected thing, calling the
      corresponding R function to round up or down, respectively.
    Example
      x = RObject exp 1
      ceiling x
      floor x
    Text
      @TO truncate@ calls R's @SAMP "trunc"@ function to round toward 0.
    Example
      truncate x
      truncate(-x)
    Text
      @TO round@ calls the corresponding R function, which rounds to even,
      just as in Macaulay2.
    Example
      round RObject 2.5
      round RObject 3.5
    Text
      It may also be called with a second argument, or equivalently, a
      @SAMP "digits"@ option, specifying the number of decimal places
      to be used when rounding.
    Example
      round(x, 2)
      round(x, digits => 3)
///

doc ///
  Key
    "trigonometric functions on RObjects"
    (acos, RObject)
    (asin, RObject)
    (atan, RObject)
    (atan2, RObject, RObject)
    (atan2, RObject, Thing)
    (atan2, Thing, RObject)
    (cos, RObject)
    (sin, RObject)
    (tan, RObject)
  Headline
    trigonometric functions on R objects
  Description
    Text
      These functions call R's corresponding trigonometric functions.
    Example
      cos RObject pi
      asin RObject 1
    Text
      For @SAMP "atan2"@, one of the operands may be a Macaulay2
      object.  It will be converted to an @TO RObject@ before the
      operation is performed.
    Example
      atan2(RObject 1, -1)
      atan2(sqrt 3, RObject 1)
///

doc ///
  Key
    "hyperbolic functions on RObjects"
    (acosh, RObject)
    (asinh, RObject)
    (atanh, RObject)
    (cosh, RObject)
    (sinh, RObject)
    (tanh, RObject)
  Headline
    hyperbolic functions on R objects
  Description
    Text
      These functions call R's corresponding hyperbolic functions.
    Example
      cosh RObject 0
      asinh RObject(3/4)
///

doc ///
  Key
    "functions on RObjects representing complex numbers"
    (conjugate, RObject)
    (imaginaryPart, RObject)
    (realPart, RObject)
  Headline
    functions on R objects representing complex numbers
  Description
    Text
      The methods @TO conjugate@, @TO imaginaryPart@, and @TO realPart@ are
      overloaded to call R's @SAMP "Conj"@, @SAMP "Im"@, and @SAMP "Re"@
      functions to return the complex conjugate, imaginary part, and real
      part respectively, of a given @TO RObject@ representing a complex number
    Example
      z = RObject(3 + 2*ii)
      conjugate z
      realPart z
      imaginaryPart z
///

doc ///
  Key
    "logarithmic and exponential functions on RObjects"
    (exp, RObject)
    (expm1, RObject)
    (log, RObject)
    (log1p, RObject)
  Headline
    logarithmic and exponential functions on R objects
  Description
    Text
      These functions do the expected thing when passed an @TO RObject@.
    Example
      exp RObject 1
      log RObject exp 2
///

doc ///
  Key
    "maxima and minima of RObjects"
    (max, RObject)
    (min, RObject)
  Headline
    maxima and minima of R objects
  Description
    Text
      These functions do the expected thing when passed an @TO RObject@.
    Example
      x = RObject {1, 3, 5}
      max x
      min x
///

doc ///
  Key
    "extract or replace parts of RObjects"
    (symbol _, RObject, Thing)
    ((symbol _, symbol =), RObject, Thing)
    (symbol SPACE, RObject, Array)
    ((symbol SPACE, symbol =), RObject, Array)
  Headline
    extract or replace parts of R objects
  Description
    Text
      To extract a single element of an @TO RObject@, use an
      underscore, which calls R's @SAMP "[["@ operator.  Note that R
      uses one-based indexing.
    Example
      x = RObject toList(5..15)
      x_1
    Text
      To extract multiple elements, use square brackets.  This calls R's
      @SAMP "["@ operator.
    Example
      x[1, 4..8]
    Text
      Parts of an R object may be replaced as well.  This only affects the return
      value.  The original object is not modified.
    Example
      x_1 = 3
      x[1..4, 8] = {1, 2, 3, 4, 5}
      x
///
doc ///
  Key
    (iterator, RObject)
  Headline
    iterate through an R object
  Usage
    iterator x
  Inputs
    x:RObject
  Outputs
    :Iterator
  Description
    Text
      This returns an @TO Iterator@ object that may be used to iterate through
      @SAMP "x"@.
    Example
      i = iterator (RSymbol "iris")_"Petal.Length"
      next i
      next i
      next i
///

doc ///
  Key
    (length, RObject)
  Headline
    the length of an R object
  Usage
    length x
  Inputs
    x:RObject
  Outputs
    :ZZ
  Description
    Text
      This function returns the length of an RObject.
    Example
      length (RSymbol "iris")_"Petal.Length"
///

doc ///
  Key
    (product, RObject)
  Headline
    product of R object
  Usage
    product x
  Inputs
    x:RObject
  Outputs
    :RObject
  Description
    Text
      This function returns the product of all the values present in @SAMP "x"@.
      It wraps around R's @SAMP "prod"@ function.
    Example
      x = RObject {2, 4, 6}
      product x
///

doc ///
  Key
    (sum, RObject)
  Headline
    sum of R object
  Usage
    sum x
  Inputs
    x:RObject
  Outputs
    :RObject
  Description
    Text
      This function returns the sum of all the values present in @SAMP "x"@.
      It wraps around R's @SAMP "sum"@ function.
    Example
      x = RObject {2, 4, 6}
      sum x
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
