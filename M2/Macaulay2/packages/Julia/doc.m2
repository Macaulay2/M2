--------------------
-- top-level node --
--------------------

doc ///
  Key
    Julia
  Headline
    interface to Julia
  Description
    Text
      This package provides an interface to
      @HREF("https://julialang.org/", "Julia")@ from within Macaulay2.
      Julia is started when the package is loaded, and Julia code may
      then be evaluated, Julia functions called, and objects converted
      back and forth between the two languages.
    Example
      x = juliaValue "[1, 2, 3] .^ 2"
      value x
      f = JuliaFunction "sum"
      f {1, 2, 3}
      JuliaObject hashTable {"foo" => 1, "bar" => 2}
    Text
      The @SAMP "julia"@ executable must be available in the user's
      @SAMP "PATH"@ when the package is loaded.
  Subnodes
    :tutorials
    "using OSCAR"
    :evaluating code
    juliaValue
    juliaGetGlobal
    juliaSymbol
    using
    :classes
    JuliaObject
    JuliaFunction
    JuliaContext
    :getting help
    (help#0, JuliaObject)
///

-----------------
-- JuliaObject --
-----------------

doc ///
  Key
    JuliaObject
    (NewFromMethod, JuliaObject, JuliaObject)
  Headline
    a Julia object
  Usage
    JuliaObject x
  Inputs
    x:Thing -- a Macaulay2 object
  Outputs
    :JuliaObject -- the Julia equivalent of @VAR "x"@
  Description
    Text
      In Macaulay2, every Julia object has the class
      @SAMP "JuliaObject"@.  Its Julia type is displayed after it is
      printed.
    Example
      JuliaObject 5
      JuliaObject "foo"
    Text
      @SAMP "JuliaObject"@ is a @TO SelfInitializingType@, and so it
      acts as its own constructor.  When passed a Macaulay2 object, the
      corresponding Julia object is returned.  See the
      @SAMP "converting to Julia objects"@ section below for the
      supported input types.  Use @TO (value, JuliaObject)@ to convert
      back to Macaulay2.
    Example
      x = JuliaObject {1, 2, 3}
      value x
    Text
      Julia objects are protected from Julia's garbage collector for as
      long as Macaulay2 holds a reference to them.
  Subnodes
    :converting to Julia objects
    (NewFromMethod, JuliaObject, Boolean)
    (NewFromMethod, JuliaObject, ZZ)
    (NewFromMethod, JuliaObject, QQ)
    (NewFromMethod, JuliaObject, RR)
    (NewFromMethod, JuliaObject, CC)
    (NewFromMethod, JuliaObject, String)
    (NewFromMethod, JuliaObject, Nothing)
    (NewFromMethod, JuliaObject, List)
    (NewFromMethod, JuliaObject, Sequence)
    (NewFromMethod, JuliaObject, HashTable)
    (NewFromMethod, JuliaObject, Set)
    (NewFromMethod, JuliaObject, Matrix)
    :converting from Julia objects
    (value, JuliaObject)
    addJuliaToM2Function
    :printing
    (toString, JuliaObject)
    (net, JuliaObject)
    (toExternalString, JuliaObject)
    :calling
    (symbol SPACE, JuliaObject, Thing)
    :indexing and properties
    (symbol _, JuliaObject, Thing)
    ((symbol _, symbol =), JuliaObject, Thing)
    (delete, JuliaObject, Thing)
    (symbol @@, JuliaObject, Thing)
    ((symbol @@, symbol =), JuliaObject, Thing)
    (symbol @@?, JuliaObject, Thing)
    :iteration
    (iterator, JuliaObject)
    (length, JuliaObject)
    :arithmetic operators
    (symbol +, JuliaObject, JuliaObject)
    (symbol -, JuliaObject, JuliaObject)
    (symbol *, JuliaObject, JuliaObject)
    (symbol /, JuliaObject, JuliaObject)
    (symbol \, JuliaObject, JuliaObject)
    (symbol //, JuliaObject, JuliaObject)
    (symbol %, JuliaObject, JuliaObject)
    (quotientRemainder, JuliaObject, JuliaObject)
    (symbol ^, JuliaObject, JuliaObject)
    :bitwise operators
    (symbol ~, JuliaObject)
    (symbol &, JuliaObject, JuliaObject)
    (symbol |, JuliaObject, JuliaObject)
    (symbol ^^, JuliaObject, JuliaObject)
    (symbol <<, JuliaObject, JuliaObject)
    (symbol >>, JuliaObject, JuliaObject)
    :logical operators
    (symbol not, JuliaObject)
    (symbol and, JuliaObject, JuliaObject)
    (symbol or, JuliaObject, JuliaObject)
    :comparison operators
    (symbol ==, JuliaObject, JuliaObject)
    (symbol ?, JuliaObject, JuliaObject)
    :rounding
    (round, JuliaObject)
    (floor, JuliaObject)
    (ceiling, JuliaObject)
    (truncate, JuliaObject)
    :number-theoretic functions
    (gcd, JuliaObject, JuliaObject)
    (lcm, JuliaObject, JuliaObject)
    (numerator, JuliaObject)
    (denominator, JuliaObject)
    :floating-point functions
    (isFinite, JuliaObject)
    (isInfinite, JuliaObject)
    :power, exponential, and logarithmic functions
    (abs, JuliaObject)
    (sqrt, JuliaObject)
    (exp, JuliaObject)
    (expm1, JuliaObject)
    (log, JuliaObject)
    (log1p, JuliaObject)
    :trigonometric functions
    (sin, JuliaObject)
    (cos, JuliaObject)
    (tan, JuliaObject)
    (cot, JuliaObject)
    (sec, JuliaObject)
    (csc, JuliaObject)
    (asin, JuliaObject)
    (acos, JuliaObject)
    (atan, JuliaObject)
    (acot, JuliaObject)
    (atan2, JuliaObject, JuliaObject)
    :hyperbolic functions
    (sinh, JuliaObject)
    (cosh, JuliaObject)
    (tanh, JuliaObject)
    (coth, JuliaObject)
    (sech, JuliaObject)
    (csch, JuliaObject)
    (asinh, JuliaObject)
    (acosh, JuliaObject)
    (atanh, JuliaObject)
    (acoth, JuliaObject)
    :complex number functions
    (realPart, JuliaObject)
    (imaginaryPart, JuliaObject)
    (conjugate, JuliaObject)
///

load "./doc/tutorials.m2"
load "./doc/evaluation.m2"
load "./doc/to-julia.m2"
load "./doc/from-julia.m2"
load "./doc/printing.m2"
load "./doc/functions.m2"
load "./doc/indexing.m2"
load "./doc/arithmetic.m2"
load "./doc/bitwise.m2"
load "./doc/logical.m2"
load "./doc/comparison.m2"
load "./doc/rounding.m2"
load "./doc/number-theory.m2"
load "./doc/math-functions.m2"
load "./doc/context.m2"
