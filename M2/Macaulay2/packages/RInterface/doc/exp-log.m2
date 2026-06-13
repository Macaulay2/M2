--------------------------------------------
-- power, exponential, and logarithmic    --
--------------------------------------------

doc ///
  Key
    (abs, RObject)
  Headline
    absolute value of an R object
  Usage
    abs x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise absolute value of @VAR "x"@
  Description
    Text
      Compute the absolute value of each element of an R numeric vector,
      calling R's @SAMP "abs"@.
    Example
      x = RObject(-2)
      abs x
  SeeAlso
    (sqrt, RObject)
///

doc ///
  Key
    (sqrt, RObject)
  Headline
    square root of an R object
  Usage
    sqrt x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise square root of @VAR "x"@
  Description
    Text
      Compute the square root of each element of an R numeric vector,
      calling R's @SAMP "sqrt"@.
    Example
      x = RObject 2
      sqrt x
  SeeAlso
    (abs, RObject)
    (symbol ^, RObject, RObject)
    (exp, RObject)
///

doc ///
  Key
    (exp, RObject)
  Headline
    exponential of an R object
  Usage
    exp x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise exponential of @VAR "x"@
  Description
    Text
      Compute @EM "e"@ raised to the power of each element of an R numeric
      vector, calling R's @SAMP "exp"@.
    Example
      exp RObject 1
  SeeAlso
    (expm1, RObject)
    (log, RObject)
///

doc ///
  Key
    (expm1, RObject)
  Headline
    exponential minus 1 of an R object
  Usage
    expm1 x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise value of @SAMP "e^x - 1"@
  Description
    Text
      Compute @SAMP "e^x - 1"@ for each element of an R numeric vector,
      calling R's @SAMP "expm1"@.  This is more accurate than
      @SAMP "exp(x) - 1"@ for small values of @VAR "x"@.
    Example
      expm1 RObject 0.001
  SeeAlso
    (exp, RObject)
    (log1p, RObject)
///

doc ///
  Key
    (log, RObject)
  Headline
    natural logarithm of an R object
  Usage
    log x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise natural logarithm of @VAR "x"@
  Description
    Text
      Compute the natural logarithm of each element of an R numeric vector,
      calling R's @SAMP "log"@.
    Example
      log RObject exp 2
  SeeAlso
    (log1p, RObject)
    (exp, RObject)
///

doc ///
  Key
    (log1p, RObject)
  Headline
    natural logarithm of 1 plus an R object
  Usage
    log1p x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise value of @SAMP "log(1 + x)"@
  Description
    Text
      Compute @SAMP "log(1 + x)"@ for each element of an R numeric vector,
      calling R's @SAMP "log1p"@.  This is more accurate than
      @SAMP "log(1 + x)"@ for small values of @VAR "x"@.
    Example
      log1p RObject 0.001
  SeeAlso
    (log, RObject)
    (expm1, RObject)
///
