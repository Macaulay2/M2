------------------------------
-- special mathematical functions --
------------------------------

doc ///
  Key
    (Beta, RObject, RObject)
    (Beta, RObject, Thing)
    (Beta, Thing, RObject)
  Headline
    beta function of R objects
  Usage
    Beta(a, b)
  Inputs
    a:RObject
    b:RObject
  Outputs
    :RObject -- the beta function @SAMP "B(a, b)"@
  Description
    Text
      Compute the beta function of two R numeric vectors,
      calling R's @SAMP "beta"@.
    Example
      Beta(RObject 1, RObject 2)
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
  SeeAlso
    (Gamma, RObject)
    (binomial, RObject, RObject)
///

doc ///
  Key
    (Digamma, RObject)
  Headline
    digamma function of an R object
  Usage
    Digamma x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise digamma function of @VAR "x"@
  Description
    Text
      Compute the digamma function (the derivative of the log-gamma function)
      of each element of an R numeric vector, calling R's @SAMP "digamma"@.
    Example
      Digamma RObject 2
  SeeAlso
    (Gamma, RObject)
    (lngamma, RObject)
///

doc ///
  Key
    (Gamma, RObject)
  Headline
    gamma function of an R object
  Usage
    Gamma x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise gamma function of @VAR "x"@
  Description
    Text
      Compute the gamma function of each element of an R numeric vector,
      calling R's @SAMP "gamma"@.
    Example
      Gamma RObject 2
  SeeAlso
    (lngamma, RObject)
    (Digamma, RObject)
    (Beta, RObject, RObject)
///

doc ///
  Key
    (lngamma, RObject)
  Headline
    log-gamma function of an R object
  Usage
    lngamma x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise log of the gamma function of @VAR "x"@
  Description
    Text
      Compute the natural logarithm of the gamma function of each element
      of an R numeric vector, calling R's @SAMP "lgamma"@.
    Example
      lngamma RObject 2
  SeeAlso
    (Gamma, RObject)
    (Digamma, RObject)
///

doc ///
  Key
    (binomial, RObject, RObject)
    (binomial, RObject, Thing)
    (binomial, Thing, RObject)
  Headline
    binomial coefficient of R objects
  Usage
    binomial(n, k)
  Inputs
    n:RObject
    k:RObject
  Description
    Text
      Compute the binomial coefficient of two R numeric vectors,
      calling R's @SAMP "choose"@.
    Example
      binomial(RObject 4, RObject 2)
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
  SeeAlso
    (symbol !, RObject)
    (Beta, RObject, RObject)
///

doc ///
  Key
    (symbol !, RObject)
  Headline
    factorial of an R object
  Usage
    x!
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise factorial of @VAR "x"@
  Description
    Text
      Compute the factorial of each element of an R numeric vector,
      calling R's @SAMP "factorial"@.
    Example
      (RObject 3)!
  SeeAlso
    (binomial, RObject, RObject)
    (Gamma, RObject)
///
