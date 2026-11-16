-------------------------
-- hyperbolic functions --
-------------------------

doc ///
  Key
    (acosh, RObject)
  Headline
    hyperbolic arccosine of an R object
  Usage
    acosh x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise hyperbolic arccosine of @VAR "x"@
  Description
    Text
      Compute the hyperbolic arccosine of each element of an R numeric
      vector, calling R's @SAMP "acosh"@.
    Example
      acosh RObject 1
  SeeAlso
    (cosh, RObject)
    (asinh, RObject)
    (atanh, RObject)
///

doc ///
  Key
    (asinh, RObject)
  Headline
    hyperbolic arcsine of an R object
  Usage
    asinh x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise hyperbolic arcsine of @VAR "x"@
  Description
    Text
      Compute the hyperbolic arcsine of each element of an R numeric
      vector, calling R's @SAMP "asinh"@.
    Example
      asinh RObject(3/4)
  SeeAlso
    (sinh, RObject)
    (acosh, RObject)
    (atanh, RObject)
///

doc ///
  Key
    (atanh, RObject)
  Headline
    hyperbolic arctangent of an R object
  Usage
    atanh x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise hyperbolic arctangent of @VAR "x"@
  Description
    Text
      Compute the hyperbolic arctangent of each element of an R numeric
      vector, calling R's @SAMP "atanh"@.
    Example
      atanh RObject(1/2)
  SeeAlso
    (tanh, RObject)
    (acosh, RObject)
    (asinh, RObject)
///

doc ///
  Key
    (cosh, RObject)
  Headline
    hyperbolic cosine of an R object
  Usage
    cosh x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise hyperbolic cosine of @VAR "x"@
  Description
    Text
      Compute the hyperbolic cosine of each element of an R numeric
      vector, calling R's @SAMP "cosh"@.
    Example
      cosh RObject 0
  SeeAlso
    (acosh, RObject)
    (sinh, RObject)
    (tanh, RObject)
///

doc ///
  Key
    (sinh, RObject)
  Headline
    hyperbolic sine of an R object
  Usage
    sinh x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise hyperbolic sine of @VAR "x"@
  Description
    Text
      Compute the hyperbolic sine of each element of an R numeric
      vector, calling R's @SAMP "sinh"@.
    Example
      sinh RObject 0
  SeeAlso
    (asinh, RObject)
    (cosh, RObject)
    (tanh, RObject)
///

doc ///
  Key
    (tanh, RObject)
  Headline
    hyperbolic tangent of an R object
  Usage
    tanh x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise hyperbolic tangent of @VAR "x"@
  Description
    Text
      Compute the hyperbolic tangent of each element of an R numeric
      vector, calling R's @SAMP "tanh"@.
    Example
      tanh RObject 0
  SeeAlso
    (atanh, RObject)
    (cosh, RObject)
    (sinh, RObject)
///
