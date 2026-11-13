----------------------------
-- trigonometric functions --
----------------------------

doc ///
  Key
    (acos, RObject)
  Headline
    arccosine of an R object
  Usage
    acos x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise arccosine of @VAR "x"@, in radians
  Description
    Text
      Compute the arccosine of each element of an R numeric vector,
      calling R's @SAMP "acos"@.
    Example
      acos RObject 1
  SeeAlso
    (cos, RObject)
    (asin, RObject)
    (atan, RObject)
///

doc ///
  Key
    (asin, RObject)
  Headline
    arcsine of an R object
  Usage
    asin x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise arcsine of @VAR "x"@, in radians
  Description
    Text
      Compute the arcsine of each element of an R numeric vector,
      calling R's @SAMP "asin"@.
    Example
      asin RObject 1
  SeeAlso
    (sin, RObject)
    (acos, RObject)
    (atan, RObject)
///

doc ///
  Key
    (atan, RObject)
  Headline
    arctangent of an R object
  Usage
    atan x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise arctangent of @VAR "x"@, in radians
  Description
    Text
      Compute the arctangent of each element of an R numeric vector,
      calling R's @SAMP "atan"@.
    Example
      atan RObject 1
  SeeAlso
    (tan, RObject)
    (acos, RObject)
    (asin, RObject)
    (atan2, RObject, RObject)
///

doc ///
  Key
    (atan2, RObject, RObject)
    (atan2, RObject, Thing)
    (atan2, Thing, RObject)
  Headline
    two-argument arctangent of R objects
  Usage
    atan2(y, x)
  Inputs
    y:RObject
    x:RObject
  Outputs
    :RObject -- the element-wise arctangent of @SAMP "y/x"@, in radians
  Description
    Text
      Compute the two-argument arctangent (i.e., the angle in radians
      between the positive x-axis and the point @SAMP "(x, y)"@),
      calling R's @SAMP "atan2"@.
    Example
      atan2(RObject 1, -1)
      atan2(sqrt 3, RObject 1)
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
  SeeAlso
    (atan, RObject)
///

doc ///
  Key
    (cos, RObject)
  Headline
    cosine of an R object
  Usage
    cos x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise cosine of @VAR "x"@ (in radians)
  Description
    Text
      Compute the cosine of each element of an R numeric vector,
      calling R's @SAMP "cos"@.
    Example
      cos RObject pi
  SeeAlso
    (acos, RObject)
    (sin, RObject)
    (tan, RObject)
///

doc ///
  Key
    (sin, RObject)
  Headline
    sine of an R object
  Usage
    sin x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise sine of @VAR "x"@ (in radians)
  Description
    Text
      Compute the sine of each element of an R numeric vector,
      calling R's @SAMP "sin"@.
    Example
      sin RObject pi
  SeeAlso
    (asin, RObject)
    (cos, RObject)
    (tan, RObject)
///

doc ///
  Key
    (tan, RObject)
  Headline
    tangent of an R object
  Usage
    tan x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise tangent of @VAR "x"@ (in radians)
  Description
    Text
      Compute the tangent of each element of an R numeric vector,
      calling R's @SAMP "tan"@.
    Example
      tan RObject pi
  SeeAlso
    (atan, RObject)
    (cos, RObject)
    (sin, RObject)
///
