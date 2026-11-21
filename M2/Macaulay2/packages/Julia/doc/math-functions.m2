-------------------------------
-- floating-point functions --
-------------------------------

doc ///
  Key
    (isFinite, JuliaObject)
  Headline
    whether a Julia number is finite
  Usage
    isFinite x
  Inputs
    x:JuliaObject
  Outputs
    :Boolean
  Description
    Text
      This calls Julia's @SAMP "isfinite"@ function.
    Example
      isFinite JuliaObject 5
      isFinite JuliaObject infinity
      isFinite JuliaObject(0/0.)
  SeeAlso
    (isInfinite, JuliaObject)
///

doc ///
  Key
    (isInfinite, JuliaObject)
  Headline
    whether a Julia number is infinite
  Usage
    isInfinite x
  Inputs
    x:JuliaObject
  Outputs
    :Boolean
  Description
    Text
      This calls Julia's @SAMP "isinf"@ function.
    Example
      isInfinite JuliaObject infinity
      isInfinite JuliaObject 5
      isInfinite JuliaObject(0/0.)
  SeeAlso
    (isFinite, JuliaObject)
///

---------------------------------------------------
-- power, exponential, and logarithmic functions --
---------------------------------------------------

doc ///
  Key
    (abs, JuliaObject)
  Headline
    absolute value of a Julia object
  Usage
    abs x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "abs"@ function.
    Example
      abs JuliaObject(-5)
      abs JuliaObject(3 + 4*ii)
///


doc ///
  Key
    (sqrt, JuliaObject)
  Headline
    square root of a Julia object
  Usage
    sqrt x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "sqrt"@ function.
    Example
      sqrt JuliaObject 2
  SeeAlso
    (exp, JuliaObject)
    (expm1, JuliaObject)
    (log, JuliaObject)
    (log1p, JuliaObject)
///

doc ///
  Key
    (exp, JuliaObject)
  Headline
    exponential of a Julia object
  Usage
    exp x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "exp"@ function.
    Example
      exp JuliaObject 1
  SeeAlso
    (sqrt, JuliaObject)
    (expm1, JuliaObject)
    (log, JuliaObject)
    (log1p, JuliaObject)
///

doc ///
  Key
    (expm1, JuliaObject)
  Headline
    exponential minus one of a Julia object
  Usage
    expm1 x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "expm1"@ function.
    Example
      expm1 JuliaObject 1e-10
  SeeAlso
    (sqrt, JuliaObject)
    (exp, JuliaObject)
    (log, JuliaObject)
    (log1p, JuliaObject)
///

doc ///
  Key
    (log, JuliaObject)
    (log, JuliaObject, JuliaObject)
    (log, JuliaObject, Thing)
    (log, Thing, JuliaObject)
  Headline
    logarithm of a Julia object
  Usage
    log x
    log(b, x)
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "log"@ function.  With two arguments, the
      first is the base.  One of them may be a Macaulay2 object.  It is
      converted to a @TO JuliaObject@ first.
    Example
      log JuliaObject 2
      log(JuliaObject 2, JuliaObject 8)
      log(2, JuliaObject 8)
  SeeAlso
    (exp, JuliaObject)
    (log1p, JuliaObject)
///

doc ///
  Key
    (log1p, JuliaObject)
  Headline
    logarithm of one plus a Julia object
  Usage
    log1p x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "log1p"@ function.
    Example
      log1p JuliaObject 1e-10
  SeeAlso
    (sqrt, JuliaObject)
    (exp, JuliaObject)
    (expm1, JuliaObject)
    (log, JuliaObject)
///

------------------------------
-- trigonometric functions --
------------------------------


doc ///
  Key
    (sin, JuliaObject)
  Headline
    sine of a Julia object
  Usage
    sin x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "sin"@ function.
    Example
      sin JuliaObject 0.5
  SeeAlso
    (cos, JuliaObject)
    (tan, JuliaObject)
    (cot, JuliaObject)
    (sec, JuliaObject)
    (csc, JuliaObject)
///

doc ///
  Key
    (cos, JuliaObject)
  Headline
    cosine of a Julia object
  Usage
    cos x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "cos"@ function.
    Example
      cos JuliaObject 0.5
  SeeAlso
    (sin, JuliaObject)
    (tan, JuliaObject)
    (cot, JuliaObject)
    (sec, JuliaObject)
    (csc, JuliaObject)
///

doc ///
  Key
    (tan, JuliaObject)
  Headline
    tangent of a Julia object
  Usage
    tan x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "tan"@ function.
    Example
      tan JuliaObject 0.5
  SeeAlso
    (sin, JuliaObject)
    (cos, JuliaObject)
    (cot, JuliaObject)
    (sec, JuliaObject)
    (csc, JuliaObject)
///

doc ///
  Key
    (cot, JuliaObject)
  Headline
    cotangent of a Julia object
  Usage
    cot x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "cot"@ function.
    Example
      cot JuliaObject 0.5
  SeeAlso
    (sin, JuliaObject)
    (cos, JuliaObject)
    (tan, JuliaObject)
    (sec, JuliaObject)
    (csc, JuliaObject)
///

doc ///
  Key
    (sec, JuliaObject)
  Headline
    secant of a Julia object
  Usage
    sec x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "sec"@ function.
    Example
      sec JuliaObject 0.5
  SeeAlso
    (sin, JuliaObject)
    (cos, JuliaObject)
    (tan, JuliaObject)
    (cot, JuliaObject)
    (csc, JuliaObject)
///

doc ///
  Key
    (csc, JuliaObject)
  Headline
    cosecant of a Julia object
  Usage
    csc x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "csc"@ function.
    Example
      csc JuliaObject 0.5
  SeeAlso
    (sin, JuliaObject)
    (cos, JuliaObject)
    (tan, JuliaObject)
    (cot, JuliaObject)
    (sec, JuliaObject)
///

doc ///
  Key
    (asin, JuliaObject)
  Headline
    arcsine of a Julia object
  Usage
    asin x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "asin"@ function.
    Example
      asin JuliaObject 0.5
  SeeAlso
    (acos, JuliaObject)
    (atan, JuliaObject)
    (acot, JuliaObject)
///

doc ///
  Key
    (acos, JuliaObject)
  Headline
    arccosine of a Julia object
  Usage
    acos x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "acos"@ function.
    Example
      acos JuliaObject 0.5
  SeeAlso
    (asin, JuliaObject)
    (atan, JuliaObject)
    (acot, JuliaObject)
///

doc ///
  Key
    (atan, JuliaObject)
  Headline
    arctangent of a Julia object
  Usage
    atan x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "atan"@ function.
    Example
      atan JuliaObject 0.5
  SeeAlso
    (asin, JuliaObject)
    (acos, JuliaObject)
    (acot, JuliaObject)
///

doc ///
  Key
    (acot, JuliaObject)
  Headline
    arccotangent of a Julia object
  Usage
    acot x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "acot"@ function.
    Example
      acot JuliaObject 2.
  SeeAlso
    (asin, JuliaObject)
    (acos, JuliaObject)
    (atan, JuliaObject)
///

doc ///
  Key
    (atan2, JuliaObject, JuliaObject)
    (atan2, JuliaObject, Thing)
    (atan2, Thing, JuliaObject)
  Headline
    two-argument arctangent of Julia objects
  Usage
    atan2(y, x)
  Inputs
    y:JuliaObject
    x:JuliaObject
  Outputs
    :JuliaObject -- the angle of the point @SAMP "(x, y)"@
  Description
    Text
      This calls Julia's two-argument @SAMP "atan"@ function.  One of the
      inputs may be a Macaulay2 object.  It is converted to a
      @TO JuliaObject@ first.
    Example
      atan2(JuliaObject 1, JuliaObject(-1))
      atan2(JuliaObject 1, -1)
      atan2(1, JuliaObject(-1))
  SeeAlso
    (atan, JuliaObject)
///


---------------------------
-- hyperbolic functions --
---------------------------


doc ///
  Key
    (sinh, JuliaObject)
  Headline
    hyperbolic sine of a Julia object
  Usage
    sinh x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "sinh"@ function.
    Example
      sinh JuliaObject 0.5
  SeeAlso
    (cosh, JuliaObject)
    (tanh, JuliaObject)
    (coth, JuliaObject)
    (sech, JuliaObject)
    (csch, JuliaObject)
    (asinh, JuliaObject)
    (acosh, JuliaObject)
    (atanh, JuliaObject)
    (acoth, JuliaObject)
///

doc ///
  Key
    (cosh, JuliaObject)
  Headline
    hyperbolic cosine of a Julia object
  Usage
    cosh x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "cosh"@ function.
    Example
      cosh JuliaObject 0.5
  SeeAlso
    (sinh, JuliaObject)
    (tanh, JuliaObject)
    (coth, JuliaObject)
    (sech, JuliaObject)
    (csch, JuliaObject)
    (asinh, JuliaObject)
    (acosh, JuliaObject)
    (atanh, JuliaObject)
    (acoth, JuliaObject)
///

doc ///
  Key
    (tanh, JuliaObject)
  Headline
    hyperbolic tangent of a Julia object
  Usage
    tanh x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "tanh"@ function.
    Example
      tanh JuliaObject 0.5
  SeeAlso
    (sinh, JuliaObject)
    (cosh, JuliaObject)
    (coth, JuliaObject)
    (sech, JuliaObject)
    (csch, JuliaObject)
    (asinh, JuliaObject)
    (acosh, JuliaObject)
    (atanh, JuliaObject)
    (acoth, JuliaObject)
///

doc ///
  Key
    (coth, JuliaObject)
  Headline
    hyperbolic cotangent of a Julia object
  Usage
    coth x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "coth"@ function.
    Example
      coth JuliaObject 0.5
  SeeAlso
    (sinh, JuliaObject)
    (cosh, JuliaObject)
    (tanh, JuliaObject)
    (sech, JuliaObject)
    (csch, JuliaObject)
    (asinh, JuliaObject)
    (acosh, JuliaObject)
    (atanh, JuliaObject)
    (acoth, JuliaObject)
///

doc ///
  Key
    (sech, JuliaObject)
  Headline
    hyperbolic secant of a Julia object
  Usage
    sech x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "sech"@ function.
    Example
      sech JuliaObject 0.5
  SeeAlso
    (sinh, JuliaObject)
    (cosh, JuliaObject)
    (tanh, JuliaObject)
    (coth, JuliaObject)
    (csch, JuliaObject)
    (asinh, JuliaObject)
    (acosh, JuliaObject)
    (atanh, JuliaObject)
    (acoth, JuliaObject)
///

doc ///
  Key
    (csch, JuliaObject)
  Headline
    hyperbolic cosecant of a Julia object
  Usage
    csch x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "csch"@ function.
    Example
      csch JuliaObject 0.5
  SeeAlso
    (sinh, JuliaObject)
    (cosh, JuliaObject)
    (tanh, JuliaObject)
    (coth, JuliaObject)
    (sech, JuliaObject)
    (asinh, JuliaObject)
    (acosh, JuliaObject)
    (atanh, JuliaObject)
    (acoth, JuliaObject)
///

doc ///
  Key
    (asinh, JuliaObject)
  Headline
    inverse hyperbolic sine of a Julia object
  Usage
    asinh x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "asinh"@ function.
    Example
      asinh JuliaObject 0.5
  SeeAlso
    (sinh, JuliaObject)
    (cosh, JuliaObject)
    (tanh, JuliaObject)
    (coth, JuliaObject)
    (sech, JuliaObject)
    (csch, JuliaObject)
    (acosh, JuliaObject)
    (atanh, JuliaObject)
    (acoth, JuliaObject)
///

doc ///
  Key
    (acosh, JuliaObject)
  Headline
    inverse hyperbolic cosine of a Julia object
  Usage
    acosh x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "acosh"@ function.
    Example
      acosh JuliaObject 2.
  SeeAlso
    (sinh, JuliaObject)
    (cosh, JuliaObject)
    (tanh, JuliaObject)
    (coth, JuliaObject)
    (sech, JuliaObject)
    (csch, JuliaObject)
    (asinh, JuliaObject)
    (atanh, JuliaObject)
    (acoth, JuliaObject)
///

doc ///
  Key
    (atanh, JuliaObject)
  Headline
    inverse hyperbolic tangent of a Julia object
  Usage
    atanh x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "atanh"@ function.
    Example
      atanh JuliaObject 0.5
  SeeAlso
    (sinh, JuliaObject)
    (cosh, JuliaObject)
    (tanh, JuliaObject)
    (coth, JuliaObject)
    (sech, JuliaObject)
    (csch, JuliaObject)
    (asinh, JuliaObject)
    (acosh, JuliaObject)
    (acoth, JuliaObject)
///

doc ///
  Key
    (acoth, JuliaObject)
  Headline
    inverse hyperbolic cotangent of a Julia object
  Usage
    acoth x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "acoth"@ function.
    Example
      acoth JuliaObject 2.
  SeeAlso
    (sinh, JuliaObject)
    (cosh, JuliaObject)
    (tanh, JuliaObject)
    (coth, JuliaObject)
    (sech, JuliaObject)
    (csch, JuliaObject)
    (asinh, JuliaObject)
    (acosh, JuliaObject)
    (atanh, JuliaObject)
///

------------------------------
-- complex number functions --
------------------------------


doc ///
  Key
    (realPart, JuliaObject)
  Headline
    real part of a Julia object
  Usage
    realPart x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "real"@ function.
    Example
      realPart JuliaObject (2 + 3*ii)
  SeeAlso
    (imaginaryPart, JuliaObject)
    (conjugate, JuliaObject)
///

doc ///
  Key
    (imaginaryPart, JuliaObject)
  Headline
    imaginary part of a Julia object
  Usage
    imaginaryPart x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "imag"@ function.
    Example
      imaginaryPart JuliaObject (2 + 3*ii)
  SeeAlso
    (realPart, JuliaObject)
    (conjugate, JuliaObject)
///

doc ///
  Key
    (conjugate, JuliaObject)
  Headline
    complex conjugate of a Julia object
  Usage
    conjugate x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "conj"@ function.
    Example
      conjugate JuliaObject (2 + 3*ii)
  SeeAlso
    (realPart, JuliaObject)
    (imaginaryPart, JuliaObject)
///
