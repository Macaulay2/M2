doc ///
  Key
    (gcd, JuliaObject, JuliaObject)
    (gcd, JuliaObject, Thing)
    (gcd, Thing, JuliaObject)
    (gcd, JuliaObject)
  Headline
    greatest common divisor of Julia objects
  Usage
    gcd(x, y, ...)
  Inputs
    x:JuliaObject
    y:JuliaObject
  Outputs
    :JuliaObject -- the greatest common divisor of the inputs
  Description
    Text
      This calls Julia's @SAMP "gcd"@ function.  Some of the inputs may be
      Macaulay2 objects.  They are converted to Julia objects first.
    Example
      gcd(JuliaObject 12, JuliaObject 18)
      gcd(10, JuliaObject 15, JuliaObject 20, 25)
  SeeAlso
    (lcm, JuliaObject, JuliaObject)
///

doc ///
  Key
    (lcm, JuliaObject, JuliaObject)
    (lcm, JuliaObject, Thing)
    (lcm, Thing, JuliaObject)
    (lcm, JuliaObject)
  Headline
    least common multiple of Julia objects
  Usage
    lcm(x, y, ...)
  Inputs
    x:JuliaObject
    y:JuliaObject
  Outputs
    :JuliaObject -- the least common multiple of the inputs
  Description
    Text
      This calls Julia's @SAMP "lcm"@ function.  Some of the inputs may be
      Macaulay2 objects.  They are converted to Julia objects first.
    Example
      lcm(JuliaObject 4, JuliaObject 6)
      lcm(10, JuliaObject 15, JuliaObject 20, 25)
  SeeAlso
    (gcd, JuliaObject, JuliaObject)
///

doc ///
  Key
    (numerator, JuliaObject)
  Headline
    numerator of a Julia rational number
  Usage
    numerator x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "numerator"@ function.
    Example
      numerator JuliaObject(3/5)
  SeeAlso
    (denominator, JuliaObject)
///

doc ///
  Key
    (denominator, JuliaObject)
  Headline
    denominator of a Julia rational number
  Usage
    denominator x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject
  Description
    Text
      This calls Julia's @SAMP "denominator"@ function.
    Example
      denominator JuliaObject(3/5)
  SeeAlso
    (numerator, JuliaObject)
///
