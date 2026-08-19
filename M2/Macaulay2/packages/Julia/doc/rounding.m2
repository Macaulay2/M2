doc ///
  Key
    (round, JuliaObject)
    (round, JuliaObject, Thing)
  Headline
    round a Julia object
  Usage
    round x
    round(T, x)
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject -- @VAR "x"@ rounded to the nearest integer
  Description
    Text
      This calls Julia's @SAMP "round"@ function, which rounds ties to
      even.
    Example
      round JuliaObject 2.5
      round JuliaObject 3.5
    Text
      If a Julia type @VAR "T"@ is given, then the result has that type.
    Example
      Int8 = juliaGetGlobal "Int8"
      round(Int8, 2.9)
  SeeAlso
    (floor, JuliaObject)
    (ceiling, JuliaObject)
    (truncate, JuliaObject)
///

doc ///
  Key
    (floor, JuliaObject)
    (floor, JuliaObject, Thing)
  Headline
    floor of a Julia object
  Usage
    floor x
    floor(T, x)
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject -- @VAR "x"@ rounded down to the nearest integer
  Description
    Text
      This calls Julia's @SAMP "floor"@ function.  If a Julia type
      @VAR "T"@ is given, then the result has that type.
    Example
      floor JuliaObject 2.9
      Int8 = juliaGetGlobal "Int8"
      floor(Int8, 2.9)
  SeeAlso
    (round, JuliaObject)
    (ceiling, JuliaObject)
    (truncate, JuliaObject)
///

doc ///
  Key
    (ceiling, JuliaObject)
    (ceiling, JuliaObject, Thing)
  Headline
    ceiling of a Julia object
  Usage
    ceiling x
    ceiling(T, x)
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject -- @VAR "x"@ rounded up to the nearest integer
  Description
    Text
      This calls Julia's @SAMP "ceil"@ function.  If a Julia type
      @VAR "T"@ is given, then the result has that type.
    Example
      ceiling JuliaObject 2.1
      Int8 = juliaGetGlobal "Int8"
      ceiling(Int8, 2.1)
  SeeAlso
    (round, JuliaObject)
    (floor, JuliaObject)
    (truncate, JuliaObject)
///

doc ///
  Key
    (truncate, JuliaObject)
    (truncate, JuliaObject, Thing)
  Headline
    truncate a Julia object
  Usage
    truncate x
    truncate(T, x)
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject -- @VAR "x"@ rounded toward zero
  Description
    Text
      This calls Julia's @SAMP "trunc"@ function.  If a Julia type
      @VAR "T"@ is given, then the result has that type.
    Example
      truncate JuliaObject 2.9
      truncate JuliaObject(-2.9)
      Int8 = juliaGetGlobal "Int8"
      truncate(Int8, 2.9)
  SeeAlso
    (round, JuliaObject)
    (floor, JuliaObject)
    (ceiling, JuliaObject)
///
