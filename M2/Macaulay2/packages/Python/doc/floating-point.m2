------------------------------
-- floating-point functions --
------------------------------

doc ///
  Key
    (ceiling, PythonObject)
  Headline
    ceiling of a python object
  Usage
    ceiling x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @TT "math.ceil"@ function, which rounds
      toward positive infinity.
    Example
      ceiling toPython 5.8
      ceiling toPython(-5.8)
  SeeAlso
    (floor, PythonObject)
    (truncate, PythonObject)
///

doc ///
  Key
    (floor, PythonObject)
  Headline
    floor of a python object
  Usage
    floor x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @TT "math.floor"@ function, which rounds
      toward negative infinity.
    Example
      floor toPython 5.8
      floor toPython(-5.8)
  SeeAlso
    (ceiling, PythonObject)
    (truncate, PythonObject)
///

doc ///
  Key
    (remainder, PythonObject, PythonObject)
    (remainder, PythonObject, Thing)
    (remainder, Thing, PythonObject)
  Headline
    remainder of python objects
  Usage
    remainder(x, y)
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.remainder"@ function, which
      computes the IEEE 754-style remainder of @VAR "x"@ with respect to @VAR
      "y"@.  In particular, this is $x - qy$, where @VAR "q"@ is the integer
      closed to $\frac{x}{y}$, breaking ties by rounding $\frac{1}{2}$ to the
      nearest even integer.
    Example
      remainder(toPython 7, toPython 2)
      remainder(toPython 9, toPython 2)
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before computing the remainder.
    Example
      remainder(toPython 7, 2)
      remainder(9, toPython 9)
///

doc ///
  Key
    (round, PythonObject, PythonObject)
    (round, PythonObject, Thing)
    (round, Thing, PythonObject)
    (round, PythonObject)
  Headline
    round a python object
  Usage
    round(n, x)
    round x
  Inputs
    n:PythonObject
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @TT "round"@ function, which round @TT "x"@
      to @TT "n"@ decimal places, or to the nearest integer if @TT "n"@ is not
      given.
    Example
      x = (import "math")@@pi
      round x
      round(3, x)
    Text
      Ties are broken by @EM "round half to even"@.
    Example
      round toPython 2.5
      round toPython 3.5
///

doc ///
  Key
    (truncate, PythonObject)
  Headline
    truncate a python object
  Usage
    truncate x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @TT "math.trunc"@ function, which rounds
      toward zero.
    Example
      truncate toPython 5.8
      truncate toPython(-5.8)
  SeeAlso
    (ceiling, PythonObject)
    (floor, PythonObject)
///

doc ///
  Key
    (isFinite, PythonObject)
  Headline
    whether a Python object is finite
  Usage
    isFinite x
  Inputs
    x:PythonObject
  Outputs
    :Boolean
  Description
    Text
      Description
        This function returns true when @VAR "x"@ is a finite number.
      Example
        isFinite toPython 3
        isFinite toPython infinity
  SeeAlso
    (isInfinite, PythonObject)
///

doc ///
  Key
    (isInfinite, PythonObject)
  Headline
    whether a Python object is infinite
  Usage
    isInfinite x
  Inputs
    x:PythonObject
  Outputs
    :Boolean
  Description
    Text
      This function returns true when @VAR "x"@ is an infinite number.
    Example
      isInfinite toPython infinity
      isInfinite toPython 3
  SeeAlso
    (isFinite, PythonObject)
///
