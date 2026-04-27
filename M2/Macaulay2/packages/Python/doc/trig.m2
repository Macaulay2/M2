-----------------------------
-- trigonometric functions --
-----------------------------

doc ///
  Key
    (acos, PythonObject)
  Headline
    arc cosine of a python object
  Usage
    acos x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.acos"@ function, which computes
      the arc cosine of @VAR "x"@.
    Example
      acos toPython 1
      acos toPython 0
  SeeAlso
    (cos, PythonObject)
    (asin, PythonObject)
    (atan, PythonObject)
///

doc ///
  Key
    (asin, PythonObject)
  Headline
    arc sine of a python object
  Usage
    asin x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.asin"@ function, which computes
      the arc sine of @VAR "x"@.
    Example
      asin toPython 1
      asin toPython 0
  SeeAlso
    (sin, PythonObject)
    (acos, PythonObject)
    (atan, PythonObject)
///

doc ///
  Key
    (atan, PythonObject)
  Headline
    arc tangent of a python object
  Usage
    atan x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.atan"@ function, which computes
      the arc tangent of @VAR "x"@.
    Example
      atan toPython 1
      atan toPython 0
  SeeAlso
    (tan, PythonObject)
    (asin, PythonObject)
    (acos, PythonObject)
    (atan2, PythonObject, PythonObject)
///

doc ///
  Key
    (atan2, PythonObject, PythonObject)
    (atan2, PythonObject, Thing)
    (atan2, Thing, PythonObject)
  Headline
    arc tangent of quotient of python objects
  Usage
    atan2(y, x)
  Inputs
    y:PythonObject
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.atan2"@ function, which computes
      the arc tangent of $frac{y}{x}$ between $-\pi$ and $\pi$.
    Example
      atan2(toPython 1, toPython 1)
      atan2(toPython(-1), toPython(-1))
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before computing the arc tangent.
    Example
      atan2(1, toPython 1)
      atan2(toPython 0, 1)
  SeeAlso
    (tan, PythonObject)
    (atan, PythonObject)
///

doc ///
  Key
    (cos, PythonObject)
  Headline
    cosine of a python object
  Usage
    cos x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.cos"@ function, which computes
      the cosine of @VAR "x"@.
    Example
      cos toPython 0
      cos toPython (pi / 2)
  SeeAlso
    (acos, PythonObject)
    (sin, PythonObject)
    (tan, PythonObject)
///

doc ///
  Key
    (sin, PythonObject)
  Headline
    sine of a python object
  Usage
    sin x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.sin"@ function, which computes
      the sine of @VAR "x"@.
    Example
      sin toPython 0
      sin toPython (pi / 2)
  SeeAlso
    (asin, PythonObject)
    (cos, PythonObject)
    (tan, PythonObject)
///

doc ///
  Key
    (tan, PythonObject)
  Headline
    tangent of a python object
  Usage
    tan x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.tan"@ function, which computes
      the tangent of @VAR "x"@.
    Example
      tan toPython 0
      tan toPython (pi / 4)
  SeeAlso
    (atan, PythonObject)
    (atan2, PythonObject, PythonObject)
    (sin, PythonObject)
    (cos, PythonObject)
///

