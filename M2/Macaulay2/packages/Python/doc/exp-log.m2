---------------------------------------------------
-- power, exponential, and logarithmic functions --
---------------------------------------------------

doc ///
  Key
    (exp, PythonObject)
  Headline
    exponential of a python object
  Usage
    exp x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.exp"@ function, which computes
      the exponential of @VAR "x"@.
    Example
      exp toPython 1
      exp toPython 0
  SeeAlso
    (log, PythonObject)
    (expm1, PythonObject)
///

doc ///
  Key
    (expm1, PythonObject)
  Headline
    exponential minus one of a python object
  Usage
    expm1 x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.expm1"@ function, which computes
      the exponential of @VAR "x"@ minus one.  This is more accurate than
      computing @M2CODE "exp x - 1"@ for small values of @VAR "x"@.
    Example
      exp toPython 1e-10 - 1
      expm1 toPython 1e-10
  SeeAlso
    (exp, PythonObject)
///

doc ///
  Key
    (log, PythonObject, PythonObject)
    (log, PythonObject)
    (log, PythonObject, Thing)
    (log, Thing, PythonObject)
  Headline
    logarithm of a python object
  Usage
    log x
    log_base x
  Inputs
    x:PythonObject
    base:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.log"@ function, which computes
      the logarithm of @VAR "x"@ to the given @VAR "base"@.  If no base is
      given, then the natural logarithm is computed.
    Example
      log toPython exp 1
      log_(toPython 2) toPython 8
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before computing the logarithm.
    Example
      log_10 toPython 100
      log_(toPython 3) 27
  SeeAlso
    (exp, PythonObject)
    (log1p, PythonObject)
///

doc ///
  Key
    (log1p, PythonObject)
  Headline
    logarithm plus one of a python object
  Usage
    log1p x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.log1p"@ function, which computes
      the logarithm of @VAR "x"@ plus one.  This is more accurate than
      computing @M2CODE "log(x + 1)"@ for small values of @VAR "x"@.
    Example
      log(1 + toPython 1e-10)
      log1p toPython 1e-10
  SeeAlso
    (log, PythonObject)
///

doc ///
  Key
    (sqrt, PythonObject)
  Headline
    square root of a python object
  Usage
    sqrt x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.sqrt"@ function, which computes
      the square root of @VAR "x"@.
    Example
      sqrt toPython 4
      sqrt toPython 2
///
