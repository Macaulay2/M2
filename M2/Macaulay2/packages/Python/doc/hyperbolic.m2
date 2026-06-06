--------------------------
-- hyperbolic functions --
--------------------------

doc ///
  Key
    (acosh, PythonObject)
  Headline
    inverse hyperbolic cosine of a python object
  Usage
    acosh x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.acosh"@ function, which computes
      the inverse hyperbolic cosine of @VAR "x"@.
    Example
      acosh toPython 1
      acosh toPython 2
  SeeAlso
    (cosh, PythonObject)
    (asinh, PythonObject)
    (atanh, PythonObject)
///

doc ///
  Key
    (asinh, PythonObject)
  Headline
    inverse hyperbolic sine of a python object
  Usage
    asinh x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.asinh"@ function, which computes
      the inverse hyperbolic sine of @VAR "x"@.
    Example
      asinh toPython 0
      asinh toPython 1
  SeeAlso
    (sinh, PythonObject)
    (acosh, PythonObject)
    (atanh, PythonObject)
///

doc ///
  Key
    (atanh, PythonObject)
  Headline
    inverse hyperbolic tangent of a python object
  Usage
    atanh x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.atanh"@ function, which computes
      the inverse hyperbolic tangent of @VAR "x"@.
    Example
      atanh toPython 0
      atanh toPython 0.5
  SeeAlso
    (tanh, PythonObject)
    (asinh, PythonObject)
    (acosh, PythonObject)
///

doc ///
  Key
    (cosh, PythonObject)
  Headline
    hyperbolic cosine of a python object
  Usage
    cosh x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.cosh"@ function, which computes
      the hyperbolic cosine of @VAR "x"@.
    Example
      cosh toPython 0
      cosh toPython 1
  SeeAlso
    (acosh, PythonObject)
    (sinh, PythonObject)
    (tanh, PythonObject)
///

doc ///
  Key
    (sinh, PythonObject)
  Headline
    hyperbolic sine of a python object
  Usage
    sinh x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.sinh"@ function, which computes
      the hyperbolic sine of @VAR "x"@.
    Example
      sinh toPython 0
      sinh toPython 1
  SeeAlso
    (asinh, PythonObject)
    (cosh, PythonObject)
    (tanh, PythonObject)
///

doc ///
  Key
    (tanh, PythonObject)
  Headline
    hyperbolic tangent of a python object
  Usage
    tanh x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.tanh"@ function, which computes
      the hyperbolic tangent of @VAR "x"@.
    Example
      tanh toPython 0
      tanh toPython 1
  SeeAlso
    (atanh, PythonObject)
    (sinh, PythonObject)
    (cosh, PythonObject)
///
