-----------------------
-- special functions --
-----------------------

doc ///
  Key
    (erf, PythonObject)
  Headline
    error function of a python object
  Usage
    erf x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.erf"@ function, which computes
      the error function of @VAR "x"@.
    Example
      erf toPython 0
      erf toPython 1
  SeeAlso
    (erfc, PythonObject)
///

doc ///
  Key
    (erfc, PythonObject)
  Headline
    complementary error function of a python object
  Usage
    erfc x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.erfc"@ function, which computes
      the complementary error function of @VAR "x"@, i.e., @M2CODE "1 - erf(x)"@.
    Example
      erfc toPython 0
      erfc toPython 1
      1 - erf toPython 1
  SeeAlso
    (erf, PythonObject)
///

doc ///
  Key
    (Gamma, PythonObject)
  Headline
    gamma function of a python object
  Usage
    Gamma x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.gamma"@ function, which computes
      the gamma function of @VAR "x"@.
    Example
      Gamma toPython 5
      Gamma toPython 0.5
  SeeAlso
    (lngamma, PythonObject)
///

doc ///
  Key
    (lngamma, PythonObject)
  Headline
    natural logarithm of the gamma function of a python object
  Usage
    lngamma x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @CODE "math.lgamma"@ function, which computes
      the natural logarithm of the gamma function of @VAR "x"@.
    Example
      lngamma toPython 5
      log Gamma toPython 5
  SeeAlso
    (Gamma, PythonObject)
///
