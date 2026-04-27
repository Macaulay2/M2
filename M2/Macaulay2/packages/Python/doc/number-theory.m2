--------------------------------
-- number-theoretic functions --
--------------------------------

doc ///
  Key
    (binomial, PythonObject, PythonObject)
    (binomial, PythonObject, Thing)
    (binomial, Thing, PythonObject)
  Headline
    binomial coefficient for Python objects
  Usage
    binomial(n, k)
  Inputs
    n:PythonObject
    k:PythonObject
  Description
    Text
      This computes the number of @VAR "k"@-element subsets of a set with
      @VAR "n"@ elements.
    Example
      binomial(toPython 4, toPython 2)
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object.
    Example
      binomial(toPython 12, 3)
      binomial(15, toPython 10)
  SeeAlso
    (symbol !, PythonObject)
///

doc ///
  Key
    (symbol !, PythonObject)
  Headline
    factorial of a Python object
  Usage
    x!
  Inputs
    x:PythonObject
  Outputs
    :PythonObject -- the factorial of x
  Description
    Text
      This computes the factorial of a Python object.
    Example
      (toPython 5)!
  SeeAlso
    (binomial, PythonObject, PythonObject)
///

doc ///
  Key
    (gcd, PythonObject, PythonObject)
    (gcd, PythonObject, Thing)
    (gcd, Thing, PythonObject)
    (gcd, PythonObject)
  Headline
    greatest common divisor of Python objects
  Usage
    gcd(x, y)
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- the greatest common divisor of x and y
  Description
    Text
      This computes the greatest common divisor of two Python objects.
    Example
      gcd(toPython 12, toPython 15)
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object.
    Example
      gcd(toPython 12, 15)
      gcd(12, toPython 15)
    Text
      Since @CODE "gcd"@ is a binary method, it will accept any number of
      arguments.
    Example
      gcd toPython 5
      gcd (toPython 7, toPython 14, 28)
  SeeAlso
    (lcm, PythonObject, PythonObject)
///

doc ///
  Key
    (lcm, PythonObject, PythonObject)
    (lcm, PythonObject, Thing)
    (lcm, Thing, PythonObject)
    (lcm, PythonObject)
  Headline
    least common multiple of Python objects
  Usage
    lcm(x, y)
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- the least common multiple of x and y
  Description
    Text
      This computes the least common multiple of two Python objects.
    Example
      lcm(toPython 12, toPython 15)
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object.
    Example
      lcm(toPython 12, 15)
      lcm(12, toPython 15)
    Text
      Since @CODE "lcm"@ is a binary method, it will accept any number of
      arguments.
    Example
      lcm toPython 5
      lcm (toPython 7, toPython 14, 28)
  SeeAlso
    (gcd, PythonObject, PythonObject)
///
