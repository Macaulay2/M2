--------------------------
-- comparison operators --
--------------------------

doc ///
  Key
    (symbol ?, PythonObject, PythonObject)
    (symbol ?, PythonObject, Thing)
    (symbol ?, Thing, PythonObject)
  Headline
    compare Python objects
  Usage
    x ? y
  Inputs
    x:PythonObject
    y:PythonObject
  Description
    Text
      Compare two Python objects.  The result is one of the following symbols:
      @TO symbol <@, @TO symbol >@, @TO symbol ==@, or @TO incomparable@.
    Example
      toPython 2 ? toPython 3
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before comparing.
    Example
      toPython 4 ? 4
      7 ? toPython 6
    Text
      This method is used by the comparison operators @TO symbol <@,
      @TO symbol >@, @TO symbol <=@, and @TO symbol >=@.
    Example
      toPython 2 < toPython 3
      4 >= toPython 5
  SeeAlso
    (symbol ==, PythonObject, PythonObject)
///

doc ///
  Key
    (symbol ==, PythonObject, PythonObject)
    (symbol ==, PythonObject, Thing)
    (symbol ==, Thing, PythonObject)
  Headline
    equality of Python objects
  Usage
    x == y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :Boolean -- whether x and y are equal
  Description
    Text
      Determine whether two Python objects are equal.
    Example
      toPython 2 == toPython 3
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before comparing.
    Example
      toPython 4 == 4
      7 == toPython 6
  SeeAlso
    (symbol ?, PythonObject, PythonObject)
///

doc ///
  Key
    (symbol ??, PythonObject)
  Headline
    null coalescing operator for Python objects
  Usage
    ?? x
    x ?? y
  Inputs
    x:PythonObject
  Description
    Text
      When the first argument to the binary null coalescing operator is a
      Python object, it will be returned unless that object is @CODE "None"@,
      in which case the second argument is returned.
    Example
      toPython 3 ?? 4
      toPython null
      oo ?? 5
    Text
      For the unary version, the argument itself is returned unless it is
      @CODE "None"@, in which case @TO null@ is returned.
    Example
      ?? toPython 6
      ?? toPython null
///
