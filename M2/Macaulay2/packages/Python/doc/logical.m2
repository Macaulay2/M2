-----------------------
-- logical operators --
-----------------------

doc ///
  Key
    (symbol and, PythonObject, PythonObject)
    (symbol and, PythonObject, Thing)
    (symbol and, Thing, PythonObject)
  Headline
    logical conjunction of Python objects
  Usage
    x and y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- logical conjunction of x and y
  Description
    Text
      Perform the logical conjunction ("and") operation on Python objects.
    Example
      toPython true and toPython true
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object.
    Example
      toPython true and false
      false and toPython true
    Text
      Unlike Macaulay2, Python supports using @CODE "and"@ with non-boolean
      objects.  In this case, the first argument is returned if it is "falsy".
      Otherwise, the second argument is returned.
    Example
      toPython 0 and toPython "foo"
      toPython 5 and toPython "bar"
    Text
      In Python, when the first argument is falsy, then is is returned
      immediately without evaluating the second argument.  This is known as
      "short-circuiting".  However, in Macaulay2, both arguments are evaluated
      before the Python conjunction method is called.
    Example
      stopIfError = false
      toPython 0 and 1/0
    Text
      However, if the first argument is the Macaulay2 @TO false@ object, then
      short-cicuiting will occur.
    Example
     false and pythonValue "1/0"
  SeeAlso
    (symbol or, PythonObject, PythonObject)
    (symbol xor, PythonObject, PythonObject)
    (symbol not, PythonObject)
    (symbol &, PythonObject, PythonObject)
///

doc ///
  Key
    (symbol or, PythonObject, PythonObject)
    (symbol or, PythonObject, Thing)
    (symbol or, Thing, PythonObject)
  Headline
    logical disjunction of Python objects
  Usage
    x or y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- logical disjunction of x and y
  Description
    Text
      Perform the logical disjunction ("or") operation on Python objects.
    Example
      toPython true or toPython true
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object.
    Example
      toPython true or false
      false or toPython true
    Text
      Unlike Macaulay2, Python supports using @CODE "or"@ with non-boolean
      objects.  In this case, the first argument is returned if it is "truthy".
      Otherwise, the second argument is returned.
    Example
      toPython 0 or toPython "foo"
      toPython 5 or toPython "bar"
    Text
      In Python, when the first argument is truthy, then is is returned
      immediately without evaluating the second argument.  This is known as
      "short-circuiting".  However, in Macaulay2, both arguments are evaluated
      before the Python disjunction method is called.
    Example
      stopIfError = false
      toPython 1 or 1/0
    Text
      However, if the first argument is the Macaulay2 @TO true@ object, then
      short-cicuiting will occur.
    Example
     true or pythonValue "1/0"
  SeeAlso
    (symbol or, PythonObject, PythonObject)
    (symbol xor, PythonObject, PythonObject)
    (symbol not, PythonObject)
    (symbol &, PythonObject, PythonObject)
///

doc ///
  Key
    (symbol xor, PythonObject, PythonObject)
    (symbol xor, PythonObject, Thing)
    (symbol xor, Thing, PythonObject)
  Headline
    logical exclusive disjunction of Python objects
  Usage
    x xor y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- logical exclusive disjunction of x and y
  Description
    Text
      Perform the logical exclusive disjunction ("xor") operation on Python
      objects.
    Example
      toPython true xor toPython true
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object.
    Example
      toPython true xor false
      false xor toPython true
    Text
      Unlike Macaulay2, Python supports using @CODE "xor"@ with non-boolean
      objects. If exactly one of the arguments is truthy, then it will return
      that argument.  Otherwise, it will return @CODE "False"@.
    Example
      toPython 0 xor toPython "foo"
      toPython 5 xor toPython ""
      toPython 6 xor toPython "bar"
  SeeAlso
    (symbol or, PythonObject, PythonObject)
    (symbol and, PythonObject, PythonObject)
    (symbol not, PythonObject)
    (symbol ^^, PythonObject, PythonObject)
///

doc ///
  Key
    (symbol not, PythonObject)
  Headline
    logical negation of a python object
  Usage
    not x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject -- the logical negation of x
  Description
    Text
      Perform the logical negation ("not") operation on a Python object.
    Example
      not toPython true
      not toPython false
    Text
      Unlike Macaulay2, Python supports using @CODE "not"@ with non-boolean
      objects. It will return @CODE "False"@ if the argument is truthy and
      @CODE "True"@ if it is falsy.
    Example
      not toPython 0
      not toPython "foo"
  SeeAlso
    (symbol or, PythonObject, PythonObject)
    (symbol and, PythonObject, PythonObject)
    (symbol xor, PythonObject, PythonObject)
    (symbol ~, PythonObject)
///
