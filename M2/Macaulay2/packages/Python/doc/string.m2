doc ///
  Key
    (toString, PythonObject)
    (expression, PythonObject)
    (net, PythonObject)
    (texMath, PythonObject)
  Headline
    readable string representation of a python object
  Usage
    toString x
  Inputs
    x:PythonObject
  Outputs
    :String
  Description
    Text
      This is equivalent to Python's builtin @CODE "str"@ function.
    Example
      datetime = import "datetime"
      epoch = datetime@@"datetime"(1970, 1, 1)
      toString epoch
    Text
      Note that @TO expression@, @TO net@, and @TO texMath@ do essentially the
      same thing, but return an @TO Expression@, a @TO Net@, or a string
      for use in TeX code, respectively.
    Example
      expression epoch
      net epoch
      texMath epoch
  SeeAlso
    (toExternalString, PythonObject)
///

doc ///
  Key
    (toExternalString, PythonObject)
    (describe, PythonObject)
  Headline
    unambiguous string representation of a python object
  Usage
    toExternalString x
  Inputs
    x:PythonObject
  Outputs
    :String
  Description
    Text
      This returns a string beginning with @TO pythonValue@ and that uses
      Python's builtin @CODE "repr"@ function to obtain a string that
      potentially may be evaluated using @TO (value, String)@ to return
      a Python object equal to @VAR "x"@.
    Example
      hello = toPython "Hello, world!"
      toExternalString hello
      value oo
    Text
      Note that @TO describe@ does essentially the same thing, but returns a
      @TO Describe@ object.
    Example
      describe hello
  SeeAlso
    (toString, PythonObject)
///
