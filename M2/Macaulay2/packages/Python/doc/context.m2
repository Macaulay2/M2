doc ///
  Key
    PythonContext
    (NewMethod, PythonContext)
    (NewFromMethod, PythonContext, String)
    (symbol SPACE, PythonContext, String)
  Headline
    persistent Python execution environment
  Usage
    new PythonContext
    PythonContext s
    ctx s
  Inputs
    ctx:PythonContext
    s:String
  Outputs
    :PythonContext
  Description
    Text
      The class @CODE "PythonContext"@ represents a persistent Python execution
      environment that maintains its own global namespace. It allows you to
      evaluate Python expressions and statements across multiple calls,
      preserving defined variables and imports.

      When a @CODE "PythonContext"@ object is created, the given string is
      executed as Python code within a new global context.  Subsequent calls
      evaluate additional Python code in the same context, allowing variables
      and imports to persist.

      Each evaluation returns a @TO PythonObject@ representing the result of
      the final expression in the string, if any.
    Example
      math = PythonContext "from math import *"
      math "x = sin(3.4)"
      math "sin(3.4)"
      math "x"
      math "e"
    Text
      Here, the import from the Python @CODE "math"@ module is performed once
      during initialization.  The variable @VAR "x"@ remains available in
      subsequent calls to the same PythonContext instance.
  Subnodes
    (symbol _, PythonContext, String)
    (listSymbols, PythonContext)
    (use, PythonContext)
///

doc ///
  Key
    (symbol _, PythonContext, String)
  Headline
    get value of variable in a python context
  Usage
    ctx_s
  Inputs
    ctx:PythonContext
    s:String
  Outputs
    :PythonObject
  Description
    Text
      This gets the value of a variable in the given @TO PythonContext@.
    Example
      math = PythonContext "from math import *"
      math "x = sin(3.4)"
      math_"x"
///

doc ///
  Key
    (listSymbols, PythonContext)
    (listSymbols, PythonObject)
  Headline
    get a table of all the variables and their values in a python context
  Usage
    listSymbols ctx
  Inputs
    ctx:PythonContext
  Description
    Text
      This lists all the symbols and their values in the given
      @TO "PythonContext"@.
    Example
      math = PythonContext "from math import *"
      math "x = sin(3.4)"
      listSymbols math
    Text
      It also works for Python dictionaries.
    Example
      y = toPython hashTable{"foo" => 1, "bar" => 2}
      listSymbols y
///

doc ///
  Key
    (use, PythonContext)
  Headline
    install variables from a python context
  Usage
    use ctx
  Inputs
    ctx:PythonContext
  Description
    Text
      This method makes all the variables in a @TO PythonContext@ available
      outside the context.
    Example
      ctx = new PythonContext
      ctx "f = lambda x:x**2"
      ctx "x = 5"
      ctx "y = f(x)"
      f
      x
      y
      use ctx
      f
      x
      y
///
