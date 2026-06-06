beginDocumentation()

--------------------
-- top-level-node --
--------------------

doc ///
  Key
    Python
  Headline
    interface to Python
  Description
    Text
      This package provides a basic interface to run Python code within
      Macaulay2 and to convert back and forth between Python and Macaulay2
      objects.
    Example
      toPython {1, 2/3, "foo", (1, 2, 3), hashTable {"foo" => "bar"}}
      value pythonValue "[1, 2/3, 'foo', (1, 2, 3), {'foo' : 'bar'}]"
      math = import "math"
      math@@sqrt 2
    Text
      The @TO [loadPackage, Configuration]@ option accepts an
      "executable" option to specify the path to the Python executable, e.g.,
      for working with virtual environments.  At least Python version 3.8 is
      required for this feature to work.
  Subnodes
    :tutorials
    "Python tutorial: creating a virtual environment and installing NumPy"
    "Python tutorial: plotting the twisted cubic with Matplotlib"
    :evaluating code
    pythonValue
    pythonRunScript
    runSimpleString
    :working with modules
    setupVirtualEnvironment
    pipInstall
    installNumPyMethods
    import
    :classes
    PythonObject
    PythonContext
    :getting help
    (help#0, PythonObject)
    pythonHelp
///

load "./doc/tutorials.m2"
load "./doc/context.m2"
load "./doc/numpy.m2"
load "./doc/venv.m2"

------------------
-- PythonObject --
------------------

doc ///
  Key
    PythonObject
  Headline
    a Python object
  Description
    Text
      In Macaulay2, all Python objects have the class @CODE "PythonObject"@.
      Their Python classes are displayed after they are printed.
    Example
      toPython 3
      toPython "foo"
  Subnodes
    :converting to Python objects
    toPython
    :converting from Python objects
    (value, PythonObject)
    toFunction
    addPyToM2Function
    :string functions
    (toString, PythonObject)
    (toExternalString, PythonObject)
    :list functions
    (symbol _, PythonObject, Thing)
    ((symbol _, symbol =), PythonObject, Thing)
    (length, PythonObject)
    (isMember, PythonObject, PythonObject)
    (delete, Thing, PythonObject)
    (iterator, PythonObject)
    (next, PythonObject)
    :attributes
    objectType
    (symbol @@, PythonObject, Thing)
    (symbol @@?, PythonObject, Thing)
    ((symbol @@, symbol =), PythonObject, Thing)
    :arithmetic operators
    (symbol +,  PythonObject, PythonObject)
    (symbol -,  PythonObject, PythonObject)
    (symbol *,  PythonObject, PythonObject)
    (symbol @,  PythonObject, PythonObject)
    (symbol /,  PythonObject, PythonObject)
    (symbol //, PythonObject, PythonObject)
    (symbol %,  PythonObject, PythonObject)
    (quotientRemainder, PythonObject, PythonObject)
    (symbol ^,  PythonObject, PythonObject)
    (abs, PythonObject)
    :bitwise operators
    (symbol <<, PythonObject, PythonObject)
    (symbol >>, PythonObject, PythonObject)
    (symbol &,  PythonObject, PythonObject)
    (symbol |,  PythonObject, PythonObject)
    (symbol ^^, PythonObject, PythonObject)
    (symbol ~,  PythonObject)
    :logical operators
    (symbol and, PythonObject, PythonObject)
    (symbol or,  PythonObject, PythonObject)
    (symbol xor, PythonObject, PythonObject)
    (symbol not, PythonObject)
    :comparison operators
    (symbol ?, PythonObject, PythonObject)
    (symbol ==, PythonObject, PythonObject)
    (symbol ??, PythonObject)
    :number-theoretic functions
    (binomial, PythonObject, PythonObject)
    (symbol !, PythonObject)
    (gcd, PythonObject, PythonObject)
    (lcm, PythonObject, PythonObject)
    :floating-point functions
    (ceiling, PythonObject)
    (floor, PythonObject)
    (remainder, PythonObject, PythonObject)
    (round, PythonObject)
    (truncate, PythonObject)
    (isFinite, PythonObject)
    (isInfinite, PythonObject)
    :power, exponential, and logarithmic functions
    (exp, PythonObject)
    (expm1, PythonObject)
    (log, PythonObject)
    (log1p, PythonObject)
    (sqrt, PythonObject)
    :trigonometric functions
    (acos, PythonObject)
    (asin, PythonObject)
    (atan, PythonObject)
    (atan2, PythonObject, PythonObject)
    (cos, PythonObject)
    (sin, PythonObject)
    (tan, PythonObject)
    :hyperbolic functions
    (acosh, PythonObject)
    (asinh, PythonObject)
    (atanh, PythonObject)
    (cosh, PythonObject)
    (sinh, PythonObject)
    (tanh, PythonObject)
    :special functions
    (erf, PythonObject)
    (erfc, PythonObject)
    (Gamma, PythonObject)
    (lngamma, PythonObject)
///

load "./doc/string.m2"
load "./doc/list.m2"
load "./doc/arithmetic.m2"
load "./doc/bitwise.m2"
load "./doc/logical.m2"
load "./doc/comparison.m2"
load "./doc/number-theory.m2"
load "./doc/floating-point.m2"
load "./doc/exp-log.m2"
load "./doc/trig.m2"
load "./doc/hyperbolic.m2"
load "./doc/special.m2"

doc ///
  Key
    pythonValue
    (pythonValue, String)
    (pythonValue, Sequence)
    [pythonValue, Global]
  Headline
    execute Python source code from a string
  Usage
    pythonValue s
  Inputs
    s:{String, Sequence} -- containing Python source code
    Global => PythonObject -- globals dictionary
  Outputs
    :PythonObject -- the return value of the given code
  Description
    Text
      This function a is wrapper around the function @TT
      HREF{"https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_String",
      "PyRun_String"}@ from the Python C API.
    Example
      pythonValue "2 + 2"
    Text
      If a sequence is given, then its elements are converted to strings using
      @TO "toString"@ and then joined using @TO "concatenate"@.  You can see the
       expression sent to the Python interpreter by setting @TO "debugLevel"@
       to a positive value.
    Example
      debugLevel = 1
      x = 5
      pythonValue("3 + ", x)
  SeeAlso
    runSimpleString
    pythonRunScript
///

doc ///
  Key
    runSimpleString
  Headline
    execute Python source code from a string in __main__
  Usage
    runSimpleString s
  Inputs
    s:String -- containing Python source code
  Description
    Text
      This function a is wrapper around the function @TT
      HREF{"https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_SimpleString",
      "PyRun_SimpleString"}@ from the Python C API.  Note that, unlike
      @TO "pythonValue"@, it has no return value.
    Example
      runSimpleString "print('Hello, world!')" -* no-capture-flag *-
  SeeAlso
    pythonValue
    pythonRunScript
///

doc ///
  Key
     pythonRunScript
    (pythonRunScript, String)
    (pythonRunScript, Sequence)
    [pythonRunScript, Global]
  Headline
    execute a sequence of Python statements
  Usage
    pythonRunScript s
  Inputs
    s:{String, Sequence} -- containing Python code
    Global => PythonObject -- globals dictionary
  Outputs
    :PythonObject -- the globals dictionary
  Description
    Text
      Execute a sequence of statements as if they were read from a Python file.
      This is for multi-line code that might contain definitions, control
      structures, imports, etc.  It is great for running Python code from
      a file.

      The return value is a Python dictionary containing all the variables
      defined in the global scope.
    Example
      pyfile = temporaryFileName() | ".py"
      pyfile << "import math" << endl
      pyfile << "x = math.sin(3.4)" << endl << close
      get pyfile
      pythonRunScript oo
    Text
      The @M2CODE "Global"@ option may be used to pass a globals dictionary
      so that variables may be shared between scripts.
    Example
      pythonRunScript("y = math.cos(x)", Global => oo)
  SeeAlso
    pythonValue
    runSimpleString
///

doc ///
  Key
    (iterator, PythonObject)
  Headline
    get iterator of iterable python object
  Usage
    i = iterator x
  Inputs
    x:PythonObject -- an iterable
  Outputs
    i:PythonObject -- an iterator
  Description
    Text
      This function works just like its
      @HREF{"https://docs.python.org/3/library/functions.html#iter",
      "Python counterpart"}@.  In particular, @TT "i"@ is an iterator
      for the iterable object @TT "x"@.
    Example
      builtins = import "builtins"
      x = builtins@@range 3
      i = iterator x
  SeeAlso
    (next, PythonObject)
///

doc ///
  Key
    (next, PythonObject)
  Headline
    retrieve the next item from a python iterator
  Usage
    next i
  Inputs
    i:PythonObject -- an iterator
  Description
    Text
      This function works just like its
      @HREF{"https://docs.python.org/3/library/functions.html#next",
      "Python counterpart"}@.  In particular, it retrieves the next item
      from an iterator.
    Example
      builtins = import "builtins"
      x = builtins@@range 3
      i = iterator x
      next i
      next i
      next i
    Text
      When the iterator is exhausted, @TO "StopIteration"@ is returned.
    Example
      next i
  SeeAlso
    (iterator, PythonObject)
///

doc ///
  Key
    toFunction
    (toFunction,PythonObject)
    (symbol SPACE, PythonObject, Thing)
  Headline
    convert callable python objects to Macaulay2 functions
  Usage
    toFunction x
  Inputs
    x:PythonObject
  Outputs
    :FunctionClosure
  Description
    Text
      This function will convert a Python object into a Macaulay2 function.
    Example
      math = import "math"
      pysqrt = toFunction math@@sqrt
      pysqrt 2
    Text
      Optional arguments can be provided using options.
    Example
      builtins = import "builtins"
      int = toFunction builtins@@int
      int("deadbeef", "base" => 16)
    Text
      If a python object and a Macaulay2 thing are separated by a space, then
      @TT "toFunction"@ will be called on the python object and then resulting
      function will be called with the Macaulay2 object as its argument.
    Example
      math@@cos pi
///

doc ///
  Key
    (abs, PythonObject)
  Headline
    absolute value of a python object
  Usage
    abs x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject -- the absolute value of @TT "x"@
  Description
    Text
      This is equivalent to the Python @HREF {
      "https://docs.python.org/3/library/functions.html#abs", "abs"}@ function.
    Example
      abs toPython(-12)
///

doc ///
  Key
    (length,PythonObject)
  Headline
    returns the length of a python object
  Usage
    length x
  Inputs
    x:PythonObject
  Outputs
    :ZZ
  Description
    Text
      This is equivalent the Python @HREF {
      "https://docs.python.org/3/library/functions.html#len", "len"}@ function.
    Example
      length toPython "Hello, world!"
      length pythonValue "[1,2,3,4,5]"
///

doc ///
  Key
    (value,PythonObject)
  Headline
    convert python objects to Macaulay2 things
  Usage
    value x
  Inputs
    x:PythonObject
  Outputs
    :Thing -- the Macaulay2 equivalent of @TT "x"@
  Description
    Text
      This function attempts to convert @TT "x"@ to its corresponding
      Macaulay2 equivalent.
    Example
      value pythonValue "[1, 3.14159, 'foo', (1,2,3), {'foo':'bar'}]"
      class \ oo
    Text
      Since the type of @TT "x"@ is not initially known, a sequence of
      @TO2 {"Macaulay2Doc::using hooks", "hooks"}@ are used to determine its
      type and then convert it.
    Example
      hooks value
    Text
      If no conversion can be done, then @TT "x"@ is returned.
--    Example
--      TODO
    Text
      Users may add additional hooks using @TO "addHook"@ or the
      convenience function @TO "addPyToM2Function"@.
///

doc ///
  Key
    addPyToM2Function
    (addPyToM2Function, String, Function, String)
    (addPyToM2Function, List, Function, String)
    (addPyToM2Function, PythonObject, Function, String)
  Headline
    convenience function for adding value hooks
  Usage
    addPyToM2Function(type, f, desc)
  Inputs
    type:{PythonObject,String,List} -- the type(s) to convert
    f:Function -- the function that will do the converting
    desc:String -- passed to the @TT "Strategy"@ option of @TO "addHook"@
  Description
    Text
      Most of the hooks used by @TO "value"@ have the same general format:
      if the python object has a particular type, then use a particular
      function to convert it to a corresponding Macaulay2 thing.  This function
      simplifies the process of adding such a hook.
    Text
      For example, suppose we would like to convert @TT "Fraction"@
      objects from the Python @HREF
      {"https://docs.python.org/3/library/fractions.html",
      "fractions"}@ module to @TO "QQ"@ objects.  Without adding a hook,
      @TO "value"@ will convert these objects to @TO "RR"@ objects.
    Example
      fractions = import "fractions"
      x = fractions@@"Fraction"(2, 3)
      value x
    Text
      So we write a function to do the conversion and then install the hook
      using @TT "addPyToM2Function"@.
    Example
      toQQ = x -> value x@@"numerator" / value x@@"denominator";
      addPyToM2Function("fractions.Fraction", toQQ, "Fraction -> QQ");
      value x
      hooks value
///

doc ///
  Key
    toPython
    (toPython, Boolean)
    (toPython, CC)
    (toPython, Function)
    (toPython, HashTable)
    (toPython, Nothing)
    (toPython, Number)
    (toPython, PythonObject)
    (toPython, RR)
    (toPython, RRi)
    (toPython, Sequence)
    (toPython, Set)
    (toPython, String)
    (toPython, Symbol)
    (toPython, VisibleList)
    (toPython, ZZ)
  Headline
    convert Macaulay2 things to Python objects
  Usage
    toPython x
  Inputs
    x:Thing
  Outputs
    :PythonObject
  Description
    Text
      Attempt to convert a Macaulay2 thing to a Python object.
    Example
      toPython 2
      toPython (1/2)
      toPython pi
      toPython ii
      toPython "foo"
      toPython {1, 2, 3, 4}
      toPython (1, 2, 3, 4)
      toPython hashTable {"foo" => "bar"}
      toPython set {1, 2, 3, 4}
      toPython true
      toPython null
      m2sqrt = x -> (
	  print "calling Macaulay2 code from Python!";
	  sqrt x)
      pysqrt = toPython m2sqrt
      pysqrt 2
///

doc ///
  Key
    import
    (import, String)
  Headline
    import a Python module
  Usage
    import s
  Inputs
    s:String -- the name of a python module
  Outputs
    :PythonObject -- the imported module
  Description
    Text
      This is a wrapper around the Python C API function @HREF{
      "https://docs.python.org/3/c-api/import.html#c.PyImport_ImportModule",
      "PyImport_ImportModule"}@ and returns an imported Python module.
    Text
      Once imported, the statements and definitions from the module are
      available using @TO (symbol \@\@, PythonObject, Thing)@.
    Example
      math = import "math"
      math@@pi
      math@@sqrt 2
///

doc ///
  Key
    (symbol @@, PythonObject, Thing)
  Headline
    get an attribute of a python object
  Usage
    x@@y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
    :PythonObject
  Description
    Text
      Get an attribute of a Python object.  In Python, the @CODE "."@
      operator is used.  However, this operator has a special meaning
      in Macaulay2 and cannot be overloaded.  Since @CODE "\@\@"@ has
      similar precedence, it is used instead.
    Example
      foo = toPython "Hello, world!"
      foo@@upper()
    Text
      Note that @TO toString@ is called on @CODE "y"@ before it is used.
      Therefore, symbols and functions may be used as well as strings.
      Sometimes, however, it is necessary to use quotes, e.g., when using
      special "dunder" methods with double underscores.  These would
      result in a Macaulay2 syntax error if left unquoted.
    Example
      foo@@"__len__"()
///

doc ///
  Key
    (symbol @@?, PythonObject, Thing)
  Headline
    whether a python object has an attribute
  Usage
    x @@? y
  Outputs
    :Boolean -- whether @TT "y"@ is an attribute of @TT "x"@
  Inputs
    x:PythonObject
    y:Thing
  Description
    Text
      This is equivalent to the Python @HREF{
      "https://docs.python.org/3/library/functions.html#hasattr", "hasattr"}@
      function.
    Example
      foo = toPython "Hello, world!"
      foo@@?upper
      foo@@?bar
///

doc ///
  Key
    ((symbol @@, symbol =), PythonObject, Thing)
  Headline
    set an attribute of a python object
  Usage
    x@@y = e
  Inputs
    x:PythonObject
    y:Thing
    e:Thing
  Description
    Text
      This is equivalent to the Python @HREF{
      "https://docs.python.org/3/library/functions.html#setattr", "setattr"}@
      function.  Note that @TT "e"@ is converted to a Python object using
      @TO "toPython"@.
    Example
      math = import "math"
      math@@pi = 22/7 -* no-capture-flag *-
      math@@pi
///

doc ///
  Key
    objectType
  Headline
    type of a python object
  Usage
    objectType x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject -- the type of @TT "x"@
  Description
    Text
      This is equivalent to the @HREF{
      "https://docs.python.org/3/library/functions.html#type", "type"}@ function
      in Python.
    Example
      objectType toPython 2
      objectType toPython "Hello, world!"
///

doc ///
  Key
    (isMember, PythonObject, PythonObject)
    (isMember, PythonObject, Thing)
    (isMember, Thing, PythonObject)
  Headline
    test membership in a python object
  Usage
    isMember(x, y)
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :Boolean -- whether x is in y
  Description
    Text
      This tests whether @CODE "x"@ is a member of @CODE "y"@.  This is
      equivalent to @CODE "x in y"@ in Python.
    Example
      isMember(toPython 3, toPython {1, 2, 3})
      isMember(toPython 4, toPython {1, 2, 3})
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before testing for membership.
    Example
      isMember(toPython 3, {1, 2, 3})
      isMember(4, toPython {1, 2, 3})
    Text
      When used with dictionaries, which are Python's equivalent of Macaulay2
      hash tables, the keys are tested for membership.  In this sense, it is
      like the Macaulay2 @TO symbol #?@ and @TO symbol .?@ operators.
    Example
      isMember("foo", toPython hashTable {"foo" => "bar"})
      isMember("bar", toPython hashTable {"foo" => "bar"})
///


doc ///
  Key
    (help#0, PythonObject)
  Headline
    documentation for python object
  Usage
    help x
  Inputs
    x:PythonObject
  Outputs
    :String
  Description
    Text
      This calls Python's built-in @TT "help"@ function, which provides
      documentation for Python objects.
    Example
      math = import "math"
      help math
      help math@@sin
  SeeAlso
    pythonHelp
///

doc ///
  Key
    pythonHelp
  Headline
    run Python's interactive help utility
  Usage
    pythonHelp
  Description
    Text
      This run's Python's interactive help utility.  Type "quit" when done.
    CannedExample
      i1 : pythonHelp

      Welcome to Python 3.10's help utility!

      If this is your first time using Python, you should definitely check out
      the tutorial on the internet at https://docs.python.org/3.10/tutorial/.

      Enter the name of any module, keyword, or topic to get help on writing
      Python programs and using Python modules.  To quit this help utility and
      return to the interpreter, just type "quit".

      To get a list of available modules, keywords, symbols, or topics, type
      "modules", "keywords", "symbols", or "topics".  Each module also comes
      with a one-line summary of what it does; to list the modules whose name
      or summary contain a given string such as "spam", type "modules spam".

      help> lambda
      Lambdas
      *******

         lambda_expr ::= "lambda" [parameter_list] ":" expression

      Lambda expressions (sometimes called lambda forms) are used to create
      anonymous functions. The expression "lambda parameters: expression"
      yields a function object.  The unnamed object behaves like a function
      object defined with:

         def <lambda>(parameters):
             return expression

      See section Function definitions for the syntax of parameter lists.
      Note that functions created with lambda expressions cannot contain
      statements or annotations.

      Related help topics: FUNCTIONS

      help> quit

      You are now leaving help and returning to the Python interpreter.
      If you want to ask for help on a particular object directly from the
      interpreter, you can type "help(object)".  Executing "help('string')"
      has the same effect as typing a particular string at the help> prompt.

      o1 = None

      o1 : PythonObject of class NoneType
  SeeAlso
    (help#0, PythonObject)
///
