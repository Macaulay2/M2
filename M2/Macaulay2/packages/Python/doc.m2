beginDocumentation()

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
      value rs "[1, 2/3, 'foo', (1, 2, 3), {'foo' : 'bar'}]"
      math = import "math"
      math@@sqrt 2
///

doc ///
  Key
    PythonObject
    (symbol +, PythonObject, PythonObject)
    (symbol +, PythonObject, Thing)
    (symbol +, Thing, PythonObject)
    (symbol -, PythonObject, PythonObject)
    (symbol -, PythonObject, Thing)
    (symbol -, Thing, PythonObject)
    (symbol *, PythonObject, PythonObject)
    (symbol *, PythonObject, Thing)
    (symbol *, Thing, PythonObject)
    (symbol /, PythonObject, PythonObject)
    (symbol /, PythonObject, Thing)
    (symbol /, Thing, PythonObject)
    (symbol //, PythonObject, PythonObject)
    (symbol //, PythonObject, Thing)
    (symbol //, Thing, PythonObject)
    (symbol %, PythonObject, PythonObject)
    (symbol %, PythonObject, Thing)
    (symbol %, Thing, PythonObject)
    (symbol ^, PythonObject, PythonObject)
    (symbol ^, PythonObject, Thing)
    (symbol ^, Thing, PythonObject)
    (symbol <<, PythonObject, PythonObject)
    (symbol <<, PythonObject, Thing)
    (symbol <<, Thing, PythonObject)
    (symbol >>, PythonObject, PythonObject)
    (symbol >>, PythonObject, Thing)
    (symbol >>, Thing, PythonObject)
    (symbol &, PythonObject, PythonObject)
    (symbol &, PythonObject, Thing)
    (symbol &, Thing, PythonObject)
    (symbol |, PythonObject, PythonObject)
    (symbol |, PythonObject, Thing)
    (symbol |, Thing, PythonObject)
    (symbol ^^, PythonObject, PythonObject)
    (symbol ^^, PythonObject, Thing)
    (symbol ^^, Thing, PythonObject)
    (symbol and, PythonObject, PythonObject)
    (symbol and, PythonObject, Thing)
    (symbol and, Thing, PythonObject)
    (symbol or, PythonObject, PythonObject)
    (symbol or, PythonObject, Thing)
    (symbol or, Thing, PythonObject)
    (symbol xor, PythonObject, PythonObject)
    (symbol xor, PythonObject, Thing)
    (symbol xor, Thing, PythonObject)
    (symbol ==, PythonObject, PythonObject)
    (symbol ==, PythonObject, Thing)
    (symbol ==, Thing, PythonObject)
    (symbol ?, PythonObject, PythonObject)
    (symbol ?, PythonObject, Thing)
    (symbol ?, Thing, PythonObject)
    (symbol +, PythonObject)
    (symbol -, PythonObject)
  Headline
    a python object
  Description
    Text
      This type corresponds to all objects of the @TT
      HREF{"https://docs.python.org/3/c-api/structures.html#c.PyObject",
      "PyObject"}@ type in the Python C API, and in particular all of the
      types that users are familiar with from the Python language itself.
    Text
      You can perform basic arithmetic on python objects.
    Example
      x = rs "5"
      y = rs "2"
      x + y
      x - y
      x * y
      x / y
    Text
      You can also compare them.
    Example
      x > y
      x == y
    Text
      You can also perform operations on python objects and Macaulay2 things.
      The results will be returned as python objects.
    Example
      x + 2
    Text
      Note that many keywords in Macaulay2 are mapped to a certain
      dunder method in Python.  In particular,
    Code
      UL {
        LI {TT "+", " → ", TT "__add__", " (binary), ",
	    TT "__pos__", " (unary)"},
        LI {TT "-", " → ", TT "__sub__", " (binary), ",
	    TT "__neg__", " (unary)"},
        LI {TT "*", " → ", TT "__mul__"},
        LI {TT "/", " → ", TT "__truediv__"},
        LI {TT "//", " → ", TT "__floordiv__"},
        LI {TT "%", " → ", TT "__mod__"},
        LI {TT "^", " → ", TT "__pow__"},
        LI {TT "<<", " → ", TT "__lshift__"},
        LI {TT ">>", " → ", TT "__rshift__"},
        LI {TT "&", " → ", TT "__and__"},
        LI {TT "|", " → ", TT "__or__"},
        LI {TT "^^", " → ", TT "__xor__"},
        LI {TT "and", " → ", TT "__and__"},
        LI {TT "or", " → ", TT "__or__"},
        LI {TT "xor", " → ", TT "__xor__"}}
///

doc ///
  Key
    getitem
    (getitem, PythonObject, Thing)
    (symbol _, PythonObject, Thing)
  Headline
    get elements of python sequences
  Usage
    getitem(x, y)
    x_y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
   :PythonObject
  Description
    Text
      You may access elements of python sequences using @TT "getitem"@
      or the shortcut @TT "_"@.  This is equivalent to square brackets
      (@TT "[]"@) in Python. For example, this works for lists.
    Example
      x = rs "[1,2,3,4]"
      getitem(x, 0)
      x_1
    Text
      It also works for dictionaries.
    Example
      x = rs "{'spam':1,'eggs':2}"
      getitem(x, "spam")
      x_"eggs"
///

doc ///
  Key
    setitem
    (setitem, PythonObject, Thing, Thing)
    ((symbol _, symbol =), PythonObject, Thing)
  Headline
    set elements of mutable python sequences
  Usage
    setitem(x, y, e)
    x_y = e
  Inputs
    x:PythonObject
    y:Thing
    e:Thing
  Description
    Text
      You may set elements of mutable python sequences using @TT "setitem"@
      or the shortcut @TT "_"@.  This is equivalent to square brackets
      (@TT "[]"@) in Python. For example, this works for lists.
    Example
      x = rs "[1,2,3,4]"
      setitem(x, 0, 5)
      x
    Text
      It also works for dictionaries.
    Example
      x = rs "{'spam':1,'eggs':2}"
      x_"ham" = 3
      x
///

doc ///
  Key
    rs
    runPythonString
  Headline
    execute Python source code from a string
  Usage
    rs s
    runPythonString s
  Inputs
    s:String -- containing Python source code
  Outputs
    :PythonObject -- the return value of the given code
  Description
    Text
      This function a is wrapper around the function @TT
      HREF{"https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_String",
      "PyRun_String"}@ from the Python C API.  It is also available
      as @TT "runPythonString"@.
    Example
      rs "2 + 2"
  SeeAlso
    runSimpleString
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
      @TO "rs"@, it has no return value.
    Example
      runSimpleString "print('Hello, world!')"
  SeeAlso
    rs
///

doc ///
  Key
    iter
    (iter, PythonObject)
  Headline
    get iterator of iterable python object
  Usage
    i = iter x
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
      x = rs "range(3)"
      i = iter x
  SeeAlso
    next
    iterableToList
///

doc ///
  Key
    next
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
      x = rs "range(3)"
      i = iter x
      next i
      next i
      next i
    Text
      When the iterator is exhausted, @TO "null"@ is returned.
    Example
      next i === null
  SeeAlso
    iter
    iterableToList
///

doc ///
  Key
    iterableToList
    (iterableToList, PythonObject)
  Headline
    convert an iterable python object to a Macaulay2 list
  Usage
    iterableToList x
  Inputs
    x:PythonObject -- must be iterable
  Outputs
    :List
  Description
    Text
      A list is constructed containing each element of the iterable.
      The elements are converted to Macaulay2 objects (if
      possible) using @TO "value"@.
    Example
      x = rs "range(3)"
      iterableToList x
      class \ oo
  SeeAlso
    iter
    next
    iterableToList
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
      int = toFunction rs "int"
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
      length rs "'Hello, world!'"
      length rs "[1,2,3,4,5]"
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
      value rs "[1, 3.14159, 'foo', (1,2,3), {'foo':'bar'}]"
      class \ oo
    Text
      Since the type of @TT "x"@ is not initially known, a sequence of
      @TO2 {"using hooks", "hooks"}@ are used to determine its type
      and then convert it.
    Example
      hooks value
    Text
      If no conversion can be done, then @TT "x"@ is returned.
    Example
      rs "int"
      value oo
    Text
      Users may add additional hooks using @TO "addHook"@ or the
      convenience function @TO "addPyToM2Function"@.
///

doc ///
  Key
    addPyToM2Function
    (addPyToM2Function, String, Function, String)
    (addPyToM2Function, List, Function, String)
  Headline
    convenience function for adding value hooks
  Usage
    addPyToM2Function(type, f, desc)
  Inputs
    type:{String,List} -- the type(s) to convert
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
      @TO "value"@ will do nothing with these objects.
    Example
      fractions = import "fractions"
      x = fractions@@"Fraction"(2, 3)
      value x
    Text
      So we write a function to do the conversion and then install the hook
      using @TT "addPyToM2Function"@.
    Example
      toQQ = x -> value x@@"numerator" / value x@@"denominator";
      addPyToM2Function("Fraction", toQQ, "Fraction -> QQ");
      value x
      hooks value
///

doc ///
  Key
    toPython
    (toPython,Boolean)
    (toPython,CC)
    (toPython,Constant)
    (toPython,HashTable)
    (toPython,Nothing)
    (toPython,PythonObject)
    (toPython,QQ)
    (toPython,RR)
    (toPython,Sequence)
    (toPython,Set)
    (toPython,String)
    (toPython,VisibleList)
    (toPython,ZZ)
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
      available using @TO "getattr"@.
    Example
      math = import "math"
      getattr(math, "pi")
      math@@sqrt 2
///

doc ///
  Key
    getattr
    (getattr, PythonObject, String)
    (symbol @@, PythonObject, Thing)
  Headline
    get an attribute of a python object
  Usage
    getattr(x, y)
    x@@y
  Inputs
    x:PythonObject
    y:String
  Outputs
    :PythonObject
  Description
    Text
      This is equivalent to the Python @HREF{
      "https://docs.python.org/3/library/functions.html#getattr", "getattr"}@
      function.
    Example
      foo = rs "'Hello, world!'"
      (getattr(foo, "upper"))()
    Text
      In Python, "." is generally used as a shortcut for this function, but
      it is not easily overloadable in Macaulay2.  Instead, @TT "\@\@"@ may
      be used for this purpose, as its precedence is similar to "."  In
      this case, @TT "y"@  need not be a string.
    Example
      foo@@lower()
///

doc ///
  Key
    hasattr
    (hasattr, PythonObject, String)
  Headline
    whether a python object has an attribute
  Usage
    hasattr(x, y)
  Outputs
    :Boolean -- whether @TT "y"@ is an attribute of @TT "x"@
  Inputs
    x:PythonObject
    y:String
  Description
    Text
      This is equivalent to the Python @HREF{
      "https://docs.python.org/3/library/functions.html#hasattr", "hasattr"}@
      function.
    Example
      foo = rs "'Hello, world!'"
      hasattr(foo, "upper")
      hasattr(foo, "bar")
///

doc ///
  Key
    setattr
    (setattr, PythonObject, String, Thing)
    ((symbol @@, symbol =), PythonObject, Thing)
  Headline
    set an attribute of a python object
  Usage
    setattr(x, y, e)
    x@@y = e
  Inputs
    x:PythonObject
    y:String
    e:Thing
  Description
    Text
      This is equivalent to the Python @HREF{
      "https://docs.python.org/3/library/functions.html#setattr", "setattr"}@
      function.  Note that @TT "e"@ is converted to a Python object using
      @TO "toPython"@.
    Example
      math = import "math"
      setattr(math, "pi", 22/7)
      math@@pi
    Text
      As with @TO "getattr"@, when using the shortcut @TT "\@\@"@, @TT "y"@
      need not be a string.
    Example
      math@@e = 19/7
      math@@e
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
      objectType rs "2"
      objectType rs "'Hello, world!'"
///
