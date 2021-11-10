-* 
this does not work unless M2 is compiled --with-python
*-

pythonPresent := Core#"private dictionary"#?"runPythonString"

newPackage("Python",
    Version => "0.2",
    Date => "November 6, 2021",
    Headline => "interface to Python",
    Authors => {
	{Name => "Daniel R. Grayson",
	    Email => "danielrichardgrayson@gmail.com",
	    HomePage => "https://faculty.math.illinois.edu/~dan/"},
	{Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    OptionalComponentsPresent => pythonPresent
    )

verboseLog = if debugLevel > 0 then printerr else identity

if pythonPresent then verboseLog "success: python is present" else (
    verboseLog "warning: python is not present";
    verboseLog "specify --with-python in `configure` options and recompile M2";
    end)

exportFrom_Core {
    "runSimpleString",
    "PythonObject",
    "runPythonString",
    "objectType"}

importFrom_Core {
    "pythonComplexFromDoubles",
    "pythonDictNew",
    "pythonDictSetItem",
    "pythonFalse",
    "pythonImportImportModule",
    "pythonNone",
    "pythonListNew",
    "pythonListSetItem",
    "pythonLongAsLong",
    "pythonLongFromLong",
    "pythonFloatAsDouble",
    "pythonFloatFromDouble",
    "pythonObjectGetAttrString",
    "pythonObjectHasAttrString",
    "pythonObjectRichCompareBool",
    "pythonObjectSetAttrString",
    "pythonObjectCall",
    "pythonObjectStr",
    "pythonSetNew",
    "pythonTrue",
    "pythonTupleNew",
    "pythonTupleSetItem",
    "pythonUnicodeAsUTF8",
    "pythonUnicodeConcat",
    "pythonUnicodeFromString"
}

export { "pythonHelp", "context", "rs", "Preprocessor", "toPython",
    "addPyToM2Function",
    "getattr",
    "getitem",
    "hasattr",
    "import",
    "iter",
    "iterableToList",
    "next",
    "setattr",
    "setitem",
    "toFunction"
}

exportMutable { "val", "eval", "valuestring", "stmt", "expr", "dict", "symbols", "stmtexpr"}

pythonHelp = Command (() -> runPythonString ///help()///)

toString PythonObject := pythonUnicodeAsUTF8 @@ pythonObjectStr

PythonObject.synonym = "python object"
PythonObject#{Standard,AfterPrint} = x -> (
     << endl;
     t := toString objectType x;
     t = replace("<([a-z]+) '(.*)'>","of \\1 \\2",t);
     << concatenate(interpreterDepth:"o") << lineNumber << " : PythonObject " << t << endl;
     )

rs = s -> ( 
     s = concatenate s;
     if debugLevel > 0 then stderr << "--python command: " << s << endl; 
     runPythonString s);

numContexts = 0
nextContext = method()
installMethod(nextContext,
    () -> (
     	numContexts = numContexts + 1;
     	"context" | toString numContexts)
    )
Context = new Type of HashTable
globalAssignment Context
use Context := c -> (scanPairs(c,(k,v) -> k <- v); c)
context = method(Options => {
	  Preprocessor => ""
	  })
context String := opts -> init -> (
     dict := nextContext();
     rs("eval(compile( '",dict," = {}','','single' ),__builtins__) ");
     access := s -> concatenate(dict,"[", format s, "]");
     val := s -> rs access s;
     eval := s -> rs concatenate("eval(compile(",s,",'','single' ),",dict,")");
     evalstring := s -> eval replace("\n","\\n",format concatenate s);
     evalstring init;
     valuestring := s -> (
	  evalstring("tmp = ",s);
	  val "tmp");
     stmt := if opts.Preprocessor === ""
     then s -> (
	  evalstring s;
	  null)
     else (
	  s -> (
	       evalstring("tmp = ",opts.Preprocessor,"(",format s,")");
	       if debugLevel > 0 then stderr << "--intermediate value: tmp = " << format toString runPythonString access "tmp" << endl;
	       eval access "tmp";
	       null)
	  );
     expr := s -> (
	  s = "temp = " | s;
	  stmt s;
	  val "temp");
     stmtexpr := s -> if match(";$",s) then stmt s else expr s;
     symbols := () -> runPythonString concatenate("__builtins__[",format dict,"].keys()");
     use new Context from {
	  global dict => dict,
	  global val => val,
	  global eval => evalstring,
	  global valuestring => valuestring,
	  global stmt => stmt,
	  global expr => expr,
	  global stmtexpr => stmtexpr,
	  global symbols => symbols
	  })
Context String := (c,s) -> c.stmtexpr s

import = method()
import(String) := pythonImportImportModule

iterableToList = method()
iterableToList(PythonObject) :=  x -> (
	i := iter x;
	while (y := next i; y =!= null) list value y)

dictToHashTable = method()
dictToHashTable(PythonObject) := x -> (
    i := iter x;
    hashTable while (y := next i; y =!= null)
	list value y => value x_y)

toFunction = method()
toFunction PythonObject := x -> y -> (
    p := partition(a -> instance(a, Option),
	if instance(y, VisibleList) then y else {y});
    args := toPython if p#?false then toSequence p#false else ();
    kwargs := toPython hashTable if p#?true then toList p#true else {};
    if debugLevel > 0 then printerr(
	"callable: " | toString x    ||
	"args: "     | toString args ||
	"kwargs: "   | toString kwargs);
    r := pythonObjectCall(x, args, kwargs);
    if debugLevel > 0 then printerr("output: ", toString r);
    r)

addPyToM2Function = method()
addPyToM2Function(String, Function, String) := (type, f, desc) ->
    addPyToM2Function({type}, f, desc)
addPyToM2Function(List, Function, String) := (types, f, desc) ->
    addHook((value, PythonObject),
	x -> if member(toString (objectType x)@@"__name__", types) then f x,
	Strategy => desc)

addHook((value, PythonObject),
    x -> if toString (objectType x)@@"__name__"  != "NoneType" then x,
    Strategy => "unknown -> PythonObject")
addPyToM2Function({"function", "builtin_function_or_method", "method-wrapper"},
    toFunction, "function -> FunctionClosure")
addPyToM2Function("dict", dictToHashTable, "dict -> HashTable")
addPyToM2Function({"set", "frozenset"}, set @@ iterableToList, "set -> Set")
addPyToM2Function("list", iterableToList, "list -> List")
addPyToM2Function({"tuple", "range"}, toSequence @@ iterableToList,
    "tuple -> Sequence")
addPyToM2Function("str", toString, "str -> String")
addPyToM2Function("complex", x -> x@@"real" + ii * x@@"imag", "complex -> CC")
addPyToM2Function("float", pythonFloatAsDouble, "float -> RR")
addPyToM2Function("int", pythonLongAsLong, "int -> ZZ")
addPyToM2Function("bool", x -> toString x == "True", "bool -> Boolean")
value PythonObject := x -> runHooks((value, PythonObject), x)

-- Py_LT, Py_GT, and Py_EQ are #defines from /usr/include/python3.9/object.h
PythonObject ? PythonObject := (x, y) ->
    if pythonObjectRichCompareBool(x, y, -* Py_LT *- 0) then symbol < else
    if pythonObjectRichCompareBool(x, y, -* Py_GT *- 4) then symbol > else
    if pythonObjectRichCompareBool(x, y, -* Py_EQ *- 2) then symbol == else
    incomparable
PythonObject ? Thing := (x, y) -> x ? toPython y
Thing ? PythonObject := (x, y) -> toPython x ? y

PythonObject == PythonObject := (x, y) ->
    pythonObjectRichCompareBool(x, y, -* Py_EQ *- 2)
PythonObject == Thing := (x, y) -> x == toPython y
Thing == PythonObject := (x, y) -> toPython x == y

isimplemented = x -> value x@@"__class__"@@"__name__" != "NotImplementedType"
scan({
	(symbol +, "add"),
	(symbol -, "sub"),
	(symbol *, "mul"),
	(symbol /, "truediv"),
	(symbol //, "floordiv"),
	(symbol %, "mod"),
	(symbol ^, "pow"),
	(symbol <<, "lshift"),
	(symbol >>, "rshift"),
	(symbol &, "and"),
	(symbol |, "or"),
	(symbol ^^, "xor"),
	(symbol and, "and"),
	(symbol or, "or"),
	(symbol xor, "xor")},
    (op, name) -> (
	m := "__" | name | "__";
	rm := "__r" | name | "__";
	local r;
	installMethod(op, PythonObject, PythonObject, (x, y) ->
	    if hasattr(x, m) and isimplemented(r = x@@m y) then r else
	    if hasattr(y, rm) and isimplemented(r = y@@rm x) then r else
	    error("no method for ", format toString op));
	installMethod(op, PythonObject, Thing, (x, y) ->
	    (lookup(op, PythonObject, PythonObject))(x, toPython y));
	installMethod(op, Thing, PythonObject, (x, y) ->
	    (lookup(op, PythonObject, PythonObject))(toPython x, y))
	)
    )

PythonObject Thing := (o, x) -> (toFunction o) x

length PythonObject := x -> value x@@"__len__"()

next = method()
next PythonObject := x -> x@@"__next__"();

iter = method()
iter PythonObject := x -> x@@"__iter__"()

getitem = method()
getitem(PythonObject, Thing) :=
PythonObject_Thing := (x, i) -> x@@"__getitem__" toPython i

setitem = method()
setitem(PythonObject, Thing, Thing) := (x, i, e) -> (
    x@@"__setitem__"(i, e);
    null)
PythonObject_Thing = setitem

getattr = method()
getattr(PythonObject, String) := pythonObjectGetAttrString
PythonObject @@ Thing := (x, y) -> getattr(x, toString y)

hasattr = method()
hasattr(PythonObject, String) := pythonObjectHasAttrString

setattr = method()
setattr(PythonObject, String, Thing) := (x, y, e) ->
    pythonObjectSetAttrString(x, y, toPython e)
PythonObject @@ Thing = (x, y, e) -> setattr(x, toString y, e)

toPython = method(Dispatch => Thing)
toPython RR := pythonFloatFromDouble
toPython QQ := toPython @@ toRR
toPython CC := x -> pythonComplexFromDoubles(realPart x, imaginaryPart x)
toPython ZZ := pythonLongFromLong
toPython Boolean := x -> if x then pythonTrue else pythonFalse
toPython Constant := x -> toPython(x + 0)
toPython String := pythonUnicodeFromString
toPython Sequence := L -> (
    n := #L;
    result := pythonTupleNew n;
    for i to n - 1 do pythonTupleSetItem(result, i, toPython L_i);
    result)
toPython List := L -> (
    n := #L;
    result := pythonListNew n;
    for i to n - 1 do pythonListSetItem(result, i, toPython L_i);
    result)
toPython HashTable := x -> (
    result := pythonDictNew();
    for key in keys x do
	pythonDictSetItem(result, toPython key, toPython x#key);
    result)
toPython Set := pythonSetNew @@ toPython @@ toList
toPython Nothing := x -> pythonNone
toPython PythonObject := identity

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
        LI {TT "+", " → ", TT "__add__"},
        LI {TT "-", " → ", TT "__sub__"},
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
    (toPython,List)
    (toPython,Nothing)
    (toPython,PythonObject)
    (toPython,QQ)
    (toPython,RR)
    (toPython,Sequence)
    (toPython,Set)
    (toPython,String)
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

TEST ///
-----------
-- value --
-----------
assert Equation(value rs "True", true)
assert Equation(value rs "5", 5)
assert Equation(value rs "3.14159", 3.14159)
assert Equation(value rs "complex(1, 2)", 1 + 2*ii)
assert Equation(value rs "'foo'", "foo")
assert Equation(value rs "(1, 3, 5, 7, 9)", (1, 3, 5, 7, 9))
assert Equation(value rs "range(5)", (0, 1, 2, 3, 4))
assert Equation(value rs "[1, 3, 5, 7, 9]", {1, 3, 5, 7, 9})
assert BinaryOperation(symbol ===,
    value rs "{1, 3, 5, 7, 9}", set {1, 3, 5, 7, 9})
assert BinaryOperation(symbol ===, value rs "frozenset([1, 3, 5, 7, 9])",
    set {1, 3, 5, 7, 9})
assert BinaryOperation(symbol ===, value rs "{'a':1, 'b':2, 'c':3}",
    hashTable{"a" => 1, "b" => 2, "c" => 3})
assert Equation((value rs "abs")(-1), rs "1")
assert Equation(value rs "None", null)
///

TEST ///
----------------------
-- nested iterators --
----------------------
assert Equation(value rs "[[1,2]]", {{1,2}})
assert Equation(value rs "[(1,2)]", {(1,2)})
assert BinaryOperation(symbol ===, value rs "[{1,2}]", {set {1,2}})
assert BinaryOperation(symbol ===, value rs "[{1:2}]", {hashTable {1 => 2}})
assert Equation(value rs "([1,2],)", 1:{1,2})
assert Equation(value rs "((1,2),)", 1:(1,2))
assert BinaryOperation(symbol ===, value rs "({1,2},)", 1:set {1,2})
assert BinaryOperation(symbol ===, value rs "({1:2},)", 1:hashTable {1 => 2})
assert BinaryOperation(symbol ===, value rs "{(1,2)}", set {(1,2)})
assert BinaryOperation(symbol ===, value rs "{(1,2):[3,4]}",
    hashTable {(1,2) => {3,4}})
assert BinaryOperation(symbol ===, value rs "{(1,2):(3,4)}",
    hashTable {(1,2) => (3,4)})
assert BinaryOperation(symbol ===, value rs "{(1,2):{3,4}}",
    hashTable {(1,2) => set {3,4}})
assert BinaryOperation(symbol ===, value rs "{(1,2):{3:4}}",
    hashTable {(1,2) => hashTable {3 => 4}})
///

TEST ///
-----------------------
-- binary operations --
-----------------------
x = rs "5"
y = rs "2"

-- addition
assert Equation(x + y, rs "7")
assert Equation(x + 2, 7)
assert Equation(5 + y, 7)

-- subtraction
assert Equation(x - y, rs "3")
assert Equation(x - 2, 3)
assert Equation(5 - y, 3)

-- multiplication
assert Equation(x * y, rs "10")
assert Equation(x * 2, 10)
assert Equation(5 * y, 10)

-- true division
assert Equation(x / y, rs "2.5")
assert Equation(x / 2, 2.5)
assert Equation(5 / y, 2.5)

-- floor division
assert Equation(x // y, rs "2")
assert Equation(x // 2, 2)
assert Equation(5 // y, 2)

-- modulo
assert Equation(x % y, rs "1")
assert Equation(x % 2, 1)
assert Equation(5 % y, 1)

-- power
assert Equation(x ^ y, rs "25")
assert Equation(x ^ 2, 25)
assert Equation(5 ^ y, 25)

-- left shift
assert Equation(x << y, rs "20")
assert Equation(x << 2, 20)
assert Equation(5 << y, 20)

-- right shift
assert Equation(x >> y, rs "1")
assert Equation(x >> 2, 1)
assert Equation(5 >> y, 1)

-- and
assert Equation(x & y, rs "0")
assert Equation(x & 2, 0)
assert Equation(5 & y, 0)
assert Equation(x and y, rs "0")
assert Equation(x and 2, 0)
assert Equation(5 and y, 0)

-- or
assert Equation(x | y, rs "7")
assert Equation(x | 2, 7)
assert Equation(5 | y, 7)
assert Equation(x or y, rs "7")
assert Equation(x or 2, 7)
assert Equation(5 or y, 7)

-- xor
assert Equation(x ^^ y, rs "7")
assert Equation(x ^^ 2, 7)
assert Equation(5 ^^ y, 7)
assert Equation(x xor y, rs "7")
assert Equation(x xor 2, 7)
assert Equation(5 xor y, 7)
///

TEST ///
-----------------------
-- string operations --
-----------------------
foo = rs "'foo'"
bar = rs "'bar'"

-- concatenation
assert Equation(foo + bar, rs "'foobar'")
assert Equation(foo + "bar", "foobar")
assert Equation("foo" + bar, "foobar")

-- repetition
assert Equation(foo * rs "2", rs "'foofoo'")
assert Equation(foo * 2, "foofoo")
assert Equation("foo" * rs "2", "foofoo")
assert Equation(rs "2" * foo, rs "'foofoo'")
assert Equation(2 * foo, "foofoo")
assert Equation(rs "2" * "foo", "foofoo")

-- check a few methods
assert Equation(foo@@capitalize(), rs "'Foo'")
assert Equation(foo@@center(5, "x"), rs "'xfoox'")
assert Equation((rs "'{0}, {1}!'")@@format("Hello", "world"),
    rs "'Hello, world!'")
assert Equation(foo@@replace("f", "F"), rs "'Foo'")
assert Equation(foo@@upper(), rs "'FOO'")
///

end --------------------------------------------------------


restart
debugLevel = 1
debuggingMode = false
loadPackage "Python"

pythonHelp
quit

runSimpleString "x=2"
runSimpleString "print x"
rs "dir()"
rs "dict"
rs "__builtins__.keys()"
rs "range(2,100)"

-- module sys
-- http://docs.python.org/library/sys.html#module-sys
sys = context "import sys";
expr "sys.version"

sys2 = context "from sys import *";
sys2 "version"
sys2 "modules.keys()"
sys2 "copyright"
sys2 "prefix"
sys2 "executable"

os = context "from os import *; import os";
os "os.__doc__"
os "os.name"
os "dir()"
os "link"
os "dir(link)"
os "link.__name__"
os "link.__doc__"
ascii toString os "linesep"
os "path"
os "path.__doc__"
os "dir(path)"
os "import os.path;"
os "os.path.join.__doc__"
os "os.path.join('asdf','qwer','wert')"

math = context "from math import *";
symbols()
math "x = sin(3.4);"
math "sin(3.4)"
math "x"
math "e"

sage = context("from sage.all import *", Preprocessor => "preparse");
sage "x = var('x');"
sage "plot(sin(x));"
sage "320"
sage "sage"
sage "dir(sage)"
sage "sage.version"
sage "version()"
sage "dir(sage.version)"
sage "sage.version.version"
sage "dir(sage.categories.morphism)"
sage "sage.categories.morphism.__file__"
sage "sage.categories.morphism.__doc__"
sage "sage.categories.morphism.homset.Hom"
sage "dir(sage.categories.morphism.homset.Hom)"
sage "sage.categories.morphism.homset.Hom.__doc__"
hash sage "SymmetricGroup(3)"
hash sage "SymmetricGroup(3)" == hash sage "SymmetricGroup(3)"
hash sage "SymmetricGroup(2)"
hash sage "SymmetricGroup(2)" == hash sage "SymmetricGroup(3)"
sage "G = SymmetricGroup(3);"
sage "G"
sage "dir(G)"
sage "G.set()"
sage "G.sylow_subgroup(3)"
sage "G.sylow_subgroup(2)"
sage "G.dump.__doc__"
sage "G.multiplication_table()"
sage "plot"
sage "preparse"
sage "preparse('x=1')"
sage "x=2^100"
sage "x"
sage "R.<x,y,z> = QQ[];;"
sage "R"
sage "x = var('x');"
sage "plot(sin(x))"
sage "plot(sin(x));"
sage "show(plot(sin(x)))"
sage "I = ideal(x^2,y*z);"
sage "I"
sage "dir(I)"
sage "R.<t> = PowerSeriesRing(QQ);"
sage "R"
sage "exp(t)"

sage "p = plot(sin(x));"
p = sage "p"
hash p			  -- this displays the plot and gives a hash code of 0!


initspam()
spam = context "from spam import *";
symbols()
expr "system"
expr "system('echo hi there')"

gc = context "import gc"
expr "gc.set_debug(gc.DEBUG_LEAK)"
expr "gc.set_debug(gc.DEBUG_STATS)"

turtle = context "from turtle import *";
t = turtle.stmt
t "x=Pen()"
t "x.color('blue')"
t "x.forward(200)"
t "x.left(200)"
turtle "dir()"
turtle "x.speed"
t "x.speed('fastest')"
turtle "speeds"
