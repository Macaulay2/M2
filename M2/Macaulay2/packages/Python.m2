-* 
this does not work unless M2 is compiled --with-python
*-

pythonPresent := Core#"private dictionary"#?"pythonRunString"

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
    AuxiliaryFiles => true,
    CacheExampleOutput => true,
    OptionalComponentsPresent => pythonPresent
    )

verboseLog = if debugLevel > 0 then printerr else identity

if pythonPresent then verboseLog "success: python is present" else (
    verboseLog "warning: python is not present";
    verboseLog "specify --with-python in `configure` options and recompile M2";
    load "Python/no-python.m2";
    load "Python/doc.m2";
    end)

exportFrom_Core {
    "runSimpleString",
    "PythonObject",
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
    "pythonRunString",
    "pythonSetNew",
    "pythonTrue",
    "pythonTupleNew",
    "pythonTupleSetItem",
    "pythonUnicodeAsUTF8",
    "pythonUnicodeConcat",
    "pythonUnicodeFromString"
}

export { "pythonHelp", "context", "Preprocessor", "toPython",
    "addPyToM2Function",
    "getattr",
    "getitem",
    "hasattr",
    "import",
    "iter",
    "iterableToList",
    "next",
    "pythonValue",
    "setattr",
    "setitem",
    "toFunction"
}

exportMutable { "val", "eval", "valuestring", "stmt", "expr", "dict", "symbols", "stmtexpr"}

pythonHelp = Command (() -> pythonValue ///help()///)

toString PythonObject := pythonUnicodeAsUTF8 @@ pythonObjectStr

PythonObject.synonym = "python object"
PythonObject#{Standard,AfterPrint} = x -> (
     << endl;
     t := toString objectType x;
     t = replace("<([a-z]+) '(.*)'>","of \\1 \\2",t);
     << concatenate(interpreterDepth:"o") << lineNumber << " : PythonObject " << t << endl;
     )

pythonValue = method(Dispatch => Thing)
pythonValue String := s -> (
    if debugLevel > 0 then printerr("python command: ", s);
    pythonRunString s)
pythonValue Sequence := S -> pythonValue \\ concatenate \\ toString \ S

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
     pythonValue("eval(compile( '",dict," = {}','','single' ),__builtins__) ");
     access := s -> concatenate(dict,"[", format s, "]");
     val := s -> pythonValue access s;
     eval := s -> pythonValue concatenate("eval(compile(",s,",'','single' ),",dict,")");
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
	       if debugLevel > 0 then stderr << "--intermediate value: tmp = " << format toString pythonValue access "tmp" << endl;
	       eval access "tmp";
	       null)
	  );
     expr := s -> (
	  s = "temp = " | s;
	  stmt s;
	  val "temp");
     stmtexpr := s -> if match(";$",s) then stmt s else expr s;
     symbols := () -> pythonValue concatenate("__builtins__[",format dict,"].keys()");
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
	if instance(y, Sequence) then y else 1:y);
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
addPyToM2Function("Counter", x -> new Tally from dictToHashTable x,
    "Counter -> Tally")
addPyToM2Function({"dict", "defaultdict"}, dictToHashTable, "dict -> HashTable")
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

-PythonObject := o -> o@@"__neg__"()
+PythonObject := o -> o@@"__pos__"()

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
toPython VisibleList := L -> (
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

load "Python/doc.m2"

TEST ///
-----------
-- value --
-----------
assert Equation(value pythonValue "True", true)
assert Equation(value pythonValue "5", 5)
assert Equation(value pythonValue "3.14159", 3.14159)
assert Equation(value pythonValue "complex(1, 2)", 1 + 2*ii)
assert Equation(value pythonValue "'foo'", "foo")
assert Equation(value pythonValue "(1, 3, 5, 7, 9)", (1, 3, 5, 7, 9))
assert Equation(value pythonValue "range(5)", (0, 1, 2, 3, 4))
assert Equation(value pythonValue "[1, 3, 5, 7, 9]", {1, 3, 5, 7, 9})
assert BinaryOperation(symbol ===,
    value pythonValue "{1, 3, 5, 7, 9}", set {1, 3, 5, 7, 9})
assert BinaryOperation(symbol ===,
    value pythonValue "frozenset([1, 3, 5, 7, 9])",
    set {1, 3, 5, 7, 9})
assert BinaryOperation(symbol ===, value pythonValue "{'a':1, 'b':2, 'c':3}",
    hashTable{"a" => 1, "b" => 2, "c" => 3})
assert Equation((value pythonValue "abs")(-1), pythonValue "1")
assert Equation(value pythonValue "None", null)
///

TEST ///
----------------------
-- nested iterators --
----------------------
assert Equation(value pythonValue "[[1,2]]", {{1,2}})
assert Equation(value pythonValue "[(1,2)]", {(1,2)})
assert BinaryOperation(symbol ===, value pythonValue "[{1,2}]", {set {1,2}})
assert BinaryOperation(symbol ===, value pythonValue "[{1:2}]",
    {hashTable {1 => 2}})
assert Equation(value pythonValue "([1,2],)", 1:{1,2})
assert Equation(value pythonValue "((1,2),)", 1:(1,2))
assert BinaryOperation(symbol ===, value pythonValue "({1,2},)", 1:set {1,2})
assert BinaryOperation(symbol ===, value pythonValue "({1:2},)",
    1:hashTable {1 => 2})
assert BinaryOperation(symbol ===, value pythonValue "{(1,2)}", set {(1,2)})
assert BinaryOperation(symbol ===, value pythonValue "{(1,2):[3,4]}",
    hashTable {(1,2) => {3,4}})
assert BinaryOperation(symbol ===, value pythonValue "{(1,2):(3,4)}",
    hashTable {(1,2) => (3,4)})
assert BinaryOperation(symbol ===, value pythonValue "{(1,2):{3,4}}",
    hashTable {(1,2) => set {3,4}})
assert BinaryOperation(symbol ===, value pythonValue "{(1,2):{3:4}}",
    hashTable {(1,2) => hashTable {3 => 4}})
///

TEST ///
-----------------------
-- binary operations --
-----------------------
x = pythonValue "5"
y = pythonValue "2"

-- addition
assert Equation(x + y, pythonValue "7")
assert Equation(x + 2, 7)
assert Equation(5 + y, 7)

-- subtraction
assert Equation(x - y, pythonValue "3")
assert Equation(x - 2, 3)
assert Equation(5 - y, 3)

-- multiplication
assert Equation(x * y, pythonValue "10")
assert Equation(x * 2, 10)
assert Equation(5 * y, 10)

-- true division
assert Equation(x / y, pythonValue "2.5")
assert Equation(x / 2, 2.5)
assert Equation(5 / y, 2.5)

-- floor division
assert Equation(x // y, pythonValue "2")
assert Equation(x // 2, 2)
assert Equation(5 // y, 2)

-- modulo
assert Equation(x % y, pythonValue "1")
assert Equation(x % 2, 1)
assert Equation(5 % y, 1)

-- power
assert Equation(x ^ y, pythonValue "25")
assert Equation(x ^ 2, 25)
assert Equation(5 ^ y, 25)

-- left shift
assert Equation(x << y, pythonValue "20")
assert Equation(x << 2, 20)
assert Equation(5 << y, 20)

-- right shift
assert Equation(x >> y, pythonValue "1")
assert Equation(x >> 2, 1)
assert Equation(5 >> y, 1)

-- and
assert Equation(x & y, pythonValue "0")
assert Equation(x & 2, 0)
assert Equation(5 & y, 0)
assert Equation(x and y, pythonValue "0")
assert Equation(x and 2, 0)
assert Equation(5 and y, 0)

-- or
assert Equation(x | y, pythonValue "7")
assert Equation(x | 2, 7)
assert Equation(5 | y, 7)
assert Equation(x or y, pythonValue "7")
assert Equation(x or 2, 7)
assert Equation(5 or y, 7)

-- xor
assert Equation(x ^^ y, pythonValue "7")
assert Equation(x ^^ 2, 7)
assert Equation(5 ^^ y, 7)
assert Equation(x xor y, pythonValue "7")
assert Equation(x xor 2, 7)
assert Equation(5 xor y, 7)

----------------------
-- unary operations --
----------------------
assert Equation(-x, -5)
assert Equation(+x, 5)
///

TEST ///
-----------------------
-- string operations --
-----------------------
foo = pythonValue "'foo'"
bar = pythonValue "'bar'"

-- concatenation
assert Equation(foo + bar, pythonValue "'foobar'")
assert Equation(foo + "bar", "foobar")
assert Equation("foo" + bar, "foobar")

-- repetition
assert Equation(foo * pythonValue "2", pythonValue "'foofoo'")
assert Equation(foo * 2, "foofoo")
assert Equation("foo" * pythonValue "2", "foofoo")
assert Equation(pythonValue "2" * foo, pythonValue "'foofoo'")
assert Equation(2 * foo, "foofoo")
assert Equation(pythonValue "2" * "foo", "foofoo")

-- check a few methods
assert Equation(foo@@capitalize(), pythonValue "'Foo'")
assert Equation(foo@@center(5, "x"), pythonValue "'xfoox'")
assert Equation((pythonValue "'{0}, {1}!'")@@format("Hello", "world"),
    pythonValue "'Hello, world!'")
assert Equation(foo@@replace("f", "F"), pythonValue "'Foo'")
assert Equation(foo@@upper(), pythonValue "'FOO'")
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
