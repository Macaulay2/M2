-* 
this does not work unless M2 is compiled --with-python
*-

pythonPresent := Core#"private dictionary"#?"pythonRunString"

newPackage("Python",
    Version => "0.5",
    Date => "May 13, 2023",
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

---------------
-- ChangeLog --
---------------

-*

0.5 (2023-05-13, M2 1.22)
* improvements for displaying python objects in webapp mode

0.4 (2022-10-31, M2 1.21)
* fix bug involving hash codes for unhashtable types
* allow subclassing of PythonObject
* add support for more operators and builtin functions
* add support for M2 iteration
* improve integer conversion
* convert M2 functions to python functions
* add support for numpy scalars

0.3 (2022-05-04, M2 1.20)
* improve handling of lists
* add support for collections module types
* add support for unary operators
* rename rs -> pythonValue

0.2 (2021-11-06, M2 1.19)
* initial release

0.1 (unofficial, not distributed)

*-

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
    "getPythonNone",
    "pythonComplexFromDoubles",
    "pythonDictNew",
    "pythonDictSetItem",
    "pythonFalse",
    "pythonImportImportModule",
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
    "pythonUnicodeAsUTF8",
    "pythonUnicodeFromString",
    "pythonWrapM2Function"
}

export { "pythonHelp", "context", "Preprocessor", "toPython",
    "addPyToM2Function",
    "getattr",
    "getitem",
    "hasattr",
    "import",
    "pythonValue",
    "setattr",
    "setitem",
    "toFunction"
}

exportMutable { "val", "eval", "valuestring", "stmt", "expr", "dict", "symbols", "stmtexpr"}

pythonHelp = Command (() -> pythonValue ///help()///)

toString PythonObject := pythonUnicodeAsUTF8 @@ pythonObjectStr

PythonObject.synonym = "python object"
PythonObject#AfterPrint = x -> (
     t := toString objectType x;
     t = replace("<([a-z]+) '(.*)'>"," of \\1 \\2",t);
     (PythonObject, t))

pythonNone = getPythonNone()

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

toFunction = method()
toFunction PythonObject := x -> y -> (
    p := partition(a -> instance(a, Option),
	if instance(y, Sequence) then y else 1:y);
    args := toPython if p#?false then p#false else ();
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
dictToHashTable = x -> hashTable for key in x list value key => value x_key
addPyToM2Function("Counter", x -> new Tally from dictToHashTable x,
    "Counter -> Tally")
addPyToM2Function({"dict", "defaultdict"}, dictToHashTable, "dict -> HashTable")
pyListToM2List = x -> for y in x list value y
addPyToM2Function({"set", "frozenset"}, set @@ pyListToM2List, "set -> Set")
addPyToM2Function("list", pyListToM2List, "list -> List")
addPyToM2Function({"tuple", "range"}, toSequence @@ pyListToM2List,
    "tuple -> Sequence")
addPyToM2Function("str", toString, "str -> String")
addPyToM2Function(
    {"complex", "complex64", "complex128", "complex256"},
    x -> toCC(pythonFloatAsDouble x@@"real", pythonFloatAsDouble x@@"imag"),
    "complex -> CC")
addPyToM2Function(
    {"float", "float16", "float32", "float64", "float128"},
    pythonFloatAsDouble,
    "float -> RR")
pyInt = toFunction pythonValue "int"
addPyToM2Function(
    {"int", "int8", "uint8", "int16", "uint16", "int32", "uint32",
	"int64", "uint64", "longlong", "ulonglong"},
    pythonLongAsLong @@ pyInt,
    "int -> ZZ")
addPyToM2Function(
    {"bool", "bool_"},
    x -> toString x == "True",
    "bool -> Boolean")
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
	(symbol @, "matmul"),
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
abs PythonObject := o -> o@@"__abs__"()
PythonObject~ := o -> o@@"__invert__"()

PythonObject Thing := (o, x) -> (toFunction o) x

length PythonObject := x -> value x@@"__len__"()

next PythonObject := x -> x@@"__next__"();

iterator PythonObject := x -> x@@"__iter__"()

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

member(Thing,        PythonObject) := (x, y) -> false
member(PythonObject, PythonObject) := (x, y) -> value y@@"__contains__" x

quotientRemainder(PythonObject, PythonObject) := (x, y) -> (
    qr := x@@"__divmod__" y;
    (qr_0, qr_1))
quotientRemainder(PythonObject, Thing) := (x, y
    ) -> quotientRemainder(x, toPython y)
quotientRemainder(Thing, PythonObject) := (x, y
    ) -> quotientRemainder(toPython x, y)

round(PythonObject, PythonObject) := (n, x) -> x@@"__round__" n
round(ZZ, PythonObject) := (n, x) -> round(toPython n, x)
round PythonObject := x -> round(pythonNone, x)
truncate PythonObject := {} >> o -> x -> x@@"__trunc__"()

-- __floor__ and __ceil__ were added for floats in Python 3.9
-- (https://bugs.python.org/issue38629), so we include backup definitions
-- for older versions
if hasattr(pythonFloatFromDouble 1.0, "__floor__") then (
    floor PythonObject := x -> x@@"__floor__"();
    ceiling PythonObject := x -> x@@"__ceil__"()
    ) else (
    math := import "math";
    floor PythonObject := toFunction math@@"floor";
    ceiling PythonObject := toFunction math@@"ceil")

help#0 PythonObject := x -> toString x@@"__doc__"

toPython = method(Dispatch => Thing)
toPython RR := pythonFloatFromDouble
toPython QQ := toPython @@ toRR
toPython CC := x -> pythonComplexFromDoubles(realPart x, imaginaryPart x)
toPython ZZ := pythonLongFromLong
toPython Boolean := x -> if x then pythonTrue else pythonFalse
toPython Constant := x -> toPython(x + 0)
toPython String := pythonUnicodeFromString
toPython Sequence := x -> pythonTupleNew \\ toPython \ x
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

toPython Function := f -> (
    pythonWrapM2Function(toString f, pyargs -> (
	    m2args := value pyargs;
	    if instance(m2args, Sequence) and #m2args == 1
	    then m2args = m2args#0;
	    toPython f m2args)))

load "Python/doc.m2"

TEST ///
-----------
-- value --
-----------
checkInM2 = x -> assert BinaryOperation(symbol ===, value toPython x, x)
checkInM2 true
checkInM2 5
checkInM2 3.14159
checkInM2 toCC(1., 2.)
checkInM2 "foo"
checkInM2 (1, 3, 5, 7, 9)
checkInM2 {1, 3, 5, 7, 9}
checkInM2 set {1, 3, 5, 7, 9}
checkInM2 hashTable {"a" => 1, "b" => 2, "c" => 3}
checkInM2 null
assert BinaryOperation(symbol ===,
    value pythonValue "frozenset([1, 3, 5, 7, 9])", set {1, 3, 5, 7, 9})

checkInPython = x -> (y := pythonValue x; assert Equation(toPython value y, y))
checkInPython "True"
checkInPython "5"
checkInPython "3.14159"
checkInPython "complex(1, 2)"
checkInPython "'foo'"
checkInPython "(1, 3, 5, 7, 9)"
checkInPython "[1, 3, 5, 7, 9]"
checkInPython "{1, 3, 5, 7, 9}"
checkInPython "{'a': 1, 'b': 2, 'c': 3}"
checkInPython "None"
assert Equation((value pythonValue "abs")(-1), pythonValue "1")
assert Equation((toPython sqrt) 2, toPython sqrt 2)
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

TEST ///
-- issue #2315
rand = import "random"
L = toPython {1, 2, 3}
assert member(value rand@@choice L, {1, 2, 3})
assert Equation(L + L, toPython {1, 2, 3, 1, 2, 3})
///

TEST ///
-- issue #2590
ChildPythonObject = new Type of PythonObject
x = new ChildPythonObject from toPython 5
y = new ChildPythonObject from toPython 10
assert BinaryOperation(symbol <, x, y)
assert hasattr(x, "__abs__")
assert Equation(x@@"__abs__"(), 5)
assert Equation(toString x, "5")
assert Equation(value x, 5)
math = new ChildPythonObject from import "math"
math@@pi = 3.14159
assert Equation(math@@pi, 3.14159)
z = new ChildPythonObject from math@@pi
assert Equation(value z, 3.14159)
hello = new ChildPythonObject from toPython "Hello, world!"
assert Equation(value hello, "Hello, world!")
assert Equation(toPython (x, y, z), (5, 10, 3.14159))
assert Equation(toPython {x, y, z}, {5, 10, 3.14159})
assert Equation(toPython hashTable {x => y}, hashTable {x => y})
///


TEST ///
-- built-in functions

-- abs
assert Equation(abs toPython(-3), 3)

-- ~ (bitwise not)
assert Equation((toPython 5)~, -6)

-- __contains__
assert member(toPython 3, toPython {1, 2, 3})
assert not member(toPython 4, toPython {1, 2, 3})
assert not member(3, toPython {1, 2, 3})

-- divmod
assert Equation(quotientRemainder(toPython 1234, toPython 456), (2, 322))
assert Equation(quotientRemainder(toPython 1234, 456), (2, 322))
assert Equation(quotientRemainder(1234, toPython 456), (2, 322))

-- round
e = (import "math")@@e
assert Equation(round e, 3)
assert Equation(round(3, e), 2.718)
assert Equation(round toPython 2.5, 2)
assert Equation(round toPython 3.5, 4)

-- math.trunc
assert Equation(truncate e, 2)
assert Equation(truncate(-e), -2)

-- math.floor
assert Equation(floor e, 2)
assert Equation(floor(-e), -3)

-- mail.ceil
assert Equation(ceiling e, 3)
assert Equation(ceiling(-e), -2)

-- help
x = help (import "math")@@cos
assert instance(x, String)
assert match("cosine", x)
///

TEST ///
-- large integers
assert Equation(toPython 10^100, pythonValue "10**100")
assert Equation(toPython(-10^100), pythonValue "-10**100")
assert Equation(value pythonValue "10**100", 10^100)
assert Equation(value pythonValue "-10**100", -10^100)
///


-- not part of default testsuite since it requires numpy
///
-----------
-- NumPy --
-----------
np = import "numpy"

-- @ (__matmul__ operator)
v = np@@array {1, 2, 3}
w = np@@array {4, 5, 6}
assert Equation(v @ w, 32)

-- scalar types
checkNumPyIntDtype = T -> assert BinaryOperation(symbol ===, value np@@T 1, 1)
checkNumPyIntDtype "int8"
checkNumPyIntDtype "uint8"
checkNumPyIntDtype "int16"
checkNumPyIntDtype "uint16"
checkNumPyIntDtype "int32"
checkNumPyIntDtype "uint32"
checkNumPyIntDtype "int64"
checkNumPyIntDtype "uint64"
checkNumPyIntDtype "byte"
checkNumPyIntDtype "ubyte"
checkNumPyIntDtype "short"
checkNumPyIntDtype "ushort"
checkNumPyIntDtype "intc"
checkNumPyIntDtype "uintc"
checkNumPyIntDtype "int_"
checkNumPyIntDtype "uint"
checkNumPyIntDtype "longlong"
checkNumPyIntDtype "ulonglong"
checkNumPyIntDtype "intp"
checkNumPyIntDtype "uintp"

checkNumPyRealDtype = T -> assert BinaryOperation(symbol ===,
    value np@@T 1, 1.0)
checkNumPyRealDtype "float16"
checkNumPyRealDtype "float32"
checkNumPyRealDtype "float64"
-- checkNumPyRealDtype "float96"
checkNumPyRealDtype "float128"
checkNumPyRealDtype "float_"
checkNumPyRealDtype "half"
checkNumPyRealDtype "single"
checkNumPyRealDtype "double"
checkNumPyRealDtype "longdouble"

assert BinaryOperation(symbol ===, value np@@"bool_" true, true)
assert BinaryOperation(symbol ===, value np@@"bool8" true, true)

checkNumPyComplexDtype = T -> assert BinaryOperation(symbol ===, value np@@T 1,
    toCC(1.0, 0.0))
checkNumPyComplexDtype "complex64"
checkNumPyComplexDtype "complex128"
-- checkNumPyComplexDtype "complex192"
checkNumPyComplexDtype "complex256"
checkNumPyComplexDtype "complex_"
checkNumPyComplexDtype "csingle"
checkNumPyComplexDtype "cdouble"
checkNumPyComplexDtype "clongdouble"
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
