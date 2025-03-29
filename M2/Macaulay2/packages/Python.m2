-* 
this does not work unless M2 is compiled --with-python
*-

pythonPresent := Core#"private dictionary"#?"pythonRunString"

newPackage("Python",
    Version => "0.6",
    Date => "January 28, 2024",
    Headline => "interface to Python",
    Authors => {
	{Name => "Daniel R. Grayson",
	    Email => "danielrichardgrayson@gmail.com",
	    HomePage => "https://faculty.math.illinois.edu/~dan/"},
	{Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    Keywords => {"Interfaces"},
    AuxiliaryFiles => true,
    CacheExampleOutput => true,
    OptionalComponentsPresent => pythonPresent
    )

---------------
-- ChangeLog --
---------------

-*

0.6 (2024-01-28, M2 1.23)
* add expression, net, texMath, describe, and toExternalString methods
* move initialization of python from M2 startup to package load time
* update int <-> ZZ conversion for python 3.12
* use a constant hash for None
* add support for augmented assignment
* add support for null coalescing operator

0.5 (2023-05-18, M2 1.22)
* improvements for displaying python objects in webapp mode
* switch member -> isMember
* add keyword

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
    "pythonComplexFromDoubles",
    "pythonDictNew",
    "pythonDictSetItem",
    "pythonFalse",
    "pythonImportImportModule",
    "pythonInitialize",
    "pythonListNew",
    "pythonListSetItem",
    "pythonLongAsLong",
    "pythonLongFromLong",
    "pythonFloatAsDouble",
    "pythonFloatFromDouble",
    "pythonNone",
    "pythonObjectGetAttrString",
    "pythonObjectHasAttrString",
    "pythonObjectSetAttrString",
    "pythonObjectCall",
    "pythonObjectStr",
    "pythonRunString",
    "pythonSetNew",
    "pythonTrue",
    "pythonTupleNew",
    "pythonUnicodeAsUTF8",
    "pythonUnicodeFromString",
    "pythonWrapM2Function",
    "toExternalFormat"
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

pythonInitialize()

pythonHelp = Command (() -> pythonValue ///help()///)

expression PythonObject := expression @@ pythonUnicodeAsUTF8 @@ pythonObjectStr
toString PythonObject := toString @@ expression
net PythonObject := net @@ expression
texMath PythonObject := texMath @@ expression

describe PythonObject := x -> Describe FunctionApplication(pythonValue,
    expression x@@"__repr__"())
toExternalString PythonObject := toExternalFormat @@ describe

PythonObject.synonym = "python object"
PythonObject#AfterPrint = x -> (
     t := toString objectType x;
     t = replace("<([a-z]+) '(.*)'>"," of \\1 \\2",t);
     (PythonObject, t))

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
    args := toPython(p#false ?? ());
    kwargs := toPython hashTable (toList p#true ?? {});
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
	x -> if isMember(toString (objectType x)@@"__name__", types) then f x,
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

-- binary operators
operator = import "operator"
truthy = value @@ (toFunction operator@@"truth")
scan({
	(symbol +,  toFunction operator@@"add"),
	(symbol -,  toFunction operator@@"sub"),
	(symbol *,  toFunction operator@@"mul"),
	(symbol @,  toFunction operator@@"matmul"),
	(symbol /,  toFunction operator@@"truediv"),
	(symbol //, toFunction operator@@"floordiv"),
	(symbol %,  toFunction operator@@"mod"),
	(symbol ^,  toFunction operator@@"pow"),
	(symbol **, toFunction operator@@"pow"),
	(symbol <<, toFunction operator@@"lshift"),
	(symbol >>, toFunction operator@@"rshift"),
	(symbol &,  toFunction operator@@"and_"),
	(symbol |,  toFunction operator@@"or_"),
	(symbol ^^, toFunction operator@@"xor"),
	(symbol ==, value @@ (toFunction operator@@"eq")),
	(symbol ?,  (x, y) -> (
		if value operator@@"lt"(x, y) then symbol <
		else if value operator@@"gt"(x, y) then symbol >
		else if value operator@@"eq"(x, y) then symbol ==
		else incomparable)),
	-- TODO: if #3229 implemented, then simplify these
	(symbol and, (x, y) -> if not truthy x then x else y),
	(symbol or,  (x, y) -> if truthy x then x else y),
	(symbol xor, (x, y) -> ( -- not a Python operator, but might as well
		if      truthy x and not truthy y then x
		else if truthy y and not truthy x then y
		else toPython false))},
    (op, f) -> (
	installMethod(op, PythonObject, PythonObject, f);
	installMethod(op, PythonObject, Thing,        f);
	installMethod(op, Thing,        PythonObject, f)))

-- augmented assignment
scan({
	(symbol +=, "iadd"),
	(symbol -=, "isub"),
	(symbol *=, "imul"),
	(symbol @=, "imatmul"),
	(symbol /=, "itruediv"),
	(symbol //=, "ifloordiv"),
	(symbol %=, "imod"),
	(symbol ^=, "ipow"),
	(symbol <<=, "ilshift"),
	(symbol >>=, "irshift"),
	(symbol &=, "iand"),
	(symbol |=, "ior"),
	(symbol ^^=, "ixor")},
    (op, name) -> installMethod(op, PythonObject, (x, y) -> (
	    m := "__" | name | "__";
	    if hasattr(x, m) then x@@m y
	    else Default)))

-- unary operators
scan({
	(symbol +,   toFunction operator@@"pos"),
	(symbol -,   toFunction operator@@"neg"),
	(symbol ??,  x -> if x != pythonNone then x),
	(symbol not, toFunction operator@@"not_"),
	(symbol ~,   toFunction operator@@"invert"),
	(abs,        toFunction operator@@"abs"),
	(iterator,   toFunction builtins@@"iter"),
	(length,     value @@ (toFunction builtins@@"len")),
	(next,       toFunction builtins@@"next"),
	(round,      toFunction builtins@@"round")
	},
    (op, f) -> installMethod(op, PythonObject, f))

PythonObject Thing := (o, x) -> (toFunction o) x

PythonObject_Thing := toFunction operator@@"getitem"

setitem = method()
setitem(PythonObject, Thing, Thing) := (x, i, e) -> (
    x@@"__setitem__"(i, e);
    null)
PythonObject_Thing = setitem

isMember(Thing,        PythonObject) := (x, y) -> false
contains = operator@@"contains"
isMember(PythonObject, PythonObject) := (x, y) -> value contains(y, x)

divmod = pythonValue "divmod"
quotientRemainder(PythonObject, PythonObject) :=
quotientRemainder(PythonObject, Thing)        :=
quotientRemainder(Thing,        PythonObject) := (x, y) -> (
    qr := divmod(x, y);
    (qr_0, qr_1))

importFrom(Core, "swap")
round(PythonObject, PythonObject) :=
round(PythonObject, Number)       :=
round(ZZ,           PythonObject) := (toFunction builtins@@"round") @@ swap

math = import "math"
truncate PythonObject := {} >> o -> toFunction math@@"trunc"
floor PythonObject := toFunction math@@"floor"
ceiling PythonObject := toFunction math@@"ceil"
-- TODO: other stuff from math module? sin, cos, exp, etc?

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
x = toPython 5
y = toPython 2

-- addition
assert Equation(x + y, 7)
assert Equation(x + 2, 7)
assert Equation(5 + y, 7)

-- subtraction
assert Equation(x - y, 3)
assert Equation(x - 2, 3)
assert Equation(5 - y, 3)

-- multiplication
assert Equation(x * y, 10)
assert Equation(x * 2, 10)
assert Equation(5 * y, 10)

-- true division
assert Equation(x / y, 2.5)
assert Equation(x / 2, 2.5)
assert Equation(5 / y, 2.5)

-- floor division
assert Equation(x // y, 2)
assert Equation(x // 2, 2)
assert Equation(5 // y, 2)

-- modulo
assert Equation(x % y, 1)
assert Equation(x % 2, 1)
assert Equation(5 % y, 1)

-- power
assert Equation(x ^ y, 25)
assert Equation(x ^ 2, 25)
assert Equation(5 ^ y, 25)

-- left shift
assert Equation(x << y, 20)
assert Equation(x << 2, 20)
assert Equation(5 << y, 20)

-- right shift
assert Equation(x >> y, 1)
assert Equation(x >> 2, 1)
assert Equation(5 >> y, 1)

-- bitwise and
assert Equation(x & y, 0)
assert Equation(x & 2, 0)
assert Equation(5 & y, 0)

-- bitwise or
assert Equation(x | y, 7)
assert Equation(x | 2, 7)
assert Equation(5 | y, 7)

-- bitwise xor
assert Equation(x ^^ y, 7)
assert Equation(x ^^ 2, 7)
assert Equation(5 ^^ y, 7)

-- logical and
assert Equation(x and y, 2)
assert Equation(y and x, 5)
assert Equation(0 and x, 0)

-- logical or
assert Equation(x or y, 5)
assert Equation(y or x, 2)
assert Equation(0 or y, 2)

-- logical xor
assert Equation(x xor y, false)
assert Equation(x xor 0, 5)
assert Equation(0 xor y, 2)

----------------------
-- unary operations --
----------------------
assert Equation(-x, -5)
assert Equation(+x, 5)
assert Equation(x~, -6)
assert Equation(not x, false)
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
assert isMember(value rand@@choice L, {1, 2, 3})
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

-- __contains__
assert isMember(toPython 3, toPython {1, 2, 3})
assert not isMember(toPython 4, toPython {1, 2, 3})
assert not isMember(3, toPython {1, 2, 3})

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

TEST ///
-- describe
assert instance(describe toPython 5, Describe)
checkDescribe = x -> assert BinaryOperation(symbol ===,
    value value describe toPython x, x)
checkDescribe true
checkDescribe 5
checkDescribe 3.14159
checkDescribe (1 + 2*ii)
checkDescribe "foo"
checkDescribe (1, 3, 5, 7, 9)
checkDescribe {1, 3, 5, 7, 9}
checkDescribe set {1, 3, 5, 7, 9}
checkDescribe hashTable {"a" => 1, "b" => 2, "c" => 3}
checkDescribe null

-- toExternalString
assert instance(toExternalString toPython 5, String)
checkToExternalString = x -> assert BinaryOperation(symbol ===,
    value value toExternalString toPython x, x)
checkToExternalString true
checkToExternalString 5
checkToExternalString 3.14159
checkToExternalString (1 + 2*ii)
checkToExternalString "foo"
checkToExternalString (1, 3, 5, 7, 9)
checkToExternalString {1, 3, 5, 7, 9}
checkToExternalString set {1, 3, 5, 7, 9}
checkToExternalString hashTable {"a" => 1, "b" => 2, "c" => 3}
checkToExternalString null
///

TEST ///
-- augmented assignment
-- if x is a list, then x += y should modify x directly, i.e., its
-- hash shouldn't change, unlike x = x + y, which would create a new list
x = toPython {1, 2, 3}
oldhash = hash x
x += {4}
assert Equation(hash x, oldhash)
///

TEST ///
-- null coalescing operator
x = toPython null
y = toPython 2
assert Equation(x ?? y, y)
assert Equation(y ?? x, y)
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
runSimpleString "print(x)"
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
sage "R.<x,y,z> = QQ[];"
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
