-* 
this does not work unless M2 is compiled --with-python
*-

newPackage("Python",
    Version => "1.0",
    Date => "November 8, 2025",
    Headline => "interface to Python",
    Authors => {
	{Name => "Daniel R. Grayson",
	    Email => "danielrichardgrayson@gmail.com",
	    HomePage => "https://faculty.math.illinois.edu/~dan/"},
	{Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    Configuration => {"executable" => null},
    Keywords => {"Interfaces"},
    PackageImports => {"Text"},
    AuxiliaryFiles => true,
    OptionalComponentsPresent => Core#"private dictionary"#?"pythonTrue"
    )

---------------
-- ChangeLog --
---------------

-*

1.0 (2025-11-08, M2 1.25.11)
* New PythonContext class replacing undocumented Context class
* getattr, hasattr, and settattr removed (use @@ and @@? now)
* getitem and setitem removed (use _ now)
* NumPy support added
* New "pythonRunScript" for running multiline Python code
* Add support for working with virtual environments and installing modules
  using pip.
* Significant documentation improvements, including two tutorials
* Stop shipping cached examples since Python is supported by default now.

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

if not (options currentPackage).OptionalComponentsPresent
then (
    document {Key => "Python",
	"Macaulay2 was built without Python support, so the Python package is ",
	"not functional."};
    printerr "warning; M2 was not compiled with Python support; ending";
    end)

exportFrom_Core {
    "PythonObject",
    "objectType",
    "runSimpleString",
    }

importFrom_Core {
    "pythonComplexFromDoubles",
    "pythonDictNew",
    "pythonDictSetItem",
    "pythonEvalGetBuiltins",
    "pythonFalse",
    "pythonFloatAsDouble",
    "pythonFloatFromDouble",
    "pythonImportImportModule",
    "pythonInitialize",
    "pythonListNew",
    "pythonListSetItem",
    "pythonLongAsLong",
    "pythonLongFromLong",
    "pythonNone",
    "pythonObjectCall",
    "pythonObjectGetAttrString",
    "pythonObjectHasAttrString",
    "pythonObjectIsTrue",
    "pythonObjectSetAttrString",
    "pythonObjectStr",
    "pythonRunStringEval",
    "pythonRunStringFile",
    "pythonSetNew",
    "pythonTrue",
    "pythonTupleNew",
    "pythonUnicodeAsUTF8",
    "pythonUnicodeFromString",
    "pythonWrapM2Function",
    "toExternalFormat",
}

export {
    -- class
    "PythonContext",

    -- methods
    "addPyToM2Function",
    "import",
    "installNumPyMethods",
    "pipInstall",
    "pythonHelp",
    "pythonRunScript",
    "pythonValue",
    "setupVirtualEnvironment",
    "toFunction",
    "toPython",
}

executable = ((options currentPackage).Configuration#"executable" ??
    get "!command -v python3 | tr -d '\n'")

-- raise an error if we try initializing with a new Python executable
-- store the information in ZZ since it will persist when reloading
if ZZ#?"Python executable" and ZZ#"Python executable" != executable
then error "can't reload package with a new python executable; restart first"
pythonInitialize(ZZ#"Python executable" = executable)

pythonHelp = Command (() -> builtins@@help())

expression PythonObject := expression @@ pythonUnicodeAsUTF8 @@ pythonObjectStr
toString PythonObject := toString @@ expression
net PythonObject := net @@ expression
texMath PythonObject := texMath @@ expression

describe PythonObject := x -> Describe FunctionApplication(pythonValue,
    expression x@@"__repr__"())
toExternalString PythonObject := toExternalFormat @@ describe

typename = x -> (
    T := objectType x;
    m := toString T@@"__module__";
    (if m == "builtins" then "" else (m | ".")) | toString T@@"__qualname__")

PythonObject.synonym = "python object"
PythonObject.AfterPrint = x -> (PythonObject, " of class ", typename x)

pythonValue = method(
    Dispatch => Thing,
    Options => {Global => null})
pythonValue String := o -> s -> (
    if debugLevel > 0 then printerr("python command: ", s);
    pythonRunStringEval(s, toPython(o.Global ?? pythonDictNew())))
pythonValue Sequence := o -> s -> pythonValue(concatenate \\ toString \ s, o)

pythonRunScript = method(
    Dispatch => Thing,
    Options => {Global => null})
pythonRunScript String := o -> s -> (
    if debugLevel > 0 then printerr("python command: ", s);
    globals := toPython o.Global ?? pythonDictNew();
    globals_"__builtins__" ??= pythonEvalGetBuiltins();
    r := pythonRunStringFile(s, globals);
    if isMember("__builtins__", r) then delete("__builtins__", r);
    r)
pythonRunScript Sequence := o -> s -> pythonRunScript(
    concatenate \\ toString \ s, o)

PythonObject @@  Thing := (x, y) -> pythonObjectGetAttrString(x, toString y)
PythonObject @@? Thing := (x, y) -> pythonObjectHasAttrString(x, toString y)
PythonObject @@  Thing  = (x, y, e) -> (
    pythonObjectSetAttrString(x, toString y, toPython e))

------------
-- import --
------------

import = method()
import String := pythonImportImportModule

-- import modules we'll use
ast      = import "ast"
builtins = import "builtins"
abc      = import "collections.abc"
math     = import "math"
numbers  = import "numbers"
operator = import "operator"
sys      = import "sys"

-------------------------------------
-- Python -> M2 conversion methods --
-------------------------------------

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

isinstance = pythonObjectIsTrue @@ (toFunction builtins@@"isinstance")
checktype = method()
checktype(PythonObject, String)       := (x, type) -> typename x == type
checktype(PythonObject, PythonObject) := isinstance

-- remove hooks when reloading package
importFrom(Core, "Hooks")
remove(PythonObject, Hooks)

addPyToM2Function = method()
addPyToM2Function(String,       Function, String) :=
addPyToM2Function(PythonObject, Function, String) := (type, f, desc) ->
    addPyToM2Function({type}, f, desc)
addPyToM2Function(List, Function, String) := (types, f, desc) -> (
    addHook((value, PythonObject),
	x -> if any(types, type -> checktype(x, type)) then f x,
	Strategy => desc))

addHook((value, PythonObject),
    x -> ??x,
    Strategy => "unknown -> PythonObject")

addPyToM2Function(
    abc@@"Callable",
    toFunction,
    "collections.abc.Callable -> FunctionClosure")
dictToHashTable = x -> hashTable for key in x list value key => value x_key
addPyToM2Function(
    abc@@"Mapping",
    dictToHashTable,
    "collections.abc.Mapping -> HashTable")
pyListToM2List = x -> for y in x list value y
addPyToM2Function(
    abc@@"Set",
    set @@ pyListToM2List,
    "collections.abc.Set -> Set")
addPyToM2Function(
    abc@@"Sequence",
    toSequence @@ pyListToM2List,
    "collections.abc.Sequence -> Sequence")
addPyToM2Function(
    abc@@"MutableSequence",
    pyListToM2List,
    "collections.abc.MutableSequence -> List")
addPyToM2Function(
    "str",
    toString,
    "str -> String")
addPyToM2Function(
    numbers@@"Complex",
    x -> toCC(pythonFloatAsDouble x@@"real", pythonFloatAsDouble x@@"imag"),
    "numbers.Complex -> CC")
addPyToM2Function(
    numbers@@"Real",
    pythonFloatAsDouble,
    "numbers.Real -> RR")
addPyToM2Function(
    numbers@@"Integral",
    pythonLongAsLong,
    "numbers.Integral -> ZZ")
addPyToM2Function(
    {builtins@@"bool", "numpy.bool_"},
    pythonObjectIsTrue,
    "bool -> Boolean")
value PythonObject := x -> runHooks((value, PythonObject), x)

-- binary operators
truthy = x -> pythonObjectIsTrue toPython x
importFrom(Core, "swap")
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
	(symbol ==, pythonObjectIsTrue @@ (toFunction operator@@"eq")),
	(symbol ?,  (x, y) -> (
		if pythonObjectIsTrue operator@@"lt"(x, y) then symbol <
		else if pythonObjectIsTrue operator@@"gt"(x, y) then symbol >
		else if pythonObjectIsTrue operator@@"eq"(x, y) then symbol ==
		else incomparable)),
	(isMember,  pythonObjectIsTrue @@ (toFunction operator@@"contains") @@ swap),
	(quotientRemainder, (x, y) -> (
		qr := builtins@@"divmod"(x, y);
		(qr_0, qr_1))),
	(round, (x, y) -> builtins@@"round"(y, x)),
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

delete(Thing, PythonObject) := (i, x) -> (
    (operator@@"delitem")(x, i);)

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
	(symbol **=, "ipow"),
	(symbol <<=, "ilshift"),
	(symbol >>=, "irshift"),
	(symbol &=, "iand"),
	(symbol |=, "ior"),
	(symbol ^^=, "ixor")},
    (op, name) -> installMethod(op, PythonObject, (x, y) -> (
	    m := "__" | name | "__";
	    if x@@?m then x@@m y
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
	(length,     pythonLongAsLong @@ (toFunction builtins@@"len")),
	(next,       toFunction builtins@@"next"),
	(round,      toFunction builtins@@"round")
	},
    (op, f) -> installMethod(op, PythonObject, f))

PythonObject Thing := (o, x) -> (toFunction o) x

PythonObject_Thing := toFunction operator@@"getitem"
PythonObject_Thing = (x, i, e) -> (
    operator@@"setitem"(x, i, e);
    e)

-----------------
-- math module --
-----------------

-- unary methods
scan({
	acos,
	acosh,
	asin,
	asinh,
	atan,
	atanh,
	cos,
	cosh,
	erf,
	erfc,
	exp,
	expm1,
	floor,
	log,
	log1p,
	sin,
	sinh,
	sqrt,
	tan,
	tanh
	},
    f -> installMethod(f, PythonObject, toFunction math@@f))

scan({
	(ceiling,  "ceil"),
	(symbol !, "factorial"),
	(Gamma,    "gamma"),
	(lngamma,  "lgamma")
	},
    (m2f, pyf) -> installMethod(m2f, PythonObject, toFunction math@@pyf))

isFinite   PythonObject := pythonObjectIsTrue @@ (toFunction math@@"isfinite")
isInfinite PythonObject := pythonObjectIsTrue @@ (toFunction math@@"isinf")
truncate   PythonObject := {} >> o -> toFunction math@@"trunc"

-- binary methods
scan({
	atan2,
	gcd
	},
    f -> (
	g := toFunction math@@f;
	installMethod(f, PythonObject, PythonObject, g);
	installMethod(f, PythonObject, Thing,        g);
	installMethod(f, Thing,        PythonObject, g)))

log(PythonObject, PythonObject) :=
log(PythonObject, Thing)        :=
log(Thing,        PythonObject) := (toFunction math@@log) @@ swap

-- lcm & 1-arg gcd not added until Python 3.9
if math@@?lcm then (
    gcd PythonObject := toFunction math@@gcd;
    lcm PythonObject :=
    lcm(PythonObject, PythonObject) :=
    lcm(PythonObject, Thing)        :=
    lcm(Thing,        PythonObject) := toFunction math@@lcm
) else (
    gcd PythonObject :=
    lcm PythonObject := x -> (
	if isinstance(x, numbers@@"Integral") then x
	else error "expected an integer");
    lcm(PythonObject, PythonObject) :=
    lcm(PythonObject, Thing)        :=
    lcm(Thing,        PythonObject) := (x, y) -> abs(x * y // gcd(x, y)))

-- comb not added until Python 3.8
if math@@?"comb" then (
    binomial(PythonObject, PythonObject) :=
    binomial(PythonObject, Thing)        :=
    binomial(Thing,        PythonObject) := toFunction math@@"comb"
) else (
    binomial(PythonObject, PythonObject) :=
    binomial(PythonObject, Thing)        :=
    binomial(Thing,        PythonObject) := (x,y) -> (
	if x < y then pythonLongFromLong 0
	else (
	    r := d := pythonLongFromLong 1;
	    while d <= y do (
		r *= x;
		r //= d;
		x -= 1;
		d += 1);
	    r)))

-- remainder not added until Python 3.7
if math@@?remainder then (
    remainder(PythonObject, PythonObject) :=
    remainder(PythonObject, Thing)        :=
    remainder(Thing,        PythonObject) := toFunction math@@remainder
) else (
    remainder(PythonObject, PythonObject) :=
    remainder(PythonObject, Thing)        :=
    remainder(Thing,        PythonObject) := (x, y) -> x - y * round(x/y))

help#0 PythonObject := x -> toString x@@"__doc__"

-------------------
-- PythonContext --
-------------------

-- based on Dan's original Context class

PythonContext = new SelfInitializingType of MutableHashTable
PythonContext.synonym = "Python context"
globalAssignment PythonContext

eval = toFunction builtins@@"eval"
compile = toFunction builtins@@"compile"

stmtexpr = (s, dict) -> (
    try ast@@"parse"(s, "mode" => "eval")
    then  eval(compile(s, "<string>", "eval"), dict)    -- expression
    else (eval(compile(s, "<string>", "exec"), dict);)) -- statement

new PythonContext := T -> T {symbol Dictionary => pythonDictNew()}
new PythonContext from String := (T, s) -> (
    dict := pythonDictNew();
    stmtexpr(s, dict);
    T {symbol Dictionary => dict})

PythonContext String := (ctx, s) -> stmtexpr(s, ctx.Dictionary)
PythonContext_String := (ctx, key) -> ctx.Dictionary_key

importFrom(Core, "Abbreviate")
listSymbols PythonObject := x -> (
    if not isinstance(x, builtins@@"dict")
    then error "expected a dictionary"
    else TABLE prepend(
	apply({"symbol", "class", "value"}, s -> TH {s}),
	apply(toList x,
	    symb -> (
		val := x_symb;
		apply({
			symb,
			typename val,
			Abbreviate {val}}, s-> TD {s})))))
listSymbols PythonContext := ctx -> listSymbols ctx.Dictionary

use PythonContext := ctx -> (
    scan(ctx.Dictionary, key -> (
	    if not match("_", toString key)
	    then getSymbol toString key <- ctx.Dictionary_key)))

-------------------------------------
-- M2 -> Python conversion methods --
-------------------------------------

toPython = method(Dispatch => Thing)
toPython RR := pythonFloatFromDouble
toPython RRi := pythonFloatFromDouble @@ midpoint
toPython CC := x -> pythonComplexFromDoubles(realPart x, imaginaryPart x)
toPython ZZ := pythonLongFromLong
toPython Number := toPython @@ numeric
toPython Boolean := x -> if x then pythonTrue else pythonFalse
toPython String := pythonUnicodeFromString
toPython Symbol := toPython @@ toString
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

--------------------------
-- virtual environments --
--------------------------

setupVirtualEnvironment = method()
setupVirtualEnvironment String := dir -> (
    if fileExists dir then error(dir, " already exists");
    venv := try import "venv" else error("venv module not found");
    builder := venv@@"EnvBuilder"("with_pip" => true);
    builder@@"create" realpath dir;)

pipInstall = method()
pipInstall String := pkg -> (
    py := toString sys@@"executable";
    if run(py | " -m pip install " | pkg) != 0 then error "pip install failed")

-------------------
-- NumPy methods --
-------------------

-- we can't guarantee that numpy is available at startup, so we install dummy
-- methods initially, and replace them once the user calls installNumPyMethods
toPython Matrix        :=
toPython Vector        :=
toPython MutableMatrix := x -> error "call 'installNumPyMethods()' first"

np = null
installNumPyMethods = () -> (
    if np === null then (
	np = import "numpy";
	toPython Matrix        :=
	toPython Vector        :=
	toPython MutableMatrix := (toFunction np@@"array") @@ entries;
	addPyToM2Function(
	    np@@"ndarray",
	    x -> (
		if x@@"ndim" == 0
		then value x_()
		else if x@@"ndim" == 1
		then vector(value \ toList x)
		else if x@@"ndim" == 2
		then matrix apply(toList x, row -> value \ toList row)),
	    "numpy.ndarray -> Matrix/Vector");
	PythonObject#"numpy methods installed" = true);
    np)

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

builtins = import "builtins"
assert BinaryOperation(symbol ===,
    value builtins@@frozenset {1, 3, 5, 7, 9}, set {1, 3, 5, 7, 9})

checkInPython = x -> (y := pythonValue x; assert Equation(toPython value y, y))
checkInPython "True"
checkInPython "5"
checkInPython "3.14159"
checkInPython "'foo'"
checkInPython "(1, 3, 5, 7, 9)"
checkInPython "[1, 3, 5, 7, 9]"
checkInPython "{1, 3, 5, 7, 9}"
checkInPython "{'a': 1, 'b': 2, 'c': 3}"
checkInPython "None"
assert Equation(builtins@@complex(1, 2), toPython(1 + 2*ii))
assert Equation((value builtins@@abs)(-1), pythonValue "1")
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
foo = toPython "foo"
bar = toPython "bar"

-- concatenation
assert Equation(foo + bar, "foobar")
assert Equation(foo + "bar", "foobar")
assert Equation("foo" + bar, "foobar")

-- repetition
assert Equation(foo * toPython 2, "foofoo")
assert Equation(foo * 2, "foofoo")
assert Equation("foo" * toPython 2, "foofoo")
assert Equation(toPython 2 * foo, "foofoo")
assert Equation(2 * foo, "foofoo")
assert Equation(toPython 2 * "foo", "foofoo")

-- check a few methods
assert Equation(foo@@capitalize(), "Foo")
assert Equation(foo@@center(5, "x"), "xfoox")
assert Equation(
    (toPython "{0}, {1}!")@@format("Hello", "world"),
    "Hello, world!")
assert Equation(foo@@replace("f", "F"), "Foo")
assert Equation(foo@@upper(), "FOO")
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
assert x@@?"__abs__"
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
assert isMember(3, toPython {1, 2, 3})

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

-- del
x = toPython hashTable {"foo" => "bar"}
delete("foo", x)
assert Equation(x, hashTable {})

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

TEST ///
-- pythonValue & pythonRunScript
r = pythonRunScript "x = 5"
x = pythonValue("x + 2", Global => r)
r = pythonRunScript("x += 5", Global => hashTable{"x" => x})
x = pythonValue("x + 7", Global => r)
assert Equation(x, 19)
///

TEST ///
-----------
-- NumPy --
-----------
try np = installNumPyMethods() else end -- skip if numpy not present

-- @ (matmul operator)
v = toPython vector {1, 2, 3}
w = toPython vector {4, 5, 6}
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

TEST ///
-- Python < 3.9 compatibility
assert Equation(gcd toPython 200, 200)
assert Equation(lcm(toPython 200, toPython 300), 600)
///

end --------------------------------------------------------
