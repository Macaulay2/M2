-- Julia package for Macaulay2
-- Copyright (C) 2026 Doug Torrance

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, see <https://www.gnu.org/licenses/>.

newPackage("Julia",
    Version => "0.1",
    Date => "November 6, 2026",
    Headline => "interface to Julia",
    Authors => {{
            Name => "Doug Torrance",
            Email => "dtorrance9@gatech.edu",
            HomePage => "https://d-torrance.github.io"}},
    PackageImports => {"ForeignFunctions"},
    Keywords => {"Interfaces"},
    OptionalComponentsPresent => run("command -v julia > /dev/null") == 0,
    AuxiliaryFiles => true)

---------------
-- ChangeLog --
---------------

-*

0.1 (2026-11-06, M2 1.26.11)
* initial release

*-

endpkg = msg -> (
    document {Key => Julia,
	Headline => (options currentPackage).Headline,
	"Warning: Julia was loaded without key components."};
    printerr("warning: ", msg, "; ending");
    end)

if not (options currentPackage).OptionalComponentsPresent
then endpkg "julia cannot be found"

if not ForeignFunctions#"private dictionary"#?"foreignFunction"
then endpkg "foreign function interface is not available"

export {
    -- classes
    "JuliaContext",
    "JuliaFunction",
    "JuliaObject",

    -- methods
    "addJuliaToM2Function",
    "juliaGetGlobal",
    "juliaSymbol",
    "juliaValue",
    "using",
}

--------------------------------
-- foreign function interface --
--------------------------------

libfile = get "!julia -e 'using Libdl; print(Libdl.dlpath(\"libjulia\"))' < /dev/null 2> /dev/null"
libjulia = openSharedLibrary("libjulia", FileName => libfile)

-- functions
jlAtexitHook = foreignFunction(libjulia, "jl_atexit_hook", void, int)
jlBoxBool = foreignFunction(libjulia, "jl_box_bool", voidstar, int8)
jlBoxFloat64 = foreignFunction(libjulia, "jl_box_float64", voidstar, double)
jlBoxInt64 = foreignFunction(libjulia, "jl_box_int64", voidstar, int64)
jlBoxUint64 = foreignFunction(libjulia, "jl_box_uint64", voidstar, uint64)
jlBoxVoidpointer = foreignFunction(libjulia, "jl_box_voidpointer", voidstar, voidstar)
jlCall = foreignFunction(libjulia, "jl_call", voidstar, {voidstar, voidstarstar, uint32})
jlCall0 = foreignFunction(libjulia, "jl_call0", voidstar, voidstar)
jlCall1 = foreignFunction(libjulia, "jl_call1", voidstar, {voidstar, voidstar})
jlCall2 = foreignFunction(libjulia, "jl_call2", voidstar, {voidstar, voidstar, voidstar})
jlCall3 = foreignFunction(libjulia, "jl_call3", voidstar, {voidstar, voidstar, voidstar, voidstar})
jlCall4 = foreignFunction(libjulia, "jl_call4", voidstar, {voidstar, voidstar, voidstar, voidstar, voidstar})
jlCstrToString = foreignFunction(libjulia, "jl_cstr_to_string", voidstar, charstar)
jlEvalString = foreignFunction(libjulia, "jl_eval_string", voidstar, charstar)
jlExceptionClear = foreignFunction(libjulia, "jl_exception_clear", void, void)
jlExceptionOccurred = foreignFunction(libjulia, "jl_exception_occurred", voidstar, void)
jlGetGlobal = foreignFunction(libjulia, "jl_get_global", voidstar, {voidstar, voidstar})
jlInit = foreignFunction(libjulia, "jl_init", void, void)
jlIsa = foreignFunction(libjulia, "jl_isa", int, {voidstar, voidstar})
jlLoad = foreignFunction(libjulia, "jl_load", voidstar, {voidstar, charstar})
jlStringPtr = foreignFunction(libjulia, "jl_string_ptr", charstar, voidstar)
jlSymbol = foreignFunction(libjulia, "jl_symbol", voidstar, charstar)
jlUnboxBool = foreignFunction(libjulia, "jl_unbox_bool", int8, voidstar)
jlUnboxFloat32 = foreignFunction(libjulia, "jl_unbox_float32", float, voidstar)
jlUnboxFloat64 = foreignFunction(libjulia, "jl_unbox_float64", double, voidstar)
jlUnboxInt16 = foreignFunction(libjulia, "jl_unbox_int16", int16, voidstar)
jlUnboxInt32 = foreignFunction(libjulia, "jl_unbox_int32", int32, voidstar)
jlUnboxInt64 = foreignFunction(libjulia, "jl_unbox_int64", int64, voidstar)
jlUnboxInt8 = foreignFunction(libjulia, "jl_unbox_int8", int8, voidstar)
jlUnboxUint16 = foreignFunction(libjulia, "jl_unbox_uint16", uint16, voidstar)
jlUnboxUint32 = foreignFunction(libjulia, "jl_unbox_uint32", uint32, voidstar)
jlUnboxUint64 = foreignFunction(libjulia, "jl_unbox_uint64", uint64, voidstar)
jlUnboxUint8 = foreignFunction(libjulia, "jl_unbox_uint8", uint8, voidstar)
jlUnboxVoidpointer = foreignFunction(libjulia, "jl_unbox_voidpointer", voidstar, voidstar)

-- symbols
jlModuleType = foreignSymbol(libjulia, "jl_module_type", voidstar)
jlNothing = foreignSymbol(libjulia, "jl_nothing", voidstar)

--------------------
-- initialization --
--------------------

jlInit()
addEndFunction(() -> jlAtexitHook 0)

-----------------
-- JuliaObject --
-----------------

JuliaObject = new SelfInitializingType of voidstar
JuliaObject.synonym = "Julia object"

toString JuliaObject := x -> value jlString x
net JuliaObject := x -> net value jlRepr("text/plain", x)
describe JuliaObject := x -> Describe FunctionApplication(juliaValue, value jlRepr x)
importFrom(Core, "toExternalFormat")
toExternalString JuliaObject := toExternalFormat @@ describe
JuliaObject.AfterPrint = x -> (JuliaObject, " of type ", jlTypeof x)

-- modules
jlBaseModule = JuliaObject foreignSymbol(libjulia, "jl_base_module", voidstar)
jlCoreModule = JuliaObject foreignSymbol(libjulia, "jl_core_module", voidstar)
jlMainModule = JuliaObject foreignSymbol(libjulia, "jl_main_module", voidstar)
jlMetaModule = JuliaObject jlGetGlobal(jlMainModule, jlSymbol "Meta")
jlM2JuliaModule = JuliaObject jlLoad(jlMainModule, currentFileDirectory | "Julia/M2Julia.jl")

-- symbols not exported by C API
jlDeleteGlobal = jlGetGlobal(jlMainModule, jlSymbol "delete!")
jlGetGlobalGlobal = jlGetGlobal(jlMainModule, jlSymbol "getglobal")
jlKwcall = jlGetGlobal(jlCoreModule, jlSymbol "kwcall")
jlSetindexGlobal = jlGetGlobal(jlMainModule, jlSymbol "setindex!")
jlShowerror = jlGetGlobal(jlMainModule, jlSymbol "showerror")

-- keep a dict of known julia objects so they don't get garbage
-- collected out from under us
knownObjects = jlGetGlobal(jlM2JuliaModule, jlSymbol "known_objects")
knownObjectCount = 0
finalizer = key -> x -> jlCall2(jlDeleteGlobal, knownObjects, jlBoxInt64 key)
new JuliaObject from voidstar := (T, x) -> (
    jlCall3(jlSetindexGlobal, knownObjects, x, jlBoxInt64 knownObjectCount);
    registerFinalizer(x, finalizer knownObjectCount);
    knownObjectCount += 1;
    x)
new JuliaObject from JuliaObject := (T, x) -> x

--------------------
-- error handling --
--------------------

JuliaError = new SelfInitializingType of Error

new JuliaError := T -> (
    exc := jlExceptionOccurred();
    if value exc === nullPointer
    then error "no Julia error occurred"
    else (
        jlExceptionClear();
        T value jlSprint(jlShowerror, exc)))

JuliaObjectOrError = ptr -> (
    if value ptr === nullPointer
    then error new JuliaError
    else JuliaObject ptr)

-----------------
-- juliaSymbol --
-----------------

juliaSymbol = method()
juliaSymbol String := s -> JuliaObject jlSymbol s
juliaSymbol Thing := juliaSymbol @@ toString

--------------------
-- juliaGetGlobal --
--------------------

juliaGetGlobal = method()
juliaGetGlobal(JuliaObject, String) := (M, s) -> JuliaObjectOrError(
    jlCall2(jlGetGlobalGlobal, M, jlSymbol s))
juliaGetGlobal(JuliaObject, Thing) := (M, s) -> juliaGetGlobal(M, toString s)
juliaGetGlobal String := s -> juliaGetGlobal(jlMainModule, s)
juliaGetGlobal Thing := juliaGetGlobal @@ toString

-------------------
-- JuliaFunction --
-------------------

JuliaFunction = new SelfInitializingType of FunctionClosure
JuliaFunction.synonym = "Julia function"

net      JuliaFunction :=
toString JuliaFunction := f -> toString (frames f)#0#1

optionsToNamedtuple = jlGetGlobal(jlM2JuliaModule, jlSymbol "options_to_namedtuple")

juliaCallHelper = (f, x) -> JuliaObjectOrError(
    x = apply(x, y -> JuliaObject y);
    if #x == 0 then jlCall0 f
    else if #x == 1 then jlCall1(f, x#0)
    else if #x == 2 then jlCall2(f, x#0, x#1)
    else if #x == 3 then jlCall3(f, x#0, x#1, x#2)
    else if #x == 4 then jlCall4(f, x#0, x#1, x#2, x#3)
    else jlCall(f, toList x, #x))

juliaCall = method()
juliaCall(JuliaObject, Thing) := (f, x) -> (
    JuliaObjectOrError jlCall1(f, JuliaObject x))
juliaCall(JuliaObject, Sequence) := (f, x) -> (
    p := partition(y -> instance(y, Option), x);
    if p#?true then (
        names := apply(p#true, y -> toString first y);
        vals := apply(p#true, y -> last y);
        kwargs := juliaCallHelper(optionsToNamedtuple, (names, vals));
        juliaCallHelper(jlKwcall, join((kwargs, f), p#false ?? ())))
    else juliaCallHelper(f, x))
juliaCall(JuliaObject, Option) := (f, x) -> juliaCall(f, 1:x)

new JuliaFunction from JuliaObject := (T, f) -> x -> juliaCall(f, x)
new JuliaFunction from String := (T, s) -> T juliaGetGlobal s
new JuliaFunction from Function :=
new JuliaFunction from Symbol   := (T, s) -> T toString s
new JuliaFunction from (JuliaObject, String) := (T, M, s) -> T juliaGetGlobal(M, s)
new JuliaFunction from (JuliaObject, Function) :=
new JuliaFunction from (JuliaObject, Symbol)   := (T, M, s) -> T(M, toString s)
JuliaObject Thing := (f, x) -> juliaCall(f, x)

-- functions we'll use
jlDelete = JuliaFunction "delete!"
jlDict = JuliaFunction "Dict"
jlEachrow = JuliaFunction "eachrow"
jlEltype = JuliaFunction "eltype"
jlEval = JuliaFunction(jlBaseModule, "eval")
jlFloat32 = JuliaFunction "Float32"
jlGetindex = JuliaFunction "getindex"
jlGetproperty = JuliaFunction "getproperty"
jlHasproperty = JuliaFunction "hasproperty"
jlHvcat = JuliaFunction "hvcat"
jlInt = JuliaFunction "Int"
jlIsdefined = JuliaFunction "isdefined"
jlIterate = JuliaFunction "iterate"
jlKeys = JuliaFunction "keys"
jlPair = JuliaFunction "Pair"
jlMetaParseall = JuliaFunction(jlMetaModule, "parseall")
jlModule = JuliaFunction "Module"
jlNames = JuliaFunction "names"
jlRationalDivision = JuliaFunction "//"
jlRepr = JuliaFunction "repr"
jlSet = JuliaFunction "Set"
jlSetindex = JuliaFunction "setindex!"
jlSetproperty = JuliaFunction "setproperty!"
jlSize = JuliaFunction "size"
jlSprint = JuliaFunction "sprint"
jlString = JuliaFunction "string"
jlTrunc = JuliaFunction "trunc"
jlTuple = JuliaFunction "tuple"
jlTypeof = JuliaFunction "typeof"
jlVect = JuliaFunction(jlBaseModule, "vect")
jlZeros = JuliaFunction "zeros"

-- globals we'll use
jlIm = juliaGetGlobal "im"

-----------------
-- M2 -> julia --
-----------------

new JuliaObject from Boolean := (T, x) -> jlBoxBool if x then 1 else 0
zzToInt128 = JuliaFunction(jlM2JuliaModule, "zz_to_int128")
zzToBigInt = JuliaFunction(jlM2JuliaModule, "zz_to_bigint")
rrToBigFloat = JuliaFunction(jlM2JuliaModule, "rr_to_bigfloat")
new JuliaObject from ZZ := (T, x) -> (
    if x < -2^127 or x > 2^127 - 1
    then zzToBigInt jlBoxVoidpointer mpzT x
    else if x < -2^63 or x > 2^63 - 1
    then zzToInt128(jlBoxInt64(x // 2^64), jlBoxUint64(x % 2^64))
    else T jlBoxInt64 x)
new JuliaObject from QQ := (T, x) -> jlRationalDivision(numerator x, denominator x)
new JuliaObject from RR := (T, x) -> (
    prec := precision x;
    if prec <= 53 then T jlBoxFloat64 x
    else rrToBigFloat(jlBoxVoidpointer mpfrT x, prec))
new JuliaObject from CC := (T, x) -> realPart x + jlIm * imaginaryPart x
new JuliaObject from Number := (T, x) -> T numeric x
new JuliaObject from String := (T, x) -> T jlCstrToString x
new JuliaObject from List := (T, x) -> jlVect toSequence x
new JuliaObject from Sequence := (T, x) -> jlTuple x
new JuliaObject from HashTable := (T, x) -> jlDict(jlPair \ toSequence pairs x)
new JuliaObject from Set := (T, x) -> jlSet toList x
new JuliaObject from Nothing := (T, x) -> jlNothing
new JuliaObject from Matrix := (T, x) -> (
    if numRows x == 0 or numColumns x == 0
    then jlZeros(jlTypeof 0_(ring x), numRows x, numColumns x)
    else jlHvcat prepend(numColumns x, toSequence flatten entries x))

-----------------
-- julia -> M2 --
-----------------

getJlBool = x -> value jlUnboxBool x == 1

setValueKey = JuliaFunction(jlM2JuliaModule, "set_value_key")
juliaToM2Functions = new MutableList
addJuliaToM2Function = method()
addJuliaToM2Function(String, Function) := (typename, f) -> (
    type := JuliaObjectOrError jlEvalString typename;
    key := #juliaToM2Functions;
    setValueKey(type, key);
    juliaToM2Functions#key = f)

addJuliaToM2Function("Bool", getJlBool)
addJuliaToM2Function("Int8", value @@ jlUnboxInt8)
addJuliaToM2Function("Int16", value @@ jlUnboxInt16)
addJuliaToM2Function("Int32", value @@ jlUnboxInt32)
addJuliaToM2Function("Int64", value @@ jlUnboxInt64)
addJuliaToM2Function("UInt8", value @@ jlUnboxUint8)
addJuliaToM2Function("UInt16", value @@ jlUnboxUint16)
addJuliaToM2Function("UInt32", value @@ jlUnboxUint32)
addJuliaToM2Function("UInt64", value @@ jlUnboxUint64)
int128ToZZ = JuliaFunction(jlM2JuliaModule, "int128_to_zz")
addJuliaToM2Function("Int128", x -> (
    (hi, lo) := value int128ToZZ x;
    hi * 2^64 + lo))
uint128ToZZ = JuliaFunction(jlM2JuliaModule, "uint128_to_zz")
addJuliaToM2Function("UInt128", x -> (
    (hi, lo) := value uint128ToZZ x;
    hi * 2^64 + lo))
addJuliaToM2Function("BigInt", x -> value mpzT x)
addJuliaToM2Function("Float16", x -> numeric(11, value jlUnboxFloat32 jlFloat32 x))
addJuliaToM2Function("Float32", value @@ jlUnboxFloat32)
addJuliaToM2Function("Float64", value @@ jlUnboxFloat64)
bigfloatToRR = JuliaFunction(jlM2JuliaModule, "bigfloat_to_rr")
addJuliaToM2Function("BigFloat", x -> value mpfrT jlUnboxVoidpointer bigfloatToRR x)
addJuliaToM2Function("Rational", x -> value numerator x / value denominator x)
addJuliaToM2Function("Complex", x -> value realPart x + ii * value imaginaryPart x)
addJuliaToM2Function("Char", utf8 @@ value @@ jlInt)
addJuliaToM2Function("String", value @@ jlStringPtr)
addJuliaToM2Function("Symbol", toString)
addJuliaToM2Function("AbstractArray", x -> value \ toList x)
addJuliaToM2Function("Tuple", x -> value \ toSequence x)
addJuliaToM2Function("NamedTuple", x -> apply(toString \ toSequence jlKeys x, name -> name => value x@@name))
addJuliaToM2Function("AbstractDict", x -> hashTable apply(toList x, kv -> value \ (kv_1, kv_2)))
addJuliaToM2Function("AbstractSet", x -> set(value \ toList x))
addJuliaToM2Function("Nothing", x -> null)
addJuliaToM2Function("AbstractMatrix", x -> (
    (m, n) := value jlSize x;
    if m == 0 or n == 0 then (
        R := ring value (jlEltype x) 0;
        map(R^m, R^n, 0))
    else matrix value jlEachrow x))

valueKey = juliaGetGlobal(jlM2JuliaModule, "value_key")
value JuliaObject := x -> (
    key := value jlUnboxInt64 jlCall1(valueKey, x);
    if key == -1 then error("no method found for applying 'value' to: ", newline,
                            "\t", x, " (of type ", jlTypeof x, ")")
    else juliaToM2Functions#key x)

---------------
-- iterators --
---------------

JuliaObject_Thing := jlGetindex
JuliaObject_Sequence := jlGetindex @@ prepend
JuliaObject_Thing = (x, i, e) -> jlSetindex(x, e, i)
JuliaObject_Sequence = (x, i, e) -> jlSetindex((x, e) | i)
delete(JuliaObject, Thing) := jlDelete

JuliaObject@@Thing := (x, i) -> jlGetproperty(x, jlSymbol toString i)
JuliaObject@@Thing = (x, i, e) -> jlSetproperty(x, jlSymbol toString i, e)
JuliaObject@@?Thing := (x, i) -> value jlHasproperty(x, jlSymbol toString i)

iterator JuliaObject := x -> Iterator (
    iter := jlIterate x;
    () -> (
        if iter == jlNothing then StopIteration
        else first(
            iter_1,
            iter = jlIterate(x, iter_2))))

-------------------
-- unary methods --
-------------------

scan({
    symbol +,
    symbol -,
    symbol ~,
    abs,
    round,
    floor,
    sqrt,
    exp,
    expm1,
    log,
    log1p,
    sin,
    cos,
    tan,
    cot,
    sec,
    csc,
    sinh,
    cosh,
    tanh,
    coth,
    sech,
    csch,
    asin,
    acos,
    atan,
    acot,
    asinh,
    acosh,
    atanh,
    acoth,
    numerator,
    denominator
}, op -> (
    f := JuliaFunction op;
    installMethod(op, JuliaObject, f)))

scan({
    (symbol not, symbol !),
    (ceiling, "ceil"),
    (realPart, "real"),
    (imaginaryPart, "imag"),
    (conjugate, "conj")
}, (m2op, jlop) -> (
    f := JuliaFunction jlop;
    installMethod(m2op, JuliaObject, f)))

isFinite JuliaObject := getJlBool @@ (JuliaFunction "isfinite")
isInfinite JuliaObject := getJlBool @@ (JuliaFunction "isinf")
length JuliaObject := value @@ (JuliaFunction length)

--------------------
-- binary methods --
--------------------

scan({
    symbol +,
    symbol -,
    symbol *,
    symbol /,
    symbol \,
    symbol ^,
    symbol %,
    symbol &,
    symbol |,
    symbol >>,
    symbol <<,
    gcd,
    lcm,
    log
},
     op -> (
         f := JuliaFunction op;
         installMethod(op, JuliaObject, JuliaObject, f);
         installMethod(op, JuliaObject, Thing, f);
         installMethod(op, Thing, JuliaObject, f)))

scan({
    (symbol //, "÷"),
    (symbol ^^, "⊻"),
    (atan2, atan)
},
     (m2op, jlop) -> (
         f := JuliaFunction jlop;
         installMethod(m2op, JuliaObject, JuliaObject, f);
         installMethod(m2op, JuliaObject, Thing, f);
         installMethod(m2op, Thing, JuliaObject, f)))

-- can't use JuliaFunction w/ &&/||, so roll our own
-- use Thing on RHS to support (greedy) short-circuiting
JuliaObject and JuliaObject := (x, y) -> value x and value y
JuliaObject and Thing       := (x, y) -> value x and y
Boolean     and JuliaObject := (x, y) -> x and value y

JuliaObject or JuliaObject := (x, y) -> value x or value y
JuliaObject or Thing       := (x, y) -> value x or y
Boolean     or JuliaObject := (x, y) -> x or value y

jleq = JuliaFunction symbol ==
JuliaObject == JuliaObject :=
JuliaObject == Thing       :=
Thing       == JuliaObject := getJlBool @@ jleq

jlle = JuliaFunction symbol <
jlge = JuliaFunction symbol >
JuliaObject ? JuliaObject :=
JuliaObject ? Thing       :=
Thing       ? JuliaObject := (x, y) -> (
    if getJlBool jlle(x, y) then symbol <
    else if getJlBool jlge(x, y) then symbol >
    else if x == y then symbol ==
    else incomparable)

-- these only accept a julia type as the first argument, so
-- (Thing, JuliaObject) doesn't make sense
round(JuliaObject, Thing) := lookup(round, JuliaObject)
floor(JuliaObject, Thing) := lookup(floor, JuliaObject)
ceiling(JuliaObject, Thing) := lookup(ceiling, JuliaObject)

truncate JuliaObject         := {} >> o -> x -> jlTrunc x
truncate(JuliaObject, Thing) := {} >> o -> (T, x) -> jlTrunc(T, x)

quotientRemainder(JuliaObject, JuliaObject) :=
quotientRemainder(JuliaObject, Thing)       :=
quotientRemainder(Thing,       JuliaObject) := toSequence @@ (JuliaFunction "divrem")

gcd JuliaObject := lookup(gcd, JuliaObject, JuliaObject)
lcm JuliaObject := lookup(lcm, JuliaObject, JuliaObject)

----------------
-- evaluation --
----------------

juliaValue = method(
    Dispatch => Thing,
    Options => {Global => null})
juliaValue String := o -> s -> (
    if o.Global === null then JuliaObjectOrError jlEvalString s
    else jlEval(o.Global, jlMetaParseall s))
juliaValue Sequence := o -> s -> juliaValue(concatenate \\ toString \ s, o)

JuliaContext = new SelfInitializingType of MutableHashTable
JuliaContext.synonym = "Julia context"
globalAssignment JuliaContext

new JuliaContext := T -> T {Global => jlModule()}
new JuliaContext from String := (T, s) -> (
    env := jlModule();
    juliaValue(s, Global => env);
    T {Global => env})

JuliaContext String := (ctx, s) -> juliaValue(s, Global => ctx.Global)
JuliaContext_String := (ctx, s) -> ctx.Global@@s

importFrom(Core, {"Abbreviate", "TABLE", "TD", "TH"})
listSymbols JuliaObject := x -> (
    if not value jlIsa(x, jlModuleType) == 1
    then error "expected a module"
    else TABLE prepend(
        apply({"symbol", "class", "value"}, s -> TH {s}),
        apply(select(toList jlNames(x, all => true),
                     name -> getJlBool jlIsdefined(x, name)),
              name -> (
                  val := x@@name;
                  apply({name, jlTypeof val, Abbreviate {val}}, s -> TD {s})))))
listSymbols JuliaContext := ctx -> listSymbols ctx.Global

use JuliaContext := ctx -> (
    scan(toString \ toList jlNames(ctx.Global, all => true),
         name -> try getSymbol name <- ctx.Global@@name))

using = method()
using String := s -> (
    juliaValue("using ", s);
    juliaGetGlobal s)

jlREPLModule = null
help#0 JuliaObject := x -> (
    jlREPLModule ??= (juliaValue "import REPL"; juliaGetGlobal "REPL");
    net jlBaseModule@@"Docs"@@"doc" x)

beginDocumentation()

load "./Julia/doc.m2"
