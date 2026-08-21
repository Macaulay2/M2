newPackage "Julia"

export {
    -- classes
    "JuliaFunction",
    "JuliaObject",

    -- methods
    "addJuliaToM2Function",
    "juliaSymbol",
    "juliaValue",
}

needsPackage "ForeignFunctions"

--------------------------------
-- foreign function interface --
--------------------------------

-- TODO: make this configurable
-- maybe autodetect
libjulia = openSharedLibrary("libjulia", FileName => "/home/profzoom/.julia/juliaup/julia-1.12.7+0.x64.linux.gnu/lib/libjulia.so.1.12.7")

-- functions
jlBoxBool = foreignFunction(libjulia, "jl_box_bool", voidstar, int8)
jlBoxFloat64 = foreignFunction(libjulia, "jl_box_float64", voidstar, double)
jlBoxInt64 = foreignFunction(libjulia, "jl_box_int64", voidstar, int64)
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
jlStringPtr = foreignFunction(libjulia, "jl_string_ptr", charstar, voidstar)
jlSymbol = foreignFunction(libjulia, "jl_symbol", voidstar, charstar)
jlUnboxBool = foreignFunction(libjulia, "jl_unbox_bool", int, voidstar)
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

-- symbols
jlBaseModule = foreignSymbol(libjulia, "jl_base_module", voidstar)
jlNothing = foreignSymbol(libjulia, "jl_nothing", voidstar)
jlTypeType = foreignSymbol(libjulia, "jl_type_type", voidstar)

--------------------
-- initialization --
--------------------

jlInit()

-- symbols not exported by C API (now that we're initialized)
jlDeleteGlobal = jlGetGlobal(jlBaseModule, jlSymbol "delete!")
jlGetGlobalGlobal = jlGetGlobal(jlBaseModule, jlSymbol "getglobal")
jlSetindexGlobal = jlGetGlobal(jlBaseModule, jlSymbol "setindex!")
jlShowerror = jlGetGlobal(jlBaseModule, jlSymbol "showerror")

-----------------
-- JuliaObject --
-----------------

JuliaObject = new SelfInitializingType of voidstar
JuliaObject.synonym = "Julia object"

toString JuliaObject := x -> value jlString x
net JuliaObject := x -> value jlRepr("text/plain", x)
toExternalString JuliaObject := x -> value jlRepr x
JuliaObject.AfterPrint = x -> (JuliaObject, " of type ", jlTypeof x)

-- keep a dict of known julia objects so they don't get garbage
-- collected out from under us
knownObjects = jlEvalString ///
module M2Julia
    const known_objects = Dict{Int64, Any}()
end
M2Julia.known_objects
///
knownObjectCount = 0
finalizer = key -> x -> jlCall2(jlDeleteGlobal, knownObjects, jlBoxInt64 key)
new JuliaObject from voidstar := (T, x) -> (
    jlCall3(jlSetindexGlobal, knownObjects, x, jlBoxInt64 knownObjectCount);
    registerFinalizer(x, finalizer knownObjectCount);
    knownObjectCount += 1;
    x)

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
juliaSymbol String := s -> JuliaObjectOrError jlCall2(jlGetGlobalGlobal, jlBaseModule, jlSymbol s)
juliaSymbol Symbol   :=
juliaSymbol Function := juliaSymbol @@ toString

-------------------
-- JuliaFunction --
-------------------

JuliaFunction = new SelfInitializingType of FunctionClosure
JuliaFunction.synonym = "Julia function"

net      JuliaFunction :=
toString JuliaFunction := f -> (frames f)#0#1

juliaCall = (f, x) -> JuliaObjectOrError(
    if instance(x, Sequence) then (
        x = apply(x, y -> JuliaObject y);
        if #x == 0 then jlCall0 f
        else if #x == 1 then jlCall1(f, x#0)
        else if #x == 2 then jlCall2(f, x#0, x#1)
        else if #x == 3 then jlCall3(f, x#0, x#1, x#2)
        else if #x == 4 then jlCall4(f, x#0, x#1, x#2, x#3)
        else jlCall(f, toList x, #x))
    else jlCall1(f, JuliaObject x))

new JuliaFunction from JuliaObject := (T, f) -> x -> juliaCall(f, x)
new JuliaFunction from String := (T, s) -> T juliaSymbol s
new JuliaFunction from Function :=
new JuliaFunction from Symbol   := (T, s) -> T toString s

-- functions we'll use
jlDelete = JuliaFunction "delete!"
jlDict = JuliaFunction "Dict"
jlGetindex = JuliaFunction "getindex"
jlIterate = JuliaFunction "iterate"
jlPair = JuliaFunction "Pair"
jlRepr = JuliaFunction "repr"
jlSetindex = JuliaFunction "setindex!"
jlSprint = JuliaFunction "sprint"
jlString = JuliaFunction "string"
jlTuple = JuliaFunction "tuple"
jlTypeof = JuliaFunction "typeof"
jlVect = JuliaFunction "vect"

-----------------
-- M2 -> julia --
-----------------

new JuliaObject from Boolean := (T, x) -> jlBoxBool if x then 1 else 0
new JuliaObject from ZZ := (T, x) -> jlBoxInt64 x
new JuliaObject from RR := (T, x) -> jlBoxFloat64 x
new JuliaObject from Number := (T, x) -> T numeric x
new JuliaObject from String := (T, x) -> jlCstrToString x
new JuliaObject from List := (T, x) -> jlVect toSequence x
new JuliaObject from Sequence := (T, x) -> jlTuple x
new JuliaObject from HashTable := (T, x) -> jlDict(jlPair \ toSequence pairs x)
new JuliaObject from Nothing := (T, x) -> jlNothing

-----------------
-- julia -> M2 --
-----------------

getJlBool = x -> value jlUnboxBool x == 1

juliaToM2Functions = new MutableList
addJuliaToM2Function = method()
addJuliaToM2Function(String, Function) := (typename, f) -> (
    type := JuliaObjectOrError jlEvalString typename;
    if value jlIsa(type, jlTypeType) == 0
    then error "expected argument 1 to be a Julia type";
    key := #juliaToM2Functions;
    jlEvalString concatenate("@eval M2Julia value_key(x::", typename, ") = ", toString key);
    juliaToM2Functions#key = f)

jlEvalString "@eval M2Julia value_key(x) = -1"
addJuliaToM2Function("Bool", getJlBool)
addJuliaToM2Function("Int8", value @@ jlUnboxInt8)
addJuliaToM2Function("Int16", value @@ jlUnboxInt16)
addJuliaToM2Function("Int32", value @@ jlUnboxInt32)
addJuliaToM2Function("Int64", value @@ jlUnboxInt64)
addJuliaToM2Function("UInt8", value @@ jlUnboxUint8)
addJuliaToM2Function("UInt16", value @@ jlUnboxUint16)
addJuliaToM2Function("UInt32", value @@ jlUnboxUint32)
addJuliaToM2Function("UInt64", value @@ jlUnboxInt64)
-- TODO: Int128, UInt128, Float16
addJuliaToM2Function("Float32", value @@ jlUnboxFloat32)
addJuliaToM2Function("Float64", value @@ jlUnboxFloat64)
addJuliaToM2Function("String", value @@ jlStringPtr)
addJuliaToM2Function("AbstractArray", x -> value \ toList x)
addJuliaToM2Function("Tuple", x -> value \ toSequence x)
addJuliaToM2Function("AbstractDict", x -> hashTable apply(toList x, kv -> value \ (kv_1, kv_2)))
addJuliaToM2Function("Nothing", x -> null)

m2JuliaValueKey = jlEvalString "M2Julia.value_key"
value JuliaObject := x -> (
    key := value jlUnboxInt64 jlCall1(m2JuliaValueKey, x);
    if key == -1 then error("no method found for applying 'value' to: ", newline,
                            "\t", x, " (of type ", jlTypeof x, ")")
    else juliaToM2Functions#key x)

---------------
-- iterators --
---------------

JuliaObject_Thing := jlGetindex
JuliaObject_Thing = (x, i, e) -> jlSetindex(x, e, i)
delete(JuliaObject, Thing) := jlDelete

iterator JuliaObject := x -> Iterator (
    iter := jlIterate x;
    () -> (
        if iter == jlNothing then StopIteration
        else first(
            iter_1,
            iter = jlIterate(x, iter_2))))

---------------------
-- unary operators --
---------------------

scan({
    symbol +,
    symbol -,
    symbol ~
}, op -> (
    f := JuliaFunction op;
    installMethod(op, JuliaObject, f)))

scan({
    (symbol not, symbol !)
}, (m2op, jlop) -> (
    f := JuliaFunction jlop;
    installMethod(m2op, JuliaObject, f)))

----------------------
-- binary operators --
----------------------

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
    symbol >>, -- TODO: what to do about >>>?
    symbol <<
},
     op -> (
         f := JuliaFunction op;
         installMethod(op, JuliaObject, JuliaObject, f);
         installMethod(op, JuliaObject, Thing, f);
         installMethod(op, Thing, JuliaObject, f)))

scan({
    (symbol //, "÷"),
    (symbol ^^, "⊻")
},
     (m2op, jlop) -> (
         f := JuliaFunction jlop;
         installMethod(m2op, JuliaObject, JuliaObject, f);
         installMethod(m2op, JuliaObject, Thing, f);
         installMethod(m2op, Thing, JuliaObject, f)))

-------------
-- methods --
-------------

JuliaObject Thing := (f, x) -> juliaCall(f, x)

isFinite JuliaObject := getJlBool @@ (JuliaFunction "isfinite")
isInfinite JuliaObject := getJlBool @@ (JuliaFunction "isinf")


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

----------------
-- evaluation --
----------------

juliaValue = method()
juliaValue String := JuliaObjectOrError @@ jlEvalString
juliaValue Sequence := s -> juliaValue(concatenate \\ toString \ s)

beginDocumentation()

TEST ///
-- roundtrip
assertRoundTrip = x -> assert Equation(value JuliaObject x, x)
assertRoundTrip true
assertRoundTrip 5
assertRoundTrip pi
assertRoundTrip "foo"
assertRoundTrip null
assertRoundTrip {1, 2, 3}
assertRoundTrip (1, 2, 3)
x = hashTable {(true, 5), (numeric pi, "foo"), (null, {1, 2, 3})}
assert BinaryOperation(symbol ===, value JuliaObject x, x)
///

TEST ///
-- integer types
assert Equation(value (juliaSymbol "Int8") 5, 5)
assert Equation(value (juliaSymbol "Int16") 5, 5)
assert Equation(value (juliaSymbol "Int32") 5, 5)
assert Equation(value (juliaSymbol "Int64") 5, 5)
assert Equation(value (juliaSymbol "UInt8") 5, 5)
assert Equation(value (juliaSymbol "UInt16") 5, 5)
assert Equation(value (juliaSymbol "UInt32") 5, 5)
assert Equation(value (juliaSymbol "UInt64") 5, 5)
-- floating-point types
assert Equation(value (juliaSymbol "Float32") 5, 5)
assert Equation(value (juliaSymbol "Float64") 5, 5)
///

TEST ///
----------------
-- arithmetic --
----------------
-- unary plus
assert Equation(+JuliaObject 6, JuliaObject 6)

-- unary minus
assert Equation(-JuliaObject 6, JuliaObject(-6))

-- binary plus
assert Equation(JuliaObject 6 + JuliaObject 3, JuliaObject 9)
assert Equation(JuliaObject 6 + 3, JuliaObject 9)
assert Equation(6 + JuliaObject 3, JuliaObject 9)

-- binary minus
assert Equation(JuliaObject 6 - JuliaObject 3, JuliaObject 3)
assert Equation(JuliaObject 6 - 3, JuliaObject 3)
assert Equation(6 - JuliaObject 3, JuliaObject 3)

-- times
assert Equation(JuliaObject 6 * JuliaObject 3, JuliaObject 18)
assert Equation(JuliaObject 6 * 3, JuliaObject 18)
assert Equation(6 * JuliaObject 3, JuliaObject 18)

-- divide
assert Equation(JuliaObject 7 / JuliaObject 2, JuliaObject 3.5)
assert Equation(JuliaObject 7 / 2, JuliaObject 3.5)
assert Equation(7 / JuliaObject 2, JuliaObject 3.5)

-- integer divide
assert Equation(JuliaObject 7 // JuliaObject 3, JuliaObject 2)
assert Equation(JuliaObject 7 // 3, JuliaObject 2)
assert Equation(7 // JuliaObject 3, JuliaObject 2)

-- inverse divide
assert Equation(JuliaObject 2 \ JuliaObject 7, JuliaObject 3.5)
assert Equation(JuliaObject 2 \ 7, JuliaObject 3.5)
assert Equation(2 \ JuliaObject 7, JuliaObject 3.5)

-- power
assert Equation((JuliaObject 6) ^ (JuliaObject 3), JuliaObject 216)
assert Equation((JuliaObject 6) ^ 3, JuliaObject 216)
assert Equation(6 ^ (JuliaObject 3), JuliaObject 216)

-- remainder
assert Equation(JuliaObject 7 % JuliaObject 3, JuliaObject 1)
assert Equation(JuliaObject 7 % 3, JuliaObject 1)
assert Equation(7 % JuliaObject 3, JuliaObject 1)

-- negation
assert Equation(not JuliaObject true, JuliaObject false)

-- and
assert(JuliaObject true and JuliaObject true)
assert(JuliaObject true and true)
assert(true and JuliaObject true)
assert not (JuliaObject false and 5) -- short-circuits

-- or
assert(JuliaObject false or JuliaObject true)
assert(JuliaObject false or true)
assert(false or JuliaObject true)
assert(JuliaObject true or 5) -- short-circuits

-- bitwise not
assert Equation(~JuliaObject 5, JuliaObject(-6))

-- bitwise and
assert Equation(JuliaObject 5 & JuliaObject 6, JuliaObject 4)
assert Equation(JuliaObject 5 & 6, JuliaObject 4)
assert Equation(5 & JuliaObject 6, JuliaObject 4)

-- bitwise or
assert Equation(JuliaObject 5 | JuliaObject 6, JuliaObject 7)
assert Equation(JuliaObject 5 | 6, JuliaObject 7)
assert Equation(5 | JuliaObject 6, JuliaObject 7)

-- bitwise xor
assert Equation(JuliaObject 5 ^^ JuliaObject 6, JuliaObject 3)
assert Equation(JuliaObject 5 ^^ 6, JuliaObject 3)
assert Equation(5 ^^ JuliaObject 6, JuliaObject 3)

-- arithmetic shift right
assert Equation(JuliaObject 192 >> JuliaObject 5, JuliaObject 6)
assert Equation(JuliaObject 192 >> 5, JuliaObject 6)
assert Equation(192 >> JuliaObject 5, JuliaObject 6)

-- arithmetic shift left
assert Equation(JuliaObject 6 << JuliaObject 5, JuliaObject 192)
assert Equation(JuliaObject 6 << 5, JuliaObject 192)
assert Equation(6 << JuliaObject 5, JuliaObject 192)
///

TEST ///
-- comparison
assert BinaryOperation(symbol <, JuliaObject 2, JuliaObject 3)
assert BinaryOperation(symbol <, JuliaObject 2, 3)
assert BinaryOperation(symbol <, 2, JuliaObject 3)
assert BinaryOperation(symbol <=, JuliaObject 2, JuliaObject 3)
assert BinaryOperation(symbol <=, JuliaObject 2, 3)
assert BinaryOperation(symbol <=, 2, JuliaObject 3)
assert BinaryOperation(symbol <=, JuliaObject 2, JuliaObject 2)
assert BinaryOperation(symbol <=, JuliaObject 2, 2)
assert BinaryOperation(symbol <=, 2, JuliaObject 2)
assert BinaryOperation(symbol >, JuliaObject 4, JuliaObject 3)
assert BinaryOperation(symbol >, JuliaObject 4, 3)
assert BinaryOperation(symbol >, 4, JuliaObject 3)
assert BinaryOperation(symbol >=, JuliaObject 4, JuliaObject 3)
assert BinaryOperation(symbol >=, JuliaObject 4, 3)
assert BinaryOperation(symbol >=, 4, JuliaObject 3)
assert BinaryOperation(symbol >=, JuliaObject 2, JuliaObject 2)
assert BinaryOperation(symbol >=, JuliaObject 2, 2)
assert BinaryOperation(symbol >=, 2, JuliaObject 2)
///

TEST ///
-- methods
assert isFinite JuliaObject 5
assert isInfinite JuliaObject infinity
///

end

restart
loadPackage("Julia", FileName => "~/src/macaulay2/M2-2/M2/Macaulay2/packages/Julia.m2", Reload => true)
check oo
