newPackage "Julia"

export {
    "JuliaFunction",
    "JuliaObject",
}

needsPackage "ForeignFunctions"

libjulia = openSharedLibrary("libjulia", FileName => "/home/profzoom/.julia/juliaup/julia-1.12.7+0.x64.linux.gnu/lib/libjulia.so.1.12.7")
jlInit = foreignFunction(libjulia, "jl_init", void, void)
jlInit()

-----------------
-- JuliaObject --
-----------------

protect Address
JuliaObject = new SelfInitializingType of HashTable
JuliaObject.synonym = "Julia object"

new JuliaObject from voidstar := (T, x) -> T {Address => x}

jlTypeofStr = foreignFunction(libjulia, "jl_typeof_str", charstar, voidstar)
JuliaObject.AfterPrint = x -> (JuliaObject, " of class ", value jlTypeofStr x.Address)

jlEvalString = foreignFunction(libjulia, "jl_eval_string", voidstar, charstar)
-- jlEvalString "print(sqrt(2.0))"

-------------------
-- JuliaFunction --
-------------------

jlGetGlobal = foreignFunction(libjulia, "jl_get_global", voidstar, {voidstar, voidstar})
jlBaseModule = foreignSymbol(libjulia, "jl_base_module", voidstar)
jlCall0 = foreignFunction(libjulia, "jl_call0", voidstar, voidstar)
jlCall1 = foreignFunction(libjulia, "jl_call1", voidstar, {voidstar, voidstar})
jlCall2 = foreignFunction(libjulia, "jl_call2", voidstar, {voidstar, voidstar, voidstar})
jlCall3 = foreignFunction(libjulia, "jl_call3", voidstar, {voidstar, voidstar, voidstar, voidstar})
jlCall4 = foreignFunction(libjulia, "jl_call4", voidstar, {voidstar, voidstar, voidstar, voidstar})
jlSymbol = foreignFunction(libjulia, "jl_symbol", voidstar, charstar)

JuliaFunction = new SelfInitializingType of FunctionClosure
JuliaFunction.synonym = "Julia function"

new JuliaFunction from String := (T, s) -> (
                           f := jlGetGlobal(jlBaseModule, jlSymbol s);
                           if value f === nullPointer
                           then error("'", s, "' is not a Julia function");
                           x -> (
                               ptr :=  if instance(x, Sequence) then (
                                   x = apply(x, y -> (JuliaObject y).Address);
                                   if #x == 0 then jlCall0 f
                                   else if #x == 1 then jlCall1(f, x#0)
                                   else if #x == 2 then jlCall2(f, x#0, x#1)
                                   else if #x == 3 then jlCall3(f, x#0, x#1, x#2)
                                   else if #x == 3 then jlCall4(f, x#0, x#1, x#2, x#3)
                                   else error("expected 0 - 4 arguments"))
                               else jlCall1(f, (JuliaObject x).Address);
                               if value ptr === nullPointer
                               then error new JuliaError
                               else JuliaObject ptr))
new JuliaFunction from Symbol := (T, s) -> T toString s

repr = JuliaFunction "repr"

jlStringPtr = foreignFunction(libjulia, "jl_string_ptr", charstar, voidstar)
net JuliaObject := toString JuliaObject := x -> value jlStringPtr (repr x).Address

--------------------
-- error handling --
--------------------

jlExceptionOccurred = foreignFunction(libjulia, "jl_exception_occurred", voidstar, void)
jlExceptionClear = foreignFunction(libjulia, "jl_exception_clear", void, void)
sprint = JuliaFunction "sprint"
showerror = jlGetGlobal(jlBaseModule, jlSymbol "showerror")

JuliaError = new SelfInitializingType of Error

new JuliaError := T -> (
        exc := jlExceptionOccurred();
        if value exc === nullPointer
        then error "no Julia error occurred"
        else (
            jlExceptionClear();
            T value sprint(showerror, exc)))

-----------------
-- M2 -> julia --
-----------------

jlBoxBool = foreignFunction(libjulia, "jl_box_bool", voidstar, int8)
new JuliaObject from Boolean := (T, x) -> T jlBoxBool if x then 1 else 0

jlBoxInt64 = foreignFunction(libjulia, "jl_box_int64", voidstar, int64)
new JuliaObject from ZZ := (T, x) -> T jlBoxInt64 x

jlBoxFloat64 = foreignFunction(libjulia, "jl_box_float64", voidstar, double)
new JuliaObject from RR := (T, x) -> T jlBoxFloat64 x
new JuliaObject from Number := (T, x) -> T numeric x

jlCstrToString = foreignFunction(libjulia, "jl_cstr_to_string", voidstar, charstar)
new JuliaObject from String := (T, x) -> T jlCstrToString x

-----------------
-- julia -> M2 --
-----------------

jlUnboxBool = foreignFunction(libjulia, "jl_unbox_bool", int, voidstar)
getJlBool = x -> value jlUnboxBool x.Address == 1

isa = getJlBool @@ (JuliaFunction "isa")

jlBoolType = JuliaObject foreignSymbol(libjulia, "jl_bool_type", voidstar)
addHook((value, JuliaObject), x ->
                              if isa(x, jlBoolType) then getJlBool x,
           Strategy => "Bool -> Boolean")

jlInt64Type = JuliaObject foreignSymbol(libjulia, "jl_int64_type", voidstar)
jlUnboxInt64 = foreignFunction(libjulia, "jl_unbox_int64", int, voidstar)
addHook((value, JuliaObject), x ->
                              if isa(x, jlInt64Type) then value jlUnboxInt64 x.Address,
           Strategy => "Int64 -> ZZ")

jlFloat64Type = JuliaObject foreignSymbol(libjulia, "jl_float64_type", voidstar)
jlUnboxFloat64 = foreignFunction(libjulia, "jl_unbox_float64", double, voidstar)
addHook((value, JuliaObject), x ->
                              if isa(x, jlFloat64Type) then value jlUnboxFloat64 x.Address,
           Strategy => "Float64 -> RR")

jlStringType = JuliaObject foreignSymbol(libjulia, "jl_string_type", voidstar)
addHook((value, JuliaObject), x ->
                              if isa(x, jlStringType) then value jlStringPtr x.Address,
           Strategy => "String -> String")


value JuliaObject := x -> runHooks((value, JuliaObject), x)

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

TEST ///
-- roundtrip
assertRoundTrip = x -> assert Equation(value JuliaObject x, x)
assertRoundTrip true
assertRoundTrip 5
assertRoundTrip pi
assertRoundTrip "foo"
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

end

restart
loadPackage("Julia", FileName => "~/src/macaulay2/M2-2/M2/Macaulay2/packages/Julia.m2", Reload => true)
check oo
