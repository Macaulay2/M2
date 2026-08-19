newPackage "Julia"

export {
    "JuliaObject"
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

jlTypeofStr = foreignFunction(libjulia, "jl_typeof_str", charstar, voidstar)
JuliaObject.AfterPrint = x -> (JuliaObject, " of class ", value jlTypeofStr x.Address)

jlEvalString = foreignFunction(libjulia, "jl_eval_string", voidstar, charstar)
-- jlEvalString "print(sqrt(2.0))"

-------------------
-- JuliaFunction --
-------------------

JuliaFunction = new SelfInitializingType of FunctionClosure
JuliaFunction.synonym = "Julia function"

jlGetGlobal = foreignFunction(libjulia, "jl_get_global", voidstar, {voidstar, voidstar})
jlBaseModule = foreignSymbol(libjulia, "jl_base_module", voidstar)
jlCall0 = foreignFunction(libjulia, "jl_call0", voidstar, voidstar)
jlCall1 = foreignFunction(libjulia, "jl_call1", voidstar, {voidstar, voidstar})
jlCall2 = foreignFunction(libjulia, "jl_call2", voidstar, {voidstar, voidstar, voidstar})
jlCall3 = foreignFunction(libjulia, "jl_call3", voidstar, {voidstar, voidstar, voidstar, voidstar})
jlCall4 = foreignFunction(libjulia, "jl_call4", voidstar, {voidstar, voidstar, voidstar, voidstar})
jlSymbol = foreignFunction(libjulia, "jl_symbol", voidstar, charstar)

new JuliaFunction from String := (T, s) -> x -> JuliaObject {Address => (
                                                                f := jlGetGlobal(jlBaseModule, jlSymbol s);
                                                                if instance(x, Sequence) then (
                                                                    x = apply(x, y -> (JuliaObject y).Address);
                                                                    if #x == 0 then jlCall0 f
                                                                    else if #x == 1 then jlCall1(f, x#0)
                                                                    else if #x == 2 then jlCall2(f, x#0, x#1)
                                                                    else if #x == 3 then jlCall3(f, x#0, x#1, x#2)
                                                                    else if #x == 3 then jlCall4(f, x#0, x#1, x#2, x#3)
                                                                    else error("expected 0 - 4 arguments"))
                                                                else jlCall1(f, (JuliaObject x).Address))}
new JuliaFunction from Symbol := (T, s) -> T toString s

repr = new JuliaFunction from "repr"
jlStringPtr = foreignFunction(libjulia, "jl_string_ptr", charstar, voidstar)
net JuliaObject := toString JuliaObject := x -> value jlStringPtr (repr x).Address


-----------------
-- M2 -> julia --
-----------------

jlBoxBool = foreignFunction(libjulia, "jl_box_bool", voidstar, int8)
new JuliaObject from Boolean := (T, x) -> new T from {Address => jlBoxBool if x then 1 else 0}

jlBoxInt32 = foreignFunction(libjulia, "jl_box_int32", voidstar, int32)
new JuliaObject from ZZ := (T, x) -> new T from {Address => jlBoxInt32 x}

jlBoxFloat64 = foreignFunction(libjulia, "jl_box_float64", voidstar, double)
new JuliaObject from RR := (T, x) -> new T from {Address => jlBoxFloat64 x}

jlCstrToString = foreignFunction(libjulia, "jl_cstr_to_string", voidstar, charstar)
new JuliaObject from String := (T, x) -> new T from {Address => jlCstrToString x}

-----------------
-- julia -> M2 --
-----------------

jlUnboxBool = foreignFunction(libjulia, "jl_unbox_bool", int, voidstar)
jlUnboxInt32 = foreignFunction(libjulia, "jl_unbox_int32", int, voidstar)

-- TODO: make it work for something other than bool!
value JuliaObject := x -> value jlUnboxBool x.Address == 1

----------------------
-- binary operators --
----------------------

scan({
        symbol +,
        symbol -,
        symbol *,
        symbol /,
        symbol ^
    },
        op -> (
            f := JuliaFunction op;
            installMethod(op, JuliaObject, JuliaObject, f);
            installMethod(op, JuliaObject, Thing, (x, y) -> f(x, JuliaObject y));
            installMethod(op, Thing, JuliaObject, (x, y) -> f(JuliaObject x, y))))

jeq = JuliaFunction symbol ==
JuliaObject == JuliaObject := value @@ jeq
JuliaObject == Thing := (x, y) -> value jeq(x, JuliaObject y)
Thing == JuliaObject := (x, y) -> value jeq(JuliaObject x, y)

TEST ///
assert Equation(JuliaObject 6 + JuliaObject 3, JuliaObject 9)
assert Equation(JuliaObject 6 - JuliaObject 3, JuliaObject 3)
assert Equation(JuliaObject 6 * JuliaObject 3, JuliaObject 18)
assert Equation(JuliaObject 6 / JuliaObject 3, JuliaObject 2)
assert Equation((JuliaObject 6) ^ (JuliaObject 3), JuliaObject 216)
///

end

restart
loadPackage("Julia", FileName => "~/src/macaulay2/M2-2/M2/Macaulay2/packages/Julia.m2", Reload => true)
check oo

JuliaObject true
JuliaObject 200
JuliaObject numeric pi
JuliaObject "Hello, world!"

JuliaObject 12 + JuliaObject 7
new JuliaObject from {Address => oo}
