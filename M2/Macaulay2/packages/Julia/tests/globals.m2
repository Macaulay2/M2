-- juliaSymbol
assert Equation(value juliaSymbol "foo", "foo")
assert Equation(value juliaSymbol foo, "foo")
assert Equation(value juliaSymbol 5, "5")
assert Equation(toString (JuliaFunction "typeof") juliaSymbol "foo", "Symbol")
assert(juliaSymbol "foo" == juliaValue ":foo")

-- juliaGetGlobal
juliaValue "m2testglobal = 42"
assert Equation(value juliaGetGlobal "m2testglobal", 42)
assert Equation(value juliaGetGlobal m2testglobal, 42)
Main = juliaGetGlobal "Main"
BaseModule = juliaGetGlobal "Base"
assert Equation(value juliaGetGlobal(Main, "m2testglobal"), 42)
assert Equation(value juliaGetGlobal(Main, m2testglobal), 42)
assert Equation(value (juliaGetGlobal(BaseModule, "sqrt")) 4, 2)
assert Equation(value (juliaGetGlobal(BaseModule, symbol sqrt)) 4, 2)
assert Equation(value (juliaGetGlobal(BaseModule, sqrt)) 4, 2)
assert(try juliaGetGlobal "m2testundefined" then false else true)
assert(try juliaGetGlobal(BaseModule, "m2testundefined") then false else true)
assert(try juliaGetGlobal(JuliaObject 5, "foo") then false else true)
