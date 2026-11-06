needsPackage "Text"

ctx = new JuliaContext
assert instance(ctx, JuliaContext)
assert Equation(net ctx, "ctx") -- globalAssignment
ctx "x = 3"
assert Equation(value ctx "x + 1", 4)
assert Equation(value ctx_"x", 3)
assert(try ctx "error(\"foo\")" then false else true)

-- contexts are isolated from Main and each other
ctx "contextonly = 3"
assert(try juliaGetGlobal "contextonly" then false else true)
ctx2 = new JuliaContext from "x = 10; f(y) = 2y"
assert Equation(value ctx2 "x", 10)
assert Equation(value ctx "x", 3)
assert Equation(value (ctx2_"f") 4, 8)
assert(try ctx "f(4)" then false else true)

-- listSymbols
L = listSymbols ctx2
assert instance(L, TABLE)
assert match(":x +Int64 +10", toString net L)
assert match(":f ", toString net L)
assert Equation(net listSymbols ctx2.Global, net L)
M = juliaValue "module M2TestModule\nglobal b\na = 1\nend"
L = listSymbols M
assert instance(L, TABLE)
assert match(":a +Int64 +1", toString net L)
-- declared globals without values are skipped
assert not match(":b ", toString net L)
assert(try listSymbols JuliaObject 5 then false else true)

-- use
use ctx2
assert Equation(value x, 10)
assert Equation(value f 5, 10)
