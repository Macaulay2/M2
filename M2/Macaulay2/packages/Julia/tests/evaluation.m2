assert Equation(value juliaValue "1 + 1", 2)
assert Equation(value juliaValue "[1, 2, 3] .^ 2", {1, 4, 9})
assert Equation(value juliaValue "let x = 2; x + 1 end", 3)
assert Equation(value juliaValue "nothing", null)

-- sequences are concatenated
assert Equation(value juliaValue("1 + ", 2, " + ", 3), 6)
assert Equation(value juliaValue("length(", "\"foo\"", ")"), 3)
assert Equation(value juliaValue(), null)

-- global definitions persist
juliaValue "m2testfunction(x) = 2x"
assert Equation(value juliaValue "m2testfunction(4)", 8)
assert Equation(value (juliaGetGlobal "m2testfunction") 4, 8)

-- Global option
M = juliaValue "Module()"
juliaValue("m2testlocal = 5", Global => M)
assert Equation(value M@@"m2testlocal", 5)
assert Equation(value juliaValue("m2testlocal + 1", Global => M), 6)
assert Equation(value juliaValue("a = 1; b = 2; a + b", Global => M), 3)
assert Equation(value juliaValue("m2testlocal", " + 2", Global => M), 7)
assert(try juliaGetGlobal "m2testlocal" then false else true)
assert(try juliaValue("error(\"foo\")", Global => M) then false else true)
