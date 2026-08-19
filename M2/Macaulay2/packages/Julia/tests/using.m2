LA = using "LinearAlgebra"
assert Equation(toString LA, "LinearAlgebra")
assert(LA == juliaGetGlobal "LinearAlgebra")
assert Equation(value (JuliaFunction(LA, "det")) matrix {{1, 2}, {3, 4}}, -2)
-- exported names are available in Main
assert Equation(value (juliaGetGlobal "det") matrix {{1, 2}, {3, 4}}, -2)
assert Equation(value (juliaGetGlobal "tr") matrix {{1, 2}, {3, 4}}, 5)
assert(try using "NotARealJuliaPackage" then false else true)
