-- these tests can't be captured (see isCapturable in m2/examples.m2),
-- so they run in their own M2 process; keep them together to start
-- julia only once

------------------------------
-- user-defined conversions --
------------------------------
juliaValue "struct M2JuliaTestPoint; x::Int; y::Int; end"
p = juliaValue "M2JuliaTestPoint(1, 2)"
assert(try value p then false else true)
addJuliaToM2Function("M2JuliaTestPoint", q -> {value q@@"x", value q@@"y"})
assert BinaryOperation(symbol ===, value p, {1, 2})
assert BinaryOperation(symbol ===, value juliaValue "[M2JuliaTestPoint(3, 4)]", {{3, 4}})

-- abstract types are dispatched on by subtyping
juliaValue "abstract type M2JuliaTestShape end"
juliaValue "struct M2JuliaTestSquare <: M2JuliaTestShape; side::Int; end"
addJuliaToM2Function("M2JuliaTestShape", s -> "a shape")
assert Equation(value juliaValue "M2JuliaTestSquare(2)", "a shape")

-- the most specific type wins
addJuliaToM2Function("M2JuliaTestSquare", s -> value s@@"side")
assert Equation(value juliaValue "M2JuliaTestSquare(2)", 2)

-- re-registering a type replaces its conversion
addJuliaToM2Function("M2JuliaTestPoint", q -> "a point")
assert Equation(value p, "a point")

-- the type is evaluated as Julia code
addJuliaToM2Function("Vector{M2JuliaTestShape}", v -> "shapes")
assert Equation(value juliaValue "M2JuliaTestShape[M2JuliaTestSquare(1)]", "shapes")
assert(try addJuliaToM2Function("NotARealJuliaType", identity) then false else true)

----------------
-- AfterPrint --
----------------
assert Equation(toString JuliaObject.AfterPrint JuliaObject 5,
    toString(JuliaObject, " of type ", JuliaObject "Int64"))
assert Equation(toList apply(JuliaObject.AfterPrint JuliaObject 5, toString),
    {"JuliaObject", " of type ", "Int64"})
