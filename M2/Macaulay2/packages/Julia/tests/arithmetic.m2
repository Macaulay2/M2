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


-- rational and floating-point arithmetic
assert Equation(JuliaObject(1/2) + JuliaObject(1/3), JuliaObject(5/6))
assert Equation(JuliaObject 1.5 * 2, JuliaObject 3.0)
assert Equation(JuliaObject(2 + ii) * JuliaObject(2 - ii), JuliaObject(5 + 0*ii))
assert Equation(JuliaObject 2^100 + 1, JuliaObject(2^100 + 1))

-- results are julia objects
assert instance(JuliaObject 6 + 3, JuliaObject)
assert instance(6 + JuliaObject 3, JuliaObject)

-- overflow follows julia semantics
assert Equation(value(JuliaObject(2^63 - 1) + 1), -2^63)

-- errors
assert(try JuliaObject 1 // 0 then false else true)
assert(try JuliaObject "foo" + 1 then false else true)
