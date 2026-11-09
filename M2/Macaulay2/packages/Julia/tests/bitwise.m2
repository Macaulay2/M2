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
