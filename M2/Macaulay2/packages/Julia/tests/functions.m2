-- constructors
f = JuliaFunction "sqrt"
assert instance(f, JuliaFunction)
assert instance(f, Function)
assert Equation(value f 4, 2)
assert Equation(value (JuliaFunction sqrt) 4, 2)
assert Equation(value (JuliaFunction symbol +)(1, 2), 3)
BaseModule = juliaGetGlobal "Base"
assert Equation(value (JuliaFunction(BaseModule, "sqrt")) 9, 3)
assert Equation(value (JuliaFunction(BaseModule, sqrt)) 9, 3)
assert Equation(value (JuliaFunction(BaseModule, symbol +))(1, 2), 3)
assert(try JuliaFunction "notAJuliaFunction" then false else true)

-- number of arguments
assert instance((JuliaFunction "time")(), JuliaObject)
jlplus = JuliaFunction symbol +
assert Equation(value jlplus 1, 1)
assert Equation(value jlplus(1, 2), 3)
assert Equation(value jlplus(1, 2, 3), 6)
assert Equation(value jlplus(1, 2, 3, 4), 10)
assert Equation(value jlplus(1, 2, 3, 4, 5), 15)
assert Equation(value jlplus(1, 2, 3, 4, 5, 6), 21)
assert Equation(value jlplus toSequence(1..20), 210)

-- arguments are converted to julia objects
assert Equation(value (JuliaFunction "length") {1, 2, 3}, 3)
assert Equation(value (JuliaFunction "length")({1, 2, 3}), 3)
assert Equation(value (JuliaFunction "string")("foo", 1, 2/3), "foo12//3")
-- sequences are argument lists; tuples must be converted explicitly
assert Equation(toString (JuliaFunction "typeof") JuliaObject (1, 2), "Tuple{Int64, Int64}")
assert(try (JuliaFunction "typeof")(1, 2) then false else true)

-- keyword arguments
assert Equation(value (JuliaFunction "round")(pi, digits => 2), 3.14)
assert Equation(value (JuliaFunction "round")(pi, "digits" => 3), 3.142)
k = juliaValue "(x = 0, y = 0; a = 0, b = 0) -> x + 10y + 100a + 1000b"
assert Equation(value k(1, 2, a => 3), 321)
assert Equation(value k(1, a => 3, 2, b => 4), 4321)
assert Equation(value k(a => 3, b => 4), 4300)
assert Equation(value k(a => 3), 300)
assert(try k(c => 5) then false else true)
kw = juliaValue "(; kw...) -> kw[:a]"
assert Equation(value kw(a => 5), 5)

-- julia objects may be called directly
assert Equation(value (juliaGetGlobal "sqrt") 4, 2)
assert Equation(value (juliaValue "(x, y) -> x * y")(3, 4), 12)
assert Equation(value (juliaValue "(x; a = 1) -> a * x")(3, a => 2), 6)

-- functions returning nothing
assert Equation(value (juliaValue "() -> nothing")(), null)

-- printing
assert instance(toString f, String)
assert Equation(toString f, "sqrt")
assert instance(net f, Net)
