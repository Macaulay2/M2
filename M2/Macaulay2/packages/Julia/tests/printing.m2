x = JuliaObject {1, 2}
assert Equation(toString x, "[1, 2]")
assert Equation(toString JuliaObject "foo", "foo")
assert Equation(toString JuliaObject 5, "5")
assert instance(net x, Net)
assert Equation(toString net x, "2-element Vector{Int64}:\n 1\n 2")
assert Equation(net JuliaObject "foo", net "\"foo\"")
assert instance(describe x, Describe)
assert Equation(toExternalString x, "juliaValue \"[1, 2]\"")
assert Equation(toExternalString JuliaObject "foo", "juliaValue \"\\\"foo\\\"\"")

-- toExternalString round trips
scan({5, 2^100, 2/3, 2.5, "foo", {1, 2}, (1, "a"), null},
    y -> assert BinaryOperation(symbol ===,
	value value toExternalString JuliaObject y, y))
