-- negation
assert Equation(not JuliaObject true, JuliaObject false)

-- and
assert(JuliaObject true and JuliaObject true)
assert(JuliaObject true and true)
assert(true and JuliaObject true)
assert not (JuliaObject false and 5) -- short-circuits

-- or
assert(JuliaObject false or JuliaObject true)
assert(JuliaObject false or true)
assert(false or JuliaObject true)
assert(JuliaObject true or 5) -- short-circuits
assert not (JuliaObject false and JuliaObject true)
assert not (JuliaObject true and JuliaObject false)
assert not (false and JuliaObject true)
assert not (JuliaObject false or JuliaObject false)
assert not (JuliaObject false or false)
assert not (false or JuliaObject false)
assert(true or JuliaObject 5) -- short-circuits
assert not (false and JuliaObject 5) -- short-circuits

-- results are M2 booleans
assert instance(JuliaObject true and JuliaObject true, Boolean)
assert instance(JuliaObject false or JuliaObject true, Boolean)
