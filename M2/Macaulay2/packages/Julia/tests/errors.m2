assert(try juliaValue "error(\"foo\")" then false else true)
assert(try juliaValue "throw(DomainError(-1))" then false else true)
assert(try juliaValue "1 +" then false else true)
assert(try juliaValue "m2testundefined" then false else true)
assert(try (JuliaFunction "sqrt")(-1) then false else true)
assert(try (JuliaFunction "sqrt")("foo") then false else true)
assert(try (JuliaObject {1, 2, 3})_10 then false else true)

-- julia still works after an error
assert(try juliaValue "error(\"foo\")" then false else true)
assert Equation(value juliaValue "1 + 1", 2)
assert(try (JuliaFunction "sqrt")(-1) then false else true)
assert Equation(value (JuliaFunction "sqrt") 4, 2)

-- repeated errors
scan(10, i -> assert(try juliaValue "error(\"foo\")" then false else true))
assert Equation(value juliaValue "1 + 1", 2)
