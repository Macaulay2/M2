-- one object per construction path: strings, boxed ints and floats,
-- and objects returned by julia function calls
vals = flatten apply(25, i -> {toString i, 2^40 + i, 0.5 + i, {i}})
xs = JuliaObject \ vals
juliaValue "GC.gc()"

-- temporary objects reuse any memory that was wrongly freed
numKnown = () -> value juliaValue "length(M2Julia.known_objects)"
n = numKnown()
scan(1000, i -> value JuliaObject toString i)
collectGarbage()
juliaValue "GC.gc()"
collectGarbage()

-- live objects survive, and finalizers remove dead objects from known_objects
assert Equation(value \ xs, vals)
assert(numKnown() < n + 500)
