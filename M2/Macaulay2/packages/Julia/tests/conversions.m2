typeOf = toString @@ (JuliaFunction "typeof")

-----------------
-- round trips --
-----------------
assertRoundTrip = (x, T) -> (
    y := JuliaObject x;
    assert Equation(typeOf y, T);
    assert BinaryOperation(symbol ===, value y, x))

assertRoundTrip(true, "Bool")
assertRoundTrip(false, "Bool")

-- integers
assertRoundTrip(0, "Int64")
assertRoundTrip(5, "Int64")
assertRoundTrip(-5, "Int64")
assertRoundTrip(2^63 - 1, "Int64")
assertRoundTrip(-2^63, "Int64")
assertRoundTrip(2^63, "Int128")
assertRoundTrip(-2^63 - 1, "Int128")
assertRoundTrip(2^64, "Int128")
assertRoundTrip(-2^64 - 1, "Int128")
assertRoundTrip(2^127 - 1, "Int128")
assertRoundTrip(-2^127, "Int128")
assertRoundTrip(2^127, "BigInt")
assertRoundTrip(-2^127 - 1, "BigInt")
assertRoundTrip(2^1000 + 1, "BigInt")
assertRoundTrip(-2^1000 - 1, "BigInt")

-- rationals
assertRoundTrip(2/3, "Rational{Int64}")
assertRoundTrip(-2/3, "Rational{Int64}")

-- floating point
assertRoundTrip(0., "Float64")
assertRoundTrip(-2.5, "Float64")
assertRoundTrip(numeric pi, "Float64")
assertRoundTrip(numeric(100, pi), "BigFloat")
assertRoundTrip(numeric(1000, pi), "BigFloat")
assert Equation(precision value JuliaObject numeric(100, pi), 100)
assert Equation(precision value JuliaObject numeric(1000, pi), 1000)
assert Equation(value (JuliaFunction "precision") JuliaObject numeric(100, pi), 100)
assert isInfinite value JuliaObject infinity
assert isInfinite value JuliaObject(-infinity)
assert isANumber value JuliaObject 0.
assert not isANumber value JuliaObject(0/0.)

-- complex numbers
assertRoundTrip(2 + 3*ii, "ComplexF64")
assertRoundTrip(-1.5*ii, "ComplexF64")

-- other numbers are converted with numeric
assert Equation(typeOf JuliaObject pi, "Float64")
assert Equation(value JuliaObject pi, numeric pi)

-- strings
assertRoundTrip("foo", "String")
assertRoundTrip("", "String")
assertRoundTrip("🐂 ünïcödé", "String")
assertRoundTrip("multiple\nlines", "String")

-- nothing
assertRoundTrip(null, "Nothing")

-- lists and sequences
assertRoundTrip({1, 2, 3}, "Vector{Int64}")
assertRoundTrip({1, "foo", 2.5}, "Vector{Any}")
assertRoundTrip({}, "Vector{Any}")
assertRoundTrip({{1, 2}, {3}}, "Vector{Vector{Int64}}")
assertRoundTrip((1, 2, 3), "Tuple{Int64, Int64, Int64}")
assertRoundTrip((1, "foo"), "Tuple{Int64, String}")
assertRoundTrip((), "Tuple{}")
assertRoundTrip(({1}, (2, 3)), "Tuple{Vector{Int64}, Tuple{Int64, Int64}}")

-- hash tables
assertRoundTrip(hashTable {"a" => 1, "b" => 2}, "Dict{String, Int64}")
assertRoundTrip(hashTable {}, "Dict{Any, Any}")

-- sets
assertRoundTrip(set {1, 2, 3}, "Set{Int64}")
assertRoundTrip(set {}, "Set{Any}")

-- matrices
assertRoundTrip(matrix {{1, 2}, {3, 4}}, "Matrix{Int64}")
assertRoundTrip(matrix {{1, 2, 3}}, "Matrix{Int64}")
assertRoundTrip(matrix {{1}, {2}, {3}}, "Matrix{Int64}")
assertRoundTrip(map(ZZ^0, ZZ^3, 0), "Matrix{Int64}")
assertRoundTrip(map(ZZ^3, ZZ^0, 0), "Matrix{Int64}")
assertRoundTrip(map(RR^2, RR^0, 0), "Matrix{Float64}")
assert Equation(value (JuliaFunction "size") JuliaObject map(ZZ^0, ZZ^3, 0), (0, 3))
assert Equation(value (JuliaFunction "size") JuliaObject map(ZZ^3, ZZ^0, 0), (3, 0))

--------------------------
-- julia-native objects --
--------------------------
-- integer types
scan({"Int8", "Int16", "Int32", "Int64", "Int128",
	"UInt8", "UInt16", "UInt32", "UInt64", "UInt128", "BigInt"},
    T -> (
	x := juliaValue(T, "(5)");
	assert Equation(typeOf x, T);
	assert BinaryOperation(symbol ===, value x, 5)))
scan({"Int8", "Int16", "Int32", "Int64", "Int128"},
    T -> assert BinaryOperation(symbol ===, value juliaValue(T, "(-5)"), -5))
scan({(8, "Int8"), (16, "Int16"), (32, "Int32"), (64, "Int64"), (128, "Int128")},
    (n, T) -> (
	assert BinaryOperation(symbol ===,
	    value juliaValue("typemax(", T, ")"), 2^(n - 1) - 1);
	assert BinaryOperation(symbol ===,
	    value juliaValue("typemin(", T, ")"), -2^(n - 1))))
scan({(8, "UInt8"), (16, "UInt16"), (32, "UInt32"), (64, "UInt64"), (128, "UInt128")},
    (n, T) -> (
	assert BinaryOperation(symbol ===,
	    value juliaValue("typemax(", T, ")"), 2^n - 1);
	assert BinaryOperation(symbol ===,
	    value juliaValue("typemin(", T, ")"), 0)))
assert BinaryOperation(symbol ===, value juliaValue "big(2)^200", 2^200)
assert BinaryOperation(symbol ===, value juliaValue "-big(2)^200", -2^200)

-- floating-point types
x = value juliaValue "Float16(0.5)"
assert Equation(x, 0.5)
assert Equation(precision x, 11)
x = value juliaValue "Float32(0.5)"
assert Equation(x, 0.5)
assert Equation(precision x, 24)
x = value juliaValue "Float64(0.5)"
assert Equation(x, 0.5)
assert Equation(precision x, 53)
x = value juliaValue "BigFloat(0.5; precision = 200)"
assert Equation(x, 0.5)
assert Equation(precision x, 200)
x = value juliaValue "BigFloat(pi; precision = 200)"
assert Equation(x, numeric(200, pi))

-- other numbers
assert BinaryOperation(symbol ===, value juliaValue "Int8(2)//Int8(3)", 2/3)
assert BinaryOperation(symbol ===, value juliaValue "big(2)^100//3", 2^100/3)
assert BinaryOperation(symbol ===, value juliaValue "2 + 3im", 2 + 3*ii)
assert Equation(value juliaValue "2.5 + 3.5im", 2.5 + 3.5*ii)
assert BinaryOperation(symbol ===, value juliaValue "true", true)

-- characters and symbols become strings
assert BinaryOperation(symbol ===, value juliaValue "'a'", "a")
assert BinaryOperation(symbol ===, value juliaValue "'🐂'", "🐂")
assert BinaryOperation(symbol ===, value (JuliaObject "🐂")_1, "🐂")
assert BinaryOperation(symbol ===, value juliaValue ":foo", "foo")
assert BinaryOperation(symbol ===, value juliaSymbol "foo", "foo")

-- arrays
assert BinaryOperation(symbol ===, value juliaValue "1:3", {1, 2, 3})
assert BinaryOperation(symbol ===, value juliaValue "1:0", {})
assert BinaryOperation(symbol ===, value juliaValue "view([1, 2, 3], 2:3)", {2, 3})
assert BinaryOperation(symbol ===, value juliaValue "Any[]", {})
assert Equation(value juliaValue "[1 2; 3 4]", matrix {{1, 2}, {3, 4}})
assert Equation(value juliaValue "zeros(Int, 0, 2)", map(ZZ^0, ZZ^2, 0))
assert Equation(value juliaValue "zeros(2, 0)", map(RR^2, RR^0, 0))

-- tuples and named tuples
assert BinaryOperation(symbol ===, value juliaValue "(1, 'a', :b)", (1, "a", "b"))
assert BinaryOperation(symbol ===, value juliaValue "(a = 1, b = \"foo\")",
    ("a" => 1, "b" => "foo"))
assert BinaryOperation(symbol ===, value juliaValue "NamedTuple()", ())

-- dictionaries and sets
assert BinaryOperation(symbol ===, value juliaValue "Dict(:a => 1, :b => 2)",
    hashTable {"a" => 1, "b" => 2})
assert BinaryOperation(symbol ===, value juliaValue "BitSet([1, 2])", set {1, 2})

-- nothing
assert BinaryOperation(symbol ===, value juliaValue "nothing", null)

-- no conversion
x = juliaValue("struct Unconvertible end; Unconvertible()",
    Global => juliaValue "Module()")
assert(try value x then false else true)
