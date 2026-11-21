-- getindex/setindex!
v = JuliaObject {1, 2, 3}
assert Equation(v_1, 1)
assert Equation(v_3, 3)
v_1 = 10
assert Equation(value v, {10, 2, 3})
assert(try v_0 then false else true)
assert(try v_4 then false else true)

A = JuliaObject matrix {{1, 2}, {3, 4}}
assert Equation(A_(1, 2), 2)
assert Equation(A_(2, 1), 3)
A_(1, 2) = 20
assert Equation(value A, matrix {{1, 20}, {3, 4}})

t = JuliaObject (1, "foo")
assert Equation(t_2, "foo")
assert(try t_1 = 5 then false else true)

s = JuliaObject "foo"
assert Equation(value s_1, "f")

d = JuliaObject hashTable {"a" => 1}
assert Equation(d_"a", 1)
d_"b" = 2
assert BinaryOperation(symbol ===, value d, hashTable {"a" => 1, "b" => 2})
delete(d, "a")
assert BinaryOperation(symbol ===, value d, hashTable {"b" => 2})
assert(try d_"a" then false else true)

-- getproperty/setproperty!
juliaValue "mutable struct M2TestMutable; a::Int; end"
m = juliaValue "M2TestMutable(1)"
assert Equation(m@@"a", 1)
assert Equation(m@@a, 1)
assert m@@?"a"
assert m@@?a
assert not m@@?"b"
m@@"a" = 5
assert Equation(m@@"a", 5)
m@@a = 6
assert Equation(m@@a, 6)
assert(try m@@"b" then false else true)
assert Equation(value ((juliaGetGlobal "Base")@@"sqrt") 4, 2)

-- iteration
assert Equation(value \ toList JuliaObject {1, 2, 3}, {1, 2, 3})
assert Equation(value \ toList JuliaObject (1, "foo"), {1, "foo"})
assert Equation(value \ toList JuliaObject {}, {})
assert Equation(value \ toList juliaValue "1:4", {1, 2, 3, 4})
assert Equation(value \ toList JuliaObject "foo", {"f", "o", "o"})
assert Equation(toList apply(JuliaObject {1, 2}, x -> value x + 1), {2, 3})
assert Equation(for x in JuliaObject {1, 2} list value x, {1, 2})
assert Equation(apply(toList JuliaObject hashTable {1 => 2}, p -> value \ (p_1, p_2)), {(1, 2)})
assert Equation(sum(toList JuliaObject {1, 2, 3}), JuliaObject 6)

-- length
assert Equation(length JuliaObject {1, 2, 3}, 3)
assert Equation(length JuliaObject {}, 0)
assert Equation(length JuliaObject "foo", 3)
assert Equation(length JuliaObject hashTable {1 => 2}, 1)
assert Equation(length JuliaObject matrix {{1, 2}, {3, 4}}, 4)
