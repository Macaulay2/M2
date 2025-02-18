-- test each binary operator using global variables
x = 2
x += 3
assert Equation(x, 5)
x -= 4
assert Equation(x, 1)
x *= 5
assert Equation(x, 5)
x ^= 6
assert Equation(x, 15625)
x /= 7
assert Equation(x, 15625/7)
x = 2
x <<= 3
assert Equation(x, 16)
x >>= 4
assert Equation(x, 1)
x ^^= 5
assert Equation(x, 4)
x &= 6
assert Equation(x, 4)
x //= 3
assert Equation(x, 1)
x %= 1
assert Equation(x, 0)
x = matrix {{1}}
x ||= matrix {{2}}
assert Equation(x, matrix{{1}, {2}})
x |= matrix {{3}, {4}}
assert Equation(x, matrix {{1, 3}, {2, 4}})
x \\= matrix {{4, 3}, {2, 1}}
assert Equation(x, matrix {{-3, 3}, {-1, 1}})
x = 1:(1)
x ..= 1:(3)
assert Equation(x, (1:(1), 1:(2), 1:(3)))
x = 1:(1)
x ..<= 1:(3)
assert Equation(x, (1:(1), 1:(2)))
x = ZZ^1
x ++= ZZ^1
assert Equation(x, ZZ^2)
x **= ZZ^2
assert Equation(x, ZZ^4)
x ^**= 3
assert Equation(x, ZZ^64)
f = y -> 2*y
f \= {1, 2, 3} -- no one should do this!  just testing that \= works
assert Equation(f, {2, 4, 6})
f = y -> 2*y
f @@= y -> 3*y
assert Equation(f 1, 6)
R = QQ[symbol x]
x = 2
x _= R
assert BinaryOperation(symbol ===, x, 2_R)

-- other binary operators that aren't used in Core
-- install silly methods for them so we can test augmented assignment
Foo = new SelfInitializingType of List
new Foo from ZZ := (T, x) -> T {x}
Foo <==> Foo := (x, y) -> Foo(x#0 + y#0)
x = Foo 2
x <==>= Foo 3
assert Equation(x#0, 5)
Foo |- Foo := (x, y) -> Foo(x#0 - y#0)
x |-= Foo 4
assert Equation(x#0, 1)
Foo ==> Foo := (x, y) -> Foo(x#0 * y#0)
x ==>= Foo 5
assert Equation(x#0, 5)
Foo ===> Foo := (x, y) -> Foo(2*x#0 + 3*y#0)
x ===>= Foo 6
assert Equation(x#0, 28)
Foo @ Foo := (x, y) -> Foo(4*x#0 - 5*y#0)
x @= Foo 7
assert Equation(x#0, 77)

-- local variable
x := 2
x += 3
assert Equation(x, 5)

-- thread variable
threadVariable y
y = 2
y += 3
assert Equation(y, 5)

-- binary operator
x = new MutableList
x#0 = 2
x#0 += 3
assert Equation(x#0, 5)

x = new MutableHashTable
x.foo = 2
x.foo += 3
assert Equation(x.foo, 5)

x = new IndexedVariableTable
x_0 = 2
x_0 += 3
assert Equation(x_0, 5)

-- adjacent
f = memoize(x -> x)
f 0 = 2
f 0 += 3
assert Equation(f 0, 5)

-- postfix
x = new RingFamily
x_* = 2
x_* += 3 -- silly example, but this is Core's only postfix assignment method
assert Equation(x_*, 5)

-- unary (no assignment methods in Core, so make our own)
Bar = new SelfInitializingType of MutableList
new Bar from ZZ := (T, x) -> Bar {x}
+Bar := identity
+Bar = (x, y) -> (x#0 = y#0; x)
Bar + Bar := (x, y) -> Bar(x#0 + y#0)
x = Bar 2
+x += Bar 3
assert Equation(x#0, 5)

-- install custom method
Bar - Bar := (x, y) -> Bar(x#0 - y#0)
installMethod(symbol -=, Bar, (x, y) -> if even y#0 then x#0 = 0 else Default)
x = Bar 5
x -= Bar 2
assert Equation(x#0, 0)
x -= Bar(-5)
assert Equation(x#0, 5)

-- null coalescing operator
x = 5
assert Equation(?? x, 5)
assert Equation(x ?? 2, 5)
x = null
assert Equation(?? x, null)
assert Equation(x ?? 2, 2)
x ??= 2
assert Equation(x, 2)
x = new MutableList
assert Equation(?? x#0, null)
assert Equation(x#0 ?? 2, 2)
x#0 ??= 2
assert Equation(x#0, 2)
X = new Type of BasicList
installMethod(symbol ??, X, x -> if #x > 0 then x)
x = new X from {}
y = new X from {5}
assert BinaryOperation(symbol ===, ?? x, null)
assert BinaryOperation(symbol ===, ?? y, y)
assert BinaryOperation(symbol ===, y ?? x, y)
assert BinaryOperation(symbol ===, x ?? y, y)
x ??= y
assert BinaryOperation(symbol ===, x, y)

-- issue #3612
h = new HashTable from { symbol cache => new CacheTable };
h.cache ??= new CacheTable
