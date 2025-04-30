-- global variales
(a,b,c) = (4,5,6)
assert( a == 4 and b == 5 and c == 6 )

-- local variables
g = () -> (
    (a, b, c) := (7, 8, 9);
    assert Equation(a, 7);
    assert Equation(b, 8);
    assert Equation(c, 9))
g()
assert Equation(a, 4)
assert Equation(b, 5)
assert Equation(c, 6)

-- thread local variables
threadLocal d
threadLocal e
threadLocal f
t = schedule(() -> (
	(d, e, f) = (10, 11, 12);
	d == 10 and e == 11 and f == 12))
assert taskResult t
assertNull = x -> assert BinaryOperation(symbol ===, x, null)
assertNull d
assertNull e
assertNull f

-- binary
x = new MutableHashTable
(x.foo, x#0) = (1, 2)
assert Equation(x.foo, 1)
assert Equation(x#0, 2)

X = new Type of HashTable
(X + X, X - X) := ((x, y) -> 3, (x, y) -> 4)
(x, y) = (new X, new X)
assert Equation(x + y, 3)
assert Equation(x - y, 4)

(X + X, X - X) = ((x, y, e) -> 5, (x, y, e) -> 6)
assert Equation(x + y = 7, 5)
assert Equation(x - y = 8, 6)

-- adjacent
foo = method()
(foo ZZ, foo(QQ, RR), X X) := (x -> 9, (x, y) -> 10, (x, y) -> 11)
assert Equation(foo 11, 9)
assert Equation(foo(1/2, 3.14159), 10)
assert Equation(x y, 11)

bar = memoize(x -> x^2)
(bar 0, X X) = (12, (x, y, e) -> 13)
assert Equation(bar 0, 12)
assert Equation(x y = 14, 13)

-- unary
(+X, X~) := (x -> 15, x -> 16)
assert Equation(+x, 15)
assert Equation(x~, 16)

(+X, X~) = ((x, e) -> 17, (x, e) -> 18)
assert Equation(+x = 19, 17)
assert Equation(x~ = 20, 18)

-- new
a = symbol a
(new X, new X from ZZ, new X of X, new X of X from ZZ) := (
    T       -> new T from {a => 1},
    (T,n)   -> new T from {a => n},
    (S,T)   -> new S from {a => 2},
    (S,T,n) -> new S from {a => 2*n})
x = new X
assert Equation(x.a, 1)
x = new X from 3
assert Equation(x.a, 3)
x = new X of X
assert Equation(x.a, 2)
x = new X of X from 3
assert Equation(x.a, 6)

-- recursive parallel assignment
((a, b), {c, d}, [e, f], <|g, h|>) = ((1, 2), (3, 4), (5, 6), (7, 8))
assert Equation(a, 1)
assert Equation(b, 2)
assert Equation(c, 3)
assert Equation(d, 4)
assert Equation(e, 5)
assert Equation(f, 6)
assert Equation(g, 7)
assert Equation(h, 8)

-- null
(a, , , b) = (9, 10, 11, 12)
assert Equation(a, 9)
assert Equation(b, 12)

-- augmented
(x, y, z) = (1, 2, 3)
(x, y, z) += (4, 5, 6)
assert Equation(x, 5)
assert Equation(y, 7)
assert Equation(z, 9)

-- (x) = y should behave like x = y
assert Equation((x) = 5, 5)
assert Equation((x) += 2, 7)
assert Equation((x) = 1:6, 6) -- except when the RHS is a sequence of length 1

-- 0-length LHS
assert Equation(()  = (), ())
assert Equation(() := (), ())
assert Equation(() += (), ())

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test parallel.out"
-- End:
