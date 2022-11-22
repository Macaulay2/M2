BoundedPrimeNumbers = new SelfInitializingType of HashTable
iterator BoundedPrimeNumbers := P -> Iterator(
    q := 1;
    () -> (
	q = nextPrime(q + 1);
	if q > 20 then StopIteration else q))
P = new BoundedPrimeNumbers

i = iterator P
assert Equation(next i, 2)
assert Equation(next i, 3)
assert Equation(next i, 5)
assert Equation(toList P, {2, 3, 5, 7, 11, 13, 17, 19})
assert Equation(toSequence P, (2, 3, 5, 7, 11, 13, 17, 19))
n = 0
scan(P, x -> n = n + x)
assert Equation(n, 77)
assert Equation(scan(P, x -> if x == 11 then break x), 11)
assert Equation(for x in P list x, {2, 3, 5, 7, 11, 13, 17, 19})
assert Equation(for x in P when x < 10 list x, {2, 3, 5, 7})
n = 0
for x in P do n = n + x
assert Equation(n, 77)
assert Equation(for x in P list if x % 4 == 1 then x else continue, {5, 13, 17})
assert Equation(for x in P list if x == 11 then break x, 11)

assert Equation(toList apply(P, x -> x + 1), {3, 4, 6, 8, 12, 14, 18, 20})
assert Equation(toList select(iterator P, x -> x % 4 == 1), {5, 13, 17})

assert Equation(toList accumulate(plus, P), {5, 10, 17, 28, 41, 58, 77})
i = iterator P
assert Equation(toList accumulate(plus, next i, i), {5, 10, 17, 28, 41, 58, 77})
assert Equation(fold(plus, P), 77)
i = iterator P
assert Equation(fold(plus, next i, i), 77)

assert Equation(take(P, 0), {})
assert Equation(take(P, 5), {2, 3, 5, 7, 11})
assert Equation(take(P, 20), {2, 3, 5, 7, 11, 13, 17, 19})

assert Equation(toList iterator {1, 2, 3}, {1, 2, 3})
assert Equation(toList iterator (1, 2, 3), {1, 2, 3})
assert Equation(toList iterator "foo", {"f", "o", "o"})

assert Equation(toList join(iterator {1, 2}, iterator {3, 4}, iterator {5, 6}),
    {1, 2, 3, 4, 5, 6})
assert Equation(toList join(iterator {1, 2}, (3, 4), {5, 6}),
    {1, 2, 3, 4, 5, 6})
assert Equation(toList(iterator {1, 2, 3} | iterator {4, 5, 6}),
    {1, 2, 3, 4, 5, 6})
