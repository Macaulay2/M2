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
