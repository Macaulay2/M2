BoundedPrimeIterator = new SelfInitializingType of MutableHashTable;
iterator BoundedPrimeIterator := identity;
next BoundedPrimeIterator := i -> (
    q := nextPrime(i#"val" + 1);
    if q > i#"max" then StopIteration else i#"val" = q);
BoundedPrimeIterable = new SelfInitializingType of HashTable;
iterator BoundedPrimeIterable := x -> BoundedPrimeIterator {
    "val" => 1, "max" => x#"max"};
P = BoundedPrimeIterable {"max" => 20};
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
