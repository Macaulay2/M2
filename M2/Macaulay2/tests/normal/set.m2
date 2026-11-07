-- TODO: add more set-related tests here
assert(union {} == set {})

-- partition
someprimes = for i from 1 to 10 list nextPrime(100 * i);
data = apply(someprimes, p -> {p, legendreSymbol(-1, p), p % 4});

-- -1 is a quadratic residue mod p iff p % 4 == 1
result = partition(row -> row_1, data, row -> row_2)
assert(all(result#1, r -> r % 4 == 1))
assert(all(result#(-1), r -> r % 4 == 3))
