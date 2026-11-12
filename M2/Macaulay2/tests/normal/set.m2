-- TODO: add more set-related tests here
assert(union {} == set {})

-- partition
someprimes = for i from 1 to 10 list nextPrime(100 * i);
data = apply(someprimes, p -> {p, legendreSymbol(-1, p), p % 4});

-- -1 is a quadratic residue mod p iff p % 4 == 1
result = partition(row -> row_1, data, row -> row_2)
assert(all(result#1, r -> r % 4 == 1))
assert(all(result#(-1), r -> r % 4 == 3))

-- some basic test cases from docs
L = {1,3,6,5,3,1,2,8,8,8}
result = partition(L, odd)
assert(result#true == {1,3,5,3,1})
assert(result#false == {6,2,8,8,8})

result = partition(set L, odd)
assert(result#true == new Set from {1,3,5})
assert(result#false == new Set from {2,6,8})

result = partition(tally L, odd)
assert(result#true == tally {1,3,5,3,1})
assert(result#false == tally {6,2,8,8,8})

result = partition ({3,3,5},even, {true,false})
assert(result#true == {})
assert(result#false == {3,3,5})

R = QQ[a..f]
I = ideal"ab,ade,ac3,d4,b3,adf,f4,e10"
result = partition(flatten entries gens I, f -> first degree f)
assert(#result#2 == 1)
assert(#result#3 == 3)
assert(#result#4 == 3)
assert(#result#10 == 1)
