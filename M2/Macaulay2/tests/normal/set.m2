-- TODO: add more set-related tests here
assert(union {} == set {})

-- partition
someprimes = for i from 1 to 10 list nextPrime(100 * i);
data = apply(someprimes, p -> {p, legendreSymbol(-1, p), p % 4});

-- -1 is a quadratic residue mod p iff p % 4 == 1
result = partition(row -> row_1, data, row -> row_2)
assert(all(result#1, r -> r % 4 == 1))
assert(all(result#(-1), r -> r % 4 == 3))

-- alias for partition(List, Function, Function)
result' = partition(data, row -> row_1, row -> row_2)
assert(result === result')

-- some basic test cases from docs
L = {1,3,6,5,3,1,2,8,8,8}
result = partition(L, odd)
assert(result#true == {1,3,5,3,1})
assert(result#false == {6,2,8,8,8})
result' = partition(odd, L)
assert(result === result')

result = partition(set L, odd)
assert(result#true == new Set from {1,3,5})
assert(result#false == new Set from {2,6,8})

result = partition(tally L, odd)
assert(result#true == tally {1,3,5,3,1})
assert(result#false == tally {6,2,8,8,8})
result' = partition(odd, tally L)
assert(result === result')

result = partition(tally L, odd, x -> x > 1)
assert(result#true == tally {false, true, true, true, false})
assert(result#false == tally {true, true, true, true, true})
result' = partition(odd, tally L, x -> x > 1)
assert(result === result')

allfruits = {"apple", "banana", "grape", "avocado", "grape", "grape", "avocado"}
result = partition(tally allfruits, f -> f#0, {"a", "b", "c", "d", "e", "f"})
assert(result#"a" == tally {"apple", "avocado", "avocado"})
assert(result#"b" == tally {"banana"})
assert(result#"c" == tally {})
assert(result#"d" == tally {})
assert(result#"e" == tally {})
assert(result#"f" == tally {})
assert(result#"g" == tally {"grape", "grape", "grape"})

result = partition ({3,3,5}, even, {true,false})
assert(result#true == {})
assert(result#false == {3,3,5})


R = QQ[a..f]
I = ideal"ab,ade,ac3,d4,b3,adf,f4,e10"
result = partition(flatten entries gens I, f -> first degree f)
assert(#result#2 == 1)
assert(#result#3 == 3)
assert(#result#4 == 3)
assert(#result#10 == 1)


