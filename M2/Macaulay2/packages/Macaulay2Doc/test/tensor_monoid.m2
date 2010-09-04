M = monoid[]
N = monoid[Join=>false]
-- Associativity of tensor is important:
-- The left hand operand is the only one where Join is obeyed, as it corresponds to the most recently adjoined
-- monoid in a tower of polynomial rings.
assert ( degreeLength tensor(tensor(N,M),M) === 1 )
assert ( 1 === degreeLength tensor(N,tensor(M,M)) )
assert ( 3 == degreeLength tensor(tensor(M,N),M) )
assert ( 3 == degreeLength tensor(M,tensor(M,N)) )
assert ( 3 == degreeLength tensor(tensor(M,M),N) )
