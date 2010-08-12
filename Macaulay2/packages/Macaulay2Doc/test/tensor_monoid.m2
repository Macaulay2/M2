M = monoid[]
N = monoid[Join=>false]
assert ( degreeLength tensor(tensor(N,M),M) === degreeLength tensor(N,tensor(M,M)) )
