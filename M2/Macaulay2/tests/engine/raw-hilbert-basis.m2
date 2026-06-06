debug Core

assert(map(ZZ, rawHilbertBasis raw matrix {{1, 3}, {2, 1}}) == matrix {{1, 1}, {1, 2}, {1, 3}, {2, 1}})

m = matrix {
    {0, 0, 0, 1, 0, 1, 0, 1},
    {1, 0, 0, 0, 0, 0, 0, 0},
    {0, 1, 0, 0, 0, 0, 0, 0},
    {0, 0, 1, 1, 0, 0, 0, 0},
    {0, 0, 0, 0, 1, 1, 0, 0},
    {0, 0, 0, 0, 0, 0, 1, 1}};
assert(rank target rawHilbertBasis raw m == 6)

m = matrix {
    {0, 1, 0, 0, 0, 0, 0, 0},
    {0, 0, 1, 0, 0, 0, 0, 0},
    {0, 0, 0, 1, 0, 0, 0, 0},
    {0, 0, 0, 0, 1, 0, 0, 0},
    {0, 0, 0, 0, 0, 1, 0, 0},
    {1, 0, 0, 0, 0, 15, 0, 0},
    {0, 0, 0, 0, 0, 0, 1, 0},
    {1, 0, 0, 0, 0, 0, 15, 0},
    {0, 0, 0, 0, 0, 0, 0, 1},
    {1, 0, 0, 0, 0, 0, 0, 15}};
assert(rank target rawHilbertBasis raw m == 143)

end--

restart
debug Core
needsPackage "Normaliz"
needsPackage "FourTiTwo"
needsPackage "Polyhedra"

A = transpose map(ZZ, rawHilbertBasis raw m)
B = hilbertBasis(coneFromVData transpose m, InputType => "lattice") / vector // matrix
assert(A_(sortColumns A) == B_(sortColumns B))

-- TODO: add test with non-trivial hyperplanes
C = coneFromVData transpose m
A' = facets C * transpose map(ZZ, rawHilbertBasis raw transpose rays C)
B' = transpose hilbertBasis(transpose facets C, InputType=>"lattice")
assert(A_(sortColumns A) == B_(sortColumns B))

-- benchmarking 3 different methods
C = coneFromVData transpose m
benchmark "facets C * transpose (normaliz(transpose rays C, \"cone\"))#\"gen\""  -- ~0.02
benchmark "facets C * transpose map(ZZ, rawHilbertBasis(raw transpose rays C))"  -- ~0.003
benchmark "transpose hilbertBasis(transpose facets C, InputType => \"lattice\")" -- ~0.01
