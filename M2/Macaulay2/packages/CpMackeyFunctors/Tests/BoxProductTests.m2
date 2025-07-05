TEST ///
needsPackage "Isomorphism"

-- Verify that the dimensions match up when box product with Burnside
A = makeBurnsideMackeyFunctor(7)
R = makeComplexRepresentationMackeyFunctor(7)
assert(rank R.Underlying == rank (A**R).Underlying)
assert(rank R.Underlying == rank (R**A).Underlying)
assert(rank R.Fixed == rank (A**R).Fixed)
assert(rank R.Fixed == rank (R**A).Fixed)

-- Verify that fixed modules of A and A ** R are isomorphic
isIsomorphic(R.Fixed, (A ** R).Fixed)

-- Verifying that induced maps on box products are well-defined
L = complexLinearizationMap(7)
assert(class( boxProduct(A,L))===MackeyFunctorHomomorphism)
assert(class( boxProduct(L,A))===MackeyFunctorHomomorphism)

-- A ** A == A. We can see this with prune!
A = makeBurnsideMackeyFunctor 5
B = A ** A
assert(prune B === A)
f = (prune B).cache.pruningMap;
assert (class(f) === MackeyFunctorHomomorphism)
assert (class (inverse f) ===MackeyFunctorHomomorphism)

-- Test induced map on box products
for i to 5 do (
    p = (random {2,3,5,7})#0;
    n = (random toList (0..20))#0;
    M = makeRandomCpMackeyFunctor(p);
    A = makeBurnsideMackeyFunctor(p);
    timesn = map(A,A,n);
    assert (M**timesn == map(M**A,M**A,n));
    assert (timesn**M == map(A**M,A**M,n));
);

///