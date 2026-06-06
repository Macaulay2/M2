-- Small helper method for returning a list of n random elements in a module
getRandomElementsInModule = method()
getRandomElementsInModule(Module, ZZ) := Matrix => (M, n) -> (
    matrix {for i to n-1 list matrix random M}
)

protect GenBound;

-- Make a random Cp-Mackey functor! Input is a prime p, and a list {n,m,k,l}, where
--    n = max number of generators on the free part
--    m = max number of generators on the underlying part
--    k = max number of relations on the free part
--    l = max number of relations on the underlying part
makeRandomCpMackeyFunctor = method(Options=>{GenBound=>5})
makeRandomCpMackeyFunctor(ZZ, List):= CpMackeyFunctor => opts -> (p, L) -> (

    if not isPrime p then error " -- p is not prime!";
    if not (length L === 4) then error " -- expected a list of length 4.";

    n := L#0;
    m := L#1;
    k := L#2;
    l := L#3;

    if not (class n === ZZ and class m === ZZ and class k === ZZ and class l === ZZ) then error " -- not a list of integers.";

    A := makeBurnsideMackeyFunctor p;
    B := makeUnderlyingFreeMackeyFunctor p;

    -- Build the Macky functor B^n + A^m
    X := directSum ((for i to n-1 list B) | (for j to m-1 list A));

    -- Pick random elements in the underlying and fixed modules,
    -- yielding a random map from B^k + A^l
    RandomUnderlyingElements := getRandomElementsInModule(X.Underlying, k);
    RandomFixedElements := getRandomElementsInModule(X.Fixed, l);

    return prune cokernel makeUniversalMap(X, RandomUnderlyingElements, RandomFixedElements)
)

-- Make a random Cp Mackey functor, given an input prime
makeRandomCpMackeyFunctor(ZZ):= CpMackeyFunctor => opts -> p-> (
    n := random(1,opts.GenBound);
    m := random(1,opts.GenBound);
    k := random(1,2*n);
    l := random(1,2*m);
    return makeRandomCpMackeyFunctor(p,{n,m,k,l});
)

makeRandomMackeyFunctorHomomorphism = method()
makeRandomMackeyFunctorHomomorphism(CpMackeyFunctor, CpMackeyFunctor) := MackeyFunctorHomomorphism => (M,N) -> (
    if not M.PrimeOrder === N.PrimeOrder then error " -- prime orders are not the same!";
    homomorphism random Hom(M,N)
)
