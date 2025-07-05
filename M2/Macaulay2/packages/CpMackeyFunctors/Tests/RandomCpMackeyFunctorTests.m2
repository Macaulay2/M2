TEST ///

-- Make sure we can make a random Cp Mackey functor with the numbers of generators and relations specified
assert (class(makeRandomCpMackeyFunctor(5,{4,3,2,6})) === CpMackeyFunctor)

-- Make sure we can build one with just a prime as input
assert (class( makeRandomCpMackeyFunctor(2)) === CpMackeyFunctor)

-- Make sure we can modify the generator bound
assert (class( makeRandomCpMackeyFunctor(3,GenBound=>6))=== CpMackeyFunctor)

assert (class(makeRandomCpMackeyFunctor(2,{1,2,3,4}) ++ makeRandomCpMackeyFunctor(2,{4,3,2,6}))=== CpMackeyFunctor)

assert (class (makeRandomCpMackeyFunctor(5,GenBound=>6) ++ makeRealRepresentationMackeyFunctor(5))=== CpMackeyFunctor)

assert (class (makeRandomMackeyFunctorHomomorphism(makeRealRepresentationMackeyFunctor(3), makeComplexRepresentationMackeyFunctor(3))) === MackeyFunctorHomomorphism)

assert (class (makeRandomMackeyFunctorHomomorphism(makeRandomCpMackeyFunctor(2,{1,2,3,4}), makeRandomCpMackeyFunctor(2,GenBound=>4) ))=== MackeyFunctorHomomorphism)

assert (class (coker (makeRandomMackeyFunctorHomomorphism(makeRandomCpMackeyFunctor(2,{1,2,3,4}), makeRandomCpMackeyFunctor(2,GenBound=>3) )))=== CpMackeyFunctor)

M = makeBurnsideMackeyFunctor(5);
N = makeUnderlyingFreeMackeyFunctor(5);
P = makeComplexRepresentationMackeyFunctor(5);
f = makeRandomMackeyFunctorHomomorphism(M,N);
g = makeRandomMackeyFunctorHomomorphism(N,P);
assert (class(g*f) === MackeyFunctorHomomorphism)
assert (source(g*f) == M)
assert (target(g*f) == P)

///