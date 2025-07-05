TEST ///

-- Test constructors
assert(class makeBurnsideMackeyFunctor(17) === CpMackeyFunctor);
assert(class makeUnderlyingFreeMackeyFunctor(23) === CpMackeyFunctor);

assert(class makeComplexRepresentationMackeyFunctor(29) === CpMackeyFunctor);
assert(class makeRealRepresentationMackeyFunctor(2) === CpMackeyFunctor);
assert(class makeRealRepresentationMackeyFunctor(47) === CpMackeyFunctor);

assert(class makeZeroMackeyFunctor(3) === CpMackeyFunctor);

assert(class makeFixedPointMackeyFunctor(41,matrix{}) === CpMackeyFunctor);
assert(class makeFixedPointMackeyFunctor(2,matrix{{0,1,0},{1,0,0},{0,0,1}}) === CpMackeyFunctor);

assert(class makeOrbitMackeyFunctor(31,matrix{}) === CpMackeyFunctor);
assert(class makeOrbitMackeyFunctor(2,matrix{{0,1,0},{1,0,0},{0,0,1}}) === CpMackeyFunctor);

assert(class makeFixedTrivMackeyFunctor(5) === CpMackeyFunctor);

assert(class makeFixedSignMackeyFunctor() === CpMackeyFunctor);

///