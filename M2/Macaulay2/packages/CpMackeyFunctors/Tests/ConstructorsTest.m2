TEST ///
assertLevel = 0;

assert(isWellDefined makeBurnsideMackeyFunctor(17));
assert(isWellDefined makeUnderlyingFreeMackeyFunctor(23));
assert(isWellDefined makeComplexRepresentationMackeyFunctor(29));
assert(isWellDefined makeRealRepresentationMackeyFunctor(2));
assert(isWellDefined makeRealRepresentationMackeyFunctor(47));
assert(isWellDefined makeZeroMackeyFunctor(3));
assert(isWellDefined makeFixedPointMackeyFunctor(41,matrix{}));
assert(isWellDefined makeFixedPointMackeyFunctor(2,matrix{{0,1,0},{1,0,0},{0,0,1}}));
assert(isWellDefined makeOrbitMackeyFunctor(31,matrix{}));
assert(isWellDefined makeOrbitMackeyFunctor(2,matrix{{0,1,0},{1,0,0},{0,0,1}}));
assert(isWellDefined makeFixedTrivMackeyFunctor(5));
assert(isWellDefined makeFixedSignMackeyFunctor());
///