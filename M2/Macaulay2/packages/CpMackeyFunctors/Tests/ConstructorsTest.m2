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

-- makeKGroupMackeyFunctor was previously untested.
TEST ///
assertLevel = 0;
-- K_n(F_q) of a degree-p extension is a Mackey functor with finite
-- Underlying = ZZ/(q^(np)-1) and Fixed = ZZ/(q^n-1).
KG := makeKGroupMackeyFunctor(2, 3, 1);
assert(isWellDefined KG);
assert(KG.PrimeOrder == 2);
assert(isCohomological KG);
-- both summands are torsion, so the free rank is zero.
assert(rank KG.Underlying == 0);
assert(rank KG.Fixed == 0);
-- p=2, q=3, n=1: Underlying = ZZ/(3^2-1) = ZZ/8, Fixed = ZZ/(3-1) = ZZ/2.
assert(prune KG.Underlying == cokernel matrix {{8}});
assert(prune KG.Fixed == cokernel matrix {{2}});
-- input validation
assert(try (makeKGroupMackeyFunctor(4, 3, 1); false) else true);  -- p not prime
assert(try (makeKGroupMackeyFunctor(2, 6, 1); false) else true);  -- q not a prime power
assert(try (makeKGroupMackeyFunctor(2, 3, 0); false) else true);  -- n < 1
///

-- makeZeroOnUnderlyingMackeyFunctor was only exercised indirectly.
TEST ///
assertLevel = 0;
-- For a module N, makeZeroOnUnderlyingMackeyFunctor(p, N) builds a
-- cohomological Mackey functor whose underlying part is 0 and fixed
-- part is N.
N := cokernel matrix {{3}};
M := makeZeroOnUnderlyingMackeyFunctor(3, N);
assert(isWellDefined M);
assert(isCohomological M);
assert(M.PrimeOrder == 3);
assert(prune M.Underlying == 0);
assert(prune M.Fixed == N);
///