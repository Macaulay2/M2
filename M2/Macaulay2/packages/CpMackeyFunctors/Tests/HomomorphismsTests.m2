TEST ///
assertLevel = 0;

-- verify the zero map to the zero Mackey functor is well-defined

A := makeBurnsideMackeyFunctor(7);
Z := makeZeroMackeyFunctor(7);

fixedLevelMap:=map(Z.Fixed,A.Fixed,0);
underlyingLevelMap:=map(Z.Underlying,A.Underlying,0);
F := map(Z,A,underlyingLevelMap,fixedLevelMap);
assert (isWellDefined F);

-- verify that the linearization maps are well-defined
assert( isWellDefined complexLinearizationMap(5))
assert( isWellDefined realLinearizationMap(5))

-- This homomorphism is not well-defined!
assert not isWellDefined map(A,A,matrix {{2}},matrix {{3,0},{0,3}});

-- verify composition does what we want it to do ?
A :=makeBurnsideMackeyFunctor(11);
assert (isWellDefined (id_(A) * id_(A)))
assert (id_(A) * id_(A) === id_(A))

assert (isWellDefined (complexLinearizationMap(11) * id_(A)));


-- test isTrivialMackeyFunctor
assert not (isTrivialMackeyFunctor(makeBurnsideMackeyFunctor(7)));
assert isTrivialMackeyFunctor(makeZeroMackeyFunctor(3));

-- test isIsomorphism
assert not (isIsomorphism(realLinearizationMap(7)));
assert not (isIsomorphism(realLinearizationMap(11)));
assert isIsomorphism(realLinearizationMap(2));
assert isIsomorphism(realLinearizationMap(3));

-- Checking direct sum of homomorphisms
A = makeBurnsideMackeyFunctor 2;
U = makeUnderlyingFreeMackeyFunctor 2;
f = map(U, A, matrix {{2},{2}}, matrix {{2,4}});
assert( isWellDefined directSum({f,id_U,f}));

-- Checking arithmetic of homomorphisms
h = id_A;
assert(h + h + h == 3 * h)
assert(-h == (-1)*h)
assert(h-h == 0*h)

///