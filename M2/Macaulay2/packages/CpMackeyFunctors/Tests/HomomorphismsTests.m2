TEST ///

-- verify the zero map to the zero Mackey functor is well-defined

A := makeBurnsideMackeyFunctor(7);
Z := makeZeroMackeyFunctor(7);

fixedLevelMap:=map(Z.Fixed,A.Fixed,0);
underlyingLevelMap:=map(Z.Underlying,A.Underlying,0);
F := map(Z,A,underlyingLevelMap,fixedLevelMap);
assert (class F === MackeyFunctorHomomorphism)

-- verify that the linearization maps are well-defined
assert( class complexLinearizationMap(5) === MackeyFunctorHomomorphism)
assert( class realLinearizationMap(5) === MackeyFunctorHomomorphism)



-- verify composition does what we want it to do ?
A :=makeBurnsideMackeyFunctor(11);
assert (class(id_(A) * id_(A)) === MackeyFunctorHomomorphism)
assert (id_(A) * id_(A) === id_(A))

assert (class ( complexLinearizationMap(11) * id_(A)) === MackeyFunctorHomomorphism);


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
assert( class(directSum({f,id_U,f})) === MackeyFunctorHomomorphism)

-- Checking arithmetic of homomorphisms
h = id_A;
assert(h + h + h == 3 * h)
assert(-h == (-1)*h)
assert(h-h == 0*h)

///