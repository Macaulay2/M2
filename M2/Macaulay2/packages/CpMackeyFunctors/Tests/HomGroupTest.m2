TEST ///

x := Hom(makeBurnsideMackeyFunctor 3, makeBurnsideMackeyFunctor 3)
assert(isFreeModule prune x and rank x == 2)

y := Hom(makeUnderlyingFreeMackeyFunctor 5, makeUnderlyingFreeMackeyFunctor 5)
assert(isFreeModule prune y and rank y == 5)

z := Hom(makeBurnsideMackeyFunctor 2, makeUnderlyingFreeMackeyFunctor 2)
assert(isFreeModule prune z and rank z == 1)

A := makeBurnsideMackeyFunctor(5)
f := complexLinearizationMap(5)
Hom(A,f)

///