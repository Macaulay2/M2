kk = ZZ/101
S = kk[a]

--in the following, "prune" preserves the degree structure
E = S[e_0]
degrees (E^{{1,2}}/ideal(E_0))
degrees prune (E^{{1,2}}/ideal(E_0))
assert ( oo == ooo )

--but in this one "prune" destroys the degree structure
E = S[e_0,SkewCommutative => true]
degrees (E^{{1,2}}/ideal(E_0))
degrees prune (E^{{1,2}}/ideal(E_0))
assert ( oo == ooo )

--some similar examples:
E = S/(ideal vars S)[e_0,SkewCommutative => true]
degrees (E^{{1,2}}/ideal(E_0))
degrees prune (E^{{1,2}}/ideal(E_0))
assert ( oo == ooo )

E = S/(ideal (a^2))[e_0]
degrees (E^{{1,2}}/ideal(E_0))
degrees prune (E^{{1,2}}/ideal(E_0))
assert ( oo == ooo )

E = S/(ideal(a^2))[e_0,SkewCommutative => true]
degrees (E^{{1,2}}/ideal(E_0))
degrees prune (E^{{1,2}}/ideal(E_0))
assert ( oo == ooo )

