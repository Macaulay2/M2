R = QQ[x,y,z]
m = ideal vars R
M = m^2 / m^3
M_{2,3}

-- gens of rings

k = GF 25
A = k[x,y]
assert( # gens(A) == 2 )
assert( # gens(A,CoefficientRing=>ZZ) == 3 )
assert( # gens(A,CoefficientRing=>ZZ/5) == 3 )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test gens.out"
-- End:
