R = QQ[a..d,SkewCommutative=>true]
I = monomialIdeal(0_R)
assert(gens I == 0)
J = ideal I
J^2 -- also very bad...
