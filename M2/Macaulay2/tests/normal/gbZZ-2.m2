-- github issue #242

rng = ZZ[x,y,z]
I = ideal(-5*z+4, 6*y-1, 9*z+8)
gI = ideal gens gb I
ggI = ideal gens gb gI
assert(leadTerm ggI_2 % leadTerm gI_2 == 0)
assert(leadTerm gI_2 % leadTerm ggI_2 == 0)
assert(gI_* == {19, z+3, y+3})
assert(ggI_* == {19, z+3, y+3})

-- github issue #243

x = symbol x
rng = ZZ[x_1]
I = ideal(-6*x_1^3+10*x_1-10,-2*x_1^2-x_1,-x_1^3,-4*x_1^3+8*x_1^2)
gI = ideal gens gb I;
assert( ( (gens I)%gI) == 0 ); --failed previous to fix for issue #243.
assert(gI_* == {10, x_1})

rng = ZZ[x_1]
I = ideal(-6*x_1^3-10,-2*x_1^2-x_1,-4*x_1^3)
gI = ideal gens gb I;
assert( ( (gens I)%gI) == 0 ); --failed previous to fix for issue #243.
assert(gI_* == {10, x_1})

-- github issue #244

rng = ZZ[x_1, x_2, x_3 ]
I   = ideal(6*x_3,2*x_3-12,-3*x_1+1)
gI  = ideal gens gb  I;
ggI = ideal gens gb gI;
assert(numColumns (gens gI) == numColumns(gens ggI)); --failed previous to fix for issue #244.
assert(gI_* == {4, 2*x_3, x_1+1})
assert(gI_* == ggI_*)

