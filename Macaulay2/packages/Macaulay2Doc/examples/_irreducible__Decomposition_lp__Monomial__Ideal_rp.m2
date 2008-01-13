QQ[x..z];
I = monomialIdeal (x*y^3, x*y^2*z)
w = irreducibleDecomposition I
assert( I == intersect w )
