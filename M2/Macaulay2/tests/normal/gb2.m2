gbTrace = 3
R = ZZ/101[a,b,c,d]
f = matrix{{a*b-c,b^2-d}}
I = ideal f
S = R/I
gb f

use ring I
I : a
--
ZZ[x]
g = gens gb ideal (x,2)
ideal(x^3) : x
assert( g_(0,0) == 2 )					    -- code in 'char' depends on the first entry being in ZZ

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test gb2.out"
-- End:
