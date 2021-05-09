R = ZZ/101[x,y,z]
S = R/(y * (z^2 - x^2 - y*z))
T = singularLocus S
comp = decompose ideal presentation T
use R
assert( 
     comp == { ideal(y,x-z), ideal(y,x+z) } 
     or
     comp == { ideal(y,x+z), ideal(y,x-z) } 
     )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test singular.out"
-- End:
