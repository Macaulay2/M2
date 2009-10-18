-- taken from:
-- Bernd Sturmfels, FOUR COUNTEREXAMPLES IN COMBINATORIAL ALGEBRAIC GEOMETRY.

-- 1
R = QQ[a,b,c,d,e,f]
M = ideal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f, b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)
assert( regularity module M == 3 )
assert( regularity module M^2 == 7 )

S = ZZ/2[a,b,c,d,e,f]
M = ideal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f, b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)
assert( regularity module M == 4 )
assert( regularity module M^2 == 7 )

use R
M = ideal(d*e*f,c*e*f,c*d*f,c*d*e,b*e*f,b*c*d,a*c*f,a*d*e)
assert( regularity module M == 3 )
assert( regularity module M^2 == 7 )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test 4a.out"
-- End:

restart
R = QQ[a]
f = id_(QQ^1)
debug Core
rawPromote(raw (R^1), raw f)

