-- taken from:
-- Bernd Sturmfels, FOUR COUNTEREXAMPLES IN COMBINATORIAL ALGEBRAIC GEOMETRY.

-- warning : these tests require 84MB of memory, so if your machine doesn't
-- have that, just skip them

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
