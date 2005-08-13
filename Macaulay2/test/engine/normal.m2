R = ZZ/101[symbol x..symbol z,Degrees=>{2,5,6}]/(z*y^2-x^5*z-x^8)

time J = integralClosure (R,Variable => symbol b) 
use ring ideal J
ideal J == ideal(b_1*x^2-42*y*z, x^6+12*b_1*y+x^3*z, b_1^2-47*x^4*z-47*x*z^2)
ICfractions R == substitute(matrix {{(42*y*z)/x^2, x, y, z}},frac R)

R = ZZ/101[a..d]
print "BUG: ntegral closure of a poly ring gives error"
--integralClosure R -- BUG

R = ZZ/101[symbol x_1,a..d]
S = R/(x_1^2+a+d)
use S
assert(ring(x_1) === S)
use R
assert(ring(x_1) === R)

-- Bugs found:
-- (1) integralClosure of a poly ring
-- (2) somewhere, indexed variables aren't being set in the correct ring, during a 'use' FIXED
-- (3) ICfractions: ringmaps to fraction fields not yet operational FIXED
-- (4) ICmap, conductor?
-- General problems:
--  The monomial order is not the best possible?
