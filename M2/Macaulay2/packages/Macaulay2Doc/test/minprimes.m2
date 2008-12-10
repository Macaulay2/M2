debugLevel = 1 -- I installed some error detection in minimalPrimes.  This turns it on.
R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4)
time S = integralClosure R
assert isHomogeneous S
ideal S

-- i8 : ideal S
-- 
--              2                    2         2    2    2
-- o8 = ideal (x  - w z, w x - w z, w  - w x, w  - y  - z )
--                   6    6     7    6    7    7

use ring ideal S
assert( ideal S == ideal(x^2-w_6*z,w_6*x-w_7*z,w_6^2-w_7*x,w_7^2-y^2-z^2) ) -- this may change, but hopefully, not by much
trim ideal S
assert( 4 == numgens trim ideal S )

R = QQ[w_1, w_2, x, y, z, Degrees => {3,3,1,1,1}, MonomialOrder => GRevLex => 5]
I = ideal (x,y^2*z+z^3,w_2*z,w_1*z,w_2^2,w_1*w_2,w_1^2)
assert isHomogeneous I
m = minimalPrimes I
assert all(m, isHomogeneous)
assert( intersect m == radical I )
