R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4)
time S = integralClosure R
assert isHomogeneous S
ideal S

-- i8 : ideal S
-- 
--              2                    2         2    2    2
-- o8 = ideal (x  - w z, w x - w z, w  - w x, w  - y  - z )
--                   6    6     7    6    7    7
T = ring ideal S
use T
assert( ideal S == ideal(x^2-T_1*z,T_1*x-T_0*z,T_1^2-T_0*x,T_0^2-y^2-z^2) ) -- this may change and still be correct
trim ideal S
assert( 4 == numgens trim ideal S )			    -- the ideal has swelled since 1.1

R = QQ[w_1, w_2, x, y, z, Degrees => {3,3,1,1,1}, MonomialOrder => GRevLex => 5]
I = ideal (x,y^2*z+z^3,w_2*z,w_1*z,w_2^2,w_1*w_2,w_1^2)
assert isHomogeneous I
m = minimalPrimes I
assert all(m, isHomogeneous)
assert( intersect m == radical I )
