restart
loadPackage "IntegralClosure"

R = ZZ/101[symbol x..symbol z,Degrees=>{2,5,6}]/(z*y^2-x^5*z-x^8)
time J = integralClosure (R,Variable => symbol b) 
P = icFractions R
ring P

use ring ideal J
answer = ideal(b_1*x^2-y*z, x^6-b_1*y+x^3*z, -b_1^2+x^4*z+x*z^2)
assert(ideal J == answer)
use R
assert(conductor(R.icMap) == ideal(x^2,y))
