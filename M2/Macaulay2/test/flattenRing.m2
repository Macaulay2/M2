-- test flattenRing

A = QQ[x,y]
(S,p,q) = flattenRing A
describe S
assert( S === A )
B = A/x^5
(S,p,q) = flattenRing B
describe S
assert( S === B )
C = B[t,u]
(S,p,q) = flattenRing C
describe S
assert( numgens S == 4 )
use ring ideal S
assert( ideal S == ideal( x^5 ) )
use C
D = C/t^6
(S,p,q) = flattenRing D
describe S
use ring ideal S
assert( ideal S == ideal( t^6, x^5 ) )


R = ZZ/101
(S,p,q) = flattenRing R
describe S
assert( R === S )

(S,p,q) = flattenRing(R,CoefficientRing => ZZ)
describe S
assert( R === S )

E = ZZ/5[c]/(c^2+c+1)
k = GF(E,PrimitiveElement => - c - 2)
(S,p,q) = flattenRing E
describe S
assert( S === E )

F = k[t]/t^10
(S,p,q) = flattenRing F
describe S
assert( S === F )

(S,p,q) = flattenRing(F, CoefficientRing => ZZ/5)
describe S
use ring ideal S
assert( ideal S == ideal (t^10,c^2+c+1) )

(S,p,q) = flattenRing(F, CoefficientRing => ZZ)
describe S
use ring ideal S
assert( ideal S == ideal (5,t^10,c^2+c+1) )
