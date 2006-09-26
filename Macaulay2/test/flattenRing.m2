-- test flattenRing

A = QQ[x,y]
(S,p,q) = flattenRing A
assert( S === target p and A === source p )
assert( S === source q and A === target q )
assert( p * q == 1 )
assert( q * p == 1 )
describe S
assert( S === A )

B = A/x^5
(S,p,q) = flattenRing B
assert( S === target p and B === source p )
assert( S === source q and B === target q )
assert( p * q == 1 )
assert( q * p == 1 )
describe S
assert( S === B )
C = B[t,u]
(S,p,q) = flattenRing C
assert( S === target p and C === source p )
assert( S === source q and C === target q )
assert( p * q == 1 )
assert( q * p == 1 )
describe S
assert( numgens S == 4 )
use ring ideal S
assert( ideal S == ideal( x^5 ) )
use C
D = C/t^6
(S,p,q) = flattenRing D
assert( S === target p and D === source p )
assert( S === source q and D === target q )
assert( p * q == 1 )
assert( q * p == 1 )
describe S
use ring ideal S
assert( ideal S == ideal( t^6, x^5 ) )


R = ZZ/101
(S,p,q) = flattenRing R
assert( S === target p and R === source p )
assert( S === source q and R === target q )
assert( p * q == 1 )
assert( q * p == 1 )
describe S
assert( R === S )

(S,p,q) = flattenRing(R,CoefficientRing => ZZ)
assert( S === target p and R === source p )
assert( S === source q and R === target q )
assert( p * q == 1 )
assert( q * p == 1 )
describe S
assert( R === S )

E = ZZ/5[c]/(c^2+c+1)
(S,p,q) = flattenRing E
assert( S === target p and E === source p )
assert( S === source q and E === target q )
assert( p * q == 1 )
assert( q * p == 1 )
describe S
assert( S === E )

allGenerators := R -> gens(R,CoefficientRing=>ZZ)

k = GF(E,PrimitiveElement => - c - 2)
(S,p,q) = flattenRing k
assert( S === target p and k === source p )
assert( S === source q and k === target q )
scan(allGenerators k, x -> assert( q p x == x ) )
scan(allGenerators S, x -> assert( p q x == x ) )
describe S
assert( S === E )

F = k[t]/t^10
(S,p,q) = flattenRing F
assert( S === target p and F === source p )
assert( S === source q and F === target q )
scan(allGenerators k, x -> assert( q p x == x ) )
scan(allGenerators S, x -> assert( p q x == x ) )
describe S
assert( S === F )

(S,p,q) = flattenRing(F, CoefficientRing => ZZ/5)
assert( S === target p and F === source p )
assert( S === source q and F === target q )
scan(allGenerators k, x -> assert( q p x == x ) )
scan(allGenerators S, x -> assert( p q x == x ) )
describe S
use ring ideal S
assert( ideal S == ideal (t^10,c^2+c+1) )

(S,p,q) = flattenRing(F, CoefficientRing => ZZ)
assert( S === target p and F === source p )
assert( S === source q and F === target q )
scan(allGenerators k, x -> assert( q p x == x ) )
scan(allGenerators S, x -> assert( p q x == x ) )
describe S
use ring ideal S
assert( ideal S == ideal (5,t^10,c^2+c+1) )

-- now check kernels of ringmaps involving these things

assert ( ker map(k,E) == ideal 0_E )
assert ( ker map(E,k) == ideal 0_k )
assert ( ker map(k,E[t]) == ideal t )
assert ( ker map(E,k[t]) == ideal t )
assert ( ker map(k[t],E) == ideal 0_E )
assert ( ker map(E[t],k) == ideal 0_k )
