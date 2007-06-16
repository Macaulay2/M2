-- test flattenRing

A = QQ[x,y]
(S,p) = flattenRing A
q = p^-1
assert(q^-1 == p)
assert( S === target p and A === source p )
assert( S === source q and A === target q )
assert( p * q == 1 )
assert( q * p == 1 )
describe S
assert( S === A )

B = A/x^5
(S,p) = flattenRing B
q = p^-1
assert(q^-1 == p)
assert( S === target p and B === source p )
assert( S === source q and B === target q )
assert( p * q == 1 )
assert( q * p == 1 )
describe S
assert( S === B )
C = B[t,u]
(S,p) = flattenRing C
q = p^-1
assert(q^-1 == p)
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
(S,p) = flattenRing D
q = p^-1
assert(q^-1 == p)
assert( S === target p and D === source p )
assert( S === source q and D === target q )
assert( p * q == 1 )
assert( q * p == 1 )
describe S
use ring ideal S
assert( ideal S == ideal( t^6, x^5 ) )


R = ZZ/101
(S,p) = flattenRing R
q = p^-1
assert(q^-1 == p)
assert( S === target p and R === source p )
assert( S === source q and R === target q )
assert( p * q == 1 )
assert( q * p == 1 )
describe S
assert( R === S )

(S,p) = flattenRing(R,CoefficientRing => ZZ)
q = p^-1
assert(q^-1 == p)
assert( S === target p and R === source p )
assert( S === source q and R === target q )
assert( p * q == 1 )
assert( q * p == 1 )
describe S
assert( R === S )

E = ZZ/5[c]/(c^2+c+1)
(S,p) = flattenRing E
q = p^-1
assert(q^-1 == p)
assert( S === target p and E === source p )
assert( S === source q and E === target q )
assert( p * q == 1 )
assert( q * p == 1 )
describe S
assert( S === E )

allGenerators := R -> gens(R,CoefficientRing=>ZZ)

k = GF(E,PrimitiveElement => - c - 2)
(S,p) = flattenRing k
q = p^-1
assert(q^-1 == p)
assert( S === target p and k === source p )
assert( S === source q and k === target q )
scan(allGenerators k, x -> assert( q p x == x ) )
scan(allGenerators S, x -> assert( p q x == x ) )
describe S
assert( S === E )

F = k[t]/t^10
(S,p) = flattenRing F
q = p^-1
assert(q^-1 == p)
assert( S === target p and F === source p )
assert( S === source q and F === target q )
scan(allGenerators k, x -> assert( q p x == x ) )
scan(allGenerators S, x -> assert( p q x == x ) )
describe S
assert( S === F )

(S,p) = flattenRing(F, CoefficientRing => ZZ/5)
q = p^-1
assert(q^-1 == p)
assert( S === target p and F === source p )
assert( S === source q and F === target q )
scan(allGenerators k, x -> assert( q p x == x ) )
scan(allGenerators S, x -> assert( p q x == x ) )
describe S
use ring ideal S
assert( ideal S == ideal (t^10,c^2+c+1) )

(S,p) = flattenRing(F, CoefficientRing => ZZ)
q = p^-1
assert(q^-1 == p)
assert( S === target p and F === source p )
assert( S === source q and F === target q )
scan(allGenerators k, x -> assert( q p x == x ) )
scan(allGenerators S, x -> assert( p q x == x ) )
describe S
use ring ideal S
assert( ideal S == ideal (5,t^10,c^2+c+1) )


-- Why does selectInSubring depend on the degrees instead of just on the
-- elimination ordering?  Perhaps it is intended, and the caveat in the
-- documentation can be strengthened a bit, so the user can predict what will
-- happen.
-- 
--     i14 : R = QQ[x..z, MonomialOrder => Eliminate 2];

R = QQ[x..z, MonomialOrder => Eliminate 2];

--     i16 : selectInSubring(0,vars R)
-- 
--     o16 = | x y z |
-- 
-- 		     1       3
--     o16 : Matrix R  <--- R

p = selectInSubring(1,vars R)

--     i17 : selectInSubring(1,vars R)
-- 
--     o17 = | z |
-- 
-- 		     1       1
--     o17 : Matrix R  <--- R

assert( entries p == {{z}} )

--     i18 : selectInSubring(2,vars R)
-- 
--     o18 = 0
-- 
-- 		     1
--     o18 : Matrix R  <--- 0

R = QQ[x..z, MonomialOrder => Eliminate 2, Degrees => {1,0,1}];

-- 
--     i19 : R = QQ[x..z, MonomialOrder => Eliminate 2, Degrees => {1,0,1}];
--
--     i20 : selectInSubring(0,vars R)
-- 
--     o20 = | x y z |
-- 
-- 		     1       3
--     o20 : Matrix R  <--- R

p' = selectInSubring(1,vars R)

--     i21 : selectInSubring(1,vars R)
-- 
--     o21 = | y z |
-- 
-- 		     1       2
--     o21 : Matrix R  <--- R

assert( entries p' == {{z}} )

--     i22 : selectInSubring(2,vars R)
-- 
--     o22 = 0
-- 
-- 		     1
--     o22 : Matrix R  <--- 0
-- 


-- make sure

L = QQ[x,Degrees=>{0}]
L' = QQ[x]
assert( ker map(L,L') == 0 )
assert( ker map(L',L) == 0 )

-- now check kernels of ringmaps involving these things

assert ( ker map(k,E) == ideal 0_E )
assert ( ker map(E,k) == ideal 0_k )
assert ( ker map(k,E[t]) == ideal t )
assert ( ker map(E,k[t]) == ideal t )
assert ( ker map(k[t],E) == ideal 0_E )
assert ( ker map(E[t],k) == ideal 0_k )
