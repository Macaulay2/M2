-- test flattenRing

A = QQ[x,y]
(S,p) = flattenRing A
q = p^-1
assert(q^-1 === p)
assert( S === target p and A === source p )
assert( S === source q and A === target q )
assert( p * q === id_S )
assert( q * p === id_A )
describe S
assert( S === A )

B = A/x^5
(S,p) = flattenRing B
q = p^-1
assert(q^-1 === p)
assert( S === target p and B === source p )
assert( S === source q and B === target q )
assert( p * q === id_S )
assert( q * p === id_B )
describe S
assert( S === B )
C = B[t,u]
(S,p) = flattenRing C
q = p^-1
assert(q^-1 === p)
assert( S === target p and C === source p )
assert( S === source q and C === target q )
assert( p * q === id_S )
assert( q * p === id_C )
describe S
assert( numgens S == 4 )
use ring ideal S
assert( ideal S == ideal( x^5 ) )
use C
D = C/t^6
(S,p) = flattenRing D
q = p^-1
assert(q^-1 === p)
assert( S === target p and D === source p )
assert( S === source q and D === target q )
assert( p * q === id_S )
assert( q * p === id_D )
describe S
use ring ideal S
assert( ideal S == ideal( t^6, x^5 ) )


R = ZZ/101
(S,p) = flattenRing R
q = p^-1
assert(q^-1 === p)
assert( S === target p and R === source p )
assert( S === source q and R === target q )
assert( p * q === id_S )
assert( q * p === id_R )
describe S
assert( R === S )

(S,p) = flattenRing(R,CoefficientRing => ZZ)
q = p^-1
assert(q^-1 === p)
assert( S === target p and R === source p )
assert( S === source q and R === target q )
assert( p * q === id_S )
assert( q * p === id_R )
describe S
assert( R === S )

E = ZZ/5[c]/(c^2+c+1)
(S,p) = flattenRing E
q = p^-1
assert(q^-1 === p)
assert( S === target p and E === source p )
assert( S === source q and E === target q )
assert( p * q === id_S )
assert( q * p === id_E )
describe S
assert( S === E )

allGenerators := R -> gens(R,CoefficientRing=>ZZ)

k = GF(E,PrimitiveElement => - c - 2)
(S,p) = flattenRing k
q = p^-1
assert(q^-1 === p)
assert( S === target p and k === source p )
assert( S === source q and k === target q )
scan(allGenerators k, x -> assert( q p x == x ) )
scan(allGenerators S, x -> assert( p q x == x ) )
describe S
assert( S === E )

F = k[t]/t^10
(S,p) = flattenRing F
q = p^-1
assert(q^-1 === p)
assert( S === target p and F === source p )
assert( S === source q and F === target q )
scan(allGenerators k, x -> assert( q p x == x ) )
scan(allGenerators S, x -> assert( p q x == x ) )
describe S
assert( S === F )

(S,p) = flattenRing(F, CoefficientRing => ZZ/5)
q = p^-1
assert(q^-1 === p)
assert( S === target p and F === source p )
assert( S === source q and F === target q )
scan(allGenerators k, x -> assert( q p x == x ) )
scan(allGenerators S, x -> assert( p q x == x ) )
describe S
use ring ideal S
assert( ideal S == ideal (t^10,c^2+c+1) )

(S,p) = flattenRing(F, CoefficientRing => ZZ)
q = p^-1
assert(q^-1 === p)
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

-- check degrees

assert ( {{2}} === degrees flattenRing(QQ[a][DegreeMap=>x->2*x,Join=>false],Result=>Ring) )


-- quotients of quotients

A = QQ[x]
(S,p) = flattenRing A
assert( S === A )
assert( matrix p == matrix {{x}} )
B = A/x^5
(S,p) = flattenRing B
assert( ambient S === A )
assert( ideal S == ideal A_0^5 )
C = B/x^3
(S,p) = flattenRing C
assert( ambient S === A )
assert( ideal S == ideal A_0^3 )

-- Result option

A = QQ[x]
I = ideal x^5
B = A/I
assert( B === flattenRing(B,Result=>null) )
assert( B === flattenRing(B,Result=>Thing) )
assert( B === flattenRing(B,Result=>Ring) )
assert( I == flattenRing(B,Result=>Ideal) )
(J,p,q) = flattenRing(B,Result=>(Ideal,,))
assert( I == J )
assert( target p === A )
assert( source p === B )
assert( target q === B )
assert( source q === A )
assert( I === flattenRing(I,Result=>null) )
assert( I === flattenRing(I,Result=>Thing) )
assert( B === flattenRing(I,Result=>Ring) )
assert( I == flattenRing(I,Result=>Ideal) )
(J,p,q) = flattenRing(I,Result=>(Ideal,,))
assert( J === I )
assert( target p === ring J )
assert( source p === ring J )
assert( target q === ring I )
assert( source q === ring I )
