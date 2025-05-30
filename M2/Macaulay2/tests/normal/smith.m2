-- 
-- i2 : (g,t,s) = smithNormalForm ( f = random(ZZ^4,ZZ^2) * matrix "14,;,140" * random(ZZ^2,ZZ^3) )
-- 
-- o2 = (| -420 0  0 |, | 0 14       15       2188    |, | -179551 399   31  |)
--       | 0    14 0 |  | 0 0        -1       -32801  |  | 452     -1    -22 |
--       | 0    0  0 |  | 1 34440910 36736969 3061414 |  | 957592  -2128 -19 |
--       | 0    0  0 |  | 0 -45      -48      -4      |
-- 
-- o2 : Sequence
-- 
-- i3 : t*f*s == g
-- 
-- o3 = true
-- 

setRandomSeed 5						    -- the seed affects the time a lot

(g,t,s) = smithNormalForm ( f = random(ZZ^15,ZZ^3) * matrix "14,,;,140,;,,1260" * random(ZZ^3,ZZ^20) );
g
assert ( t*f*s == g )


(g,t,s) = smithNormalForm ( f = random(ZZ^15,ZZ^3,MaximalRank=>true) * matrix "14,,;,140,;,,1260" * random(ZZ^3,ZZ^20,MaximalRank=>true) );
g
assert ( t*f*s == g )
assert ( # pivots g == 3 )

R = QQ[x]
time (g,t,s) = smithNormalForm ( f = random(R^5,R^3,MaximalRank=>true) * matrix "14+x,,;,140-x2,;,,1261+2x" * random(R^3,R^4,MaximalRank=>true) );
time assert ( t*f*s == g )
time assert ( # pivots g == 3 )

S = QQ [x, MonomialOrder => {Position => Down}]
f = first smithNormalForm(vars S,ChangeMatrix => {true, false})
debug Core
assert( degrees raw source f == degrees source raw f )

chk = M -> (
     (D,P,Q) = smithNormalForm M;
     assert( P*M*Q === D );
     assert( target P === target D );
     assert( target M === source P );
     assert( target Q === source M );
     assert( source Q === source D );
     )
chk vars R
chk matrix {{x^5}}
chk matrix {{x^5,x^7},{0,x^3}}
chk matrix {{x^5,x^7,x^11},{x^2,x^3,x^3},{x^11,x^7,x^2}}


--this test would have failed before 2025
M = matrix{{4,0},{0,6}};
snfM = matrix{{2,0},{0,12}};
assert(first smithNormalForm M == snfM)

--this test also would have failed before 2025
--issue related to calling syz vs trim gens syz
--this example used to output a 2x2 matrix, which was wrong

R = QQ[x]
h = matrix{{x^3+1},{x^2+1}}
chk h

-- this is the beginning of `examples "smithNormalForm"`
M = matrix{{1,2,3},{1,34,45},{2213,1123,6543},{0,0,0}}
(D,P,Q) = smithNormalForm M
assert(D == P * M * Q)
(D,P) = smithNormalForm(M, ChangeMatrix=>{true,false}, KeepZeroes=>false)
assert(numrows D == 3)
D = smithNormalForm(M, ChangeMatrix=>{false,false})
assert(numrows D == numrows M and numcols D == numcols M)
