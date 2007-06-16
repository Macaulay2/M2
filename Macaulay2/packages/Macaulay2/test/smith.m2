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


(g,t,s) = smithNormalForm ( f = random(ZZ^15,ZZ^3) * matrix "14,,;,140,;,,1260" * random(ZZ^3,ZZ^20) );
g
assert ( t*f*s == g )


(g,t,s) = smithNormalForm ( f = random(ZZ^15,ZZ^3,MaximalRank=>true) * matrix "14,,;,140,;,,1260" * random(ZZ^3,ZZ^20,MaximalRank=>true) );
g
assert ( t*f*s == g )
assert ( # pivots g == 3 )

R = QQ[x]
time (g,t,s) = smithNormalForm ( f = random(R^8,R^3,MaximalRank=>true) * matrix "14+x,,;,140-x2,;,,1261+2x" * random(R^3,R^10,MaximalRank=>true) );
time assert ( t*f*s == g )
time assert ( # pivots g == 3 )

