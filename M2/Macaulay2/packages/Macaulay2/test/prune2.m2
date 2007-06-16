for i to 5 do (
     N = prune (M = coker ( random (ZZ^11, ZZ^3) * matrix "2,,;,3,;,,7" ) ) ;
     assert (numgens source presentation N < 6);
     assert isIsomorphism N.cache.pruningMap ;
     )

--
inc = random(ZZ^11, ZZ^3, MaximalRank => true)
f = inc * matrix "2,,;,5,;,,3"
g = inc * matrix "0,,;,385,;,,21"
M = subquotient(
     f * random(ZZ^3,ZZ^4,MaximalRank => true),
     g * random(ZZ^3,ZZ^4,MaximalRank => true)
     )	-- isomorphic to ZZ ++ ZZ/77 ++ ZZ/7
N = prune M
factor M
assert ( ideal(0) == fittingIdeal_0 N)
assert ( ideal(7*7*11) == fittingIdeal_1 N)
assert ( ideal(7) == fittingIdeal_2 N)
assert ( ideal(1) == fittingIdeal_3 N)
assert ( rank M == 1 )
--
R = ZZ/101[x,MonomialOrder => {Position => Up}]
inc = random(R^6, R^3, MaximalRank => true)
f = inc * diagonalMatrix{x, x-1          ,x^2-2         }
g = inc * diagonalMatrix{0,(x-1)*(x-3),(x^2-2)*(x-3)*(x^2-7)}
M = subquotient(
     f * random(R^3,R^4,MaximalRank => true),
     g * random(R^3,R^4,MaximalRank => true)
     )	-- isomorphic to R ++ R/(x-3) ++ R/(x-3)(x^2-7)
N = prune M
factor M
assert ( ideal(0_R) == fittingIdeal_0 N)
assert ( ideal((x-3)^2*(x^2-7)) == fittingIdeal_1 N)
assert ( ideal(x-3) == fittingIdeal_2 N)
assert ( ideal(1_R) == fittingIdeal_3 N)
assert ( rank M == 1 )
factor M

-- 


R = QQ[x,y]
assert ( R^0 == prune coker id_(R^3) )

R = ZZ[x,y]
assert ( R^0 == prune coker id_(R^3) )

