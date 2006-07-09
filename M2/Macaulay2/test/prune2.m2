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
     f  * random(ZZ^3,ZZ^4,MaximalRank => true),
     g * random(ZZ^3,ZZ^4,MaximalRank => true)
     )	-- isomorphic to ZZ ++ ZZ/77 ++ ZZ/7
N = prune M
assert ( ideal(0) == fittingIdeal_0 N)
assert ( ideal(7*7*11) == fittingIdeal_1 N)
assert ( ideal(7) == fittingIdeal_2 N)
assert ( ideal(1) == fittingIdeal_3 N)
assert ( rank M == 1 )
