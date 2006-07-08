assert ( 8 == rank coker (random (ZZ^11, ZZ^3, MaximalRank => true) * matrix "2,,;,5,;,,11" * random(ZZ^3,ZZ^5,MaximalRank => true)))
assert ( 8 == rank coker (random (QQ^11, QQ^3, MaximalRank => true) * matrix "2,,;,5,;,,11" * random(QQ^3,QQ^5,MaximalRank => true)))
assert ( 3 == rank (random (ZZ^11, ZZ^3, MaximalRank => true) * matrix "2,,;,5,;,,11" * random(ZZ^3,ZZ^5,MaximalRank => true)))
assert ( 3 == rank (random (QQ^11, QQ^3, MaximalRank => true) * matrix "2,,;,5,;,,11" * random(QQ^3,QQ^5,MaximalRank => true)))

inc = random(ZZ^11, ZZ^3, MaximalRank => true)
M = subquotient(
     inc * matrix "2,,;,5,;,,3"   * random(ZZ^3,ZZ^4,MaximalRank => true),
     inc * matrix "0,,;,385,;,,21" * random(ZZ^3,ZZ^4,MaximalRank => true)
     )	-- isomorphic to ZZ ++ ZZ/77 ++ ZZ/7
prune M

end


R = QQ[x,y]
r = rank coker (random (R^11, R^3, MaximalRank => true) * matrix "x3+2,,;,5,;,,x+1" * random(R^3,R^5,MaximalRank => true))
assert ( 3 == r )
