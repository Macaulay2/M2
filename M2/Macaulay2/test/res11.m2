inc = random(ZZ^11, ZZ^4, MaximalRank => true)
M = subquotient(
     inc * diagonalMatrix {4,2,5     ,3  } * random(ZZ^4,ZZ^5,MaximalRank => true),
     inc * diagonalMatrix {0,0,5*7*11,3*7} * random(ZZ^4,ZZ^5,MaximalRank => true)
     )	-- isomorphic to ZZ^2 ++ ZZ/77 ++ ZZ/7
C = res M
assert (length C <= 1)
