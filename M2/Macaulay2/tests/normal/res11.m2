inc = random(ZZ^11, ZZ^4, MaximalRank => true)
M = subquotient(
     inc * diagonalMatrix {4,2,5     ,3  } * random(ZZ^4,ZZ^5,MaximalRank => true),
     inc * diagonalMatrix {0,0,5*7*11,3*7} * random(ZZ^4,ZZ^5,MaximalRank => true)
     )	-- isomorphic to ZZ^2 ++ ZZ/77 ++ ZZ/7
C = res M
assert (length C <= 1)
assert isIsomorphism map(M,HH_0 C,id_(cover M))
assert ( HH_1 C == 0 )


--
R = QQ[x]
M = coker matrix(R, {{1},{0}})
C = res(M, Strategy=>2)  -- this one fails too...
C = res M
HH_0 C
assert ( M == HH_0 C )

M = coker matrix(R, {{0},{1}})
C = res M
HH_0 C
assert ( M == HH_0 C )
