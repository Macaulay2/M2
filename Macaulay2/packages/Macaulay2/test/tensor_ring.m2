A = QQ[a,b]
B = QQ[x,y,z]
C = tensor(A,B,Degrees=>{{1,1},{1,2},{1,3},{0,1},{1,-3}},DegreeRank=>2)
assert( degrees C === {{1, 1}, {1, 2}, {1, 3}, {0, 1}, {1, -3}} )

-- tensor products
A = QQ[vars(0..2)]
B = QQ[x,y,Degrees=>{2:{0,1}}]
A ** B
B ** A
tensor(A,B,Degrees=>{5:{1,2,3}})
assert( degrees oo === {{1, 2, 3}, {1, 2, 3}, {1, 2, 3}, {1, 2, 3}, {1, 2, 3}} )

A = QQ[vars(0..2),DegreeRank=>3]
describe A
toExternalString A
A = QQ[vars(0..2),DegreeRank=>2]
D = A ** A
assert( degrees D === {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}, {0, 0, 0, 1}} )
tensor(A,A,MonomialOrder=>Eliminate numgens A)
E = tensor(A,A,DegreeRank=>6)
assert( degrees E === {{1, 0, 0, 0, 0, 0}, {0, 1, 0, 0, 0, 0}, {0, 0, 1, 0, 0, 0}, {0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 0, 1}} )

F = tensor(A,A, Degrees => degrees A | degrees A)
assert( degrees F == {{1, 0}, {0, 1}, {0, 1}, {1, 0}, {0, 1}, {0, 1}} )
