A = QQ[a,b]
B = QQ[x,y,z]
C = tensor(A,B,Degrees=>{{1,1},{1,2},{1,3},{0,1},{1,-3}},DegreeRank=>2)

-- tensor products
A = QQ[vars(0..2)]
B = QQ[x,y,Degrees=>{2:{0,1}}]
A ** B
B ** A
tensor(A,B,Degrees=>{3:{1,2,3}})  -- error

A = QQ[vars(0..2),DegreeRank=>3]
describe A
toExternalString A
A = QQ[vars(0..2),DegreeRank=>2]
A ** A
tensor(A,A,MonomialOrder=>Eliminate numgens A)
tensor(A,A,DegreeRank=>6)  -- error
