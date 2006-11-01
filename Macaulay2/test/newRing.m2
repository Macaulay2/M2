R = ZZ/101[a,b,Degrees=>{{1,2},{3,4}}]
A = newRing(R, DegreeRank=>1)
assert(degrees A == {{1}, {1}})
B = newRing(R, DegreeRank=>1, Degrees=>{1,1})
assert(degrees B == {{1}, {1}})
C = newRing(R, Degrees=>{1,1})
assert(degrees C == {{1}, {1}})

