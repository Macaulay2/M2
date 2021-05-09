-- kernel(RingMap) depends on the generators of the graph ideal all being "good"
assert( numgens graphIdeal map(QQ[x],QQ) == 0 )
assert( numgens graphIdeal map(QQ[x],QQ[]) == 0 )

A = QQ[x,Degrees=>{3}]
B = QQ[y,Degrees=>{2}]
f = map(B,A,{11*y^3},DegreeMap => d -> 2*d)
assert isHomogeneous f
J = graphIdeal f
assert isHomogeneous J

A = QQ[x,Degrees=>{3}]
B = QQ[y,Degrees=>{{2,2}},Heft => {0,1}]
f = map(B,A,{11*y^3},DegreeMap => d -> 2*join(d,d))
assert isHomogeneous f
J = graphIdeal f
assert isHomogeneous J
assert( heft B == heft ring J )
assert( degreesRing B === degreesRing ring J )
