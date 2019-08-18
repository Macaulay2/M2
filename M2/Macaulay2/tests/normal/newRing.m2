R = ZZ/101[a,b,Degrees=>{{1,2},{3,4}}]
A = newRing(R, DegreeRank=>1)
assert(degrees A == {{1}, {1}})
B = newRing(R, DegreeRank=>1, Degrees=>{1,1})
assert(degrees B == {{1}, {1}})
C = newRing(R, Degrees=>{1,1})
assert(degrees C == {{1}, {1}})

R = QQ[x,y,z,Heft=>{4}]
assert( (options R).Heft == {4} )
T = newRing(R, Heft => {5})
assert( (options T).Heft == {5} )
T = newRing(R, Heft => {5})
assert( (options T).Heft == {5} )
T = newRing(R, Heft => {5}, Degrees => {2,3,4})
assert( (options T).Heft == {5} )
T = newRing(R, Degrees => {2,3,4})
assert( (options T).Heft == {1} )
T = newRing(R, DegreeRank => 3)
assert( (options T).Heft == {1,1,1} )
T = newRing(R, DegreeRank => 3, Heft=>{1,0,1}) -- invalid heft vector automatically replaced by FourierMotzkin
assert( (options T).Heft == {1,1,1} )
T = newRing(R, DegreeRank => 3, Heft=>{1,2,3}) -- valid heft vector is used
assert( (options T).Heft == {1,2,3} )
