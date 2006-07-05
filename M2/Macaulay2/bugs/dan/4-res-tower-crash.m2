A = QQ[u,v]
B = A/(u^2-v^2,u*v)
C = B[x,y,z]
I = ideal(u*x+v*y+z)
E = C/I
gbTrace=3
errorDepth=0
C = res(coker vars E, LengthLimit=>5)
betti C
assert ( betti C === new BettiTally from {(2,{2}) => 4, (2,{3}) => 5, (3,{3}) => 4, (3,{4}) => 31, (4,{4}) => 4, (4,{5}) => 101, (5,{5}) => 4, (4,{6}) => 25, (5,{6}) => 239, (5,{7}) => 235, (0,{0}) => 1, (1,{1}) => 3} )
