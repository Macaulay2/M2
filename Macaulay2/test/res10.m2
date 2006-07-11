R = ZZ[x,y]
f = random(R^2,R^{4:-2})
gbTrace = 3
C = res coker f
b = betti C
assert( b == new BettiTally from {(1,{2}) => 4, (2,{4}) => 2, (2,{5}) => 2, (3,{5}) => 2, (0,{0}) => 2} )

