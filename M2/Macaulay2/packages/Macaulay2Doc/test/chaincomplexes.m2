R = QQ[x_0..x_3];
b = betti (random(R^{1,2},R^{0,0,1}))	
C = R^b
assert(b == betti C)
assert( C.dd == 0 ) 

