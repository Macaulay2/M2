R = QQ[x_0..x_4]
a = {1,0,0,0,0}
b = {0,1,0,0,1}
c = {0,0,1,1,0}
M1 = matrix table(5,5, (i,j)-> x_((i+j)%5)*a_((i-j)%5))
M2 = matrix table(5,5, (i,j)-> x_((i+j)%5)*b_((i-j)%5))
M3 = matrix table(5,5, (i,j)-> x_((i+j)%5)*c_((i-j)%5))
M = M1 | M2 | M3
betti (C=res coker M)
dim coker M  -- it is a finite length module
hilbertFunction(5, coker M) -- this is past the length
