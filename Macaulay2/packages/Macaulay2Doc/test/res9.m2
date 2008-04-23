kk = ZZ/101
R = kk[x,y,z,w,Degrees=>{{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}]
I = ideal(x,w)*ideal(y,z)
res I							    -- this crashes in 1.1
