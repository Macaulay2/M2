kk = ZZ/101
R = kk[x,y,z,w,Degrees=>{{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}]
I = ideal(x,w)*ideal(y,z)
--old status: this used to crash, but now it gives an error message about the heft vector
--old status: a future version will generate a good heft vector automatically
res I
