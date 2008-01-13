R = ZZ/101[x,y,z,w];
M = cokernel matrix {{x*y-z^2,y^2-w^2,w^4}}
res(M,SyzygyLimit => 1)
res(M,SyzygyLimit => 2)
res(M,SyzygyLimit => infinity)
