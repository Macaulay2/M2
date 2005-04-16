R = ZZ/101[x,y,z,w];
M = cokernel matrix {{x*y-z^2,y^2-w^2}}
res(M,DegreeLimit => 1)
res(M,DegreeLimit => 2)
