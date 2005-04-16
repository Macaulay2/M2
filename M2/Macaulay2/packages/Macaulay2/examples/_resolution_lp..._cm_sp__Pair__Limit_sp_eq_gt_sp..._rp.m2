R = QQ[x,y,z,w]
M = cokernel matrix {{x*y-z,y^2-w-1,w^4-3}}
res(M, PairLimit => 1)
res(M, PairLimit => 10)
res(M, PairLimit => 20)
