kk=ZZ/101
R=kk[x,y,u,v,SkewCommutative=>true]
i=ideal(x+u*v)
M=coker gens i
F=res(M, LengthLimit=>5)
--The following command causes the system to hang
P = image F.dd_2
xP = ideal vars R * P
P1 = P/xP
presentation P1
prune P1
f = modulo ( gens P, gens xP)
g = matrix {{1, 0, 0, 0, 0, -v, 0, -u, -y, y, -x, x, -x, x, x, -x, 0, 0, 0, 0, 0}, {1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, -1, 1, 0, 0, 1, -v, 0, 0, 0, -u, 0, 0, 0, 0, 0, -y, -x, 0, 0, 0}, {0, 0, 0, -1, 1, 0, 0, 1, 0, 0, 0, -v, 0, 0, 0, 0, 0, 0, -u, -y, -x}}
assert(f-g==0)
