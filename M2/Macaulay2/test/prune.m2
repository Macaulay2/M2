kk=ZZ/101
R=kk[a,b,c,SkewCommutative=>true]
m=map(R^{-1,0},R^{-2,-1},matrix{{a,0},{b*c,a}})
betti m
F=res(coker m, LengthLimit=>5)
betti F
assert( prune coker F.dd_2 == prune image F.dd_1 )
