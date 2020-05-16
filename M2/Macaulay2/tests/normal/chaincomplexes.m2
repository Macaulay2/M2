R = QQ[x_0..x_3];
b = betti (random(R^{1,2},R^{0,0,1}))	
C = R^b
assert(b == betti C)
assert( C.dd == 0 ) 


R=ZZ/31991[x,y,z];
M=random(R^{2:0},R^{3:-1});
cpx=eagonNorthcott M;
assert(isHomogeneous cpx)


R=ZZ/31991[x,y,z];
M=random(R^{2:0},R^{2:-1});
cpx=eagonNorthcott M;
assert(isHomogeneous cpx)
assert(det M ==det cpx.dd_1)



R=ZZ/31991[x,y,x,w];
M=random(R^{2:0},R^{4:-2});
eagonNorthcott M;


R=ZZ/31991[s,t,u,v,Degrees=>{{1,0},{1,0},{0,1},{0,1}}]
M=random(R^{{0,0},{0,0}},R^({{-1,-1},{-1,-1},{-1,-1},{-1,-1}}))
cpx=eagonNorthcott M
assert(isHomogeneous cpx)



R=ZZ/31991[s,t,u,v,Degrees=>{{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}]
M=matrix{{random(R^{{0,0,0,0},{0,0,0,0}},R^{4:{-1,-1,-1,-1}})}}
cpx=eagonNorthcott M
assert(isHomogeneous cpx)


