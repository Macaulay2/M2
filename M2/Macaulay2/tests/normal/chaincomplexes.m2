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


--- The following occur in github issue #1208
-- and were bugs before 25 June 2020.

-- Not fixed yet.
-*
R = QQ[x]
X = R^1 / x^9
Ext^0(id_X,X)
Ext^0(X,id_X)
Tor_0(id_X,X) -- not implemented !?
Tor_0(X,id_X) -- not implemented !?
*-

-- Fixed.
R = QQ[x]
f = Ext^(-1)(id_(R^1),R^1)
assert instance(f, Matrix)
assert(f == map(R^0, R^0, {})) -- returns a module??
assert(Ext^(1)(id_(R^1),R^1) == map(R^0, R^0, {}))

-- Not fixed yet
-*
R = QQ[x]
C = res(R^1)
assert(Hom(C,R^1) != 0) -- zero!?  This is the original bug found
assert(C ** R^1 != 0)
assert(Hom(R^1,C) != 0)
assert(R^1 ** C != 0)
assert(C ** C != 0) -- this one passes!
*-

-- Fixed.
-- LengthLimit
kk = ZZ/32003;
S = kk[w,x,y,z];
F = S[symbol f];
M = cokernel matrix {{f, y^2, x*z, -w*y, -x^2, 0, 0, 0},
                     {0, -x, -w, 0, 0, f, 0, 0}, 
                     {0, z, y, x, w, 0, f, 0}, 
                     {0, 0, 0, z, y, 0, 0, f}, 
                     {-x*y+w*z, 0, 0, 0, 0, -y^3+x*z^2, -w*y^2+x^2*z, -x^3+w^2*y}};
resM = res M;
flattenModule = m -> ((flattenRing ring m)#1) ** m;
M' = flattenModule M;
resM' = res M';
assert((HH_2 resM)==0)
assert((HH_2 resM')==0)
h1 = prune homology resM
h2 = prune homology resM'
-- the following is sort of using that the modules in h1, h2 are cokernel modules (or 0).
-- I suppose that they do not have to be identically equal either, mathematically...
for i from 0 to 4 do (
    relations h1_i == sub(relations h2_i, ring h1)
    )
