TEST ///

p = 2;
F = (cokernel matrix {{84}}) ++ (cokernel matrix {{28}}) 
U = (cokernel matrix {{2}}) ++ module ZZ
r = map(U,F, matrix {{1,1}, {0,0}})
t = map(F,U, matrix {{42,42}, {0,14}})
c = map(U,U, matrix {{1, 0}, {0,-1}})
cursedMF := makeCpMackeyFunctor(p,r,t,c)

d = res(cursedMF,10);


-- check that Tor_i(A,-) and Tor_i(B,-) and Ext^i(A,-) and Ext^i(B,-) are zero for i > 0
A = makeBurnsideMackeyFunctor p;
B = makeBurnsideMackeyFunctor p;
for i from 1 to 5 do (
    M = makeRandomCpMackeyFunctor p;
    assert(isTrivialMackeyFunctor Tor_i(A,M));
    assert(isTrivialMackeyFunctor Tor_i(B,M));
    assert(isTrivialMackeyFunctor Ext^i(A,M));
    assert(isTrivialMackeyFunctor Ext^i(B,M));
)

-- testing Ext
-- check that Ext^0 is InternalHom
M = makeRandomCpMackeyFunctor p;
assert(prune Ext^0(A,A) == prune InternalHom(A,A))
assert(prune Ext^0(A,cursedMF) == prune InternalHom(A,cursedMF))
assert(prune Ext^0(M,cursedMF) == prune InternalHom(M,cursedMF))

///