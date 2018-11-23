TEST///
L = lieAlgebra{a,b,c}
assert ({3, 3, 8, 18} == dimsLie(4))
assert(indexFormLie(defLie(mb_{2,0})) == mb_{2,0})
///

TEST///
L = lieAlgebra({a,b}, field => ZZ/5)/{b b b a,a a a b}
assert({2, 1, 2, 1, 2, 1, 2, 1, 2, 2} == dimsLie(10))
///

TEST///
L=lieAlgebra({a,b,c,r3,r4,r42},
         genWeights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},
         genSigns=>{0,0,0,1,1,0},diffl=>true)
L=diffLieAlgebra{L.zz,L.zz,L.zz,a c,a a c,r4 - a r3}
Q=L/{b c - a c,a b,b r4 - a r4}
hL = homologyTableLie 5
M=minmodelLie 5
useLie M
hM = homologyTableLie 5
assert(hL === hM)
///

TEST///
L = lieAlgebra{a,b,c}
Llist = dimTableLie 5
Ilist = idealTableLie(5,{a b + 2 b c})
Q = L/{a b + 2 b c}
Qlist = dimTableLie 5
assert(Llist-Ilist === Qlist)
///
TEST///
R=ZZ/7[x,y,z,u]
I={random(2,R),random(2,R),random(2,R),random(2,R),random(2,R)}
Q=R/I
L=koszulDualLie Q
assert(length extBasisLie(4,3)===5)
///
TEST///
L=lieAlgebra{a,b,c}
I=basisLie 2
Q=L/I
wlist=apply(extBasisLie 3,x->(weightExtLie x)_0-(weightExtLie x)_1)
assert(unique wlist==={0})
///
TEST///
L=lieAlgebra({a,b,c,d},field=>ZZ/13)
rels={randomLie 2,randomLie 2,randomLie 2,randomLie 2};
Q=L/rels
deglist=apply(extBasisLie 3,x->(weightExtLie x)_0)
assert(max deglist===2)
///
TEST///
L=lieAlgebra({a,b,c,r3,r4,r42},
         genWeights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},
         genSigns=>{0,0,0,1,1,0},diffl=>true)
Q=diffLieAlgebra{L.zz,L.zz,L.zz,a c,a a c,r4 - a r3}/{b c - a c,a b,b r4 - a r4}
d=diffLie()
d2=d d
d2list=apply(Q.gensLie,x->d2 x)
assert(unique d2list==={Q.zz})
assert(kernelBasisLie(5,1,d)===cyclesBasisLie(5,1))
///
TEST///
L=holonomyLie({{a1,a2,a3},{a1,a4,a5,a6},{a2,a4,a7}})
dL=dimsLie 5
L1=holonomyLie({{a2,a3},{a4,a5,a6}},{{a2,a4,a7}})
dL1=dimsLie 5
assert(drop(dL,1)===drop(dL1,1))
///