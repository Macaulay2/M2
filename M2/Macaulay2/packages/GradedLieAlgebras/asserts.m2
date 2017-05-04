TEST///
L = lieAlgebra({a,b,c},{})
assert ({3, 3, 8, 18} == computeLie(4))
assert(indexFormLie(defLie(mb_{2,0})) == mb_{2,0})
///

TEST///
L = lieAlgebra({a,b},{[b,b,b,a],[a,a,a,b]}, field => ZZ/5)
assert({2, 1, 2, 1, 2, 1, 2, 1, 2, 2} == computeLie(10))
///

TEST///
L=lieAlgebra({a,b,c,r3,r4,r42},
    {{{1,-1},{[b,c],[a,c]}},[a,b],{{1,-1},{[b,r4],[a,r4]}}},
    genWeights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},
    genDiffs=>{[],[],[],[a,c],[a,a,c],{{1,-1},{[r4],[a,r3]}}},
    genSigns=>{0,0,0,1,1,0}) 
hL = homologyLie 5
M=minmodelLie 5
useLie M
hM = homologyLie 5
assert(hL === hM)
///

TEST///
L = lieAlgebra({a,b,c},{})
Llist = computeLie 5
Ilist = idealLie(5,{{{1,2}, {[a,b],[b,c]}}})
Q = lieAlgebra({a,b,c},{{{1,2}, {[a,b],[b,c]}}})
Qlist = computeLie 5
assert(Llist-Ilist === Qlist)
///
TEST///
R=QQ[x,y,z,u]
I={random(4,R),random(4,R),random(4,R),random(4,R)}
Q=R/I
L=koszulDualLie Q
assert(dimLie 2===0)
///
TEST///
L=lieAlgebra({a,b,c},{})
I=basisLie 2
LQ=lieAlgebra({a,b,c},I)
minmodelLie 3
wlist=apply(gens LQ.cache.extAlgRing,x->(degree x)_0-(degree x)_1)
assert(unique wlist==={0})
///
TEST///
L=lieAlgebra({a,b,c,d},{},field=>ZZ/13)
rels=rels={randomLie({2,0}),randomLie({2,0}),randomLie({2,0}),randomLie({2,0})};
LQ=lieAlgebra({a,b,c,d},rels,field=>ZZ/13)
minmodelLie 3
deglist=apply(gens LQ.cache.extAlgRing,x->(degree x)_0)
assert(max deglist===2)
///
TEST///
L=lieAlgebra({a,b,c,r3,r4,r42},
    {{{1,-1},{[b,c],[a,c]}},[a,b],{{1,-1},{[b,r4],[a,r4]}}},
    genWeights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},
    genDiffs=>{[],[],[],[a,c],[a,a,c],{{1,-1},{[r4],[a,r3]}}},
    genSigns=>{0,0,0,1,1,0}) 
d=diffLie()
d2=multDerLie(d,d)
d2list=apply(L.gensLie,x->evalDerLie(d2,[x]))
assert(unique d2list==={[]})
///