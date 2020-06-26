TEST///
L = lieAlgebra{a,b,c}
assert ({3, 3, 8, 18} == dims(1,4,L))
assert(indexForm(standardForm(mb_{2,0},L)) == mb_{2,0})
///

TEST///
L = lieAlgebra({a,b}, Field => ZZ/5)/{b b b a,a a a b}
assert({2, 1, 2, 1, 2, 1, 2, 1, 2, 2} == dims(1,10,L))
///

TEST///
L=lieAlgebra({a,b,c,r3,r4,r42},
         Weights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},
         Signs=>{0,0,0,1,1,0},
	 LastWeightHomological=>true)
L=differentialLieAlgebra{0_L,0_L,0_L,a c,a a c,r4 - a r3}
Q=L/{b c - a c,a b,b r4 - a r4}
hL = lieHomology Q
dhL = dims(5,hL)
M=minimalModel(5,Q)
hM = lieHomology M
dhM = dims(5,hM)
assert(dhL === dhM) 
///

TEST/// 
L = lieAlgebra{a,b,c}
Llist = dims(5,L)
I = lieIdeal{a b + 2 b c}
Ilist = dims(5,I)
Q = L/I
Qlist = dims(5,Q)
assert(Llist-Ilist === Qlist)
///
TEST///
L = lieAlgebra({a,b,c,d,e}, Field=>ZZ/7)
Q = L/apply(7,i->random(2,L))
assert(unique dims(1,8,Q)==={5,3})
///
TEST///
L=lieAlgebra{a,b,c}
I=basis(2,L)
Q=L/I
E=extAlgebra(3,Q)
bE=join(basis(1,E),basis(2,E),basis(3,E))
wlist=apply(bE,x->(weight x)_0-(weight x)_1)
assert(unique wlist==={0})
///
TEST///
L=lieAlgebra({a,b,c,d},Field=>ZZ/13)
rels={random(2,L),random(2,L),random(2,L),random(2,L)};
Q=L/rels
E=extAlgebra(3,Q)
bE=join(basis(1,E),basis(2,E),basis(3,E))
deglist=apply(bE,x->(weight x)_0)
assert(max deglist===2)
///
TEST///
L=lieAlgebra({a,b,c,r3,r4,r42},
         Weights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},
         Signs=>{0,0,0,1,1,0},LastWeightHomological=>true)
Q=differentialLieAlgebra{0_L,0_L,0_L,a c,a a c,r4 - a r3}/{b c - a c,a b,b r4 - a r4}
C=cycles Q
B=boundaries Q
assert(member((basis(4,C))_1 (basis(4,B))_0,B))
///
TEST///
L=holonomy({{a1,a2,a3},{a1,a4,a5,a6},{a2,a4,a7}})
dL=dims(1,5,L)
L1=holonomy({{a2,a3},{a4,a5,a6}},{{a2,a4,a7}})
dL1=dims(1,5,L1)
assert(drop(dL,1)===drop(dL1,1))
///
TEST///
L=lieAlgebra{a,b}
M=lieAlgebra{a,b,c}/{a a a b,b b b a,a c}
f=map(M,L)
I=kernel f
S=image f
assert(dims(1,8,L/I)===dims(1,8,S))
///
