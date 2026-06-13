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

-- derivations of Anick's algebra <a,b>/(aaab,bbba): lieDerivation, innerDerivation, isWellDefined(LieDerivation)
TEST///
L = lieAlgebra{a,b}/{a a a b,b b b a}
da61 = lieDerivation{a b a b a b a,0_L}
db61 = lieDerivation{0_L,a b a b a b a}
da62 = lieDerivation{b b a b a b a,0_L}
db62 = lieDerivation{0_L,b b a b a b a}
assert(isWellDefined(4,da61))
assert(not isWellDefined(4,db61))
assert(not isWellDefined(4,da62))
assert(isWellDefined(4,db62))
assert(da61 + db62 === innerDerivation(b a b a b a))
da7 = lieDerivation{b a b a b a b a,0_L}
db7 = lieDerivation{0_L,b a b a b a b a}
assert(isWellDefined(4,da7))
assert(isWellDefined(4,db7))
assert(da7 === innerDerivation(b b a b a b a))
assert(db7 === innerDerivation(a b a b a b a))
///

-- subspace constructors: lieSubAlgebra (closed under bracket) vs lieSubSpace (linear span)
TEST///
L = lieAlgebra{a,b,c}
A = lieSubAlgebra{a,b c}
S = lieSubSpace{a,b c}
assert(dims(1,4,A) === {1,1,1,1})
assert(dims(1,4,S) === {1,1,0,0})
///

-- ideal sum (+) and intersection (@); property test: dim(I+J)+dim(I@J) = dim(I)+dim(J)
TEST///
L = lieAlgebra{a,b,c}
I = lieIdeal{a b}
J = lieIdeal{b c}
T = I+J
U = I@J
assert(dims(1,5,I) === {0,1,3,8,24})
assert(dims(1,5,J) === {0,1,3,8,24})
assert(dims(1,5,T) === {0,2,6,15,42})
assert(dims(1,5,U) === {0,0,0,1,6})
assert(dims(1,5,T) + dims(1,5,U) === dims(1,5,I) + dims(1,5,J))
///

-- zeroIdeal: the zero ideal {0_L} (boundary case), built two ways
TEST///
L = lieAlgebra{a,b}
Z = zeroIdeal L
assert(Z === lieIdeal{0_L,a a})
assert(dims(1,5,Z) === {0,0,0,0,0})
///

-- quotient(LieIdeal,FGLieSubAlgebra) and ideal membership
TEST///
L = lieAlgebra{a,b,c}
I = lieIdeal{a a c+b a c-a b a,c c a-b b a}
M = L/I
J = lieIdeal{a b}
A = quotient(J,lieSubAlgebra{a c})
assert(dims(1,3,A) === {2,3,5})
assert(member((c b) (a c),J))
///

-- koszulDual(QQ[x]) and the Koszul-duality round-trip: the Ext-algebra is the identity matrix
TEST///
R = QQ[x]
L = koszulDual R
assert(dims(1,6,L) === {1,0,0,0,0,0})
E = extAlgebra(4,L)
assert(#gens E === 4)
assert(dims(4,E) == matrix{{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}})
assert(weight(ext_0 ext_0 ext_0 ext_0) === {4,4})
///

-- Lech's non-Koszul algebra: its Ext-algebra carries a tell-tale off-diagonal entry
TEST///
R = QQ[x,y,z,u]
I = {x^2,y^2,z^2,u^2,x*y+z*u}
S = R/I
L = koszulDual S
E = extAlgebra(4,L)
assert(dims(4,E) == matrix{{4,0,0,0},{0,5,0,0},{0,0,0,5},{0,0,0,0}})
///

-- normalForm of a formal-operator (@, ++, /) expression that reduces to 0
TEST///
L = lieAlgebra{a,b,c}
assert(normalForm(a@b@c++3@a@c@b++2@c@b@a/2@b@c@a) === 0_L)
///

-- holonomyLocal: a local Lie algebra of a holonomy Lie algebra is free (here on 2 generators)
TEST///
L = holonomy({{a2,a3},{a4,a5}},{{a2,a4,a6}})
free2 = dims(1,6,lieAlgebra{x,y})
assert(dims(1,6,holonomyLocal(0,L)) === free2)
assert(dims(1,6,holonomyLocal(1,L)) === free2)
hl2 = dims(1,6,holonomyLocal(2,L))
assert(hl2#0 === 3)
assert(drop(hl2,1) === drop(free2,1))
///

-- decompose: the ideal that vanishes exactly when a holonomy Lie algebra is the direct sum of its local algebras
TEST///
L1 = holonomy({{a2,a3},{a4,a5}},{{a2,a4,a6}})
assert(dims(1,4,decompose L1) === {0,0,0,0})
Q = holonomy({{a1,a2,a3},{a1,a4,a5},{a2,a4,a6},{a3,a5,a6}})
dQ = decompose Q
assert(dims(1,5,dQ) === {0,0,2,9,30})
assert(#basis(3,dQ) === 2)
///

-- map(LieAlgebra,LieAlgebra,List) and isIsomorphism: a generator permutation gives an
-- automorphism of the quadrangle holonomy algebra for (231645) but not for (231564)
TEST///
Q = holonomy({{a1,a2,a3},{a1,a4,a5},{a2,a4,a6},{a3,a5,a6}})
assert(isIsomorphism map(Q,Q,{a2,a3,a1,a6,a4,a5}))
assert(not isIsomorphism map(Q,Q,{a2,a3,a1,a5,a6,a4}))
///

-- zeroMap (the zero homomorphism) and zeroDerivation (the zero derivation)
TEST///
L = lieAlgebra{a,b}
M = lieAlgebra{a,b}
zm = zeroMap(M,L)
assert(zm === map(M,L,{0_M,0_M}))
assert(isWellDefined(2,zm))
use L
assert(zm a === 0_M)
assert(differential L === zeroDerivation L)
///

-- center: the ideal of central elements of a Lie algebra
TEST///
L = lieAlgebra{a,b,c}/{a b,a c,c b c,b b c b}
assert(dims(1,5,center L) === {1,0,1,0,0})
///

-- firstDegree (degree of an element or derivation) and computedDegree (degree computed so far)
TEST///
L = lieAlgebra({a,b,c},Weights=>{1,2,3})
assert(dims(1,3,L) === {1,1,2})
assert(computedDegree L === 3)
assert(firstDegree(c a b+a b c) === 6)
assert(firstDegree(lieDerivation{a c b,b b c,c c b}) === 5)
///

-- lieRing and mbRing: the internal polynomial rings representing a Lie algebra
TEST///
L = lieAlgebra{a,b,c}/{a b-a c}
dims(1,5,L)
assert(instance(L#cache.lieRing, PolynomialRing))
assert(instance(L#cache.mbRing, PolynomialRing))
///

-- listMultiply: pairwise products of two lists; row i is x#i bracketed with every element of y
TEST///
K = lieAlgebra{a,b,c}
b1 = basis(1,K)
b2 = basis(2,K)
lm = listMultiply(b1,b2)
assert(#lm === #b1)
assert(#(lm#0) === #b2)
assert(lm#0#0 === b1#0 b2#0)
///

-- VectorSpace: the parent class of the subspace/ideal hierarchy and of Lie homology
TEST///
L = lieAlgebra{a,b}
assert(instance(lieIdeal{a}, VectorSpace))
assert(instance(lieHomology L, VectorSpace))
///

-- Signs option: an even generator has [a,a]=0, an odd one (Signs=>1) has [a,a]=!=0
TEST///
Leven = lieAlgebra{a}
assert(a a === 0_Leven)
assert(dims(1,3,Leven) === {1,0,0})
Lodd = lieAlgebra({a},Signs=>1)
assert(a a =!= 0_Lodd)
assert(dims(1,3,Lodd) === {1,1,0})
///

-- Weights option: a generator of weight w first appears in degree w, changing the dimensions
TEST///
dw = dims(1,4,lieAlgebra({a,b},Weights=>{1,2}))
du = dims(1,4,lieAlgebra{a,b})
assert(dw === {1,1,1,1})
assert(du === {2,1,2,3})
assert(dw =!= du)
///

-- Field option: the relation 3[a,b] is nontrivial over QQ but vacuous over ZZ/3
TEST///
Lq = lieAlgebra{a,b}/{3 a b}
Lf = lieAlgebra({a,b},Field=>ZZ/3)/{3 a b}
assert(dims(1,3,Lq) === {2,0,0})
assert(dims(1,3,Lf) === {2,1,2})
///

-- characteristic 2: the axiom [a,a]=0 holds even for an odd (Signs=>1) generator
TEST///
L = lieAlgebra({a},Field=>ZZ/2,Signs=>1)
assert(a a === 0_L)
assert(dims(1,4,L) === {1,0,0,0})
///
