-- This file is in the public domain

TEST ///
e = cellComplex(QQ,{});
assert(isWellDefined e);
assert(dim e === -infinity);
assert(#maxCells e == 0);
assert(#maxCells skeleton(0,e) == 0);
complex e;
///


TEST ///
v1 = newSimplexCell {};
v2 = newSimplexCell {};
assert(isSimplex v1);
assert(isSimplex v2);
assert(dim v1===0);
assert(dim v2===0);
assert(v1 =!= v2);
assert(cellLabel v1 === 1)
l1 = newSimplexCell {(v1,1),(v2,-1)};
l2 = newSimplexCell {v1,v2};
assert(isSimplex l1);
assert(isSimplex l2);
assert(l1=!=l2);
assert(dim l1==1);
assert(dim l2==1);
C = cellComplex(QQ,{l1,l2});
Ccomplex = complex C;
assert(HH_0(Ccomplex)==0);
assert(prune HH_1(Ccomplex)==QQ^1);
assert(HH_2(Ccomplex)==0);
f1 = newCell {l1,l2};
C = cellComplex(QQ,{f1});
assert(dim C==2);
assert(dim f1==2);
assert(cellLabel f1 == 1);
del0 = boundaryMap(0,C);
del1 = boundaryMap(1,C);
del2 = boundaryMap(2,C);
del100 = boundaryMap(100,C);
assert(del0 == map(QQ^1,QQ^2, {{1,1}}));
assert(rank source del1 == 2);
assert(rank target del1 == 2);
assert(rank del1 == 1);
assert(rank source del2 == 1);
assert(rank target del2 == 2);
assert(rank del2 == 1);
assert(del100 == map(QQ^0,QQ^0,{}));
Ccomplex = complex C;
assert(HH_0(Ccomplex)==0);
assert(HH_1(Ccomplex)==0);
assert(HH_2(Ccomplex)==0);
assert(HH C == HH Ccomplex);
assert(isFree(C));
///

-- RP2
TEST ///
v = newCell {};
l = newCell {v,v};
f = newCell {(l,1),(l,1)};
C = cellComplex(ZZ,{f});
assert(dim C===2);
prune HH complex C
assert(HH_0 C == 0);
assert(homology(0,complex(C,Reduced=>false))==ZZ^1);
assert(homology(1,complex(C,Reduced=>false))==ZZ^1/(2*ZZ^1));
assert(HH_1 complex C == cokernel matrix {{2}})
assert(HH^2 C == cokernel matrix {{2}});
assert(HH^1 C == 0);
assert(dim skeleton_1 C == 1);
///

TEST ///
a = newCell {};
b1 = newCell {(a,1),(a,-1)};
b2 = newCell {(a,1),(a,-1)};
D = cellComplex(QQ[x],{b1,b2});
assert(dim D == 1);
assert(isSimplex a);
assert(not isSimplex b1);
assert(not isSimplex b2);
Dcomplex = complex D;
assert(HH_0(Dcomplex)==0);
R = ring D;
assert(prune HH_1(Dcomplex)==R^2);
assert(HH_2(Dcomplex)==0);
///


TEST ///
R = QQ[w,x,y,z]
C = simplicialComplex monomialIdeal(w*x,w*y);
dim C
D = cellComplex(QQ,C);
D
assert(dim D==2);
assert(#cells(2,D)==1);
assert(#cells(1,D)==4);
assert(#cells(0,D)==4);
assert(#cells(-1,D)==0);
///

TEST ///
S = QQ[x,y,z];
R = QQ[a,b,c];
f = map(R,S,matrix{{a,b,c^2}});
v1 = newCell({},x);
v2 = newCell({},y);
v3 = newCell({},z);
e12 = newCell({v1,v2});
e23 = newCell({v2,v3});
C = cellComplex(S,{e12,e23});
assert(ring C === S);
D = f ** C;
assert(ring D === R);
assert(#cells(1,D) == #cells(1,C));
assert(set (cells(1,D)/cellLabel) === set {a*b, b*c^2});
///


--Koszul Complex via Taylor resolutions
TEST ///
R = QQ[x,y,z];
vx = newSimplexCell({},x);
vy = newSimplexCell({},y);
vz = newSimplexCell({},z);
lxy = newSimplexCell({vx,vy});
lyz = newSimplexCell({vy,vz});
lxz = newSimplexCell({vx,vz});
fxyz = newSimplexCell({lxy,lyz,lxz});
assert(cellLabel fxyz === x*y*z);
D = cellComplex(R,{fxyz});
C = (complex D);
assert(HH_(-1)(C)==cokernel matrix {{x,y,z}});
assert(C.dd^2==0);
assert(degrees C_0 == {{1}, {1}, {1}});
///

--Monomial ideal labels
TEST ///
R = QQ[x,y,z];
vx = newSimplexCell({},ideal(x));
vy = newSimplexCell({},ideal(y));
vz = newSimplexCell({},ideal(z));
lxy = newSimplexCell({vx,vy});
lyz = newSimplexCell({vy,vz});
lxz = newSimplexCell({vx,vz});
fxyz = newSimplexCell({lxy,lyz,lxz});
D = cellComplex(R,{fxyz});
C = (complex D)[-1];
assert(HH_0(C)==R^1/module ideal(x,y,z))
assert(HH_1(C)==0)
assert(C.dd^2==0);
///

--Non principal labels
TEST ///
R = QQ[x,y,z];
vx = newSimplexCell({},ideal(x,y));
vy = newSimplexCell({},ideal(y,z));
vz = newSimplexCell({},ideal(x,z));
lxy = newSimplexCell {vx,vy};
lyz = newSimplexCell {vy,vz};
lxz = newSimplexCell {vx,vz};
fxyz = newSimplexCell {lxy,lyz,lxz};
D = cellComplex(R,{fxyz});
C = (complex D)[-1];
assert(C.dd^2==0);
assert(not isFree(D));
///

--Relabel test
TEST ///
R = QQ[a,b,c];
v1 = newSimplexCell {};
v2 = newSimplexCell {};
v3 = newSimplexCell {};
v4 = newSimplexCell {};
e12 = newSimplexCell {v1,v2};
e23 = newSimplexCell {v2,v3};
e34 = newSimplexCell {v3,v4};
e14 = newSimplexCell {v1,v4};
e24 = newSimplexCell {v2,v4};
f124 = newSimplexCell {e12,e24,e14};
f234 = newSimplexCell {e23,e34,e24};
C = cellComplex(R,{f124,f234});
T = new HashTable from {v1 => a^2*b, v2 => a*c, v3 => b*c^2, v4 => b^2};
D = relabelCellComplex(C,T);
assert(HH_(-1) D == R^1/ideal(a^2*b,a*c,b*c^2,b^2));
assert(HH_0 D == 0);
assert(HH_1 D == 0);
assert(HH_2 D == 0);
labelsD2 = for c in cells(2,D) list cellLabel(c);
assert(set labelsD2 === set {a^2*b^2*c, a*b^2*c^2});
///

--Polytope test 1
TEST ///
R = QQ;
P = hypercube 3;
C = cellComplex(R,P);
assert(dim C==3);
assert(# cells(0,C)==8);
assert(# cells(1,C)==12);
assert(# cells(2,C)==6);
assert(# cells(3,C)==1);
assert(HH_1(C)==0);
assert(HH_2(C)==0);
assert(HH_3(C)==0);
assert((complex C).dd^2==0);
C1 = skeleton(1,C);
assert(dim C1 == 1);
assert(rank HH_1 C1 == 5);
assert(rank HH_2 C1 == 0);
C2 = skeleton(2,C);
assert(dim C2 == 2);
assert(HH_1 C2 == 0);
assert(rank HH_2 C2 == 1);
///

--Polytope test 2
TEST ///
R = QQ[x];
M = transpose matrix {{0,0},{1,0},{2,1},{2,2},{1,2},{0,1}}; -- this is a hexagon
P = convexHull M;
C = cellComplex(R,P);
assert(dim C==2);
assert(# cells(0,C)==6);
assert(# cells(1,C)==6);
assert(# cells(2,C)==1);
assert(# cells(3,C)==0);
for i to 3 do assert(HH_i C==0);
C1 = skeleton(1,C);
assert(rank HH_1 C1 == 1);
///

--Polytope test 3
TEST ///
R = ZZ[x];
M = transpose matrix {{0,0,0},{0,1,0},{0,0,1},{1,0,0},{1,1,0},{1,0,1}};
P = convexHull M;
C = cellComplex(R,P);
assert(dim C==3);
assert(# cells(0,C)==6);
assert(# cells(1,C)==9);
assert(# cells(2,C)==5);
assert(# cells(3,C)==1);
assert(# cells(4,C)==0);
///

--Polytope test 4 (LabelFunction) test
TEST ///
R = ZZ[x,y,z];
M = transpose matrix {{0,0,0},{0,1,0},{0,0,1},{1,0,0},{1,1,0},{1,0,1}};
P = convexHull M;
V = vertices P;
H = hashTable apply(numColumns V, i -> (v := V_i;(v,x^(lift(v_0,ZZ))*y^(lift(v_1,ZZ))*z^(lift(v_2,ZZ)))));
C = cellComplex(R,P,Labels => H);
assert(dim C==3);
assert(# cells(0,C)==6);
assert(# cells(1,C)==9);
assert(# cells(2,C)==5);
assert(# cells(3,C)==1);
assert(# cells(4,C)==0);
assert(sort apply(cells(0,C),cellLabel) == sort {x*z,x*y,x,z,y,1});
///

--Polyhedral complex test 1
TEST ///
R = QQ[x];
P1 = convexHull matrix {{2,2,0},{1,-1,0}};
P2 = convexHull matrix {{2,-2,0},{1,1,0}};
P3 = convexHull matrix {{-2,-2,0},{1,-1,0}};
P4 = convexHull matrix {{-2,2,0},{-1,-1,0}};
F = polyhedralComplex {P1,P2,P3,P4};
C = cellComplex(R,F);
assert(# cells(0,C)==5);
assert(# cells(1,C)==8);
assert(# cells(2,C)==4);
assert(# cells(3,C)==0);
for i to 2 do assert(HH_i C==0);
C1 = skeleton(1,C);
assert(rank HH_1 C1 == 4);
///


--Polyhedral complex test 2
TEST ///
R = frac(ZZ[x,y]);
P1 = convexHull matrix {{2,2,0},{1,-1,0}};
P2 = convexHull matrix {{2,-2,0},{1,1,0}};
P3 = convexHull matrix {{-2,-2,0},{1,-1,0}};
P4 = convexHull matrix {{-2,2,0},{-1,-1,0}};
F = polyhedralComplex {P1,P2,P3,P4};
V = vertices F;
H = hashTable apply(numColumns V, i -> (v := V_i;(v,x^(lift(v_0,ZZ))*y^(lift(v_1,ZZ)))));
C = cellComplex(R,F,Labels=>H);
assert(# cells(0,C)==5);
assert(# cells(1,C)==8);
assert(# cells(2,C)==4);
assert(# cells(3,C)==0);
assert(sort apply(cells(0,C),cellLabel) == sort {x^2*y,x^2/y,1,y/x^2,1/(x^2*y)});
///


--Face poset
TEST ///
R = QQ;
v1 = newCell {};
v2 = newCell {};
v3 = newCell {};
v4 = newCell {};
e12 = newCell({v1,v2});
e23 = newCell({v2,v3});
e34 = newCell({v3,v4});
e41 = newCell({v4,v1});
f = newCell({e12,e23,e34,e41});
C = cellComplex(R,{f}); -- C is a square
assert(dim C == 2);
P = facePoset C;
assert(compare(P,v1,e12));
assert(not compare(P,v1,e23));
assert(compare(P,P_*#0,f));
assert(isGraded P);
assert(maximalElements P === {f});
///

--Minimality check
TEST ///
R = QQ[x,y,z];
v1 = newCell({},x^2*y);
v2 = newCell({},y*z);
v3 = newCell({},z^3);
e12 = newCell({v1,v2});
e13 = newCell({v1,v3});
e23 = newCell({v2,v3});
f123 = newCell({e12,e13,e23});
Cnonmin = cellComplex(R,{f123});
assert(not isMinimal(Cnonmin));
Cmin = cellComplex(R,{e12,e23});
assert(isMinimal(Cmin));
///

--RingMap**CellComplex check
TEST ///
R = ZZ;
M = transpose matrix {{0,0,0},{0,1,0},{0,0,1},{1,0,0},{1,1,0},{1,0,1}};
P = convexHull M;
C = cellComplex(R,P);
S = ZZ[x];
f = map(S,R,{});
D = f**C;
chainD = complex D;
assert(ring chainD === S);
assert(HH_1(D)==0);
assert(HH_2(D)==0);
assert(HH_3(D)==0);
///

TEST ///
R = ZZ;
v = newCell({},1);
f = newCell({(v,0)},1,CellDimension=>2);
C = cellComplex(R,{f});
assert(HH_0(C)==0);
assert(HH_1(C)==0);
assert(HH_2(C)==ZZ^1);
///

--Sphere test
TEST ///
C = cellComplexSphere(QQ,3);
assert(dim C==3);
for i in {1,2,4} do assert(# cells(i,C) == 0);
for i in {0,3} do assert(# cells(i,C) == 1);
for i in {-1,0,1,2,4,20} do assert(HH_i(C)==0);
assert(HH_3(C)== QQ^1);
///

--RPn test
TEST ///
C = cellComplexRPn(ZZ,4);
assert(dim C==4);
for i in (0..4) do assert(# cells(i,C) == 1);
for i in {-1,5} do assert(# cells(i,C) == 0);
for i in {-1,0,2,4} do assert(HH_i C == 0);
for i in {1,3} do assert(prune HH_i C == cokernel matrix {{2}});
///

-- Scarf complex Test 1
TEST ///
R = QQ[w,x,y,z];
I = monomialIdeal {w*x,x*y,y*z,w*z}
C = scarfComplex I
assert(dim C == 1);
assert(prune HH_1 C != 0);
///

-- Scarf complex Test 2
TEST ///
R = QQ[x_0,x_1,x_2,y_0,y_1]
I = monomialIdeal(x_0*x_1,x_1*x_2,x_2*x_0,x_0*y_1,y_0)
B = intersect(ideal(x_0,x_1,x_2),ideal(y_0,y_1))
C =scarfComplex I;
assert(#cells_0 C == 5)
assert(#cells_1 C == 6)
assert(#cells_2 C == 2)
///

-- Scarf complex Test 3
TEST ///
R = QQ[u,w,x,y,z];
I = monomialIdeal(w*x,y*z,x*y,u);
C = scarfComplex I;
assert(#cells_0 C == 4)
assert(#cells_1 C == 5)
assert(#cells_2 C == 2)
///

TEST ///
S = QQ[x,y,z,w];
v1 = newCell({},y*w);
v2 = newCell({},x*y*z);
v3 = newCell({},x^2*y);
v4 = newCell({},z^4*w);
e12 = newCell({v1,v2});
e13 = newCell({v1,v3});
e23 = newCell({v2,v3});
e14 = newCell({v1,v4});
f123 = newCell({e12,e13,e23});
Delta = cellComplex(S, {f123,e14});
C = complex Delta;
assert (dim Delta == 2);
assert (length C == 3);
///


TEST ///
R = QQ[x,y,z];
vx = newSimplexCell({},x);
vy = newSimplexCell({},y);
vz = newSimplexCell({},z);
lxy = newSimplexCell({vx,vy});
lyz = newSimplexCell({vy,vz});
lxz = newSimplexCell({vx,vz});
assert(isCycle {(lxy,1)} == false);
assert(isCycle {{lxy,1},{lyz,1},{lxz,-1}} == true);
assert(isCycle {{lxy,1},{lyz,1},{lxz,1}} == false);
assert(isCycle {} == true);
assert(isCycle {(vx,1)} == false);
assert(isCycle {(vx,1),(vy,-1)} == true);
assert(isCycle {(vx,0)} == true);
///


--Irrelevant ideal of a toric variety
TEST ///
needsPackage "NormalToricVarieties"
X = toricProjectiveSpace(1) ** toricProjectiveSpace(2);
S = ring X;
B = ideal X;
Sigma = fan X;
P = polytope Sigma;
d = dim P;
--Faces knows the order of the vertices relative to the output of vertices P
F = faces_d P;
m = product(apply(numgens S, i -> S_i));
G = apply(max X, l -> m//product(apply(l,i -> S_i)));
H = hashTable apply(#G, i -> (j := F#i#0#0;((vertices P)_j,G_i)))
C = cellComplex(S,P,Labels => H);
Cres = (complex C)[-1];
assert(betti (res B) == betti Cres);
assert(HH_0 Cres == S^1/B);
assert(HH_1 Cres == 0);
assert(HH_2 Cres == 0);
assert(HH_3 Cres == 0);
///

-- Hull complex test
TEST ///
R = QQ[x,y,z];
I = monomialIdeal (x^2*z, x*y*z, y^2*z, x^3*y^5, x^4*y^4, x^5*y^3);
H = hullComplex I;
complex H;
assert(isMinimal H);
assert(HH_(-1) complex H == R^1/I);
assert((HH_0 complex H)==0);
H2 = hullComplex (3/2,I)
assert((HH_0 complex H2)!=0);
///

--isWellDefined test
TEST ///
R = QQ[x,y];
S = ZZ[a,b];
v1 = newCell({},x);
v2 = newCell({},y);
v3 = newCell({},a);
v4 = newCell({},b);
assert(isWellDefined v1);
assert(isWellDefined v2);
assert(isWellDefined v3);
assert(isWellDefined v4);
C1 = cellComplex(R,{v1,v3});
--incompatible rings/two different rings
assert(not isWellDefined C1);
e1 = newCell({v1,v3},x);
assert(not isWellDefined e1);
C2 = cellComplex(R,{e1});
assert(not isWellDefined C2);
--label is not divisible by the labels in the boundary
e2 = newSimplexCell({v1,v2},x);
assert(not isWellDefined e2);
C3 = cellComplex(R,{e2});
assert(not isWellDefined C3);
e3 = newSimplexCell({v1,v2});
assert(isWellDefined e3);
C4 = cellComplex(R,{e3});
assert(isWellDefined C4);
///

--subcomplex test
TEST ///
R = QQ[x,y,z];
v1 = newCell({},x*y);
v2 = newCell({},x*z);
v3 = newCell({},y*z);
e1 = newSimplexCell {v1,v2};
e2 = newSimplexCell {v1,v3};
e3 = newSimplexCell {v2,v3};
f = newSimplexCell {e1,e2,e3};
C = cellComplex(R,{f});
Cxy = subcomplex(C,x*y);
assert(isWellDefined Cxy);
assert(#flatten values cells Cxy == 1);
Cxyz = subcomplex(C,x*y*z);
assert(isWellDefined Cxy);
assert(#flatten values cells Cxyz == #flatten values cells C)
///
