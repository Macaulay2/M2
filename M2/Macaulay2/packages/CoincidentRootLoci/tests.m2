TEST /// -- internal tests
debug CoincidentRootLoci;
F := matrix{{randomRealRootedBinaryForm(3,QQ),randomBinaryForm 3,randomBinaryForm 3,randomBinaryForm 3,randomBinaryForm 3}};
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert isSomeLinearCombinationRealRooted(F,Verbose=>true);
F = matrix{{randomBinaryForm 3,randomRealRootedBinaryForm(3,QQ),randomBinaryForm 3,randomBinaryForm 3,randomBinaryForm 3}};
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert isSomeLinearCombinationRealRooted(F,Verbose=>true);
F = matrix{{randomBinaryForm 3,randomBinaryForm 3,randomRealRootedBinaryForm(3,QQ),randomBinaryForm 3}};
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert isSomeLinearCombinationRealRooted(F,Verbose=>true);
x := local x;
y := local y;
R := QQ[x,y];
f := (x-y)*(x-2*y)*(x-3*y)*(x-4*y);
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert isSomeLinearCombinationRealRooted(matrix f,Verbose=>true);
f = (x-y)*(x-2*y)*(x^2+y^2);
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert not isSomeLinearCombinationRealRooted(matrix f,Verbose=>true);
(t1,t2) := (local t1,local t2);
y = 1;
R = QQ[t1,t2][x];
f = (x-t1*y)*(x-t2*y)*(x^2-2*y^2);
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert isRealRootedViaQEPCAD f;
f = (x-t1*y)*(x-t2*y)*(x^2+2*y^2);
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert not isRealRootedViaQEPCAD f;
eps := local eps;
R = QQ[eps][t1,t2][x];
f = (x-t1*y)*(x-t2*y)*(x^2-eps*y^2);
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert isRealRootedViaQEPCAD(f,Range=>(0,1/2));
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert not isRealRootedViaQEPCAD(f,Range=>(-1/2,0));
-- isRealRootedViaQEPCAD(f,Range=>(-1/2,1/2),Verbose=>true);
///

TEST /// -- apolar and recover
F = randomBinaryForm 6
I = apolar F
assert(F == recover I)
assert(F == recover apolar(F,4))
assert(F == recover apolar(F,5))
assert(F == recover apolar(F,6))
tt := local tt;
F = randomBinaryForm(7,Variable=>tt)
assert(F == recover(apolar F))
assert(F == recover(apolar(F,5)))
assert(F == recover(apolar(F,6)))
assert(F == recover(apolar(F,7)))
-- assert(F == recover(apolar(F,4)))
R := QQ[eps][u,v]
J = ideal(u*(u-eps*v)*(u+eps*v)*(v-eps*u)*(v+eps*u),(u^4-v^4)*u,(u^4-v^4)*v)
F = recover J
I = apolar(F,5)
assert(I == J)
assert(F == recover I)
assert(F == recover apolar F)
T := ZZ/3331[t_0..t_2]
S := T[x,y]
F = random(1,T)*x^6+random(1,T)*x*y^5+random(1,T)*x^2*y^4+random(1,T)*y^6
assert(ideal F == ideal recover apolar F)
F = random(1,T)*x^5+(random(0,T) + random(1,T))*x*y^4+random(1,T)*x^2*y^3+(random(1,T)+random(2,T))*y^5
G = recover apolar F
assert(ideal F == ideal G)
assert(F ~ G)
///

TEST /// -- realrank: Comon Ottaviani method for quintics
t = gens Grass(0,1);
F = {52521875*t_0^5+90037500*t_0^4*t_1+61740000*t_0^3*t_1^2+21168000*t_0^2*t_1^3+3628800*t_0*t_1^4+248832*t_1^5,
     33571239*t_0^5+36736175*t_0^4*t_1+16087190*t_0^3*t_1^2+3525550*t_0^2*t_1^3+386995*t_0*t_1^4+17050*t_1^5,
     24*t_0^5+12*t_0^4*t_1+120*t_0^3*t_1^2+36*t_0^2*t_1^3+20*t_0*t_1^4+27*t_1^5,
     t_0^5+3*t_0^4*t_1+3*t_0^3*t_1^2+3*t_0^2*t_1^3+2*t_0*t_1^4,
     8400*t_0^5+129620*t_0^4*t_1+173908*t_0^3*t_1^2+79803*t_0^2*t_1^3+14210*t_0*t_1^4+784*t_1^5};
F' = flatten entries sub(matrix{F},QQ[][gens ring matrix {F}])
if CoincidentRootLoci.Options.OptionalComponentsPresent then for i from 1 to 4 list assert(realrank(F'_(i-1)) == i)
for i from 3 to 5 list assert(realrank F_(i-1) == i)
///

TEST /// -- real rank
f = (eps,x,y) -> recover(x^2*(x^2+y^2),(y^2+eps*x^2)^2*(x-2*y));
R := QQ[x,y];
F = f(0,x,y)
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert(realrank(F,Limit=>6) == (6,7))
F = f(1/10,x,y)
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert(realrank(F,Limit=>6) == (6,7))
F = f(1/100,x,y)
-- if CoincidentRootLoci.Options.OptionalComponentsPresent then assert(realrank(F,Limit=>6) == (6,7))
F = f(-1/10,x,y)
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert(realrank F == 5)
F = f(-1/100,x,y)
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert(realrank F == 5)
R = QQ[eps][x,y]
F = f(eps,x,y)
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert(realrank(F,Range=>(0,1/2),Limit=>4) == (5, 6, 7))
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert(realrank(F,Range=>[0,infinity],Limit=>5) == (5, 6, 7))
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert(realrank(F,Range=>[-1/2,-1/2]) == 5)
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert(realrank(F,Range=>[1/2,1/2],Limit=>6) == (6,7))
F = f(-eps,x,y)
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert(realrank(F,Range=>(-infinity,infinity),Limit=>4) == (5, 6, 7))
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert(realrank(F,Range=>(0,1/2),Limit=>5) == (5, 6, 7))
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert(realrank(F,Range=>(0,1/2)) == 5)
///

TEST /// -- randomBinaryForm
setRandomSeed 123456789
for i from 1 to 6 do assert(complexrank randomBinaryForm(6,,i) == i)
for i from 1 to 7 do assert(complexrank randomBinaryForm(7,,i) == i)
if CoincidentRootLoci.Options.OptionalComponentsPresent then for i from 1 to 4 do assert(realrank randomBinaryForm(6,i,) == i)
assert(realrank randomBinaryForm(6,6,) == 6)
if CoincidentRootLoci.Options.OptionalComponentsPresent then for i from 1 to 4 do (F = randomBinaryForm(6,i,i); assert(realrank F == i and complexrank F == i))
///

TEST /// -- real rank boundary (odd)
k = 3
n = 2*k-1
lambda = {3}|toList((k-2):2)
describe coincidentRootLocus lambda
time D = ideal dual(coincidentRootLocus lambda);  
time R = realRankBoundary(n,k);
assert(degree D == 2*k*(k-1))
assert(ideal R == D)
///

TEST /// -- real rank boundary (even)
k = 3
n = 6
time R = realRankBoundary(n,k+1)
time idR = (ideal R_0,ideal R_1); -- used 5.09527 seconds
assert(# terms idR_0_0 == 560)
assert(# terms idR_1_0 == 3140)
lambda1 = {3,3}|toList((k-3):2)
lambda2 = {4}|toList((k-2):2)
describe coincidentRootLocus lambda1
describe coincidentRootLocus lambda2
D0 = dual(coincidentRootLocus lambda1)
assert(D0 == R_0)
assert(degree D0 == 2*k*(k-1)*(k-2)) 
D1 = dual(coincidentRootLocus lambda2)
assert(D1 == R_1)
assert(degree D1 == 3*k*(k-1))
///

TEST /// -- isInCoisotropic(Ideal,CoincidentRootLocus)
K := ZZ/33331;
X = coincidentRootLocus({2,1,1,1},K);
L = randomInCoisotropic(X,2)
assert isInCoisotropic(L,X)
L = randomInCoisotropic(X,1)
assert isInCoisotropic(L,X)
L = randomInCoisotropic(X,0)
assert isInCoisotropic(L,X)
R := QQ[x,y];
eps = 0;
g4 = x^2*(x^2+y^2);
g5 = (y^2+eps*x^2)^2*(x-2*y);
F = recover(g4,g5)
plane = apolar(F,5)
X311 = coincidentRootLocus {3,1,1}
X221 = coincidentRootLocus {2,2,1}
assert isInCoisotropic(plane,X311)
assert isInCoisotropic(plane,X221)
///

TEST///
R := QQ[x,y]
g = y^6+5*x^2*y^4-5*x^4*y^2-x^6
f = y^6+15*x^4*y^2
if CoincidentRootLoci.Options.OptionalComponentsPresent then assert(realrank f == 5)
X42 = coincidentRootLocus {4,2};
X33 = coincidentRootLocus {3,3};
assert(member(f,dual X42) and (not member(f,dual X33)) and (not member(g,dual X42)) and member(g,dual X33))
--
K := ZZ/33331
X = coincidentRootLocus({3,2,2},K)
Y = dual X
F = random X
G = random Y
H = random(first degree F,ring F)
assert(member(F,X) and member(G,Y) and (not member(H,X)) and (not member(H,Y)))
--
Y = dual(coincidentRootLocus({5,2,2,1},K))
F = random Y
H = random(first degree F,ring F)
assert(member(F,Y) and (not member(H,Y)))
///

TEST /// -- singularLocus
X10 = coincidentRootLocus(toList(10:1),ZZ/101,Variable=>x)
A10 = {{},{{10}},{{10}},{{9,1}},{{10}},{{7,3},{8,2},{9,1}},{{8,1,1}},{{10}},{{6,4},{7,3},{9,1}},{{8,2}},{{6,2,2},{6,3,1},{7,2,1},{8,1,1}},{{7,1,1,1}},{},{{5,5},{6,4},{9,1}},{{5,5},{7,3},{8,2}},{{5,4,1},{6,3,1},{8,1,1}},{{5,3,2},{6,2,2},{7,2,1}},{{5,2,2,1},{5,3,1,1},{6,2,1,1},{7,1,1,1}},{{5,5},{6,1,1,1,1}},{{6,4}},{{5,4,1}},{{7,3}},{{4,3,3},{4,4,2},{5,3,2},{5,4,1},{6,3,1},{7,2,1}},{{4,3,3},{4,4,1,1},{5,3,1,1},{7,1,1,1}},{{4,4,2},{6,2,2}},{{4,2,2,2},{4,3,2,1},{4,4,1,1},{5,2,2,1},{6,2,1,1}},{{4,2,2,1,1},{4,3,1,1,1},{4,4,2},{5,2,1,1,1},{6,1,1,1,1}},{{4,4,1,1},{5,1,1,1,1,1}},{{4,3,3}},{{5,3,2}},{{3,3,2,2},{3,3,3,1},{4,3,2,1},{5,3,1,1}},{{3,3,3,1},{4,3,1,1,1}},{{3,3,2,2},{4,2,2,2},{5,2,2,1}},{{3,2,2,2,1},{3,3,2,1,1},{3,3,2,2},{4,2,2,1,1},{5,2,1,1,1}},{{3,2,2,1,1,1},{3,3,1,1,1,1},{3,3,2,1,1},{4,2,1,1,1,1},{5,1,1,1,1,1}},{{3,3,1,1,1,1},{4,1,1,1,1,1,1}},{},{{2,2,2,2,2},{3,2,2,2,1}},{{2,2,2,2,1,1},{3,2,2,1,1,1},{4,4,2}},{{2,2,2,1,1,1,1},{3,2,1,1,1,1,1},{4,4,1,1}},{{2,2,1,1,1,1,1,1},{3,1,1,1,1,1,1,1}},{}}
assert(sort apply(apply(subsets X10,singularLocus),L -> sort apply(L,partition)) === sort apply(A10,sort))
X11 = coincidentRootLocus(toList(11:1),ZZ/101,Variable=>x)
A11 = {{},{{11}},{{11}},{{10,1}},{{11}},{{8,3},{9,2},{10,1}},{{9,1,1}},{{11}},{{7,4},{8,3},{10,1}},{{9,2}},{{7,2,2},{7,3,1},{8,2,1},{9,1,1}},{{8,1,1,1}},{{11}},{{6,5},{7,4},{10,1}},{{6,5},{8,3},{9,2}},{{6,4,1},{7,3,1},{9,1,1}},{{6,3,2},{7,2,2},{8,2,1}},{{6,2,2,1},{6,3,1,1},{7,2,1,1},{8,1,1,1}},{{7,1,1,1,1}},{{6,5}},{{6,5},{7,4},{9,2}},{{5,5,1},{6,4,1},{9,1,1}},{{8,3}},{{5,3,3},{5,4,2},{5,5,1},{6,3,2},{7,3,1},{8,2,1}},{{5,3,3},{5,4,1,1},{6,3,1,1},{8,1,1,1}},{{7,2,2}},{{5,2,2,2},{5,3,2,1},{6,2,2,1},{7,2,1,1}},{{5,2,2,1,1},{5,3,1,1,1},{6,2,1,1,1},{7,1,1,1,1}},{{5,5,1},{6,1,1,1,1,1}},{{7,4}},{{4,4,3},{5,4,2},{6,4,1}},{{5,4,1,1}},{{4,4,3},{5,3,3},{7,3,1}},{{4,4,3},{5,4,2},{6,3,2},{7,2,2}},{{4,3,2,2},{4,3,3,1},{4,4,2,1},{5,3,2,1},{5,4,1,1},{6,3,1,1},{7,2,1,1}},{{4,3,3,1},{4,4,1,1,1},{4,4,3},{5,3,1,1,1},{7,1,1,1,1}},{{4,3,2,2},{4,4,2,1},{5,2,2,2},{6,2,2,1}},{{4,2,2,2,1},{4,3,2,1,1},{4,4,1,1,1},{5,2,2,1,1},{6,2,1,1,1}},{{4,2,2,1,1,1},{4,3,1,1,1,1},{4,4,2,1},{5,2,1,1,1,1},{6,1,1,1,1,1}},{{4,4,1,1,1},{5,1,1,1,1,1,1}},{{5,3,3}},{{4,3,3,1}},{{3,3,3,2},{4,3,2,2},{5,3,2,1}},{{3,3,2,2,1},{3,3,3,1,1},{3,3,3,2},{4,3,2,1,1},{5,3,1,1,1}},{{3,3,3,1,1},{4,3,1,1,1,1}},{{5,2,2,2}},{{3,2,2,2,2},{3,3,2,2,1},{4,2,2,2,1},{5,2,2,1,1}},{{3,2,2,2,1,1},{3,3,2,1,1,1},{3,3,2,2,1},{4,2,2,1,1,1},{4,4,3},{5,2,1,1,1,1}},{{3,2,2,1,1,1,1},{3,3,1,1,1,1,1},{3,3,2,1,1,1},{4,2,1,1,1,1,1},{5,1,1,1,1,1,1}},{{3,3,1,1,1,1,1},{4,1,1,1,1,1,1,1}},{{3,2,2,2,2}},{{2,2,2,2,2,1},{3,2,2,2,1,1}},{{2,2,2,2,1,1,1},{3,2,2,1,1,1,1},{4,4,2,1}},{{2,2,2,1,1,1,1,1},{3,2,1,1,1,1,1,1},{4,4,1,1,1}},{{2,2,1,1,1,1,1,1,1},{3,1,1,1,1,1,1,1,1}},{}}
assert(sort apply(apply(subsets X11,singularLocus),L -> sort apply(L,partition)) === sort apply(A11,sort))
///

TEST ///
K := ZZ/10000019
X = coincidentRootLocus({7,3,3,2,2,2},K)
Y = dual X
assert(dual X == Y and dual Y == X)
assert(dim X == 6 and codim X == 13 and dim Y == 18 and codim Y == 1 and degree X == 30240 and degree Y == 10080)
S = {coincidentRootLocus({7,5,3,2,2},K),coincidentRootLocus({7,6,6},K),coincidentRootLocus({9,3,3,2,2},K),coincidentRootLocus({10,3,2,2,2},K)}
assert(singularLocus X == sort S)
apply(S,s -> assert isSubset(s,X))
describe coincidentRootLocus({7,3,3,2,2,2},K)
--
X = coincidentRootLocus({3,3,2},K)
assert member(random X,X)
assert member(random dual X,dual X)
assert not member(random X,dual X)
assert not member(random dual X,X)
X = coincidentRootLocus {3,1,1,1}
assert member(random X,X)
assert member(random dual X,dual X)
assert not member(random X,dual X)
///

TEST ///
F = randomBinaryForm 29;
assert(F == switch switch switch F)
L = flatten entries random(QQ^1,QQ^39)
assert(switch switch switch switch L == switch L)
///

TEST /// -- ideal(dual coincidentRootLocus)
debug CoincidentRootLoci;
X = coincidentRootLocus({2,3},ZZ/10000019,Variable=>"y")
time Y = dual X
time I = ideal Y;
time J = projectiveJoin(apply(toSequence(Y#"listCRloci"),ideal),SubringLimit=>1);
assert(I == J)
time J = projectiveJoin(apply(toSequence(Y#"listCRloci"),Z -> Z#"map2"),SubringLimit=>1);
assert(I == J)
time J = projectiveJoin(apply(toSequence(Y#"listCRloci"),affineMap),SubringLimit=>1);
assert(I == J)
--
time X = coincidentRootLocus({2,4},ZZ/10000019,Variable=>"y")
time Y = dual X
time I = ideal Y;
assert(codim I == 1 and degree I == 18)
///

TEST/// -- idealCRL
for n from 1 to 7 do (
   S = subsets CRL(n:1,ZZ/3331,Variable=>"w");
   for X in S do (
      assert(isSubset(ideal X,(X#"map2")(point source X#"map2")));
      assert(degree ideal X == degree X and codim ideal X == codim X and dim ideal X -1 == dim X);
   );
)
///

