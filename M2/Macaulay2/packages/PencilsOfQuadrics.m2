///
restart
loadPackage "CompleteIntersectionResolutions"
loadPackage "PencilsOfQuadrics"
uninstallPackage("PencilsOfQuadrics")
installPackage("PencilsOfQuadrics")
check "PencilsOfQuadrics"
viewHelp "PencilsOfQuadrics"
needsPackage"CompleteIntersectionResolutions"
loadPackage("PencilsOfQuadrics", Reload=>true)
peek loadedFiles

///
     newPackage(
             "PencilsOfQuadrics",
             Version => "0.9", 
             Date => "June 17, 2020",
             Authors => {{Name => "Frank-Olaf Schreyer", 
                       Email => "schreyer@math.uni-sb.de", 
                       HomePage => ""},
             	         {Name => "David Eisenbud", 
                       Email => "de@msri.org", 
                       HomePage => "https://www.msri.org/~de"},
		       {Name => "Yeongrak Kim",
			   Email => "kim@math.uni-sb.de",
			   HomePage => "https://sites.google.com/view/yeongrak"}
		   },
	               PackageExports => {"CompleteIntersectionResolutions"},
             Headline => "Clifford Algebra of a pencil of quadratic forms",
             DebuggingMode => true
             )

     export {
	 "matrixFactorizationK",--
	 "randomNicePencil",--
	 "RandomNicePencil",
	 "cliffordOperators",--
	 "centers",--
	 --
	 "tensorProduct",
	 "randomLineBundle",
--	 "Nontrivial", -- an option which is not used in ver 0.2
	 "degOnE",
--	 "degreeOnE",
	 "orderInPic",
	 "randomExtension",
	 --
	 "randNicePencil",
         "qqRing",
         "quadraticForm",
         "baseRing",
         "isotropicSpace",
         "matFact1",
         "matFact2",
         "matFactu1",
         "matFactu2",
	 --
	 "CliffordModule",--
	 "cliffordModule",--
	 "evenOperators",
	 "oddOperators",
	 "evenCenter",
	 "oddCenter",
--	 "symmetricMatrix",--function returning the symmetric matrix
	 "symMatrix",--same as symmetricMatrix
	  "symmetricM", -- key for CliffordModule
	 "hyperellipticBranchEquation", -- key for CliffordModule
    	 --going from category to category: 
	 --Note that cliffordModule serves as matrixFactorizationToCliffordModule
	 "cliffordModuleToCIResolution",
	 "cliffordModuleToMatrixFactorization",
	 "ciModuleToMatrixFactorization",
	 "ciModuleToCliffordModule",
	 --
	 "VectorBundleOnE",
	 "vectorBundleOnE",
	 "yAction",
	 "searchUlrich",
	 "translateIsotropicSubspace",
	 "randomIsotropicSubspace"
	 }
     
needsPackage"CompleteIntersectionResolutions"

X:=local X;
Y:= local Y;
x := local x;
y := local y;
s := local s;
t := local t;
z := local z;

CliffordModule  = new Type of MutableHashTable
VectorBundleOnE = new Type of MutableHashTable
RandomNicePencil = new Type of MutableHashTable


matrixFactorizationK=method()
matrixFactorizationK(Matrix,Matrix) := (X,Y) -> (
    -- X and Y are vectors of forms in a ring S
    -- creates the Knoerrer matrix factorization of X \dot Y
    k := rank source X;
    S := ring X;
    D1 := (diagonalMatrix apply(k,i->(-1)^i))_(rsort toList(0..k-1));    
    D := (diagonalMatrix apply(2^(k-1),i->(-1)^i))_(rsort toList(0..2^(k-1)-1));    
    m1 := matFact(X*D1,Y*D1)*D;
    m2 := transpose (matFact(Y*D1,X*D1)*D**S^{-1});
    deg1 := reverse flatten apply(k//2+1,i->toList(binomial(k,2*i):-i));
    deg2 := flatten apply(k//2+1,i->toList(binomial(k,2*i+1):-i-2));
    M2 := map(S^deg2,S^(deg1)**S^{-3},m2);
    M1 := map(S^deg1,S^deg2,m1);
    (M1,M2)
    )

TEST/// 
-- test of matrixFactorizationK
kk=ZZ/101
d=2
n=2*d
R=kk[a_0..a_(binomial(n+2,2))]
S=kk[x_0..x_(n-1),a_0..a_(binomial(n+2,2))]
M=genericSymmetricMatrix(S,a_0,n)
X=(vars S)_{0..n-1}
Y=X*M
(M1,M2)=matrixFactorizationK(X,Y);
M12=M1*M2;
assert(M12-M12_(0,0)*id_(target M12)==0)
assert(isHomogeneous M1)
assert(isHomogeneous M2)
assert(source M1==target M2)
assert( source M2==target M1**S^{-3})
///


matFact = (X,Y) -> (
    --matrix factorization of X dot Y
    --used in matrixFactorizationK
    k := rank source X;
    S := ring X;
    l := k//2;
    zeroMat := map(S^1,S^(2^(k-1)),0);
    M := if k%2 == 0 then ( 
        Xpart:=directSum apply(l,i->koszul(2*i+1,X))||zeroMat;
        Ypart:=transpose zeroMat|directSum apply(l,i->koszul(2*i+2,Y));
        map(S^(2^(k-1)),,Xpart+transpose Ypart)	) else (
        Xpart = directSum apply(l+1,i->koszul(2*i+1,X));
        zero1 := map(S^1,S^(2^(k-1)-1),0);
        Ypart = (zero1||transpose directSum apply(l,i->koszul(2*i+2,Y)))| transpose zeroMat;
	map(S^(2^(k-1)),,Xpart- Ypart)
	);
    M)


randomNicePencil=method()
randomNicePencil(Ring,ZZ) := (kk,g) -> (
    --S, qq,  R,  u, M1, M2 Mu1, Mu2 are constructed. 
    --These are global variables!!
-*
    describe S --kk[x_0..y_(g-1),z_1,z_2,s,t]
      --x_0..y_(g-1),z_1,z_2 are coordinates on PP^(2g+1)
    describe qq -- is the pencil of quadrics sq_1+tq_2
    describe SQ -- the hypersurface ring over kk[s,t]
    describe R -- kk[s,t]
    X -- is the irrelevant ideal of PP^(2g+1)
    Y -- Y is the coefficient vector of qq written as a linear combination of X
    assert(X*transpose Y==qq)
    u -- are the linear equations defining a maximal isotropic saubspace
    betti M1, betti M2 -- is the matrix factorization of qq   
    --  corresponding to the resolution of kk
    betti Mu1, betti Mu2  -- is the matrix factorization of qq 
    --  corresponding to the resolution of u
*-
    n := g+1;
    S := kk[x_0..x_(g-1),y_0..y_(g-1), z_1,z_2,s,t];
    xy := (vars S)_{0..2*n-1};
    u := matrix {apply(g,i->x_i)|{z_1,z_2}};
    x':=apply(g+1,i->(matrix{apply(g,i->x_i)|{z_1,z_2}}*random(S^{g+2:-1},S^{-1}))_(0,0));
    y':=apply(g+1,i->(xy*random(S^{2*n:-1},S^{-1}))_(0,0));
    q1:= sum_g(i->x_i*y_i)-z_1^2;
    q2:= sum_(g+1)(i->x'_i*y'_i);
    qq:= s*q1+t*q2;
    SQ := S/qq;
    R:=kk[s,t];
    X := xy;
    Y := matrix {drop(last entries transpose syz (X|qq),-1)};
    (m1,m2) := matrixFactorizationK(X,Y);
    cub:=sum_n (i-> S_i^3);
    F := res(coker X**(S/cub));
    degsEven := reverse flatten degrees F_(2*g+4);
    degsOdd := flatten degrees F_(2*g+3);
    M2 := map(S^-degsOdd,S^-degsEven,m2)**S^{2*g+2};
    M1 := map(S^-degsEven**S^{3},S^-degsOdd,m1)**S^{2*g+2};
    Yu := matrix {drop(last entries transpose syz (u|qq),-1)};
    (m1u,m2u) := matrixFactorizationK(u,Yu);
    F = res(coker u**SQ);
    degsEven = reverse flatten degrees F_(2*g+4);
    degsOdd = flatten degrees F_(2*g+3);
    Mu2 := map(S^-degsOdd,S^-degsEven,m2u)**S^{2*g+2};
    Mu1 := map(S^-degsEven**S^{3},S^-degsOdd,m1u)**S^{2*g+2};
    (S, qq, R,  u, M1, M2, Mu1, Mu2))

--  (S, qq, R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g) 
-- is constructed
--    randomNicePencil := () -> print"usage: (S, qq, R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g)"

randNicePencil = method()
randNicePencil(Ring,ZZ) := (kk,g)->(
    L := randomNicePencil(kk,g);
    new RandomNicePencil from hashTable{
        (qqRing, L_0),
	(quadraticForm, L_1),
	(baseRing, L_2),
	(isotropicSpace, L_3),
	(matFact1, L_4),
	(matFact2, L_5),
	(matFactu1, L_6),
	(matFactu2, L_7)}
    )

///
restart
load "PencilsOfQuadrics.m2"
kk=ZZ/101
g=2
(S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g); 
rNP = randNicePencil(kk,g)
rNP.qqRing
betti Mu1, betti Mu2
isHomogeneous Mu1, isHomogeneous Mu2
(uOdd,uEv)=cliffordOperators(M1,M2,R);
(uOdd,uEv)=cliffordOperators(rNP.matFact1,rNP.matFact2,rNP.baseRing);
(c0,c1)=centers(uOdd,uEv);
f=det symMatrix(uOdd,uEv)
factor f
symMatrix(uOdd,uEv)
betti c0, betti c1
isHomogeneous(c0++c1)
degrees c0, degrees c1
if g==1 then assert(c0-transpose c1==0)

assert(c0^2-c1^2==0)
    c0^2-(-1)^(g+1)*f*id_(target c0)
assert(c1^2-(-1)^(g+1)*f*id_(target c1)==0)
assert(all(#uEv,i->uEv_i*c0+c1*uEv_i==0))
assert(all(#uOdd,i->uOdd_i*c1+c0*uOdd_i==0))

///


cliffordOperators=method()
cliffordOperators(Matrix,Matrix,Ring) := (M1,M2,R) -> (
    -- R= kk[s,t]
    -- M1 and M2 are the matrices defining a linear periodic resolution
    --over R[variables]/qq
    -- eOdd_i: Hom_S(target M_2,R) -> Hom_S(target M_1,R)
    -- eEv_i:  Hom_S(target M_1,R) -> Hom_S(target M_2,R) -- maybe twisted by R^{\pm 1}
    -- eOdd_i++eEv_i are the endomorphisms induced by 
    -- generators of the Clifford algebra
    -- on the module Hom_S(target M_1++target M2,R)
    -- Note that the following identities hold:
    -- transpose M1=sum(S_i*eEv_i) and transpose M2=sum(S_i*eOdd_i)
    S := ring M1;
    es := drop(gens S,-numgens R); 
        eOdd := apply(apply(es,e->contract(e,M2)),m->
	 transpose( sub(map(target M2,, m),R)**R^1));
        eEv := apply(apply(es,e->contract(e,M1)),m->
          transpose (sub(map(target M1,, m),R)**R^1));
--    << "Lists of Clifford operators eOdd and eEv constructed" << endl;
    --error();
    (eOdd,eEv))
-*
*-



symMatrix=(eOdd,eEv) -> matrix apply(#eOdd,i->apply(#eEv,j->1/2*(eOdd_i*eEv_j+eOdd_j*eEv_i)_(0,0)));
--symmetricMatrix = symMatrix
compl= (L1,L2) -> sort toList (set L2-set L1)

centers=method()
centers(List,List) := (eOdd,eEv) -> (
    -- eOdd_i and  eEv_i are the even and odd parts of the same generator of 
    -- the Clifford algebra of qq over R 
    -- The result are the center elements computed via the pfaffian formula
    M := matrix apply(#eOdd,i->apply(#eEv,j->1/2*(eOdd_i*eEv_j+eOdd_j*eEv_i)_(0,0)));
    n := rank source M;
    R := ring M;
    skewMat :=map(target M, source M,(i,j)->if j>i then M_(i,j) else if i==j then 0 else -M_(j,i));
    L := select(subsets(toList(0..n-1)),I-> #I%2==0);
    k:=0;
    oe := prepend(id_(target eEv_0),
        drop(
	apply(L,I->( 
	k=#I//2;
	product(k,j->eOdd_(I_(2*k-1-2*j))*eEv_(I_(2*k-2-2*j))))),1));
	--product(k,j->eOdd_(I_(1+2*j))*eEv_(I_(2*j))))),1));   
    eo := prepend(id_(target eOdd_0),
        drop(
	apply(L,I->( 
	k=#I//2;
	product(k,j->eEv_(I_(2*k-1-2*j))*eOdd_(I_(2*k-2-2*j))))),1));
	--product(k,j->eEv_(I_(1+2*j))*eOdd_(I_(2*j))))),1));
    cL := apply(L,I->compl(I,toList(0..n-1)));
    p:=0;
    pfaffianList := apply(cL,I->(p=pfaffians(#I,skewMat_I^I);
	    if p==0 then 0 else (-1)^(sum I)*p_0));
    c0 := sum(#L,i->pfaffianList_i*oe_i);
    c1 := sum(#L,i->pfaffianList_i*eo_i);
    c0 = map(R^(-degrees target eOdd_0),R^(-degrees target eOdd_0)**R^{-n//2},c0);
    c1 = map(R^(-degrees target eEv_0),R^(-degrees target eEv_0)**R^{-n//2},c1);
    (c0,c1))




TEST///

-- Testing the pfaffian formula

kk=ZZ/101
d=1
n=2*d
R=kk[a_0..a_(binomial(n+2,2)-1)]
S=kk[x_0..x_(n-1),a_0..a_(binomial(n+2,2)-1)]
M=genericSymmetricMatrix(S,a_0,n)
X=(vars S)_{0..n-1}
Y=X*M
(M1,M2)=matrixFactorizationK(X,Y);
(eOdd,eEv)=cliffordOperators(M1,M2,R);
symMatrix(eOdd,eEv)
(c0,c1)=centers(eOdd,eEv);
assert isHomogeneous c0
assert isHomogeneous c1
betti c0, betti c1
all(n,i->eOdd_i*c1+c0*eOdd_i==0)
all(n,i->eEv_i*c0+c1*eEv_i==0)
assert(target eEv_0 == target c1)
assert(target eOdd_0 == target c0)
determ=det symMatrix(eOdd,eEv)
-- Note the factor (-1)^d occurs in the test below
assert(c0^2-(-1)^d*determ*id_(target c0)==0)
assert(c1^2-(-1)^d*determ*id_(source c1)==0)
///


factorToList = method()
factorToList(Product) := pf ->(
    lpf := toList pf;
    llpf := apply(lpf,p->toList p);
    apply(llpf, pair-> (pair_0)^(pair_1))
)
///    
    pf = factor f
    assert (f == product factorToList factor f)
///
--randomLineBundle=method(Options => {Nontrivial =>true})
randomLineBundle=method()
  -- the option is not used
randomLineBundle( RingElement ) := f -> (
    --produces a 2x2 matrix factorization of f, hence a line bundle on the hyperelliptic curve.
    --corresponding to y^2-(-1)^g*f.
    --the first matrix has the form
    --b c
    --a -b
    --where a is the lowest degree factor of (-(-1)^g*f-b^2) =: f1, and ac=f1.
    --caveat: produces a nontrivial bundle IF there's a nontrivial factor of f1, 
    --which happens with positive probability over a finite field.
    R:= ring f;
    g:= (degree f)_0 // 2 -1;
    b:=random(g+1,R);
    lf := factorToList factor(-(-1)^g*f-b^2);
    a := lf_0;
    c := product(drop(lf,1),fi->fi);
    --c:=if #ff==1 then 1 else product(drop(ff,1),fi->fi);  
    targetm := target transpose map(R^1,,matrix{{b,a}});
    m:=map(targetm,, matrix{{b,c},{a,-b}});
    assert(isHomogeneous m);
    vectorBundleOnE m
)

--randomMatrixFactorization = method(Options => {Nontrivial =>true})
randomMatrixFactorization=method()
randomMatrixFactorization(RingElement) :=  f -> (
    --produces a 2x2 matrix factorization of f, hence a line bundle on the hyperelliptic curve.
    --corresponding to y^2-(-1)^g*f.
    --the first matrix has the form
    --b c
    --a -b
    --where a is the lowest degree factor of (-(-1)^g*f-b^2) =: f1, and ac=f1.
    --caveat: produces a nontrivial bundle IF there's a nontrivial factor of f1, 
    --which happens with positive probability over a finite field.
    R:= ring f;
    g:= (degree f)_0 // 2 -1;
    b:=random(g+1,R);
    lf := factorToList factor(-(-1)^g*f-b^2);
    a := lf_0;
    c := product(drop(lf,1),fi->fi);
    --c:=if #ff==1 then 1 else product(drop(ff,1),fi->fi);  
    targetm := target transpose map(R^1,,matrix{{b,a}});
    m:=map(targetm,, matrix{{b,c},{a,-b}});
    assert(isHomogeneous m);
    m
)

///
restart
loadPackage("PencilsOfQuadrics", Reload =>true)
check "PencilsOfQuadrics"
///
TEST///  
kk=ZZ/101
g=1
(S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g) 
(uOdd,uEv)=cliffordOperators(Mu1,Mu2,R)
(c0,c1)=centers(uOdd,uEv)
symMatrix(uOdd,uEv)
f=det symMatrix(uOdd,uEv)
assert(c0^2+(-1)^g*f*id_(target c0)==0)
L=randomLineBundle(0,f)
m=L.yAction
assert((m)^2_(0,0)+(-1)^g*f==0)
--b = random(2*g+2, R)
assert isHomogeneous m
degOnE L
orderInPic L

--lcm apply(10,c-> (L=randomLineBundle(0,f); orderInPic L))
///

degOnE = method() 
degOnE(Matrix) := (L) -> (
    f := (L^2)_(0,0);
    g := (degree ideal f )//2 -1;
    r := numrows L//2;
    sum(flatten degrees target L)+2*r+r*(g-1))
--degreeOnE = degOnE

orderInPic = method()
orderInPic(Matrix) := L->(
    -- compute the order of a line bundle on E
    L1:=L;N:=1;
    if member({0},degrees target L1) then return 1;
    while(N=N+1;
    L1=tensorProduct(L,L1);
    not member({0},degrees target L1)) do ();
N)

orderInPic(VectorBundleOnE) := L->(
    M := L.yAction;
    if numrows M != 2 or degOnE L != 0 then error("expected bundle to have rank 1 and degree 0");
    orderInPic M)

randomLineBundle(ZZ,RingElement) :=  (d,f) -> (
-- f a binary form of degree 2g+2 over a finite field
-- d an integer
-- select randomly a line L of degree d on the hyperelliptic curve
-- defined by y^2-(-1)^g*f
   while (L:=randomLineBundle f; a:=d-degOnE L; a% 2 !=0) do ();
   R:= ring f;
   Ld := vectorBundleOnE (L.yAction**R^{-a//2});
   if d == 0 then
          while (member({0},degrees target Ld.yAction)) do( 
          Ld = randomLineBundle (d,f);
      );
   Ld
   ) 

///
-- Experiments on the order of Pic^0 of an elliptic curve over a finite field
-- When f factors into a product of linear forms, the group is no more cyclic.
-- When f is (or is close to?) irreducible, then the group tends to be cyclic.

-- Note that the Hasse-Weil theorem expects that the order of Pic^0 is in between
-- p + 1 - 2*sqrt(p) and p + 1 + 2*sqrt(p)

restart
loadPackage ("PencilsOfQuadrics", Reload =>true)
kk = ZZ/101
R = kk[s,t]
g = 1
f48 = 25*s^4+19*s^3*t-5*s^2*t^2+11*s*t^3+12*t^4 
factor f48
lcm apply (10, i-> orderInPic randomLineBundle(0,f48))
-- f48 gives an elliptic curve with noncyclic pic; maybe (ZZ/48)xZZ/2

f10 = (35*s + t)*(33*s - t)*(13*s - t)*(5*s + t)*(-18)
factor f10
-- f10 gives an elliptic curve with noncyclic pic; maybe (ZZ/10)xZZ/10
-- so far all f's with g=1 with linear factorization give rise to Jacobians that are not cyclic
-- they have 2 factors.
tally apply (50, i -> orderInPic randomLineBundle(0,f10))
lcm oo
-- product of 4 linear forms => 2-torsions are Z/2 * Z/2 => Pic^0(E) is not cyclic.


p=char kk
(p+1-2*sqrt(p),p+1+2*sqrt(p))
-- so the only candidates are 90, 100, 110, 120
-- experimentally we cannot find any random line bundle of order multiples of 3, or 11
-- so Pic^0 should be of order 100

f = product(2*g+2, i-> random(1,R))
factor f

kk=ZZ/11
R=kk[s,t]
f = s^3*t+3*s^2*t^2+s*t^3 --(char = 11, factors into linears, group might be Z/4 x Z/4 or Z/2 x Z/4.
factor f
tally apply (100, i-> orderInPic randomLineBundle(0,f)) 
-- about half of order 2 elements and half of order 4 elements
-- => probably Pic^0 is Z/2 * Z/4.
-- cf. Schoof's fast algorithm to compute the order of an elliptic curve 
p=char kk
(p+1-2*sqrt(p),p+1+2*sqrt(p))
-- Pic^0 has at most 2g factors; so experimentally it should be either Z/4 * Z/4 or Z/4 * Z/2

needsPackage "TateOnProducts"
setRandomSeed "order Eight  ";
A=randomLineBundle(0,f);
B=randomLineBundle(0,f);
(orderInPic A, orderInPic B)
A.yAction
B.yAction
A2=tensorProduct(A, A)
A3=tensorProduct(A2,A)
A4=tensorProduct(A3,A)
A.yAction
tally apply (100, i->isIsomorphic(coker A.yAction, coker A3.yAction))
betti coker A.yAction
betti coker A2.yAction
betti coker A3.yAction
betti coker A4.yAction

prune coker A.yAction
prune coker A2.yAction
prune coker A3.yAction
prune coker A4.yAction

homAtoA3= Hom(coker A.yAction, coker A3.yAction)
phi=homomorphism homAtoA3_{0}
prune coker phi
basis coker A2.yAction

A.yAction
Ry=kk[s,t,y,Degrees=>{1,1,2}]
isHomogeneous (M1=map( Ry^2,,y*id_(Ry^2) + sub(A.yAction,Ry)))
isHomogeneous (M3=map( Ry^2,,y*id_(Ry^2) + sub(A3.yAction,Ry)))
tally apply (100, i->isIsomorphic(coker M1, coker M3)) -- false

-- only with yAction on PP1, sometimes we pick wrong isomorphisms which make a bad test
tally apply (100, i->isIsomorphic(coker A.yAction, coker B.yAction))
tally apply (100, i->isIsomorphic(coker A2.yAction, coker B.yAction))
tally apply (100, i->isIsomorphic(coker A3.yAction, coker B.yAction))

B2=tensorProduct(B,B)
tally apply (100, i->isIsomorphic(coker A.yAction, coker B2.yAction))
tally apply (100, i->isIsomorphic(coker A2.yAction, coker B2.yAction)) -- true
tally apply (100, i->isIsomorphic(coker A3.yAction, coker B2.yAction))

-- A, B generates a group of order 8
L={A, A2, A3, A4, B, tensorProduct(A,B), tensorProduct(A2,B), tensorProduct(A3,B)}
apply(L, C->apply(L, D->(# (select (apply(100, i->isIsomorphic(coker C.yAction, coker D.yAction)), t->t==true)) > 0)))

apply(10, i->(
C:=randomLineBundle(0,f);
tally apply(L, D-> # (select (apply(100, i->isIsomorphic(coker C.yAction, coker D.yAction)), t->t==true)) > 0)
))

f=product(2, i-> random(1,R))*random(2,R)
f=random(4,R)
factor f
///

TEST///
--test of tensorProduct of randomLineBundle and degreeOnE and orderInPic
kk = ZZ/101
R = kk[ s,t]
g = 1

f = random(2*g+2, R)
assert(dim ideal(jacobian ideal f)== 0)
L1=randomLineBundle(1,f)
assert(degOnE L1 == 1)
L2=randomLineBundle(2,f)
assert(degOnE L2 == 2)
L0 = randomLineBundle(0,f)
assert(degOnE L0 == 0)
assert(degOnE tensorProduct(L1,L1) == 2)
orderInPic randomLineBundle(0,f)
///



tensorProduct=method()
tensorProduct(Matrix,Matrix) := (phi,psi) -> (
    --tensor product of sheaves on the hyperell curve y^2 \pm f
    --with f in R
    --represented by giving sheaves on proj R (usually P^1)
    --plus the action phi or psi of the element y
    R := ring phi;
    g := degree ideal ((phi^2)_(0,0))//2 - 1;
    inclusion:=syz(phi**id_(target psi)-(id_(target phi)**psi)); 	
    F:=chainComplex(inclusion);
    F1:=F**R^{g+1}; 
    (extend(F1,F,map(F1_0,F_0,(phi**id_(target psi)))))_1
)


tensorProduct(CliffordModule, VectorBundleOnE) := (M,L) -> (
    phi0 := M.evenCenter;
    phi1 := M.oddCenter;
    psi := L.yAction;
    assert((phi0^2)_(0,0)==(psi^2)_(0,0));
    R := ring psi;
    g := degree ideal ((psi^2)_(0,0))//2 - 1;

    inclusion0 := syz(phi0**id_(target psi)-(id_(target phi0)**psi)); 	
    inclusion1 := syz(R^{1}**(phi1**id_(target psi)+(id_(target phi1)**psi))); 	    

    FEven := chainComplex(inclusion0);
    FOdd := chainComplex(inclusion1);
    Odd' := null;
    Even' := null;
    eOdd := apply(M.oddOperators, Odd->(
	Odd' = map(FEven_0, R^{-g-1}**FOdd_0, R^{-g-1}**Odd**id_(target psi));
	(extend(FEven,R^{-g-1}**FOdd, Odd'))_1));

    eEv := apply(M.evenOperators, Even->(
	Even' = map(FOdd_0, R^{-g-1}**FEven_0, R^{-g-1}**Even**id_(target psi));
	(extend(FOdd,FEven, Even'))_1));

    assert(symMatrix(eOdd,eEv) == symMatrix(M.oddOperators, M.evenOperators));
    c0 := tensorProduct(phi0, psi);
    c1 := tensorProduct(phi1, psi);
    eOdd = apply(eOdd, e->map(target c0, target c1, e));
    eEv = apply(eEv, e->map(target c1, R^{-1}**target c0, e));
    cliffordModule(eOdd,eEv)
)

///
--test of tensorProduct(CliffordModule, VectorBundle)
restart
load ("PencilsOfQuadrics.m2")
kk=ZZ/101
g=1
(S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g) ;
(uOdd,uEv)=cliffordOperators(Mu1,Mu2,R);


(c0,c1)=centers(uOdd,uEv);
symMatrix(uOdd,uEv)
f=det symMatrix(uOdd,uEv);
cMu = cliffordModule(uOdd, uEv);
M = cMu
u
betti M.evenCenter
apply(M.evenOperators, ev-> betti ev)
betti M.oddCenter
apply(M.oddOperators, e-> betti e)
apply(M.oddOperators, e-> map(target M.evenCenter, target M.oddCenter, e))/isHomogeneous
apply(M.evenOperators, e-> map(target M.oddCenter, R^{-1}**target M.evenCenter, e))/isHomogeneous

betti c0
elapsedTime cM = cliffordModule(M1, M2, R)
L = randomLineBundle(0,f);
betti(L.yAction)
M' = tensorProduct(M,L)
apply(M'.oddOperators, e-> betti e) == apply(M.oddOperators, e-> betti e) 
apply(M'.evenOperators, e-> betti e) == apply(M.evenOperators, e-> betti e) 

--elapsedTime cM' = tensorProduct(cM,L)
assert(target c0 == target (uOdd_0))
assert(target c1 == target (uEv_0))
T = tensorProduct(vectorBundleOnE(M.evenCenter),L)
assert(T.yAction - M'.evenCenter == 0)
--T = tensorProduct(vectorBundleOnE(cM.evenCenter),L)
--assert(T.yAction - cM'.evenCenter == 0)
M'1 = sum(2*g+2, i->S_i*sub(M'.evenOperators_i, S));
Mu1 = sum(2*g+2, i->S_i*sub(M.evenOperators_i, S));
I1= trim ideal Mu1_{0}
I1' = trim ideal M'1_{0}
trim(I1+I1')
vars S
qq
qq%I1'
///

vectorBundleOnE = method()
vectorBundleOnE(Matrix) := M -> (
    new VectorBundleOnE from hashTable{yAction => M})





tensorProduct(VectorBundleOnE,VectorBundleOnE) := (L1, L2) -> 
         vectorBundleOnE tensorProduct(L1.yAction,L2.yAction)

degOnE(VectorBundleOnE) := L -> degOnE L.yAction



cliffordModule = method(TypicalValue => CliffordModule)
cliffordModule(List, List) := (uOdd, uEv) ->(
     R := ring uOdd_0;
    (c0',c1') := centers(uOdd, uEv);
    uOdd' := apply(uOdd, e-> map(target c0', target c1', e));
    uEv' := apply(uEv, e-> map(target c1', R^{-1}**target c0', e));
    symm := symMatrix(uOdd', uEv');
    f := det symm;
    new CliffordModule from hashTable{
	oddOperators => uOdd', 
	evenOperators => uEv', 
	evenCenter=>c0', 
	oddCenter => c1', 
	symmetricM => symm,
	hyperellipticBranchEquation => f
	}
    )
cliffordModule(Matrix,Matrix,Ring) := (M1,M2,R)->(
    cliffordModule cliffordOperators (M1,M2,R)
    )






TEST///
kk=ZZ/101
g=1
(S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g) 
(uOdd,uEv)=cliffordOperators(Mu1,Mu2,R)
(c0,c1)=centers(uOdd,uEv)
betti c0
betti c1
symMatrix(uOdd,uEv)
f=det symMatrix(uOdd,uEv)
cMu = cliffordModule(uOdd, uEv)
cM = cliffordModule(M1, M2, R)
cM.oddOperators
cMu.symmetricM
class cM

assert ((cMu.evenCenter, cMu.oddCenter)==centers(cMu.oddOperators, cMu.evenOperators))
///

randomExtension=method()
randomExtension(Matrix,Matrix) := (m1,m2) -> (
    -- Input: m1, m2 matrices representing line bundle on E
    -- Output: an extension of these line bundle
    R := ring m1;
    kk := coefficientRing R; 
    b := symbol b;
    n:= numrows m1;
    m:= numrows m2;
    Rb :=kk[gens R|toList(b_(0,0)..b_(n-1,m-1))];    
    B :=map( Rb^(-degrees target m1),Rb^(-degrees source m2), transpose genericMatrix(Rb,b_(0,0),m,n));
    eq := flatten( sub(m1,Rb)*B+B*sub(m2,Rb));
    rex:=syz sub(transpose diff(transpose flatten transpose B,eq),R);
    ex:=transpose (rex*random(source rex, R^1));
    m12 := map(target m1,,sub(B,vars R|ex));
    t:=(flatten degrees source m12)_0-(flatten degrees source m2)_0;   
    map(target m1++target m2**R^{-t},,(m1|m12)||(map(target m2,source m1,(i,j) -> 0)|m2))
    )

///
restart
load"PencilsOfQuadrics.m2" 
kk=ZZ/101
g=2
(S,qq,R,u,M1,M2,Mu1,Mu2) =randomNicePencil(kk,g);
P = kk[drop(gens S, -2)]
qs = sub(diff(matrix{{S_(2*g+2), S_(2*g+3)}}, qq), P)
CI = P/ideal qs
Mu=cliffordModule(Mu1,Mu2,R)
f=Mu.hyperellipticBranchEquation
bet=betti cliffordModuleToCIResolution(Mu,S,CI)
bet(1)[1]+bet(0)[0]+bet(-1)[-1]
while(
L1=randomLineBundle(2,f);
L2=randomLineBundle(0,f);
V=randomExtension(L1,L2);
F=cliffordModuleToCIResolution(tensorProduct(Mu,V),S,CI);
rank F_3 > 8 ) do ()
betti F
betti cliffordModuleToCIResolution(tensorProduct(Mu,V),S,CI)

elapsedTime tally apply(2,c-> (
L1=randomLineBundle(2,f);
L0=randomLineBundle(0,f);
L1=tensorProduct(L1,L0);
L2=randomLineBundle(1,f);
L0=randomLineBundle(0,f);
L2=tensorProduct(L2,L0);
L3=randomLineBundle(0,f);
L0=randomLineBundle(0,f);
L3=tensorProduct(L3,L0);
V=randomExtension(L1,L2);
V3=randomExtension(V,L3);
F=cliffordModuleToCIResolution(tensorProduct(Mu,V3),S,CI);
betti F
))

-*
elapsedTime tally apply(10,c-> (
L1=randomLineBundle(2,f);
L0=randomLineBundle(0,f);
L1=tensorProduct(L1,L0);
L2=randomLineBundle(1,f);
L0=randomLineBundle(0,f);
L2=tensorProduct(L2,L0);
L3=randomLineBundle(0,f);
L0=randomLineBundle(0,f);
L3=tensorProduct(L3,L0);
V=randomExtension(L1,L2);
V3=randomExtension(V,L3);
F=cliffordModuleToCIResolution(tensorProduct(Mu,V3),S,CI);
betti F))
     -- 32.7006 seconds elapsed

                    0  1  2  3  4  5  6  7
o14 = Tally{total: 48 36 24 12 12 24 36 48 => 10}
                3: 48 36 24 12  .  .  .  .
                4:  .  .  .  . 12 24 36 48

o14 : Tally
-- => exist a rank 2 and 3 Ulrichbundles for genus 2

*-


///

randomExtension(VectorBundleOnE,VectorBundleOnE) := (L1,L2) -> (
    vectorBundleOnE randomExtension(L1.yAction,L2.yAction) 
    )


/// 
kk=ZZ/101
g=2
(S,qq,R,u,M1,M2,Mu1,Mu2) =randomNicePencil(kk,g);
vars S

P = kk[drop(gens S, -2)]
qs = sub(diff(matrix{{S_(2*g+2), S_(2*g+3)}}, qq), P)
CI = P/ideal qs
u
Mu=cliffordModule(Mu1,Mu2,R)
f=Mu.hyperellipticBranchEquation

L1=randomLineBundle(1,f)
L2=randomLineBundle(2,f)
V=randomExtension(L1,L2)

keys Mu
Mor=vectorBundleOnE Mu.evenCenter
degOnE Mor
Mor1=vectorBundleOnE Mu.oddCenter
degOnE Mor1
(g-1)*2^g 
2^g

L3=randomLineBundle(3,f)
MorV=tensorProduct(Mor1,L3)
degOnE MorV == (g-1)*2^g

MuV=tensorProduct(Mu,V)
F=cliffordModuleToCIResolution(MuV,S,CI)
betti F
betti cliffordModuleToCIResolution(Mu,S,CI)

M=cliffordModule(M1,M2,R)
bet=betti cliffordModuleToCIResolution(M,S,CI)

MV=tensorProduct(M,V)
betti cliffordModuleToCIResolution(MV,S,CI)
bet[1]+bet(-1)
///



searchUlrich=method()
searchUlrich(CliffordModule,Ring) :=(M,S) -> (
    Mor := vectorBundleOnE M.evenCenter;
    Mor1:= vectorBundleOnE M.oddCenter;
    f := M.hyperellipticBranchEquation;
    assert(dim ideal jacobian ideal f ==0);
   
    g:=degree ideal f//2-1;
    if (numgens S != 2*g+4) then error "S should be a polynomial ring in (2g+4) variables";
    if (rank source M.evenCenter != 2^(g+1)) then error "M should be a Clifford module associated to a maximal isotropic subspace";
    
    m1:=null; m2:= null; m12 := null; 
    Ul:= null; Ul1:= null; V:= null;
    d1 := null; d0:= null;
    while (        
    	m1=randomLineBundle(g+(g%2),f);
	m2=randomLineBundle(g%2,f);
        m12=randomExtension(m1.yAction,m2.yAction);
	V = vectorBundleOnE m12;
	Ul=tensorProduct(Mor,V);
	Ul1=tensorProduct(Mor1,V);
	d0=unique degrees target Ul.yAction;
	d1=unique degrees target Ul1.yAction;
	#d1 >=3 or #d0 >=3) do ();
    Ul = tensorProduct(M,V); 
    M1Ul:=sum(#Ul.oddOperators,i->S_i*sub(Ul.oddOperators_i,S));
    Ulrich := M1Ul^{2*2^g..4*2^g-1};
    coker map(S^(2^(g+1)),,Ulrich)
)

///
restart
load "PencilsOfQuadrics.m2"
kk=ZZ/101
R=kk[s,t]
g=3
(S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g);
M=cliffordModule(Mu1,Mu2,R)
elapsedTime Ulrich = searchUlrich(M,S);
betti res Ulrich
ann Ulrich


assert(4*2^(g-1)== numrows presentation Ulrich)

-*
M=cliffordModule(Mu1,Mu2,R)
Mor=vectorBundleOnE M.evenCenter
Mor1=vectorBundleOnE M.oddCenter
betti Mor.yAction, betti Mor1.yAction
f=M.hyperellipticBranchEquation
elapsedTime tally apply(10,c-> (
	m1=randomLineBundle(g+(g%2),f);
	m2=randomLineBundle(g%2,f);
        m12=randomExtension(m1.yAction,m2.yAction);
	V = vectorBundleOnE m12;
	Ul=tensorProduct(Mor,V);
	Ul1=tensorProduct(Mor1,V);
	(betti Ul.yAction,betti Ul1.yAction)))
betti Ul1.yAction, betti Ul.yAction
MV=tensorProduct(M,V)
P = kk[drop(gens S, -2)]
qs = sub(diff(matrix{{S_(2*g+2), S_(2*g+3)}}, qq), P)
CI = P/ideal qs

ciRes=cliffordModuleToCIResolution(MV,S,CI)
betti ciRes
betti res coker lift(ciRes.dd_5,P)
*-

///
matrixFactorizationCToCliffordModule = cliffordModule

cliffordModuleToMatrixFactorization = method()
cliffordModuleToMatrixFactorization(CliffordModule,Ring) := (M,S) ->(
    --S is a ring with first set of vars corresponding to odd operators
    --and s,t at the end.
    g := (numgens S - 4)//2;
    N1 := transpose sum(#M.evenOperators, i-> S_i*sub(M.evenOperators_i,S));
    N2 := transpose sum(#M.oddOperators, i-> S_i*sub(M.oddOperators_i, S));
    N1' := map(target (S^{1}**N1),,N1);
    N2' := map(target (S^{-1}**N2),,N2);
    (N1', N2'))
TEST///
kk = ZZ/101;g= 1
(S,qq,R,u, M1,M2, Mu1, Mu2) = randomNicePencil(kk,g);
M = cliffordModule(Mu1, Mu2, R)
(Mu1', Mu2') = cliffordModuleToMatrixFactorization (M,S);
assert(Mu1'-Mu1 == 0 and Mu2'-Mu2 ==0)
assert(Mu1'==Mu1)
assert(Mu2'==Mu2)
betti Mu1', betti Mu1
betti Mu2', betti Mu2
M = cliffordModule(M1, M2, R);
(M1', M2') = cliffordModuleToMatrixFactorization (M,S);
assert(M1'-M1 == 0 and M2'-M2 ==0)
betti M1', betti M1
assert(M1'==M1)
betti M2', betti M2
assert(M2'==M2)
--g=2

kk = ZZ/101;g=2
(S,qq,R,u, M1,M2, Mu1, Mu2) = randomNicePencil(kk,g);
M = cliffordModule(Mu1, Mu2, R)
(Mu1', Mu2') = cliffordModuleToMatrixFactorization (M,S);
assert(Mu1'-Mu1 == 0 and Mu2'-Mu2 ==0)
betti Mu1', betti Mu1
betti Mu2', betti Mu2
assert(Mu1'==Mu1)
assert(Mu2'==Mu2)


M = cliffordModule(M1, M2, R);
(M1', M2') = cliffordModuleToMatrixFactorization (M,S);
assert(M1'-M1 == 0 and M2'-M2 ==0)
assert(M1'==M1)
assert(M2'==M2)
betti M1', betti M1
betti M2', betti M2
///

-- Translating isotropic subspaces by degree 0 line bundles
translateIsotropicSubspace = method()
translateIsotropicSubspace(CliffordModule, VectorBundleOnE, PolynomialRing) := (M, L, S) -> (
    -- Input:
    -- M, a Clifford Module, corresponding to a maximal isotropic subspace u
    -- L, a degree 0 line bundle on the associated hyperelliptic curve E
    -- S, a polynomial ring kk[x_0..y_(g-1),z_0,z_1,s,t] in (2g+4) variables
    
    -- Output:
    -- uL, a matrix, presenting a maximal isotropic subspace of the translation of u by L.
   
    f:=M.hyperellipticBranchEquation;
    g:=(first degree f)//2 - 1;
    
    if (numgens S != 2*g+4) then error "S should be a polynomial ring in (2g+4) variables";
    if (rank source M.evenCenter != 2^(g+1)) then error "M should be a Clifford module associated to a maximal isotropic subspace";
    if (degOnE L != 0) or (rank source L.yAction != 2) then error "L should be a degree 0 line bundle";
    if (((L.yAction)^2)_(0,0) + (-1)^g*f != 0) then error "L should be a line bundle on E";
    
    kk:=coefficientRing ring f;
    u:=(vars S)_{0..g-1} | (vars S)_{2*g, 2*g+1};
    
    M':=tensorProduct(M,L);
    (Mqq1,Mqq2):=cliffordModuleToMatrixFactorization(M',S); -- MF of qq
    qq:=(Mqq1*Mqq2)_(0,0);
    q1:=diff(S_(numgens S-2),qq);
    q2:=diff(S_(numgens S-1),qq);
    S':=(coefficientRing S)[drop (gens S,-2)]; -- ring without s,t
    CI:=S'/ideal (sub(q1,S'), sub(q2,S')); -- complete intersection ring
    resCI:=cliffordModuleToCIResolution (M', S, CI);
    BCI:=betti resCI;
    
    d:=first last (select (keys BCI, k->BCI_k==1));
    uL:=sub(mingens ideal (resCI.dd_d)_{0}, S);
    
    assert((dim ideal uL, degree ideal uL) ==(dim ideal u, degree ideal u));
    uL
    )

-- input should be a CliffordModule.
randomIsotropicSubspace = method()
randomIsotropicSubspace(CliffordModule, PolynomialRing) := (M,S) -> (
    -- Input:
    -- M, a CliffordModule, corresponding to a maximal isotropic subspace u
    -- S = kk[x_0..y_(g-1), z_0,z_1, s, t], a poly. ring in (2g+4) variables
    
    -- Output:
    -- I, a Matrix, presenting a maximal isotropic subspace
    -- corresponding to the translation of u by a random line bundle of degree 0.
    
    g:=(numgens S)//2 - 2;
    u:=(vars S)_{0..g-1} | (vars S)_{2*g, 2*g+1};

    if (rank source M.evenCenter != 2^(g+1)) then error "M should be a Clifford module associated to a maximal isotropic subspace";
    
    f:=M.hyperellipticBranchEquation;
    L:=randomLineBundle(0,f);
    
    I:=translateIsotropicSubspace(M,L,S);
    
    sub(I, S)
    )


TEST///
-- needsPackage "PencilsOfQuadrics"
kk=ZZ/101;g=2
(S,qq,R,u, M1,M2, Mu1, Mu2) = randomNicePencil(kk,g);

M=cliffordModule (Mu1, Mu2, R);
f=M.hyperellipticBranchEquation;
L=randomLineBundle(0,f)
uL=translateIsotropicSubspace(M,L,S)

rU=randomIsotropicSubspace(M,S)
assert (betti rU == betti u)
///


///
cliffModuleToCIModule = method()
cliffModuleToCIModule(CliffordModule,Ring) := (M,R) ->()
///

cliffordModuleToCIResolution = method()
cliffordModuleToCIResolution(CliffordModule,Ring, Ring) :=(M,S,CI) ->(
    (M1,M2) := cliffordModuleToMatrixFactorization(M,S);
    StoCI := map(CI, S, gens CI | {0,0});
    i := first max degrees target M.evenCenter;
    B1 := sub(basis(i+2,target(M.oddCenter)),S);
    B2 := sub(basis(i,target(M.evenCenter)),S);
    d1 :=first degrees target transpose  M1 - (first degrees target B1);
    assert(degrees target ((transpose M1)**S^d1)==degrees target B1);
    dd1 := (((transpose M1)**S^d1)*B2)//B1;
    dd1':= StoCI (dd1);
    res coker  dd1'
    )
-*
cliffordModuleToCIResolution(CliffordModule,Ring, Ring) :=(M,S,CI) ->(
    (M1,M2) := cliffordModuleToMatrixFactorization(M,S);
    --M1 goes odd to even; M2 goes even to odd
    i := first max degrees target M.evenCenter;
    B1 := sub(basis(i+1,target(M.oddCenter)),S);
    --B1 has target odd
    B2 := sub(basis(i,target(M.evenCenter)),S); 
    --B2 has target even
    StoCI := map(CI, S, gens CI | {0,0});
    d := StoCI ((M1*B1)//B2);
    res(coker d, LengthLimit => 5)[i]
    )
*-
///
restart
loadPackage "PencilsOfQuadrics"
///
TEST///
kk = ZZ/101;g=1
(S,qq,R,u, M1,M2, Mu1, Mu2) = randomNicePencil(kk,g);
diff(matrix{{S_(2*g+2), S_(2*g+3)}}, qq)
P = kk[drop(gens S, -2)]
qs = sub(diff(matrix{{S_(2*g+2), S_(2*g+3)}}, qq), P)
CI = P/ideal qs

F = res( coker (sub(u, CI)), LengthLimit => 3)
betti (FF = res( coker transpose F.dd_3, LengthLimit => 5))
M = cliffordModule(Mu1, Mu2, R)
betti (F1=cliffordModuleToCIResolution(M,S,CI)) 
betti FF
betti (FFF = res coker FF.dd_5)
q1 = diff(S_(2*g+2),qq)
q2 = diff(S_(2*g+3),qq)
N = (S^1/(ideal(q1,q2))**coker sub(F.dd_2,S))
betti res (N, LengthLimit =>10)
betti F1.dd_(g+3-(g%2))
assert(ideal F1.dd_(g+3-(g%2))_{0} ==ideal sub(u,CI))
M = cliffordModule(M1,M2,R)
betti (F2=cliffordModuleToCIResolution(M,S,CI)) 
assert(ideal F2.dd_(2*g+3)_{0}^{0..2*g+1} == ideal gens CI)
----- now genus 2
g=2
(S,qq,R,u, M1,M2, Mu1, Mu2) = randomNicePencil(kk,g);
diff(matrix{{S_(2*g+2), S_(2*g+3)}}, qq)
P = kk[drop(gens S, -2)]
qs = sub(diff(matrix{{S_(2*g+2), S_(2*g+3)}}, qq), P)
CI = P/ideal qs

F = res( coker (sub(u, CI)), LengthLimit => 3)
betti (FF = res( coker transpose F.dd_3, LengthLimit => 5))
M = cliffordModule(Mu1, Mu2, R)
betti (F1=cliffordModuleToCIResolution(M,S,CI)) 
betti FF
betti F1.dd_(g+3-(g%2))
assert(ideal F1.dd_(g+3-(g%2))_{0} ==ideal sub(u,CI))
M = cliffordModule(M1,M2,R)
betti (F2=cliffordModuleToCIResolution(M,S,CI)) 
assert(ideal F2.dd_(2*g+3)_{0}^{0..2*g+1} == ideal gens CI)
-*
g=3
(S,qq,R,u, M1,M2, Mu1, Mu2) = randomNicePencil(kk,g);
diff(matrix{{S_(2*g+2), S_(2*g+3)}}, qq)
P = kk[drop(gens S, -2)]
qs = sub(diff(matrix{{S_(2*g+2), S_(2*g+3)}}, qq), P)
CI = P/ideal qs

F = res( coker (sub(u, CI)), LengthLimit => 3)
betti (FF = res( coker transpose F.dd_3, LengthLimit => 5))
M = cliffordModule(Mu1, Mu2, R)
betti (F1=cliffordModuleToCIResolution(M,S,CI)) 
betti FF
betti F1.dd_(g+3-(g%2))
assert(ideal F1.dd_(g+3-(g%2))_{0} ==ideal sub(u,CI))
M = cliffordModule(M1,M2,R)
betti (F2=cliffordModuleToCIResolution(M,S,CI)) 
assert(ideal F2.dd_(2*g+3)_{0}^{0..2*g+1} == ideal gens CI)
*-

///

TEST///
restart
loadPackage "PencilsOfQuadrics"
debug PencilsOfQuadrics
kk = ZZ/101
R = kk[s,t,x,y,z]
mm = ideal(x,y,z)
L = ext1Prep(R,mm)
assert ((L_0)_1 == coker matrix{{x^2,y,z}})
///    
    


ciModuleToCliffordModule = method()
ciModuleToCliffordModule Module := M ->(
(e1,e0) := ciModuleToMatrixFactorization M;
S := ring e1;
n := numgens S - 2;
kk := coefficientRing S;
R := kk[S_n, S_(n+1)];
cliffordModule(e1,e0,R)
)	

ciModuleToMatrixFactorization = method()
ciModuleToMatrixFactorization  Module := M ->(
Ubar := ring M;
n := numgens Ubar;
--needsPackage "CompleteIntersectionResolutions";
(d0,d1) := EisenbudShamashTotal M;
S' := ring d0;
kk := coefficientRing Ubar;
S := kk[gens Ubar,s,t];
S'toS := map(S, S', apply(n,i->S_(i+2))|{S_0,S_1} );
fixDegs := d->( -- are the ceiling/floor functions applied correctly?
    d' := S'toS d;
    td := degrees target d;
    sd := degrees source d;
    td':= apply(td, D -> ceiling(3*D_0/2 -D_1));
    sd':= apply(sd, D -> floor(3*D_0/2 -D_1));
    map(S^td',S^sd',d')
    );
e0 := fixDegs d0;
e1 := fixDegs d1;
assert(isHomogeneous e0 and isHomogeneous e1);
(e1,e0)
)	


///

restart
loadPackage "PencilsOfQuadrics"
setRandomSeed 0
n = 4
c = 2
kk = ZZ/101
U = kk[x_0..x_(n-1)]

qq = matrix{{x_0^2+x_1^2,x_0*x_1}}
qq = random(U^1, U^{2:-2})
Ubar = U/ideal qq
M = coker vars Ubar
M = coker random(Ubar^2, Ubar^{-1,-2,-2})
(e1,e0) = ciModuleToMatrixFactorization M
source e0 == target e1
-degrees target e0+ degrees source e1
C = ciModuleToCliffordModule M
keys C
C.evenOperators
C.symmetricM
C.evenCenter

randomNicePencil()
randomNicePencil(kk,2)


///


isMinimal = method()
isMinimal(Ideal, ChainComplex) := (mm, F) -> (
    --tests whether the differential is in mm
    n := length F;
    R := ring F;
    k := R^1/mm;
    all(n, i-> F.dd_i**k == 0)
    )
isMinimal ChainComplex := F -> (
    mm := ideal vars ring F;
    isMinimal(mm,F))

///
restart
loadPackage "PencilsOfQuadrics"
debug PencilsOfQuadrics
check PencilsOfQuadrics
///
      makeTlocal = method()
      --local copy of code from "CompleteIntersectionResolutions"
      makeTlocal(Matrix, ChainComplex,ZZ) := (ff,F,i) ->(
           -*
           If ff is an c x 1 matrix and
           F is a chain complex
           over R = S/(ideal ff), 
           of codim c this returns a list of the c ci-operators
           F_i \to F_{i-2}
           corresponding to the entries of ff.
           *-
           c := numcols ff;
           degsff := flatten((degrees ff)_1);
           R := ring F;
           S := ring ff;
           complete F;
           minF := min F;
           d0 := sub(F.dd_i, S);
           d1 := sub(F.dd_(i-1), S);
           Ftar := target d1;
           Fsour := source d0;
           d2 := d1*d0;
           T := (d2//(ff**Ftar));
           I := id_(source ff);
           u := apply(c, j-> (I^{j}**Ftar)*T);
           --check: is d1*d0 = sum ff_{i}*u_i 
           if d1*d0 != map(Ftar, Fsour, sum(c, i-> u_i**ff_{i})) then 
                        error{"doesn't add up"};
           ret := map(R,S);
           apply(u, u1 -> ret u1)
           )


ExtIntoK = method()
     ExtIntoK Module := M -> (
          --If M is a module over a complete intersection R of codim c,
          --the script returns   
          --Ext^*(M,(ring M)^1/(ideal vars ring M))
          --graded in POSITIVE degrees
          --as a module over the polynomial ring kk[X_1..X_(codim R)],
          --where the vars have degree 2
          R := ring M;
          kk := coefficientRing R;
          kkk := (ring M)^1/(ideal vars ring M);
          E := Ext(M,kkk);
          TE := ring E;
          c := numgens source presentation R;
          X := local X;
          T := kk[X_0..X_(c-1), Degrees => toList(c:{2})];
          v := map(T,
               ring E, 
               vars T | matrix{toList ((numgens R):0_T)}, 
               DegreeMap => i -> {-first i} );
          prune coker v presentation E)

ExtIntoK(Ideal, Module) := (I,M) -> (
          --If M is a module over a complete intersection R of codim c that is a flat
	  -- algebra over 
	  --a polynomial subring T (eg R = k[s,t][x_0...x_n]/s*q1(x)+t*q2(x))
          --and I is an ideal such that T = R/I,
	  --the scritp returns 
          --Ext^*(M,R/I)
          --graded in POSITIVE degrees
          --as a module over T[X_0...X_c]
          --where the vars have degree 2
          R := ring M;
          kk := coefficientRing R;
          T := R/I;
	  varsT := flatten entries compress vars T;
          E := Ext(M,R^1/I);
          TE := ring E;
	  T' := minimalPresentation (TE/sub(I,TE));
	  coker sub(presentation E, T')
	  )
    
horizontalConcatenate = L ->(
   -- L is a list of matrices with the same number of rows
   M := L_0;
   scan(#L-1, i-> M = M|L_(i+1));
   M)

	  


TEST///
restart
loadPackage "PencilsOfQuadrics"
debug PencilsOfQuadrics
N = ZZ^2
L = {id_N,id_N}
horizontalConcatenate L
///

beginDocumentation()

document {
  Key => PencilsOfQuadrics,
  Headline => "Clifford Algebra of a Pencil of quadratic forms on PP^(2g+1)",
  "The Clifford algebra forms a link between the intersection of two quadrics X
  and a hyperelliptic curve E. For example, one can recover the coordinate ring
  of the hyperelliptic curve as the center of the even Clifford algebra. Using
  a maximal linear subspace contained in the intersection, we get a Morita bundle
  that connects graded modules over the coordinate ring of the hyperelliptic
  curve and modules over the even Clifford algebra. 
  
  This leads to a proof of Reid's theorem which identifies the set of maximal isotropic 
  subspaces in the complete intersection of two quadrics to the set of degree 0 line bundles 
  on E. This approach was taken in an unpublished manuscript of Ragnar-Olaf Buchweitz and Frank-Olaf Schreyer.
  The package allows a computational approach to the result of Bondal and Orlov 
  which showed that the Kuznetsov component of X and the derived category of E are equivalent 
  by a Fourier-Mukai transformation (see Section 2 of [A. Bondal, D. Orlov, arXiv:alg-geom/9506012], 
  or Section 6 of [A. Bondal, D. Orlov, Proceedings of ICM, Vol. II (Beijing, 2002)]).",
  
  PARA{},
  "We demonstrate this,
  over finite fields, with
  the constructions of further random linear spaces on the intersection of two quadrics,
  and random Ulrich modules of lowest possible rank on the complete intersection
  of two quadrics for small g.",
    PARA{},
     SUBSECTION "Types",
     UL{
	TO CliffordModule,
	TO RandomNicePencil,
	TO VectorBundleOnE
       },
     SUBSECTION "Basic Construction of the Clifford Algebra",
     UL{
	TO cliffordOperators,
	TO symMatrix,
	TO centers,
	TO evenCenter,
	TO oddCenter,
	TO hyperellipticBranchEquation
        },
     SUBSECTION "Vector Bundles",
     UL{
      TO randomLineBundle, -- kk has to be finite
      TO vectorBundleOnE,
      TO yAction,
      TO tensorProduct,
      TO degOnE,
      TO orderInPic, -- kk has to be finite
      TO randomExtension
       },
     SUBSECTION "Clifford Modules",
     UL{   
	 TO cliffordModule,
         TO tensorProduct,
	 TO evenOperators,
	 TO oddOperators
       },
     SUBSECTION "Computations using Clifford Algebras",
     UL{
       TO translateIsotropicSubspace,
       TO randomIsotropicSubspace, -- kk has to be finite
       TO searchUlrich -- kk has to be finite
      }
   }

doc ///
   Key 
    matrixFactorizationK
    (matrixFactorizationK,Matrix,Matrix)
   Headline
    Knoerrer matrix factorization from a bilinear form X*transpose Y
   Usage
    (M1, M2) = matrixFactorizationK(X,Y) 
   Inputs
    X:Matrix
     row matrix of linear forms with constant coefficients
    Y:Matrix
     row matrix of linear forms with linear coefficents of same length as X
   Outputs
    M1:Matrix
    M2:Matrix
   Description
    Text
     Produces a matrix factorization (M1,M2) of the bilinear form X*transpose Y.
     It does this by specializing
     the formula given by Knoerrer  for $\sum X_i*Y_i$.
    Example
     kk=ZZ/101
     n=2
     R=kk[a_0..a_(binomial(n+2,2))]
     S=kk[x_0..x_(n-1),a_0..a_(binomial(n+2,2))]
     M=genericSymmetricMatrix(S,a_0,n)
     X=(vars S)_{0..n-1}
     Y=X*M
     (M1,M2)=matrixFactorizationK(X,Y)
     M12=M1*M2
///


doc ///
   Key
    ciModuleToMatrixFactorization
    (ciModuleToMatrixFactorization,Module)    
   Headline
    transforms a module over a complete intersection of 2 quadrics into a matrix factorization
   Usage
    (e1,e0) = ciModuleToMatrixFactorization M
   Inputs
    M:Module
     module over a complete intersection of 2 quadrics
   Outputs
    e1:Matrix
    e0:Matrix
     the matrix factorization, in the form needed for cliffordModule(e1,e0,R)
   Description
    Text
     Part of the series of explicit functors giving category equivalences:
    
     cliffordModule
     
     cliffordModuleToCIResolution
    
     cliffordModuleToMatrixFactorization
    
     ciModuleToMatrixFactorization
    
     ciModuleToCliffordModule
     
     This function uses the bihomogeneous matrix factorization produced by 
     the script EisenbudShamashTotal in the package CompleteIntersectionResolutions.
     Using the multigrading, a new matrix factorization in the form needed for
     cliffordModule(e1,e0,R), where R=k[s,t].
    Example
     n = 4
     c = 2
     kk = ZZ/101
     U = kk[x_0..x_(n-1)]
     qq = matrix{{x_0^2+x_1^2,x_0*x_1}}
     qq = random(U^1, U^{2:-2})
     Ubar = U/ideal qq
     M = coker vars Ubar
     betti (fM=res M)
     betti res coker transpose fM.dd_3 
     (e1,e0) = ciModuleToMatrixFactorization M;
    Text
     Check that it's a matrix factorization:
    Example
     source e0 == target e1
     0 == e0*e1 - diagonalMatrix(ring e0, apply(numcols e0, i->(e0*e1)_0_0))
     degrees source e1-degrees target e0
   Caveat
   SeeAlso
    cliffordModule
    cliffordModuleToCIResolution
    cliffordModuleToMatrixFactorization
    ciModuleToMatrixFactorization
    ciModuleToCliffordModule

    
///
doc ///
   Key
    ciModuleToCliffordModule
    (ciModuleToCliffordModule,Module)    
   Headline
    transforms a module over a complete intersection of 2 quadrics into a Clifford Module.
   Usage
    C = ciModuleToCliffordModule M
   Inputs
    M:Module
     module over a complete intersection of 2 quadrics
   Outputs
    C:CliffordModule
   Description
    Text
     Part of the series of explicit functors giving category equivalences:

     cliffordModule
     
     cliffordModuleToCIResolution
    
     cliffordModuleToMatrixFactorization
    
     ciModuleToMatrixFactorization
    
     ciModuleToCliffordModule
     
     This function uses ciModuleToMatrixFactorization, and then calls cliffordModule
    Example
     n = 4
     c = 2
     kk = ZZ/101
     U = kk[x_0..x_(n-1)]
    
     qq = matrix{{x_0^2+x_1^2,x_0*x_1}}
     qq = random(U^1, U^{2:-2})
     Ubar = U/ideal qq
     M = coker vars Ubar
     M = coker random(Ubar^2, Ubar^{-1,-2,-2})
     C = ciModuleToCliffordModule M
     keys C
     C.evenOperators
     C.symmetricM
     C.evenCenter
   SeeAlso
    cliffordModule
    cliffordModuleToCIResolution
    cliffordModuleToMatrixFactorization
    ciModuleToMatrixFactorization
    ciModuleToCliffordModule
///


doc ///
   Key
    randomNicePencil
    (randomNicePencil, Ring, ZZ)
   Headline
    sets up a random example to construct Clifford algebra and representation
   Usage
    (S, qq, R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g) 
   Inputs
    kk:Ring
     the ground field, not char 2, please!
    g:ZZ
     genus of the associated hyperelliptic curve.
   Outputs
    S:Ring
     polynomial ring in g x's, g y's z_1,z_2 and s, t
    qq:RingElement
     Element of S, quadratic in x_0..z_2 and linear in s,t
    R:Ring
     polynomial ring kk[s,t]
    u:Matrix
     1 x g+2 row matrix with entries x_0..x_{(g-1)}, z_1,z_2; 
     generators of the ideal of an isotropic subspace for qq
    M1:Matrix
    M2:Matrix
     Matrices over S such that M1*M2 = qq times an identity of size 2^{2g+1}
    Mu1:Matrix
    Mu2:Matrix
     Matrices over S such that Mu1*Mu2 = qq times an identity of size 2^{g+1}
   Description
    Text
     Chooses a random example of a pencil of quadrics qq = s*q1+t*q2 
     with a fixed isotropic subspace (defined by ideal u)
     and a fixed corank one quadric in normal form q1.
     
     When called with no arguments it prints a usage message.
     
     The variables of S that are entries of X:= matrix \{\{x_0..y_{(g-1)},z_1,z_2\}\} 
     \, represent coordinates on PP_R^{2g+1}.
     
     M1, M2 are consecutive high syzygy matrices in the miminal (periodic) resolution
     of kk[s,t] = S/(ideal X) as a module over S/qq. These are used to construct the
     Clifford algebra of qq.

     Mu1, Mu2 are consecutive high syzygy matrices in the miminal (periodic) resolution
     of S/(ideal u) as a module over S/qq. These are used to construct a Morita bundle
     between the even Clifford algebra of qq and the hyperelliptic curve
     branched over the degeneracy locus of the pencil,
 
      \{(s,t) | s*q1+t*q2 is singular\} \subset PP^1.
    Example
     kk=ZZ/101
     g=1
     (S, qq, R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g); 
     gens S
     q1 = diff(S_(2*g+2),qq)
    Text
     a quadratic form of corank 1 (corresponding to a branch point of E-->PP^1
     in normal form.
    Example
     ideal u -- an isotropic space for q1 and q2
     betti Mu1, betti Mu2
     Mu1*Mu2- qq*id_(target Mu1) == 0
   SeeAlso
    CliffordModule
    cliffordModule    
    cliffordOperators
    centers    
///

doc ///
    Key
    	randNicePencil
    	(randNicePencil, Ring, ZZ)
    Headline
        sets up a random pencil of quadrics, and returns a hash table of the type RandomNicePencil.
    Usage
    	L = randNicePencil(kk,g)
    Inputs
        kk:Ring	
	    the ground field, not char 2, please!
	g:ZZ
	    genus of the associated hyperelliptic curve.
    Outputs
    	L:RandomNicePencil
    Description
    	Text
	    Generates a random pencil of quadrics in the same way as randomNicePencil(kk,g). 
	    Returns a hash table of the type RandomNicePencil.
	Example
	    kk=ZZ/101;
	    g=1;
	    L=randNicePencil(kk,g)
    	    keys L
	    L.qqRing
	    L.quadraticForm
	    L.baseRing
	    (L.matFact1 * L.matFact2) - (L.quadraticForm)**id_(source L.matFact1)
	    (L.matFact2 * L.matFact1) - (L.quadraticForm)**id_(source L.matFact1) 
	    L.isotropicSpace
	    (L.matFactu1 * L.matFactu2) - (L.quadraticForm)**id_(source L.matFactu1)
	    (L.matFactu2 * L.matFactu1) - (L.quadraticForm)**id_(source L.matFactu1)
    SeeAlso
    	randomNicePencil
	RandomNicePencil
///


doc ///
   Key
    cliffordOperators
    (cliffordOperators, Matrix, Matrix, Ring)
   Headline
    Generators for a Clifford Algebra
   Usage
    (eOdd, eEv) = cliffordOperators (M1,M2,R)
   Inputs
    R:Ring
     polynomial ring of the form kk[U], 
     where U are parameter variables
    M1:Matrix
     over an auxilliary ring S = kk[X,Y,Z,U]
    M2:Matrix
     M1, M2 a matrix factorization: M1*M2- qq*id = 0 for a quadratic form qq on S
   Outputs
    eOdd:List
    eEv:List
     each list has length n = numgens S - numgens R; 
     the elements are matrices of size rank target M1.
   Description
    Text
     The Clifford algebra C := Cliff(qq) of a quadratic form qq in n=2d variables is a 
     free ZZ/2-graded algebra of rank 2^{n} where n = numgens S-numgens R (if R is ZZ-graded,
     and M1, M2 are linear in the variables of R, then C inherits a ZZ-grading; this is our usual case.
     As an R-module C = C0++C1, with each component of rank 2^{(n-1)}. 
     The operators eOdd_i go from C1 to C0;
     the operators eEv go from C0 to C1.
     
     We have eOdd_i*eEv_j+eOdd_j*eEv_i = B(e_i,e_j), where
     the e_i form a basis of the space on which qq acts and B is the bilinear form associated to 2qq
     thus the the pairs (eOd_i,eEv_i) form a representation of Cliff(qq). 
     --If qq is nonsingular over the generic point of R, then C is an Azumaya algebra over R, and this implies that the representation is faithful.
     
     In the following we construct the generic symmetric
     bilinear form on 2d variables and make a quadratic form
     qq out of it.
    Example
     kk=ZZ/101; d=1;
     n=2*d
     R=kk[a_0..a_(binomial(n+2,2))]
     S=kk[x_0..x_(n-1),a_0..a_(binomial(n+2,2))]
     M=genericSymmetricMatrix(S,a_0,n)
     X=(vars S)_{0..n-1}
     Y=X*M
     qq = X*transpose Y
     (M1,M2)=matrixFactorizationK(X,Y);
     (eOdd,eEv)=cliffordOperators(M1,M2,R);
    Text
     we check two of the relations of the Clifford algebra:
    Example
     (eOdd_0*eEv_0+eOdd_0*eEv_0)_(0,0) == 2*R_0
     (eOdd_0*eEv_1+eOdd_1*eEv_0)_(0,0) == 2*R_1
   SeeAlso
    centers
    cliffordModule
///
doc ///
   Key
    CliffordModule
   Headline
    hash table holding details of a Clifford module
   Description
    Text
     the keys are
	oddOperators => uOdd', 
	evenOperators => uEv', 
	evenCenter=>c0', 
	oddCenter => c1', 
	symmetricM => Q,
	hyperellipticBranchEquation => f
   SeeAlso
    cliffordModule
///

doc ///
    Key
    	RandomNicePencil
    Headline
    	hash table holding details of a random pencil of quadrics
    Description
    	Text
    	    the keys are
	    	qqRing => S, 
		quadraticForm => a pencil of quadrics qq=s*q1+t*q2, 
		baseRing => R = kk[s,t], 
		isotropicSpace => u, 
		matFact1 => M1, 
		matFact2 => M2, 
		matFactu1 => Mu1, 
		matFactu2 => Mu2, 
    SeeAlso	
    	randomNicePencil
	randNicePencil
///



    
doc ///
    Key
     evenOperators
   Headline
    part of a CliffordModule
   Usage
    uEv = M.evenOperators
   Outputs
    uEv:List
     of the even operators on M
   Description
    Text
     The list of the even operators uEv_i: M_0\to M_1
    Example
     kk = ZZ/101
     g = 1
     (S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g);
     M = cliffordModule(M1,M2, R)
     M.evenOperators
   SeeAlso
    oddOperators
    evenCenter
    oddCenter
    symMatrix
    hyperellipticBranchEquation
///
doc ///
    Key
     oddOperators
   Headline
    part of a CliffordModule
   Usage
    uOdd = M.oddOperators
   Outputs
    uOdd:List
     of the odd operators on M
   Description
    Text
     The list of the odd operators uOdd_i: M_1\to M_0
    Example
     kk = ZZ/101
     g = 1
     (S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g);
     M = cliffordModule(M1,M2, R)
     M.oddOperators
   SeeAlso
    evenOperators
    evenCenter
    oddCenter
    symMatrix
    hyperellipticBranchEquation
///
doc ///
    Key
     evenCenter
   Headline
    part of a CliffordModule
   Usage
    c0 = M.evenCenter
   Outputs
    c0:Matrix
   Description
    Text
     Gives the action of Haag's center element y of the even Clifford algebra on the even part of M
    Example
     kk = ZZ/101
     g = 1
     (S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g);
     M = cliffordModule(M1,M2, R)
     M.evenCenter
   SeeAlso
    evenOperators
    oddOperators
    oddCenter
    oddCenter
    symMatrix
    hyperellipticBranchEquation
///
doc ///
    Key
     symmetricM
   Headline
    part of a CliffordModule
   Usage
    Q = M.symmetricM
   Outputs
    Q:Matrix
     over k[s,t]
   Description
    Text
     the underlying pencil of quadratic forms
    Example
     kk = ZZ/101
     g = 1
     (S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g);
     M = cliffordModule(M1,M2, R)
     M.symmetricM
    Text
     this can also be obtained by
    Example
     symMatrix(M.evenOperators,M.oddOperators)
   SeeAlso
    evenOperators
    oddOperators
    oddCenter
    oddCenter
    symMatrix
    hyperellipticBranchEquation
///

doc ///
    Key
     oddCenter
   Headline
    part of a CliffordModule
   Usage
    c1 = M.oddCenter
   Outputs
    c1:Matrix
   Description
    Text
     Gives the action of Haag's center element y of the even Clifford algebra on the odd part of M
    Example
     kk = ZZ/101
     g = 1
     (S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g);
     M = cliffordModule(M1,M2, R)
     M.oddCenter
   SeeAlso
    evenOperators
    oddOperators
    oddCenter
    oddCenter
    symMatrix
    hyperellipticBranchEquation
///

doc ///
    Key
     symMatrix
   Headline
    part of a CliffordModule
   Usage
    s = symMatrix(eOdd,eEv)
   Inputs
    eOdd:List
    eEv:List
     operators on a Clifford Module
   Outputs
    s:Matrix
     over k[s,t], the base of the Clifford algebra
   Description
    Text
     Computes the matrix given by the pencil of quadrics defining the Clifford algebra.

    Example
     kk = ZZ/101
     g = 1
     (S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g);
     M = cliffordModule(M1,M2, R)
     M.evenOperators
--     symmetricMatrix(M.evenOperators,M.oddOperators)
     symMatrix(M.evenOperators,M.oddOperators)
   SeeAlso
    evenOperators
    oddOperators
    oddCenter
    oddCenter
    hyperellipticBranchEquation
///

-*
doc ///
    Key
     symmetricMatrix
   Headline
    part of a CliffordModule
   Usage
    s = symmetricMatrix(eOdd,eEv)
   Inputs
    eOdd:List
    eEv:List
     operators on a Clifford Module
   Outputs
    s:Matrix
     over k[s,t], the base of the Clifford algebra
   Description
    Text
     the pencil of quadratic forms defining the Clifford algebra. The functions
     symmetricMatrix and symMatrix are the same.
    Example
     kk = ZZ/101
     g = 1
     (S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g);
     M = cliffordModule(M1,M2, R)
     M.evenOperators
     symmetricMatrix(M.evenOperators,M.oddOperators)
     symMatrix(M.evenOperators,M.oddOperators)
   SeeAlso
    evenOperators
    oddOperators
    oddCenter
    oddCenter
    hyperellipticBranchEquation
///
*-

doc ///
    Key
    	hyperellipticBranchEquation
    Headline
    	part of a CliffordModule
    Usage
    	f = M.hyperellipticBranchEquation
    Outputs
    	f : RingElement
    Description
    	Text
	    Gives the branch equation of the set of 
	    points over which the associated quadratic form is singular. It is same as 
	    the determinant of the symmetric matrix M.symmetricM. 
	Example
	    kk=ZZ/101;
	    g=1;
	    rNP=randNicePencil(kk,g);
	    M=cliffordModule(rNP.matFact1,rNP.matFact2,rNP.baseRing)
	    
	    f=M.hyperellipticBranchEquation
	    sM=M.symmetricM
	    f == det sM
    SeeAlso
    	evenOperators
	oddOperators
	oddCenter
	evenCenter
	symmetricM
///   

doc ///
    Key
    	baseRing
    Headline
    	part of a RandomNicePencil
    Usage
    	R = rNP.baseRing
    Outputs
    	R : Ring
	    polynomial ring kk[s,t]
    Description
    	Text
	     The base ring kk[s,t] which is the coordnate ring of PP^1.  
	Example
	    kk=ZZ/101;
	    g=1;
	    rNP=randNicePencil(kk,g);
	    
	    R=rNP.baseRing
    SeeAlso
    	qqRing
	quadraticForm
	matFact1
	matFact2
	matFactu1
	matFactu2
	isotropicSpace
///    

doc ///
    Key
    	qqRing
    Headline
    	part of a RandomNicePencil
    Usage
    	S = rNP.qqRing
    Outputs
    	S : Ring
	    polynomial ring in g x's, g y's, z_1, z_2, and s, t.
    Description
    	Text
	     The ambient polynomial ring where the quadratic form qq = s*q1 + t*q2 lives.
	Example
	    kk=ZZ/101;
	    g=1;
	    rNP=randNicePencil(kk,g);
	    
	    S=rNP.qqRing
    SeeAlso
    	baseRing
	quadraticForm
	matFact1
	matFact2
	matFactu1
	matFactu2
	isotropicSpace
///    

doc ///
    Key
    	quadraticForm
    Headline
    	part of a RandomNicePencil
    Usage
       qq = rNP.quadraticForm
    Outputs
    	qq : RingElement
	    an element of S, quadratic in x_0..z_2 and linear in s, t.
    Description
    	Text
	     the polynomial that represents a pencil of quadrics qq=s*q1+t*q2 with a fixed isotropic subspace
	     and a fixed corank one quadric in normal form q1.
	Example
	    kk=ZZ/101;
	    g=1;
	    rNP=randNicePencil(kk,g);
	    S=rNP.qqRing;
	    
	    qq=rNP.quadraticForm
	    q1=diff(S_(2*g+2), qq)
	    q2=diff(S_(2*g+3), qq)
	    qq==S_(2*g+2)*q1+S_(2*g+3)*q2
    SeeAlso
    	qqRing
	baseRing
	matFact1
	matFact2
	matFactu1
	matFactu2
	isotropicSpace
///    

doc ///
    Key
    	isotropicSpace
    Headline
    	part of a RandomNicePencil
    Usage
       u = rNP.isotropicSpace
    Outputs
    	u : Matrix
	    1 x (g+2) row matrix with entries x_0 .. x_{(g-1)}, z_1, z_2
    Description
    	Text
	     a row matrix whose entries are generators of the ideal of an isotropic subspace for qq.
	Example
	    kk=ZZ/101;
	    g=1;
	    rNP=randNicePencil(kk,g);
	    
	    u=rNP.isotropicSpace
    SeeAlso
    	qqRing
	baseRing
	quadraticForm
	matFact1
	matFact2
	matFactu1
	matFactu2
///  

doc ///
    Key
    	matFact1
    Headline
    	part of a RandomNicePencil
    Usage
       M1 = rNP.matFact1
    Outputs
    	M1 : Matrix
	    a matrix over S such that (M1, M2) gives a matrix factorization of qq of size 2^{2g+1}.
    Description
    	Text
	     M1, M2 are consecutive high syzygy matrices in the minimal periodic resolution 
	     of the base ring R=S/(ideal matrix x_0..y_{(g-1)},z_1,z_2) as a module over S/(ideal qq). 
	     These are used to construct the Clifford algebra of qq.
	Example
	    kk=ZZ/101;
	    g=1;
	    rNP=randNicePencil(kk,g);
	    S=rNP.qqRing;
	    qq=rNP.quadraticForm;

	    M1=rNP.matFact1;
	    M2=rNP.matFact2;
	    M1*M2 - qq*id_(S^(2^(2*g+1)))
	    M1*M2 - M2*M1
    SeeAlso
    	qqRing
	baseRing
	quadraticForm
	matFact2
	matFactu1
	matFactu2
	isotropicSpace
///     

doc ///
    Key
    	matFact2
    Headline
    	part of a RandomNicePencil
    Usage
       M2 = rNP.matFact2
    Outputs
    	M2 : Matrix
	    a matrix over S such that (M1, M2) gives a matrix factorization of qq of size 2^{2g+1}.
    Description
    	Text
	     M1, M2 are consecutive high syzygy matrices in the minimal periodic resolution 
	     of the base ring R=S/(ideal matrix x_0..y_{(g-1)},z_1,z_2) as a module over S/(ideal qq). 
	     These are used to construct the Clifford algebra of qq.
	Example
	    kk=ZZ/101;
	    g=1;
	    rNP=randNicePencil(kk,g);
	    S=rNP.qqRing;
	    qq=rNP.quadraticForm;

	    M1=rNP.matFact1;
	    M2=rNP.matFact2;
	    M1*M2 - qq*id_(S^(2^(2*g+1)))
	    M1*M2 - M2*M1
    SeeAlso
    	qqRing
	baseRing
	quadraticForm
	matFact1
	matFactu1
	matFactu2
	isotropicSpace
///

doc ///
    Key
    	matFactu1
    Headline
    	part of a RandomNicePencil
    Usage
       Mu1 = rNP.matFactu1
    Outputs
    	Mu1 : Matrix
	    a matrix over S such that (Mu1, Mu2) gives a matrix factorization of qq of size 2^{g+1}
    Description
    	Text
	     Mu1, Mu2 are consecutive high syzygy matrices in the minimal periodic resolution 
	     of the isotropic subspace S/(ideal u) as a module over S/(ideal qq). 
	     These are used to construct a Morita bundle between the even Clifford algebra of qq 
	     and the hyperelliptic curve branched over the degeneracy locus of the pencil.
	Example
	    kk=ZZ/101;
	    g=1;
	    rNP=randNicePencil(kk,g);
	    S=rNP.qqRing;
	    qq=rNP.quadraticForm;

	    Mu1=rNP.matFactu1;
	    Mu2=rNP.matFactu2;
	    Mu1*Mu2 - qq*id_(S^(2^(g+1)))
	    Mu1*Mu2 - Mu2*Mu1
    SeeAlso
    	qqRing
	baseRing
	quadraticForm
	matFact1
	matFact2
	matFactu2
	isotropicSpace
///

doc ///
    Key
    	matFactu2
    Headline
    	part of a RandomNicePencil
    Usage
       Mu2 = rNP.matFactu2
    Outputs
    	Mu2 : Matrix
	    a matrix over S such that (Mu1, Mu2) gives a matrix factorization of qq of size 2^{g+1}
    Description
    	Text
	     Mu1, Mu2 are consecutive high syzygy matrices in the minimal periodic resolution 
	     of the isotropic subspace S/(ideal u) as a module over S/(ideal qq). 
	     These are used to construct a Morita bundle between the even Clifford algebra of qq 
	     and the hyperelliptic curve branched over the degeneracy locus of the pencil.
	Example
	    kk=ZZ/101;
	    g=1;
	    rNP=randNicePencil(kk,g);
	    S=rNP.qqRing;
	    qq=rNP.quadraticForm;

	    Mu1=rNP.matFactu1;
	    Mu2=rNP.matFactu2;
	    Mu1*Mu2 - qq*id_(S^(2^(g+1)))
	    Mu1*Mu2 - Mu2*Mu1
    SeeAlso
    	qqRing
	baseRing
	quadraticForm
	matFact1
	matFact2
	matFactu1
	isotropicSpace
///

doc ///
   Key
    VectorBundleOnE
   Headline
     vector bundle on a hyperelliptic curve E
   Description
    Text
     If the curve has equation y^2 +/- f, then
     the bundle is represented by a vector bundle on PP^1
     and a matrix representing the action of y.
     the only  key is
     yAction
   SeeAlso
    vectorBundleOnE
    degOnE
    orderInPic
    randomLineBundle
    randomExtension
    yAction
///

doc ///
   Key
    cliffordModule
    (cliffordModule, Matrix, Matrix, Ring)
    (cliffordModule, List, List)
   Headline
    makes a clifford Module
   Usage
    M = cliffordModule(M1,M2,R)
    M = cliffordModule(eOdd,eEv)
   Inputs
    M1:Matrix
    M2:Matrix
     M1, M2 a matrix factorization of a quadratic form qq
    R:Ring
     base ring of the quadratic form
    eEv:List
    eOdd:List
     lists such as the output of cliffordOperators(M1,M2)
   Outputs
    M:CliffordModule
   Description
    Text
     The keys
      oddOperators 
      evenOperators
     are the same as the two lists output by cliffordOperators(M1,M2)
    
     The keys 
      evenCenter
      oddCenter 
     yield the action of the center of the even 
     Clifford algebra of qq on the even, respectively odd
     parts of the Clifford module.
    
     The key
      symmetricM
     yields the matrix of coefficients of the quadratic form qq
	
     the key
      hyperellipticBranchEquation
     yields the branch equation in R -- that is, the equation
     of the set of points over which the quadratic form
     is singular.
    Example
     kk = ZZ/101
     g = 1
     (S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g);
     M = cliffordModule(M1,M2, R)
     Mu = cliffordModule(Mu1,Mu2, R)
    Text
     The symmetric matrices are the same for both:
    Example
     Mu.symmetricM
     M.symmetricM
    Text
     But the operators are twice the size for M (in both cases
     the same size as the corresponding matrix factorization
     Mu.evenCenter
     numrows(Mu.evenCenter) == numrows(Mu1)
     M.evenCenter
   SeeAlso
    matrixFactorizationK
    cliffordOperators
///

doc ///
   Key
    centers
    (centers,List,List)
   Headline
    even and odd action of the center of the even Clifford algebra
   Usage
    (c1,c2) = centers(eOdd,eEv)
   Inputs
    eOdd:List
    eEv:List
     lists output by cliffordOperators(M1, M2)
   Outputs
    c1:Matrix
    c2:Matrix
     action of the center on the even and odd modules
   Description
    Text
     The center of the even Clifford algebra of an
     even-dimensional nonsingular quadratic form
     represented by its symmetric matrix of coefficients
     sM
     is a degree 2 extension of the ground ring with
     R[y]/(y^2- (-1)^d*f), where f is the branch equation,
     f = det sM.
     The action of y on the  odd and even parts of a Clifford Module is represented
     by the pair of matrices  c0, c1 which can be computed by
     the following formula of Haag (see Satz 1 of [U. Haag, Arch. Math. 57, 546-554 (1991)]).
     
     -- , known to Buchweitz and rediscovered experimentally using this package :
      
      Let M be the clifford module, with operators
      eOdd and eEv as usual, and let sM be the 2d x 2d symmetric
      matrix of the quadratic form, produced by
      M.symmetricM. Let skM be the alternating matrix
      formed by taking the "top half" of sM and subtracting the
      "bottom half". For any even length ordered sublist
      I = i_1,i_2...i_{2k} of [2d], let
      eo_I = eEv_{i_1}*eOdd_{i_1} \cdots eEv_{i_{2k}}*eOdd_{i_{2k}}
      and, similarly,
      oe_I = eOdd_{i_1}*eEv_{i_1} \cdots eOdd_{i_{2k}}*eEv_{i_{2k}}.
      Let J be the complement of I.
      Then
      
      c1 = sum (-1)^{sgn J} Pfaffian((skM_J)^J)*eo_I
      
      c0 = sum (-1)^{sgn J} Pfaffian((skM_J)^J)*oe_I
      
      where the index I runs over all even length ordered 
      subsets of [2d].
    Example
     kk=ZZ/101; d=1;
     n=2*d
     R=kk[a_0..a_(binomial(n+2,2)-1)]
     S=kk[x_0..x_(n-1),a_0..a_(binomial(n+2,2)-1)]
     M=genericSymmetricMatrix(S,a_0,n)
     X=(vars S)_{0..n-1}
     Y=X*M
     (M1,M2)=matrixFactorizationK(X,Y);
     (eOdd,eEv)=cliffordOperators(M1,M2,R);
     sM = symMatrix(eOdd,eEv)
     f = det sM
     f == (cliffordModule(eOdd,eEv)).hyperellipticBranchEquation     
     (c0,c1)=centers(eOdd,eEv)
     assert(c0^2-(-1)^d*f*id_(source c0)==0)
     assert(c1^2-(-1)^d*f*id_(source c1)==0)
   SeeAlso
    matrixFactorizationK
    symMatrix
    hyperellipticBranchEquation
///
doc ///
   Key
    vectorBundleOnE
    (vectorBundleOnE, Matrix)
   Headline
    creates a VectorBundleOnE, represented as a matrix factorization
   Usage
    V = vectorBundleOnE M
   Inputs
    M:Matrix
     -square matrix on PP^1, representing the action of y
   Outputs
    V:VectorBundleOnE
   Description
    Text
     A vector bundle on a hyperelliptic curve E with
     equation y^2 - (-1)^g * f
     can be represeted by it's pushforward V to PP^1,
     under the degree 2 map, 
     which will be a vector bundle of twice the rank,
     together with a matrix 
     M = V.yAction,
     specifying the action of y. The matrix must therefore satisfy
     M^2 = (-1)^g * f.
     Here f is the hyperellipticBranchEquation, a form
     on PP^1 of degree 2g+2
    
     The following gives an example for g=1, constructing a
     random line bundle of degree 0, and computing
     its order in the Picard group; and the
     producing a random extension of this bundle
     by a random line bundle of order -1.
     
     The random matrix factorization of f has the form
     
     b c 
      
     a -b 
     
     where a is the lowest degree factor of
     f-b^2 (or f+b^2), depending on the desired sign,
     and values of b are taken randomly until one giving
     a nontrivial factorization over the given ground field
     is found. Note that this works well over a finite field,
     but is unlikely to work over QQ.
    Example
     kk=ZZ/101
     R = kk[s,t]
     f =(s+2*t)*(s+t)*(s-t)*(s-2*t)
     L0 = randomLineBundle(0,f)
     (L0.yAction)^2
     degOnE L0
     orderInPic L0
     L1 = randomLineBundle(-1,f)
     degOnE L1
     L1.yAction
     F = randomExtension(L1,L0)     
     F.yAction
     degOnE tensorProduct(L1,F)
   SeeAlso
    randomLineBundle
    randomExtension
    VectorBundleOnE
    yAction
    degOnE
    orderInPic
    tensorProduct
///

doc ///
    Key
    	yAction
    Headline
    	defines a vector bundle on E
    Usage
    	M = V.yAction
    Outputs
    	M:Matrix
	    a square matrix on PP^1, representing the action of y
    Description
    	Text
	    A matrix representing the action of y for the hyperelliptic curve E with equation y^2 - (-1)^g * f.
	Example
	    kk = ZZ/101;
	    R = kk[s,t];
	    f = (s+2*t)*(s+t)*(s-t)*(s-2*t);
	    L0 = randomLineBundle(0,f)
	    M = L0.yAction
	    M^2 - f*id_(source M)
    SeeAlso
    	randomLineBundle
	randomExtension
	vectorBundleOnE
	VectorBundleOnE
	degOnE
	orderInPic
	tensorProduct
///


doc ///
   Key
    tensorProduct
    (tensorProduct, Matrix, Matrix)
    (tensorProduct, CliffordModule, VectorBundleOnE)
    (tensorProduct,VectorBundleOnE, VectorBundleOnE)
   Headline
    tensor product of sheaves on the elliptic curve or sheaf times CliffordModule 
   Usage
    eta = tensorProduct(phi,psi)
    G = tensorProduct(M,V)    
    L = tensorProduct(L1,L2)
   Inputs
    phi:Matrix
    psi:Matrix
     representing the action of y on a sheaf on PP^1
    L1:VectorBundleOnE
    L2:VectorBundleOnE    
    M:CliffordModule
    V:VectorBundleOnE
   Outputs
    eta:Matrix
     action of y
    N:CliffordModule
    L:VectorBundleOnE
   Description
    Text
     Sheaves on the hyperelliptic curve y^2 -(-1)^{g}* f(s,t) are represented as sheaves on PP^1 together
     with the action of y. Clifford modules are represented as the action of maps
     eOdd_i: M_1 \to M_0 and eEv_i:M_0 \to M_1 between the even and odd parts of the 
     module. The result are the corresponding data for the tensor product. 
     
     
    Example
     kk=ZZ/101
     g=1
     (S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g) ;
     (uOdd,uEv)=cliffordOperators(Mu1,Mu2,R);
     symMatrix(uOdd,uEv)
     f=det symMatrix(uOdd,uEv);
     M = cliffordModule(uOdd, uEv);
     L = randomLineBundle(0,f);
     L.yAction
     L2 = tensorProduct(L,L)
     L2.yAction
     M' = tensorProduct(M,L)
     M.evenCenter
     M'.evenCenter     
   SeeAlso
    CliffordModule
    cliffordOperators
    symMatrix
    randomLineBundle
    evenCenter    
///

doc ///
    Key
    	degOnE
	(degOnE,VectorBundleOnE)
	(degOnE,Matrix)
    Headline
    	degree of a vector bundle on E
    Usage
    	d = degOnE(L)
	d = degOnE(phi)
    Inputs
    	L : VectorBundleOnE
	    a vector bundle on E
	phi : Matrix
	    a matrix which is the yAction of some VectorBundleOnE
    Outputs
    	d : ZZ
	    the degree of L
    Description
    	Text
	    Computes the degree of a vector bundle L on the hyperelliptic curve E.
	Example
	    kk=ZZ/101;
	    g=1;
	    
	    rNP=randNicePencil(kk,g);
	    f=(cliffordModule(rNP.matFact1,rNP.matFact2,rNP.baseRing)).hyperellipticBranchEquation;
	    
	    L0=randomLineBundle(0,f)
	    degOnE L0
	    L1=randomLineBundle(1,f)
    	    degOnE L1
	    Lm1=randomLineBundle(-1,f)
	    degOnE Lm1 
    SeeAlso
    	randomLineBundle
	vectorBundleOnE
	VectorBundleOnE
	yAction
	degOnE
///


doc ///
    Key
    	orderInPic
        (orderInPic,VectorBundleOnE)
	(orderInPic,Matrix)
    Headline
    	order of a line bundle of degree 0 in Pic(E)
    Usage
    	N = orderInPic(L)
	N = orderInPic(phi)
    Inputs
    	L : VectorBundleOnE
	    a line bundle of degree 0 on E
	phi : Matrix
	    a matrix which is the yAction of VectorBundleOnE
    Outputs
    	N : ZZ
	    the order of L in Pic(E)
    Description
    	Text
	    Computes the order of a degree 0 line bundle L on the hyperelliptic curve E by the most naive method.
	Example
	    kk=ZZ/101;
	    g=1;
	    rNP=randNicePencil(kk,g);
	    f=(cliffordModule(rNP.matFact1,rNP.matFact2,rNP.baseRing)).hyperellipticBranchEquation
	    
	    L=randomLineBundle(0,f);
	    orderInPic L
    SeeAlso
    	randomLineBundle
	vectorBundleOnE
	VectorBundleOnE
	yAction
    Caveat
    	The ground field kk has to be finite. It computes the order by checking inductively whether L^k is trivial, so it may fail 
	when kk is not finite, or has too many elements. orderInPic(phi) may not terminate if 
	it is not the yAction of a line bundle of degree 0 on E.
///

doc ///
    Key 
    	randomLineBundle
	(randomLineBundle,RingElement)
	(randomLineBundle,ZZ,RingElement)
    Headline
    	a random line bundle on the hyperelliptic curve
    Usage
    	L=randomLineBundle(f)
	Ld=randomLineBundle(d,f)
    Inputs
    	f : RingElement
	    the hyperelliptic branch equation of a CliffordModule.
	d : ZZ
    Outputs
    	L : VectorBundleOnE
	    a line bundle on E
	Ld : VectorBundleOnE
	    a line bundle on E of degree d.
    Description
    	Text
	    Chooses a random line bundle on the hyperelliptic curve E of genus g 
	    given by the equation y^2-(-1)^{g}*f, where f is the branch equation of degree 
	    (2g+2). Input with an integer d gives a random line bundle of degree d on E.
	    
	    Note that a line bundle on E is given by the y-action which is represented by 
	    a traceless 2x2 matrix 
      	    
	    b c 
	     
	    a -b 
	    
	    whose determinant equals to (-1)^{g}*f. We find such a matrix over a finite ground field 
	    by picking randomly b, a homogeneous form of degree (g+1),
	    since the binary form b^2 + (-1)^{g}*f frequently factors.
	    
	    
	    
	Example
	    kk=ZZ/101;
	    g=1;
	    rNP=randNicePencil(kk,g);
	    cM=cliffordModule(rNP.matFact1,rNP.matFact2,rNP.baseRing);
	    
	    f=cM.hyperellipticBranchEquation
	    L=randomLineBundle(f)
	    degOnE L
	    m=L.yAction
	    (m)^2_(0,0)+(-1)^g*f==0
	    
	    L0=randomLineBundle(0,f)
	    degOnE L0
	    orderInPic L0
	    
    Caveat
    	The ground field kk has to be finite.
    SeeAlso
    	vectorBundleOnE
	VectorBundleOnE
	degOnE
	orderInPic
///

doc ///
    Key 
    	randomExtension
	(randomExtension,VectorBundleOnE,VectorBundleOnE)
	(randomExtension,Matrix,Matrix)
    Headline
    	a random extension of a vector bundle on E by another vector bundle
    Usage
    	V=randomExtension(V1,V2)
	V=randomExtension(Y1,Y2)
    Inputs
    	V1 : VectorBundleOnE
	V2 : VectorBundleOnE
	Y1 : Matrix
	    a matrix which is the yAction of some VectorBundleOnE
	Y2 : Matrix
	    a matrix which is the yAction of some VectorBundleOnE
    Outputs
    	V : VectorBundleOnE
    Description
    	Text
	    Chooses a random extension of V2 by V1, where V1, V2 are vector bundles on E 
	    represented by the type VectorBundleOnE.
	Example
	    kk=ZZ/101;
	    g=1;
	    rNP=randNicePencil(kk,g);
	    cM=cliffordModule(rNP.matFact1,rNP.matFact2,rNP.baseRing);
	    
	    f=cM.hyperellipticBranchEquation
	    L1=randomLineBundle(0,f)
	    L2=randomLineBundle(2,f)
	    V=randomExtension(L1,L2)
  	    V.yAction
	    degOnE V
	    
	    V1=randomExtension(L2,V)
	    V1.yAction
	    degOnE V1	    
    SeeAlso
    	randomLineBundle
    	vectorBundleOnE
	VectorBundleOnE
	degOnE
///

--YK
doc ///
    Key
    	cliffordModuleToMatrixFactorization
	(cliffordModuleToMatrixFactorization, CliffordModule, Ring)
    Headline
    	reads off a matrix factorization from a Clifford module
    Usage
    	(M1,M2) = cliffordModuleToMatrixFactorization(M,S)
    Inputs
    	M : CliffordModule
	    a Clifford module on a hyperelliptic curve E of genus g
	S : Ring
	    a polynomial ring in x_0..y_{(g-1)},z_1,z_2,s,t
    Outputs
    	M1 : Matrix
	M2 : Matrix
            (M1, M2) a matrix factorization of qq, the equation of the associated pencil of quadrics
    Description
    	Text
	    Part of the series of explicit functors giving category equivalences:
	    
	    cliffordModule
	    
	    cliffordModuleToCIResolution
	    
	    cliffordModuleToMatrixFactorization
	    
	    ciModuleToMatrixFactorization
	    
	    ciModuleToCliffordModule
	    
	    A Clifford module M on the Clifford algebra C:=Cliff(qq) of a quadratic form qq
	    has keys evenOperator and oddOperator, the list of the even 
	    operators uEv_i : M_0 \to M_1 and the odd operators uOdd_i : M_1 \to M_0,
	    which form a representation of C. 
	    
	    From this representation we read off a matrix factorization (M1, M2) of qq.
	Example
	    kk=ZZ/101;
	    g=1;
	    rNP=randNicePencil(kk,g);
	    qq=rNP.quadraticForm;
	    S=rNP.qqRing;
	    
	    cM=cliffordModule(rNP.matFact1,rNP.matFact2,rNP.baseRing)
	    (M1,M2)=cliffordModuleToMatrixFactorization(cM,S);
	    r=rank source M1
	    M1*M2 - qq*id_(S^r) == 0
	    M1 == rNP.matFact1
	    M2 == rNP.matFact2
	    
	    cMu=cliffordModule(rNP.matFactu1,rNP.matFactu2,rNP.baseRing)
	    (Mu1,Mu2)=cliffordModuleToMatrixFactorization(cMu,S);
    	    ru=rank source Mu1	    
    	    Mu1*Mu2 - qq*id_(S^ru) == 0	    
	    Mu1 == rNP.matFactu1
	    Mu2 == rNP.matFactu2
    SeeAlso
    	cliffordModule
	cliffordModuleToCIResolution
	ciModuleToMatrixFactorization
	ciModuleToCliffordModule
///

doc ///
    Key
    	cliffordModuleToCIResolution
	(cliffordModuleToCIResolution, CliffordModule, Ring, Ring)
    Headline
    	transforms a Clifford module to a resolution over a complete intersection ring
    Usage
    	F=cliffordModuleToCIResolution(M,S,CI)
    Inputs
    	M : CliffordModule
	    a Clifford module on a hyperelliptic curve of genus g
	S : Ring
	    a polynomial ring in x_0..y_(g-1),z_1,z_2,s,t
	CI : Ring
	    a complete intersection S/ideal(q1,q2) where qq=s*q1+t*q2
    Outputs
    	F : ChainComplex
	    a resolution which represents a module over CI
    Description
    	Text
	    Part of the series of explicit functors giving category equivalences:
	    
	    cliffordModule
	    
	    cliffordModuleToCIResolution
	    
	    cliffordModuleToMatrixFactorization
	    
	    ciModuleToMatrixFactorization
	    
	    ciModuleToCliffordModule
	    
	    From Clifford module M on the Clifford algebra C:=Cliff(qq) of a quadratic form 
	    qq=s*q1+t*q2, we may construct a module over CI=P/ideal(q1,q2) where
	    P is a polynomial ring in x_0..y_{(g-1)},z_1,z_2. This function returns a part of
	    its minimal free resolution over CI. This function uses cliffordModuleToMatrixFactorization.
	Example
	    kk=ZZ/101;
	    g=1;
	    rNP=randNicePencil(kk,g);
	    qq=rNP.quadraticForm;
	    S=rNP.qqRing;
	    P=kk[drop(gens S,-2)]
	    qs=sub(diff(matrix{{S_(2*g+2), S_(2*g+3)}}, qq), P)
	    CI=P/ideal qs

	    cM=cliffordModule(rNP.matFact1,rNP.matFact2,rNP.baseRing)
	    betti (F=cliffordModuleToCIResolution(cM,S,CI))
	    
	    cMu=cliffordModule(rNP.matFactu1,rNP.matFactu2,rNP.baseRing)
	    f=cMu.hyperellipticBranchEquation
	    L=randomLineBundle(0,f);
	    betti (FL=cliffordModuleToCIResolution(tensorProduct(cM,L),S,CI))
	    betti (FuL=cliffordModuleToCIResolution(tensorProduct(cMu,L),S,CI))
    SeeAlso
    	cliffordModule
	cliffordModuleToMatrixFactorization
	ciModuleToMatrixFactorization
	ciModuleToCliffordModule
///

doc ///
    Key
    	searchUlrich
	(searchUlrich, CliffordModule, Ring)
    Headline
    	searching an Ulrich module of smallest possible rank
    Usage
    	Ulr = searchUlrich(M,S)
    Inputs
    	M : CliffordModule
	S : Ring
	    a polynomial ring in x_0..y_{(g-1)},z_1,z_2,s,t
    Outputs
    	Ulr : Module
    	    a module on S supported on x_0..y_{(g-1)},z_1,z_2
    Description
    	Text
	    M is assumed to be a Clifford module with a Morita bundle F_u, i.e., associated to a 
	    maximal isotropic subspace u. 
	    
	    Let G be a coherent sheaf on a hyperelliptic curve E, and N be the corresponding 
	    module over CI=P/ideal(q1,q2). Using the Tate resolution of u in a complete intersection 
	    of 2 quadrics, one can compute the graded Betti numbers of N by the rank 
	    of cohomology groups of G twisted by the Morita bundle F_u. In particular, an Ulrich module 
	    on CI corresponds to a sheaf G on E such that G \otimes F_u is an Ulrich bundle on E.
	    
	    From this perspective, Eisenbud and Schreyer conjectured that it is the case when 
	    G is a general vector bundle of rank \ge 2 of suitable degree. 
	    
	    searchUlrich looks for a candidate G of rank 2 on E and returns a module on S 
	    supported on a CI V(q_1,q_2) \subset PP^{2g+1}.
	Example
	    kk=ZZ/101;
	    g=2;
	    rNP=randNicePencil(kk,g);
	    S=rNP.qqRing;
	    R=rNP.baseRing;
	    qq=rNP.quadraticForm;
	    qs=apply(2,i->diff(S_(2*g+2+i),qq))
	    Mu1=rNP.matFactu1;
	    Mu2=rNP.matFactu2;
	    
	    M=cliffordModule(Mu1,Mu2,R)
	    elapsedTime Ulr = searchUlrich(M,S);
	    betti res Ulr
	    ann Ulr == ideal qs
    Caveat
    	searchUlrich uses the method randomLineBundle, so the ground field kk has to be finite.
    SeeAlso
    	cliffordModule
///

doc ///
    Key
    	translateIsotropicSubspace
	(translateIsotropicSubspace,CliffordModule,VectorBundleOnE,PolynomialRing)
    Headline
    	choose a random isotropic subspace
    Usage
    	uL=translateIsotropicSubspace(M,L,S)
    Inputs
    	M : CliffordModule
	    corresponding to a maximal isotropic subspace u
	L : VectorBundleOnE
	    a degree 0 line bundle on the associated hyperelliptic curve E of genus g.
	S : PolynomialRing 
	    a polynomial ring kk[x_0..y_{(g-1)},z_0,z_1,s,t] in (2g+4) variables
    Outputs
    	uL : Matrix
	    a matrix presenting a maximal isotropic subspace u translated by L
    Description
    	Text
	    Reid's theorem says that the set of maximal isotropic subspaces on a 
	    complete intersection of two quadrics in (2g+2) variables is isomorphic to
	    the set of degree 0 line bundles on the associated hyperelliptic curve E 
	    of genus g. The method 
	    computes the maximal isotropic subspace uL corresponding to the
	    translation of u by L.
	Example
	    kk=ZZ/101; 
	    g=2;
	    (S,qq,R,u, M1,M2, Mu1,Mu2) = randomNicePencil(kk,g);
	    
	    M=cliffordModule (Mu1, Mu2, R);
	    f=M.hyperellipticBranchEquation
	    L=randomLineBundle(0,f);
	    uL=translateIsotropicSubspace(M,L,S)
	    assert (betti uL == betti u)
    SeeAlso
    	randomIsotropicSubspace
    	randomLineBundle
	CliffordModule
///


doc ///
    Key
    	randomIsotropicSubspace
	(randomIsotropicSubspace,CliffordModule,PolynomialRing)
    Headline
    	choose a random isotropic subspace
    Usage
    	ru=randomIsotropicSubspace(M,S)
    Inputs
    	M : CliffordModule
	    corresponding to a maximal isotropic subspace u
	S : Ring 
	    a polynomial ring kk[x_0..y_{(g-1)},z_0,z_1,s,t] in (2g+4) variables
    Outputs
    	ru : Matrix
	    a matrix presenting a random maximal isotropic subspace
    Description
    	Text
	    Reid's theorem says that the set of maximal isotropic subspaces on a 
	    complete intersection of two quadrics in (2g+2) variables is isomorphic to
	    the set of degree 0 line bundles on the associated hyperelliptic curve E 
	    of genus g. The method 
	    chooses a random line bundle L of degree 0 on E, and 
	    computes the maximal isotropic subspace ru corresponding to the
	    translation of u by L.
	Example
	    kk=ZZ/101; 
	    g=2;
	    (S,qq,R,u, M1,M2, Mu1,Mu2) = randomNicePencil(kk,g);
	    
	    M=cliffordModule (Mu1, Mu2, R);
	    ru=randomIsotropicSubspace(M,S)
	    assert (betti ru == betti u)
    Caveat
    	The ground field kk (=coefficientRing S) has to be finite, since it uses the method randomLineBundle.
    SeeAlso
    	translateIsotropicSubspace
    	randomLineBundle
	CliffordModule
///


---------------
---tests-------
---------------
TEST///
kk=ZZ/101
d=2
n=2*d
R=kk[a_0..a_(binomial(n+2,2))]
S=kk[x_0..x_(n-1),a_0..a_(binomial(n+2,2))]
M=genericSymmetricMatrix(S,a_0,n)
X=(vars S)_{0..n-1}
Y=X*M
(M1,M2)=matrixFactorizationK(X,Y);
(eOdd,eEv)=cliffordOperators(M1,M2,R);
assert((eOdd_0*eEv_1+eOdd_1*eEv_0)_(0,0) == 2*R_1)
assert all(eEv,e->isHomogeneous e)
assert all(eOdd,e->isHomogeneous e)
///

end--

-- it possible to block the Ulrich presentation
-- mm=m0|m1
-- so that mi is a part of a matrix factorization of qi
--   nn=syz mm = n1||n0 the ni is a matrix factorization of q_i
--   coker mm is isomorphic to coker transpose nn
restart
uninstallPackage "PencilsOfQuadrics"
installPackage "PencilsOfQuadrics"
check "PencilsOfQuadrics"
kk=ZZ/101
R=kk[s,t]
g=2
(S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g);
M=cliffordModule(Mu1,Mu2,R)

elapsedTime Ulrich = searchUlrich(M,S);
betti res Ulrich
ann Ulrich
T=kk[drop(gens S,-2)]-- coordinate ring of PP^(2g+1)
m1=sub(presentation Ulrich,T)
Ulrich =coker m1
fU=res Ulrich
elapsedTime betti Hom(Ulrich,Ulrich)   -- 10.1731 seconds elapsed
elapsedTime betti Hom(Ulrich, T^{-2}**coker transpose fU.dd_2)
-- => Ulrich isomorphic to T^{-2}**coker transpose fU.dd_2 in case g=2, 
-- not for g=1

qs= apply(2,i->(gens ann Ulrich)*random(T^{2:-2},T^{-2}))
Mqs= apply(qs, q1->(Tq= T/ideal sub(q1,T);
     coker lift((res coker sub(u,Tq)).dd_4,T)**T^{3}))
homs=apply(Mqs,M->Hom(M,Ulrich));

ring homs_0
apply(homs,h->betti h)
-- => there are 4 homomorphisms
homsList=apply(homs,h-> apply(4,i-> matrix homomorphism h_{i}))
Cs=apply(Mqs, Mq1->chainComplex presentation Mq1)
ring Cs_0
D=chainComplex presentation Ulrich
ring D

hom1s= apply(2, j->apply(4,i->(
	    phi=map(D_0,(Cs_j)_0,homsList_j_i);
           (extend(D,Cs_j,phi))_1))) 
apply(hom1s,h->tally apply(h,m->betti m))
m0=presentation Ulrich*(hom1s_0_0)
m1=presentation Ulrich*(hom1s_1_1)
ann coker m0==ideal qs_0
ann coker m1==ideal qs_1
mm=m0|m1

nn=syz mm

n1=(nn)^{0..2^(g+1)-1}
n0=(nn)^{2^(g+1)..2^(g+2)-1}
ann coker (n1)==ideal qs_1
ann coker (n0) == ideal qs_0

secondMatrix=method()
secondMatrix(Matrix,RingElement) := (m,q) -> (
    Tq := T/q; 
    m':=lift(syz(m**Tq),T);
    t:=m*m'//q*id_(target m);    
   map(source m,,m'*inverse t)
   )
TEST///
m'=secondMatrix(m,q)
betti m, betti m'
assert(m*m' - q * id_(target m)==0)
///
q0=q=qs_0_(0,0)
q1=qs_1_(0,0)
m0'=secondMatrix(m0,q0);
m1'=secondMatrix(m1,q1);
res coker (transpose m0'| transpose m1')
mm*(res image mm).dd_1
betti Hom(T^{-2}**coker transpose fU.dd_2,coker (transpose m0'| transpose m1'))




T1=T/ideal qs_1
n0'=lift(syz sub(m0,T0),T)
n0=n0'*inverse ((m0*n0')//(qs_0_(0,0)*id_(target m0)))
m0*n0
n1'=lift(syz sub(m1,T1),T)
n1=n1'*inverse ((m1*n1')//(qs_1_(0,0)*id_(target m1)))
m1*n1
betti res coker (nn=(transpose n0|transpose n1))
betti Hom(coker mm, coker transpose syz nn)
h=Hom(coker mm, coker transpose syz mm);
betti (a=homomorphism h_{0})
G=chainComplex(mm,(syz mm)*matrix a)
ann coker G.dd_1_{0..7}
ann coker (transpose G.dd_2)_{0..7}

ann coker (syz mm)^{0..7}==ideal qs_1
ann coker (syz mm)^{8..15} == ideal qs_0
m0*m1-m1*m0

mats=apply(2,j->apply(4,i->(transpose (res Ulrich).dd_2) * syz(transpose hom1s_j_i)))
apply(flatten mats,m-> betti ann coker m)
///


///
restart
load "PencilsOfQuadrics.m2"
n=3
a=random(10^2)

p=first last toList factor (a^n-1)

a=a%p
--
kk=ZZ/p
g=n-2
--n corresponds to g+2--
S=kk[x_0..y_(n-1)]
X=map(S^1,S^{n:-1},(i,j) -> x_j)
Y=map(S^1,S^{n:-1},(i,j) -> y_j)
(M1,M2)=matrixFactorizationK(X,Y)
M1=map(S^(2^(n-1)),,M1)
M2=map(S^(2^(n-1)),,M2)
ulrich=coker(M1|transpose M2)
A=M1-transpose M2
A^2
betti res ulrich
ann ulrich
g=n-2
T=kk[z_0..z_(2*g+1)]
phi=map(T,S,random(T^1,T^{numgens S:-1}))
R=kk[s,t]
qs=ann ulrich


netList apply(10,j->(
	phi=map(T,S,random(T^1,T^{numgens S:-1}));
        qs'=phi(qs);
        f=sum(2,i->R_i*sub(diff(vars T, transpose diff(vars T, (gens qs')_i)),R));
        toList factor det f))
///






///
viewHelp "PencilsOfQuadrics"

--Yeongrak's question 
-- Let F be an Ulrich bundle on a c.i. X in P^(2g+1), and Y be its double hyperplane section.
-- Is the restriction F|Y onto Y, Ulrich on Y, splits as a direct sum?
-- Or, at least, is an extension of two Ulrich bundles on Y of smaller rank?

-- The experiment says:
-- when g>2 then F|Y does not split as a direct sum, since F|Y was simple!
-- when g=2 then H^0 End(F|Y) = 2, looks quite interesting.

-- What really I want to do:
-- find a strictly semistable Ulrich bundle on Y
-- which appears as the restriction of an Ulrich bundle on X.


-- EXPERIMENT 1. Take a double hyperplane section of g=2 case (genus 2 -> elliptic curve)
restart
load "PencilsOfQuadrics.m2"
kk=ZZ/nextPrime(10^3);
g=2;
(S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g);
M=cliffordModule(Mu1,Mu2,R);

numberOfFactors=0;
while numberOfFactors<2 do(
elapsedTime Ulrich = searchUlrich(M,S); -- 60.4611 seconds elapsed when g=3, kk=ZZ/101

-- Ulrich module on X in P^(2g+1)
T=ring Ulrich;
Tred=T/ideal(T_(2*g+2),T_(2*g+3));
UlrichWithoutst=coker sub(presentation Ulrich,Tred);

-- restriction onto a double hyperplane section
twoLinearForms=random(Tred^1,Tred^{2:-1});
restriction=vars Tred %ideal twoLinearForms;
kk=coefficientRing Tred;
PCodim2=kk[support restriction];
phi=map(PCodim2,Tred,sub(restriction,PCodim2));
rU=coker phi presentation UlrichWithoutst;

elapsedTime betti(endOfrU=Hom(rU,rU)); -- there are two endomorphisms
A=homomorphism (endOfrU_{0});
B=homomorphism (endOfrU_{1});

P1=kk[s,t];
C = s*sub(matrix A,P1) + t*sub(matrix B,P1);
numberOfFactors = # factor det C
)
-- We run the code until (det C) is a 4th power of the product of two linear forms

ann UlrichWithoutst
betti res UlrichWithoutst
betti res rU
ann rU

(factor det C)
factor1 = (factor det C)#0#0
factor2 = (factor det C)#1#0
factor1^4 * factor2^4 == det C

-- define two sheaves with respect to the eigenspace decomposition above
rU1 = prune ker (sub(diff(t,factor1),PCodim2)*A - sub(diff(s,factor1),PCodim2)*B);
rU2 = prune ker (sub(diff(t,factor2),PCodim2)*A - sub(diff(s,factor2),PCodim2)*B);

betti res rU1
betti res rU2

needsPackage "TateOnProducts"
elapsedTime tally apply(5, i->isIsomorphic(rU1++rU2,rU)) 
-- if isIsomorphic returns true, then they are isomorphic.
-- otherwise, since it is a probabilistic algorithm, it might return false even they are isomorphic.

-- On an elliptic curve, there is an 1-dim'l family of nonsplit rank 2 vector bundles
-- with fixed determinant. On the other hand, split vector bundles have 2 parameters;
-- so mostly a rank 2 vector bundle tends to split.



-- EXPERIMENT 2. Take a PP^3 (=codim 4) section of g=3 case (genus 3 -> elliptic curve)
restart
load "PencilsOfQuadrics.m2"
kk=ZZ/nextPrime(10^3);
g=3;
(S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g);
M=cliffordModule(Mu1,Mu2,R);

numberOfFactors=0;
elapsedTime while numberOfFactors<4 do(
Ulrich = searchUlrich(M,S);

-- Ulrich module on X in P^(2g+1)
T=ring Ulrich;
Tred=T/ideal(T_(2*g+2),T_(2*g+3));
UlrichWithoutst=coker sub(presentation Ulrich,Tred);

-- restriction onto a codim 4 linear section
fourLinearForms=random(Tred^1,Tred^{4:-1});
restriction=vars Tred %ideal fourLinearForms;
kk=coefficientRing Tred;
PCodim4=kk[support restriction];
phi=map(PCodim4,Tred,sub(restriction,PCodim4));
rU=coker phi presentation UlrichWithoutst;

endOfrU=Hom(rU,rU); -- there are 4 homomorphisms
listA=apply(4,i->homomorphism(endOfrU_{i}));

P3=kk[t_0..t_3];
C = sum apply(4,i->P3_i*sub(matrix listA_i, P3));
detC=det C;
numberOfFactors = # factor detC;
if numberOfFactors==4 and degree (factor detC)#2#0 == {2} then numberOfFactors=0;
)



betti endOfrU
ann UlrichWithoutst
betti res UlrichWithoutst
betti res rU
ann rU

(factor detC)
factorList=apply(4,i->(factor detC)#i#0)
factorMatrix=matrix apply(factorList, i->apply(gens P3, j->diff(j, i)))

-- Find eigenvectors of factorMatrix
P3v=P3[v]
chPolyAsProducts=factor det (sub(factorMatrix,P3v) - v*id_(P3v^4)) -- monic ch. poly.
eigenvalueList=apply(# chPolyAsProducts, i->(P3v_0-(chPolyAsProducts#i)#0))
assert(# eigenvalueList==4)
eigenvectorList = apply(4, i->syz (factorMatrix - sub(eigenvalueList_i,P3) * id_(P3^4)))

P = eigenvectorList_0 | eigenvectorList_1 | eigenvectorList_2 | eigenvectorList_3;
inverse(P)*factorMatrix*P - diagonalMatrix(eigenvalueList)

endList=apply(4, i->homomorphism (endOfrU_{i}));
--rUdecompositionList=apply(4,i->prune image (sum apply(4, j->sub((inverse P)_(j,i),PCodim4)*endList_j)));

solList=apply(4, i->(syz factorMatrix^{i})*random(P3^3,P3^1))
rUList=apply(4, i->prune ker sum apply(4, j->sub((solList_i)_(j,0), PCodim4) * endList_j))

tally apply(4,i->betti res rUdecompositionList_i)
tally apply(4,i->betti res rUList_i)
temp=rUList_0++rUList_1++rUList_2++rUList_3

rU1 = prune image (sub(diff(t,factor1),PCodim2)*A - sub(diff(s,factor1),PCodim2)*B);
rU2 = prune image (sub(diff(t,factor2),PCodim2)*A - sub(diff(s,factor2),PCodim2)*B);

betti res rU1
betti res rU2

needsPackage "TateOnProducts"
elapsedTime tally apply(5, i->isIsomorphic(temp,rU)) 
-- if isIsomorphic returns true, then they are isomorphic.
-- otherwise, since it is a probabilistic algorithm, it might return false even they are isomorphic.
