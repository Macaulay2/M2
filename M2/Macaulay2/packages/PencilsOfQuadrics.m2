newPackage(
    "PencilsOfQuadrics",
    Version => "1.1",
    Date => "October 23, 2026",
    Authors => {{Name => "Frank-Olaf Schreyer",
            Email => "schreyer@math.uni-sb.de",
            HomePage => "https://www.math.uni-sb.de/ag/schreyer/index.php/"},
        {Name => "David Eisenbud",
            Email => "de@msri.org",
            HomePage => "https://eisenbud.github.io/"},
        {Name => "Yeongrak Kim",
            Email => "yeongrak.kim@pusan.ac.kr",
            HomePage => "https://sites.google.com/view/yeongrak"}
        },
    PackageImports => {"CompleteIntersectionResolutions"},
    Headline => "Clifford Algebra of a pencil of quadratic forms",
    DebuggingMode => false,
    Keywords => {"Commutative Algebra", "Algebraic Geometry"}
    )

export {
    "matrixFactorizationK",--
    "randomNicePencil",--
    "RandomNicePencil",
    "cliffordOperators",--
    "centers",--
    "tensorProduct",
    "preRandomLineBundle", -- methods updated in ver 1.0
    "randomLineBundle", -- methods updated in ver 1.0
    "degOnE",
    "orderInPic",
    "randomExtension",
    --
    "randNicePencil",
    "qqRing",
    "quadraticForm",
    "isotropicSpace",
    "matFact1",
    "matFact2",
    "matFactu1",
    "matFactu2",
    "CliffordModule",--
    "cliffordModule",--
    "evenOperators",
    "oddOperators",
    "evenCenter",
    "oddCenter",
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
    "searchUlrich", -- methods updated in ver 1.0
    "translateIsotropicSubspace",
    "randomIsotropicSubspace",
    "LabBookProtocol",
    }

-*
---------------------
-- Changes in v1.1 --
---------------------

-- Added: synonyms and printing methods for better accessibility
-- Added: randomNicePencil() without input will print its usage
-- Added: some error messages instead of assertions
-- Added: some error messages to rule out bad cases (char=2, non-squarefree hyperelliptic equation, ...)

-- Removed: some assertions only need to test the methods
-- Removed: some unused methods
-- Removed: unncessary tests and experiments

-- Moved: some local variables defined in the beginning of the package globally are now defined inside the methods using them

-- Fixed: buggy method "translateIsotropicSubspace"
*-

CliffordModule  = new Type of HashTable
VectorBundleOnE = new Type of HashTable
RandomNicePencil = new Type of HashTable

CliffordModule.synonym = "Clifford module";
VectorBundleOnE.synonym = "vector bundle on a hyperelliptic curve E";
RandomNicePencil.synonym = "random pencil of quadrics";

-- printing helpers
expression CliffordModule := M -> (expression CliffordModule) expression (
    -- It is better not to list the matrices for the odd/even operators and centers for a better readability.
    -- Only prints: symmetricM, hyperellipticBranchEquation from the keys

    --    "oddOperators" => M.oddOperators,
    --    "evenOperators" => M.evenOperators,
    --    "evenCenter" => M.evenCenter,
    --    "oddCenter" => M.oddCenter,
    "symmetricM" => M.symmetricM,
    "hyperellipticBranchEquation" => M.hyperellipticBranchEquation
    )
toString CliffordModule := toString @@ expression
net CliffordModule := net @@ expression
texMath CliffordModule := texMath @@ expression

expression VectorBundleOnE := V -> (expression VectorBundleOnE) expression (
    -- the only key is yAction, but also prints its rank for a better readability

    "rank" => (numrows V.yAction // 2),
    "yAction" => V.yAction
    )
toString VectorBundleOnE := toString @@ expression
net VectorBundleOnE := net @@ expression
texMath VectorBundleOnE := texMath @@ expression

expression RandomNicePencil := rNP -> (expression RandomNicePencil) expression (
    -- prints the keys and the key values

    "qqRing" => rNP.qqRing,
    "quadraticForm" => rNP.quadraticForm,
    "baseRing" => rNP.baseRing,
    "isotropicSpace" => rNP.isotropicSpace,
    "matFact1" => rNP.matFact1,
    "matFact2" => rNP.matFact2,
    "matFactu1" => rNP.matFactu1,
    "matFactu2" => rNP.matFactu2
    )
toString RandomNicePencil := toString @@ expression
net RandomNicePencil := net @@ expression
texMath RandomNicePencil := texMath @@ expression

----- Methods -----
LabBookProtocol=method()
LabBookProtocol(ZZ) := (g) -> (
    str:="";
    if g==3 then (
        str="
        g=3
        kk=ZZ/101;
        elapsedTime (S,qq,R,u,M1,M2,Mu1,Mu2)=randomNicePencil(kk,g);
        -- .0778377s elapsed
        P=kk[drop(gens S,-2)]
        M=cliffordModule(Mu1,Mu2,R);
        Mor = vectorBundleOnE M.evenCenter;
        Mor1= vectorBundleOnE M.oddCenter;
        f = M.hyperellipticBranchEquation;
        assert(dim ideal jacobian ideal f ==0);
        elapsedTime while (
            m1=randomLineBundle(g+(g%2),f);
            m2=randomLineBundle(g%2,f);
            m12=randomExtension(m1.yAction,m2.yAction);
            V = vectorBundleOnE m12;
            Ul=tensor(Mor,V);
            Ul1=tensor(Mor1,V);
            d0=unique degrees target Ul.yAction;
            d1=unique degrees target Ul1.yAction;
            #d1 >=3 or #d0 >=3) do ();
        -- .159068s elapsed
        betti Ul.yAction, betti Ul1.yAction
        --further commands
        elapsedTime Ul = tensor(M,V); -- the heaviest part computing the actions of generators
        -- 5.5923s elapsed
        M1Ul=sum(#Ul.oddOperators,i->S_i*sub(Ul.oddOperators_i,S));
        r=2
        Ulrich := M1Ul^{r*2^g..(2*r)*2^g-1};
        Ulr=coker map(S^(r*2^g),,Ulrich);
        minimalBetti Ulr
        -- will give an Ulrich bundle, with betti table
        -- 16 32 16"
        ) else
    if g==4 then (
        str="
        g=4
        kk=ZZ/101;
        elapsedTime (S,qq,R,u,M1,M2,Mu1,Mu2)=randomNicePencil(kk,g);
        -- .642807s elapsed
        P=kk[drop(gens S,-2)]
        M=cliffordModule(Mu1,Mu2,R);
        Mor = vectorBundleOnE M.evenCenter;
        Mor1= vectorBundleOnE M.oddCenter;
        f = M.hyperellipticBranchEquation;
        assert(dim ideal jacobian ideal f ==0);
        elapsedTime while (
            m1=randomLineBundle(g+(g%2),f);
            m2=randomLineBundle(g%2,f);
            m12=randomExtension(m1.yAction,m2.yAction);
            V = vectorBundleOnE m12;
            Ul=tensor(Mor,V);
            Ul1=tensor(Mor1,V);
            d0=unique degrees target Ul.yAction;
            d1=unique degrees target Ul1.yAction;
            #d1 >=3 or #d0 >=3) do ();
        -- 1.28363s elapsed
        betti Ul.yAction, betti Ul1.yAction
        --further commands
        elapsedTime Ul = tensor(M,V); -- the heaviest part computing the actions of generators
        -- 263.976s elapsed
        M1Ul:=sum(#Ul.oddOperators,i->S_i*sub(Ul.oddOperators_i,S));
        r=2
        Ulrich := M1Ul^{r*2^g..(2*r)*2^g-1};
        Ulr=coker map(S^(r*2^g),,Ulrich);
        minimalBetti Ulr
        -- will give an Ulrich bundle, with betti table
        -- 32 64 32"
        ) else
    if g==5 then (
        str="
        g=5
        kk=ZZ/101;
        elapsedTime (S,qq,R,u,M1,M2,Mu1,Mu2)=randomNicePencil(kk,g);
        -- 8.74077s elapsed
        P=kk[drop(gens S,-2)]
        elapsedTime M=cliffordModule(Mu1,Mu2,R);
        -- 6.46773s elapsed
        Mor = vectorBundleOnE M.evenCenter;
        Mor1= vectorBundleOnE M.oddCenter;
        f = M.hyperellipticBranchEquation;
        assert(dim ideal jacobian ideal f ==0);
        elapsedTime while (
            m1=randomLineBundle(g+(g%2),f);
            m2=randomLineBundle(g%2,f);
            m12=randomExtension(m1.yAction,m2.yAction);
            V = vectorBundleOnE m12;
            Ul=tensor(Mor,V);
            Ul1=tensor(Mor1,V);
            d0=unique degrees target Ul.yAction;
            d1=unique degrees target Ul1.yAction;
            #d1 >=3 or #d0 >=3) do ();
        -- 18.7444s elapsed
        betti Ul.yAction, betti Ul1.yAction
        -- further commands
        -- elapsedTime Ul = tensor(M,V); -- the heaviest part computing the actions of generators
        -- 13554.4s elapsed
        -- M1Ul:=sum(#Ul.oddOperators,i->S_i*sub(Ul.oddOperators_i,S));
        -- r=2
        -- Ulrich := M1Ul^{r*2^g..(2*r)*2^g-1};
        -- Ulr=coker map(S^(r*2^g),,Ulrich);
        -- elapsedTime minimalBetti Ulr
        -- 65.24s elapsed
        -- will give an Ulrich bundle, with betti table
        -- 64 128 64"
        )
    else (str="no record"))

LabBookProtocol(ZZ,ZZ) := (g,r) -> (
    str:="";
    if (g,r)==(2,3) then (
        str="
        g=2
        r=3
        kk=ZZ/101;
        elapsedTime (S,qq,R,u,M1,M2,Mu1,Mu2)=randomNicePencil(kk,g);
        -- .0343676s elapsed
        P=kk[drop(gens S,-2)]
        gens P
        M=cliffordModule(Mu1,Mu2,R);
        Mor = vectorBundleOnE M.evenCenter;
        Mor1= vectorBundleOnE M.oddCenter;
        f = M.hyperellipticBranchEquation;
        assert(dim ideal jacobian ideal f ==0);
        degSeq={0,1,2}
        elapsedTime while (
            -- build a vector bundle V as extensions of line bundles of degrees in degSeq
            V=randomLineBundle(degSeq#0,f);
            for i from 1 to r-1 do(
                m1=randomLineBundle(degSeq#i,f);
                m12=randomExtension(m1.yAction,V.yAction);
                V=vectorBundleOnE m12;
                );
            Ul=tensor(Mor,V);
            Ul1=tensor(Mor1,V);
            d0=unique degrees target Ul.yAction;
            d1=unique degrees target Ul1.yAction;
            #d1 >=3 or #d0 >=3) do ();
        -- .0863725s elapsed
        betti Ul.yAction,betti Ul1.yAction
        --further commands
        elapsedTime Ul = tensor(M,V); -- the heaviest part computing the actions of generators
        -- .71477s elapsed
        M1Ul=sum(#Ul.oddOperators,i->S_i*sub(Ul.oddOperators_i,S));
        Ulrich = coker map(P^(r*2^g),,sub(M1Ul^{r*2^g..(2*r)*2^g-1},P));
        minimalBetti Ulrich
        -- Is an Ulrich bundle, with betti numbers
        -- r*2^g,(2*r)*2^g,r*2^g
        qs=ann Ulrich
        ideal sub(diff((vars S)_{2*g+2,2*g+3},qq),P)==qs"
        ) else
    if (g,r)==(3,4) then (
        str="
        g=3
        r=4
        kk=ZZ/101;
        elapsedTime (S,qq,R,u,M1,M2,Mu1,Mu2)=randomNicePencil(kk,g);
        -- .086491s elapsed
        P=kk[drop(gens S,-2)]
        gens P
        M=cliffordModule(Mu1,Mu2,R);
        Mor = vectorBundleOnE M.evenCenter;
        Mor1= vectorBundleOnE M.oddCenter;
        f = M.hyperellipticBranchEquation;
        assert(dim ideal jacobian ideal f ==0);
        degSeq={1,2,3,4}
        elapsedTime while (
            -- build a vector bundle V as extensions of line bundles of degrees in degSeq
            V=randomLineBundle(degSeq#0,f);
            for i from 1 to r-1 do(
                m1=randomLineBundle(degSeq#i,f);
                m12=randomExtension(m1.yAction,V.yAction);
                V=vectorBundleOnE m12;
                );
            Ul=tensor(Mor,V);
            Ul1=tensor(Mor1,V);
            d0=unique degrees target Ul.yAction;
            d1=unique degrees target Ul1.yAction;
            #d1 >=3 or #d0 >=3) do ();
        -- .721843s elapsed
        betti Ul.yAction,betti Ul1.yAction
        --further commands
        elapsedTime Ul = tensor(M,V); -- the heaviest part computing the actions of generators
        -- 46.3761s elapsed
        M1Ul=sum(#Ul.oddOperators,i->S_i*sub(Ul.oddOperators_i,S));
        Ulrich = coker map(P^(r*2^g),,sub(M1Ul^{r*2^g..(2*r)*2^g-1},P));
        minimalBetti Ulrich
        -- Is an Ulrich bundle, with betti numbers
        -- r*2^g,(2*r)*2^g,r*2^g
        elapsedTime qs=ann Ulrich
        -- 16.3937s elapsed
        ideal sub(diff((vars S)_{2*g+2,2*g+3},qq),P)==qs"
        ) else
    if (g,r)==(4,3) then (
        str="
        g=4
        r=3
        kk=ZZ/101;
        elapsedTime (S,qq,R,u,M1,M2,Mu1,Mu2)=randomNicePencil(kk,g);
        -- .622276s elapsed
        elapsedTime M=cliffordModule(Mu1,Mu2,R);
        -- .608989s elapsed
        P=kk[drop(gens S,-2)];
        Mor = vectorBundleOnE M.evenCenter;
        Mor1= vectorBundleOnE M.oddCenter;
        f = M.hyperellipticBranchEquation;
        assert(dim ideal jacobian ideal f ==0);
        degSeq={0,2,4}
        elapsedTime while (
            -- build a vector bundle V as extensions of line bundles of degrees in degSeq
            V=randomLineBundle(degSeq#0,f);
            for i from 1 to r-1 do(
                m1=randomLineBundle(degSeq#i,f);
                m12=randomExtension(m1.yAction,V.yAction);
                V=vectorBundleOnE m12;
                );
            Ul=tensor(Mor,V);
            Ul1=tensor(Mor1,V);
            d0=unique degrees target Ul.yAction;
            d1=unique degrees target Ul1.yAction;
            #d1 >=3 or #d0 >=3) do ();
        -- 4.14635s elapsed
        betti Ul.yAction,betti Ul1.yAction
        --further commands
        -- elapsedTime Ul = tensor(M,V); -- the heaviest part computing the actions of generators
        -- 1027.58s elapsed
        -- M1Ul=sum(#Ul.oddOperators,i->S_i*sub(Ul.oddOperators_i,S));
        -- Ulrich = coker map(P^(r*2^g),,sub(M1Ul^{r*2^g..(2*r)*2^g-1},P));
        -- minimalBetti Ulrich
        -- Is an Ulrich bundle, with betti numbers
        -- r*2^g,(2*r)*2^g,r*2^g
        -- elapsedTime qs=ann Ulrich
        -- 409.423s elapsed
        -- ideal sub(diff((vars S)_{2*g+2,2*g+3},qq),P)==qs"
        )
    else (str="no record")
    )


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
        map(S^(2^(k-1)),,Xpart-Ypart)
        );
    M)


randomNicePencil=method()
randomNicePencil(Ring,ZZ) := (kk,g) -> (
    --S, qq,  R,  u, M1, M2 Mu1, Mu2 are constructed.
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
    if (char kk == 2) then error "the ground field must be not of characteristic 2";
    if (g < 0) then error "genus must be a nonnegative integer";

    x := local x;
    y := local y;
    s := local s;
    t := local t;
    z := local z;

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
    F := freeResolution(coker X**(S/cub), LengthLimit => 2*g+4);
    degsEven := reverse flatten degrees F_(2*g+4);
    degsOdd := flatten degrees F_(2*g+3);
    M2 := map(S^-degsOdd,S^-degsEven,m2)**S^{2*g+2};
    M1 := map(S^-degsEven**S^{3},S^-degsOdd,m1)**S^{2*g+2};
    Yu := matrix {drop(last entries transpose syz (u|qq),-1)};
    (m1u,m2u) := matrixFactorizationK(u,Yu);
    F = freeResolution(coker u**SQ, LengthLimit => 2*g+4);
    degsEven = reverse flatten degrees F_(2*g+4);
    degsOdd = flatten degrees F_(2*g+3);
    Mu2 := map(S^-degsOdd,S^-degsEven,m2u)**S^{2*g+2};
    Mu1 := map(S^-degsEven**S^{3},S^-degsOdd,m1u)**S^{2*g+2};
    (S, qq, R,  u, M1, M2, Mu1, Mu2))

randomNicePencil() := () -> (print "Usage: (S, qq, R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g)")


randNicePencil = method()
randNicePencil(Ring,ZZ) := (kk,g)->(
    L := randomNicePencil(kk,g);
    new RandomNicePencil from hashTable{
        (qqRing, L_0),
        (quadraticForm, L_1),
        (symbol baseRing, L_2),
        (isotropicSpace, L_3),
        (matFact1, L_4),
        (matFact2, L_5),
        (matFactu1, L_6),
        (matFactu2, L_7)}
    )

-- ADDED: override baseRing
baseRing RandomNicePencil := rNP -> rNP.baseRing


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

    if (char coefficientRing ring M1 == 2) then error "the ground field must be not of characteristic 2";

    S := ring M1;
    es := drop(gens S,-numgens R);
    eOdd := apply(apply(es,e->contract(e,M2)),m->
        transpose( sub(map(target M2,, m),R)**R^1));
    eEv := apply(apply(es,e->contract(e,M1)),m->
        transpose (sub(map(target M1,, m),R)**R^1));
    --    << "Lists of Clifford operators eOdd and eEv constructed" << endl;
    (eOdd,eEv))


symMatrix=(eOdd,eEv) -> matrix apply(#eOdd,i->apply(#eEv,j->1/2*(eOdd_i*eEv_j+eOdd_j*eEv_i)_(0,0)));
compl= (L1,L2) -> sort toList (set L2-set L1)


centers=method()
centers(List,List) := (eOdd,eEv) -> (
    -- eOdd_i and  eEv_i are the even and odd parts of the same generator of
    -- the Clifford algebra of qq over R
    -- The result are the center elements computed via the pfaffian formula

    if (char coefficientRing ring eOdd_0 == 2) then error "the ground field must be not of characteristic 2";

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


factorToList = method()
factorToList(Product) := pf ->(
    lpf := toList pf;
    llpf := apply(lpf,p->toList p);
    apply(llpf, pair-> (pair_0)^(pair_1))
    )


preRandomLineBundle=method()
preRandomLineBundle( RingElement ) := f -> (
    --produces a 2x2 matrix factorization of f, hence a line bundle on the hyperelliptic curve.
    --corresponding to y^2-(-1)^g*f.
    --the first matrix has the form
    --b c
    --a -b
    --where a is the lowest degree factor of (-(-1)^g*f-b^2) =: f1, and ac=f1.
    --caveat: produces a nontrivial bundle IF there's a nontrivial factor of f1,
    --which happens with positive probability over a finite field.

    -- Input
    -- f: a binary form of degree 2g+2 over a finite field without multiple roots

    -- Output
    -- a VectorBundleOnE corresponds to a line bundle (of some degree) on the corresponding hyperelliptic curve.

    if ((# gens ring f) != 2 or ((degree f)_0)%2 !=0) then error "f should be a binary form of degree 2g+2";
    if (char ring f)==0 then error "define f over a finite field";
    if dim (ideal f + ideal jacobian ideal f) > 0 then error "f should have no multiple roots";

    R:=ring f;
    g:=(degree f)_0 // 2 -1;
    b:=random(g+1,R);
    lf := factorToList factor(-(-1)^g*f-b^2);
    a := lf_0;
    c := product(drop(lf,1),fi->fi);
    --c:=if #ff==1 then 1 else product(drop(ff,1),fi->fi);
    targetm := target transpose map(R^1,,matrix{{b,a}});
    m:=map(targetm,, matrix{{b,c},{a,-b}});
    vectorBundleOnE m

    -- an output of this method will be mostly unbalanced -- deg(a) and deg(c) may have a big gap.
    -- in the case, the corresponding line bundle will lie on a (twisted) theta divisor (or a Brill-Noether loci)
    -- so we need a method to find a "general" one by tensoring a number of degree 0 line bundles
    )

preRandomLineBundle(ZZ,RingElement) :=  (d,f) -> (
    -- f a binary form of degree 2g+2 over a finite field
    -- d an integer
    -- select randomly a line L of degree d on the hyperelliptic curve
    -- defined by y^2-(-1)^g*f

    -- Input
    --    f: a binary form of degree 2g+2 over a finite field without multiple roots

    -- Output
    -- Ld : a VectorBundleOnE corresponds to a line bundle of degree d on the corresponding hyperelliptic curve.

    if ((# gens ring f) != 2 or ((degree f)_0)%2 !=0) then error "f should be a binary form of degree 2g+2";
    if (char ring f)==0 then error "define f over a finite field";
    if dim (ideal f + ideal jacobian ideal f) > 0 then error "f should have no multiple roots";

    R:=ring f;
    g:=(degree f)_0 // 2 -1;
    wantedDeg := if d%2 == 0 then 2 else 1; -- the desired degree of the factor deg(a) depending on the parity of d

    a:=null; b:=null; lf:=null;
    tries:=0;   
        
    while (a===null) do(
        if tries >= 500 then error "recursion limit of 500 exceeded: no suitable factor found to generate a nontrivial line bundle";
        tries=tries+1;
        
        b=random(g+1,R);
        lf=toList factor (-(-1)^g*f-b^2);
        linear:=select(lf, p->degree(p#0) == {1});

        cand := if wantedDeg ==1 then apply(linear,p->p#0) else(
            -- choose one quadratic factor, or a product of two linear factors (possibly coincide)
            lin:=apply(linear, p->p#0);
            join(apply(select(lf,p->degree(p#0) == {2}), p->p#0), apply(select(linear,p->p#1 >= 2), p-> (p#0)^2), apply(subsets(lin,2), p->product p))
            );

        if #cand > 0 then a=first cand;
        );

    c := (-(-1)^g*f-b^2)//a;
    targetm:=target transpose map(R^1,,matrix{{b,a}});
    L:=vectorBundleOnE map(targetm,, matrix{{b,c},{a,-b}});
   
    Ld := vectorBundleOnE (L.yAction**R^{-(d-degOnE L)//2})
    )

-- we should apply this function only in case f has no multiple roots
randomLineBundle=method()
randomLineBundle(RingElement) := (f) -> (
    -- Input
    -- f: a binary form of degree 2g+2 over a finite field without multiple roots

    -- Output : a (much more balanced) random line bundle of degree d on the corresponding hyperelliptic curve.
    g:=(degree f)_0 // 2 - 1;
    Ld:= tensor prepend(preRandomLineBundle f, apply(g-1,i->preRandomLineBundle(0,f)))
    )

randomLineBundle(ZZ, RingElement) := (d,f) -> (
    -- Input
    -- d: an integer
    -- f: a binary form of degree 2g+2 over a finite field without multiple roots

    -- Output : a (much more balanced) random line bundle of degree d on the corresponding hyperelliptic curve.
    if ((# gens ring f) != 2 or ((degree f)_0)%2 !=0) then error "f should be a binary form of degree 2g+2";
    if (char ring f)==0 then error "define f over a finite field";
    if dim ideal jacobian ideal f >0 then error "f should have no multiple roots";

    g:=(degree f)_0 // 2 - 1;
    Ld:=tensor prepend(preRandomLineBundle (d,f), apply(g-1,i->preRandomLineBundle(0,f)))
    )


degOnE = method()
degOnE(Matrix) := (L) -> (
    f := (L^2)_(0,0);
    g := (degree ideal f )//2 -1;
    r := numrows L//2;
    sum(flatten degrees target L)+2*r+r*(g-1))


orderInPic = method()
orderInPic(Matrix) := L->(
    -- compute the order of a line bundle on E
    kk:=coefficientRing ring L;
    if not (isFinitePrimeField kk or instance(kk,GaloisField)) then error "the ground field has to be finite";

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


tensorProduct=method()
tensorProduct(Matrix,Matrix) := (phi,psi) -> (
    --tensor product of sheaves on the hyperelliptic curve y^2 \pm f
    --with f in R
    --represented by giving sheaves on proj R (usually P^1)
    --plus the action phi or psi of the element y
    R := ring phi;
    g := degree ideal ((phi^2)_(0,0))//2 - 1;
    inclusion:=syz(phi**id_(target psi)-(id_(target phi)**psi));
    F:=complex inclusion;
    F1:=F**R^{g+1};
    (extend(F1,F,map(F1_0,F_0,(phi**id_(target psi)))))_1
)

tensorProduct(CliffordModule, VectorBundleOnE) := (M,L) -> (
    phi0 := M.evenCenter;
    phi1 := M.oddCenter;
    psi := L.yAction;

    R := ring psi;
    g := degree ideal ((psi^2)_(0,0))//2 - 1;

    inclusion0 := syz(phi0**id_(target psi)-(id_(target phi0)**psi));
    inclusion1 := syz(R^{1}**(phi1**id_(target psi)+(id_(target phi1)**psi)));

    FEven := complex inclusion0;
    FOdd := complex inclusion1;
    Odd' := null;
    Even' := null;
    eOdd := apply(M.oddOperators, Odd->(
            Odd' = map(FEven_0, R^{-g-1}**FOdd_0, R^{-g-1}**Odd**id_(target psi));
            (extend(FEven,R^{-g-1}**FOdd, Odd'))_1));
    eEv := apply(M.evenOperators, Even->(
            Even' = map(FOdd_0, R^{-g-1}**FEven_0, R^{-g-1}**Even**id_(target psi));
            (extend(FOdd,R^{-g-1} ** FEven, Even'))_1));

    c0 := tensorProduct(phi0, psi);
    c1 := tensorProduct(phi1, psi);
    eOdd = apply(eOdd, e->map(target c0, target c1, e));
    eEv = apply(eEv, e->map(target c1, R^{-1}**target c0, e));
    cliffordModule(eOdd,eEv)
)


vectorBundleOnE = method()
vectorBundleOnE(Matrix) := M -> (
    new VectorBundleOnE from hashTable{yAction => M})

degOnE(VectorBundleOnE) := L -> degOnE L.yAction


tensorProduct(VectorBundleOnE,VectorBundleOnE) := (L1, L2) ->
vectorBundleOnE tensorProduct(L1.yAction,L2.yAction)

----- overload tensor product
tensor(VectorBundleOnE,VectorBundleOnE) := (L1,L2) -> tensorProduct(L1,L2)

VectorBundleOnE ** VectorBundleOnE := (L1,L2) -> tensorProduct(L1,L2)

tensor(CliffordModule,VectorBundleOnE) := (M,V) -> tensorProduct(M,V)

CliffordModule ** VectorBundleOnE := (M,V) -> tensorProduct(M,V)
-----

cliffordModule = method(TypicalValue => CliffordModule)
cliffordModule(List, List) := (uOdd, uEv) ->(
    R := ring uOdd_0;

    if (char coefficientRing R == 2) then error "the ground field must be not of characteristic 2";

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

randomExtension(VectorBundleOnE,VectorBundleOnE) := (L1,L2) -> (
    vectorBundleOnE randomExtension(L1.yAction,L2.yAction)
    )


searchUlrich=method()
searchUlrich(CliffordModule,Ring) :=(M,S) -> (
    searchUlrich(M,S,2)
    )

searchUlrich(CliffordModule,Ring,ZZ) := (M,S,r) ->(
    -- Input
    -- r: integer at least 2
    -- M: a Clifford Module associated to a pencil of quadrics in PP^(2g+1)
    -- S: associated ring of polynomials (2g+2 variables for PP^(2g+1), and two variables s, t for parametrizing the pencil of quadrics)

    -- Output: an Ulrich module of rank r*2^(g-2)

    -- By Riemann-Roch, we need a rank r vector bundle E of degree r(g-2)/2, or a twist by a line bundle
    -- so we are tempting to find a rank r, degree rg/2 vector bundle by extensions of r line bundles.

    Mor := vectorBundleOnE M.evenCenter;
    Mor1:= vectorBundleOnE M.oddCenter;
    f := M.hyperellipticBranchEquation;

    g:=degree ideal f//2-1;
    if (numgens S != 2*g+4) then error "S should be a polynomial ring in (2g+4) variables";
    if (rank source M.evenCenter != 2^(g+1)) then error "M should be a Clifford module associated to a maximal isotropic subspace";
    if (dim ideal jacobian ideal f > 0) then error "f should have no multiple roots";

    if (r <= 1) then error "r should be at least 2";
    if ((r*g)%2 != 0) then error "either r or g should be even";

    m1:=null; m2:= null; m12 := null;
    Ul:= null; Ul1:= null; V:= null;
    d1 := null; d0:= null;

    d:=g%2;
    -- build up a nondecreasing sequence begin with d, end with d+g, and their sum is an expected degree, and is balanced as possible as it can
    degSeq:=apply(r, i->( if 2*i <= r-1 then d+((2*i*g+r-1)//(2*r-2)) else d+g-((2*(r-1-i)*g+r-1)//(2*r-2))));

    targetSum := (r*g)//2 + d*r;
    while (
        -- build a vector bundle V as extensions of line bundles of degrees in degSeq
        V=randomLineBundle(degSeq#0,f);
        for i from 1 to r-1 do(
            m1=randomLineBundle(degSeq#i,f);
            m12=randomExtension(m1.yAction,V.yAction);
            V=vectorBundleOnE m12;
            );
        Ul=tensor(Mor,V);
        Ul1=tensor(Mor1,V);
        d0=unique degrees target Ul.yAction;
        d1=unique degrees target Ul1.yAction;

        #d1 >=3 or #d0 >=3) do ();

    Ul = tensor(M,V); -- the heaviest part computing the actions of generators
    M1Ul:=sum(#Ul.oddOperators,i->S_i*sub(Ul.oddOperators_i,S));
    Ulrich := M1Ul^{r*2^g..(2*r)*2^g-1};
    kk := coefficientRing S;
    P := kk[support(Ulrich)];
    coker map(P^(r*2^g),,sub(Ulrich,P))
    )


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

    M':=tensor(M,L);
    (Mqq1,Mqq2):=cliffordModuleToMatrixFactorization(M',S); -- MF of qq
    qq:=(Mqq1*Mqq2)_(0,0);
    q1:=diff(S_(numgens S-2),qq);
    q2:=diff(S_(numgens S-1),qq);
    S':=(coefficientRing S)[drop (gens S,-2)]; -- ring without s,t
    CI:=S'/ideal (sub(q1,S'), sub(q2,S')); -- complete intersection ring
    resCI:=cliffordModuleToCIResolution (M', S, CI);
    BCI:=betti resCI;

    d:=max apply(select(keys BCI, k->BCI_k==1),k->k_0); -- fixed in v1.1
    uL:=sub(mingens ideal (resCI.dd_d)_{0}, S);

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


cliffordModuleToCIResolution = method()
cliffordModuleToCIResolution(CliffordModule,Ring, Ring) :=(M,S,CI) ->(
    (M1,M2) := cliffordModuleToMatrixFactorization(M,S);
    StoCI := map(CI, S, gens CI | {0,0});
    i := first max degrees target M.evenCenter;
    B1 := sub(basis(i+2,target(M.oddCenter)),S);
    B2 := sub(basis(i,target(M.evenCenter)),S);
    d1 :=first degrees target transpose  M1 - (first degrees target B1);
    dd1 := (((transpose M1)**S^d1)*B2)//B1;
    dd1':= StoCI (dd1);
    freeResolution(coker  dd1', LengthLimit => numgens ring dd1' + 1)
    )

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
ciModuleToMatrixFactorization Module := M ->(
    Ubar := ring M;
    n := numgens Ubar;

    s := local s;
    t := local t;

    (d0,d1) := EisenbudShamashTotal M;
    S' := ring d0;
    kk := coefficientRing Ubar;
    S := kk[gens Ubar,s,t];
    S'toS := map(S, S', apply(n,i->S_(i+2))|{S_0,S_1} );
    fixDegs := d->(
        d' := S'toS d;
        td := degrees target d;
        sd := degrees source d;
        td':= apply(td, D -> ceiling(3*D_0/2 -D_1));
        sd':= apply(sd, D -> floor(3*D_0/2 -D_1));
        map(S^td',S^sd',d')
        );
    e0 := fixDegs d0;
    e1 := fixDegs d1;
    (e1,e0)
    )


----- DOCUMENTATION -----
beginDocumentation()
undocumented{    
    (expression,CliffordModule),
    (net,CliffordModule),
    (texMath,CliffordModule),
    (toString,CliffordModule),
    (expression,VectorBundleOnE),
    (net,VectorBundleOnE),
    (texMath,VectorBundleOnE),
    (toString,VectorBundleOnE),
    (expression,RandomNicePencil),
    (net,RandomNicePencil),
    (texMath,RandomNicePencil),
    (toString,RandomNicePencil)
    }
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
    "A first example constructs a random pencil of quadrics and the associated Clifford module over a finite field.",
    EXAMPLE {
        "kk = ZZ/101;",
        "setRandomSeed 0;",
        "g = 1;",
        "(S,qq,R,u,M1,M2,Mu1,Mu2) = randomNicePencil(kk,g);",
        "C = cliffordModule(M1,M2,R);",
        "C.hyperellipticBranchEquation"
        },
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
        TO preRandomLineBundle, -- kk has to be finite
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
        TO searchUlrich, -- kk has to be finite
        TO LabBookProtocol
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
     row matrix of linear forms with linear coefficients of same length as X
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
     setRandomSeed 0
     n = 4
     c = 2
     kk = ZZ/101
     U = kk[x_0..x_(n-1)]
     qq = matrix{{x_0^2+x_1^2,x_0*x_1}}
     qq = random(U^1, U^{2:-2})
     Ubar = U/ideal qq
     M = coker vars Ubar
     betti (fM=freeResolution(M, LengthLimit => n))
     betti freeResolution(coker transpose fM.dd_3, LengthLimit => n)
     (e1,e0) = ciModuleToMatrixFactorization M;
    Text
     Check that it's a matrix factorization:
    Example
     source e0 == target e1
     0 == e0*e1 - diagonalMatrix(ring e0, apply(numcols e0, i->(e0*e1)_0_0))
     degrees source e1-degrees target e0
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
    (1:randomNicePencil)
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

     M1, M2 are consecutive high syzygy matrices in the minimal (periodic) resolution
     of kk[s,t] = S/(ideal X) as a module over S/qq. These are used to construct the
     Clifford algebra of qq.

     Mu1, Mu2 are consecutive high syzygy matrices in the minimal (periodic) resolution
     of S/(ideal u) as a module over S/qq. These are used to construct a Morita bundle
     between the even Clifford algebra of qq and the hyperelliptic curve
     branched over the degeneracy locus of the pencil,

      \{(s,t) | s*q1+t*q2 is singular\} \subset PP^1.
    Example
     setRandomSeed 0
     kk=ZZ/101
     g=1
     (S, qq, R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g);
     gens S
     q1 = diff(S_(2*g+2),qq)
    Text
     a quadratic form of corank 1 (corresponding to a branch point of E-->PP^1)
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
            setRandomSeed 0
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
    generators for a Clifford Algebra
   Usage
    (eOdd, eEv) = cliffordOperators (M1,M2,R)
   Inputs
    R:Ring
     polynomial ring of the form kk[U],
     where U are parameter variables
    M1:Matrix
     over an auxiliary ring S = kk[X,Y,Z,U]
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
     thus the pairs (eOd_i,eEv_i) form a representation of Cliff(qq).
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
    Example
       kk=ZZ/101
       setRandomSeed 0
       g = 1
       (S,qq,R,u,M1,M2,Mu1,Mu2) = randomNicePencil(kk,g);
       C = cliffordModule(M1,M2,R)
       C.hyperellipticBranchEquation
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
     setRandomSeed 0
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
     setRandomSeed 0
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
     setRandomSeed 0
     kk = ZZ/101
     g = 1
     (S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g);
     M = cliffordModule(M1,M2, R)
     M.evenCenter
   SeeAlso
    evenOperators
    oddOperators
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
     setRandomSeed 0
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
    evenCenter
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
     setRandomSeed 0
     kk = ZZ/101
     g = 1
     (S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g);
     M = cliffordModule(M1,M2, R)
     M.oddCenter
   SeeAlso
    evenOperators
    oddOperators
    evenCenter
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
     setRandomSeed 0
     g = 1
     (S, qq,  R,  u, M1, M2, Mu1, Mu2)=randomNicePencil(kk,g);
     M = cliffordModule(M1,M2, R)
     M.evenOperators
     symMatrix(M.oddOperators,M.evenOperators)
   SeeAlso
    evenOperators
    oddOperators
    evenCenter
    oddCenter
    hyperellipticBranchEquation
///

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
	    points over which the associated quadratic form is singular. It is the same as
	    the determinant of the symmetric matrix M.symmetricM.
	Example
            setRandomSeed 0
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
    	(baseRing,RandomNicePencil)
    Headline
    	part of a RandomNicePencil
    Usage
    	R = rNP.baseRing
    Outputs
    	R : Ring
	    polynomial ring kk[s,t]
    Description
    	Text
	    The base ring kk[s,t] which is the coordinate ring of PP^1.
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
	    setRandomSeed 0
	    kk=ZZ/101;
	    g=1;
	    rNP=randNicePencil(kk,g);

	    S=rNP.qqRing
    SeeAlso
    	(baseRing,RandomNicePencil)
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
            setRandomSeed 0
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
	(baseRing,RandomNicePencil)
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
	    setRandomSeed 0
	    g=1;
	    rNP=randNicePencil(kk,g);

	    u=rNP.isotropicSpace
    SeeAlso
    	qqRing
	(baseRing,RandomNicePencil)
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
	    setRandomSeed 0
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
        (baseRing,RandomNicePencil)
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
	    setRandomSeed 0
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
	(baseRing,RandomNicePencil)
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
	    setRandomSeed 0
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
	(baseRing,RandomNicePencil)
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
	    setRandomSeed 0
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
	(baseRing,RandomNicePencil)
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
    Example
     kk = ZZ/101
     R = kk[s,t]
     f = (s+2*t)*(s+t)*(s-t)*(s-2*t)
     L0 = randomLineBundle(0,f)
     (degOnE L0, orderInPic L0)
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
     setRandomSeed 0
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
     But the operators are twice the size for M. Note that in both cases
     each of them have the same size as the corresponding matrix factorization.
    Example
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
     setRandomSeed 0
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
     equation y^2 - (-1)^{g+1} * f
     can be represented by its pushforward V to PP^1,
     under the degree 2 map,
     which will be a vector bundle of twice the rank,
     together with a matrix
     M = V.yAction,
     specifying the action of y. The matrix must therefore satisfy
     M^2 = (-1)^{g+1} * f.
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
     setRandomSeed 0
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
     degOnE tensor(L1,F)
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
	    A matrix representing the action of y for the hyperelliptic curve E with equation y^2 - (-1)^{g+1} * f.
	Example
	    setRandomSeed 0
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
    (tensorProduct, VectorBundleOnE, VectorBundleOnE)
    (tensor, CliffordModule, VectorBundleOnE)
    (tensor, VectorBundleOnE, VectorBundleOnE)
    (symbol **, CliffordModule, VectorBundleOnE)
    (symbol **, VectorBundleOnE, VectorBundleOnE)
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
     Sheaves on the hyperelliptic curve y^2 -(-1)^{g+1}* f(s,t) are represented as sheaves on PP^1 together
     with the action of y. Clifford modules are represented as the action of maps
     eOdd_i: M_1 \to M_0 and eEv_i:M_0 \to M_1 between the even and odd parts of the
     module. The result are the corresponding data for the tensor product.

    Example
     kk=ZZ/101
     setRandomSeed 0
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
     M' = tensor(M,L)
     M.evenCenter
     M'.evenCenter
   Caveat
     We can use tensor, or ** method to tensor two VectorBundleOnE instances or to tensor a CliffordModule and a VectorBundleOnE,
     but for matrices representing the action of y, tensorProduct(phi,psi) and tensor(phi,psi) are not the same.
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
	    setRandomSeed 0
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
	    setRandomSeed 0
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
    	preRandomLineBundle
	(preRandomLineBundle,RingElement)
	(preRandomLineBundle,ZZ,RingElement)
    Headline
    	a random line bundle on the hyperelliptic curve
    Usage
    	L=preRandomLineBundle(f)
	Ld=preRandomLineBundle(d,f)
    Inputs
    	f : RingElement
	    the squarefree hyperelliptic branch equation of a CliffordModule.
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

	    whose determinant equals to (-1)^{g+1}*f. We find such a matrix over a finite ground field
	    by picking randomly b, a homogeneous form of degree (g+1),
	    since the binary form b^2 + (-1)^{g+1}*f frequently factors.

	Example
	    kk=ZZ/101;
	    setRandomSeed 0
	    g=1;
	    rNP=randNicePencil(kk,g);
	    cM=cliffordModule(rNP.matFact1,rNP.matFact2,rNP.baseRing);

	    f=cM.hyperellipticBranchEquation
	    L=preRandomLineBundle(f)
	    degOnE L
	    m=L.yAction
	    (m)^2_(0,0)+(-1)^g*f==0

	    L0=preRandomLineBundle(0,f)
	    degOnE L0
	    orderInPic L0

    Caveat
    	The ground field kk has to be finite and f has to be squarefree.
        The method preRandomLineBundle(d,f) possibly fails to return the output when the binary form b^2 + (-1)^{g+1}*f does not factor after 500 trials which may happen when kk is too small.
    SeeAlso
    	vectorBundleOnE
	VectorBundleOnE
	degOnE
	orderInPic
	randomLineBundle
///

doc ///
    Key
    	randomLineBundle
	(randomLineBundle,RingElement)
	(randomLineBundle,ZZ,RingElement)
    Headline
    	a random balanced line bundle on the hyperelliptic curve
    Usage
    	L=randomLineBundle(f)
	Ld=randomLineBundle(d,f)
    Inputs
    	f : RingElement
	    the squarefree hyperelliptic branch equation of a CliffordModule.
	d : ZZ
    Outputs
    	L : VectorBundleOnE
	    a line bundle on E
	Ld : VectorBundleOnE
	    a line bundle on E of degree d.
    Description
    	Text
	    Chooses a random line bundle on the hyperelliptic curve E of genus g
	    given by the equation y^2-(-1)^{g+1}*f, where f is the branch equation of degree
	    (2g+2). Input with an integer d gives a random line bundle of degree d on E.

	    Note that the method preRandomLineBundle mostly constructs an unbalanced line bundle, that is,
	    the degrees of a and c for the determinantal representation of (-1)^{g+1}*f have a big gap.
	    Such a line bundle will be contained in the theta divisor (after a certain twist), so we make it into a balanced line bundle
	    by tensoring degree 0 line bundles.


	Example
	    kk=ZZ/1009;
	    g=2;
	    rNP=randNicePencil(kk,g);
	    cM=cliffordModule(rNP.matFact1,rNP.matFact2,rNP.baseRing);

	    f=cM.hyperellipticBranchEquation

	    tally apply(30, i->(Lp=preRandomLineBundle(1,f); betti Lp.yAction))

	    tally apply(30, i->(L=randomLineBundle(1,f); betti L.yAction))

    Caveat
    	The ground field kk has to be finite and f has to be squarefree.
        The method possibly fails to return the output when the binary form b^2 + (-1)^{g+1}*f does not factor after 500 trials which may happen when kk is too small.
    SeeAlso
    	preRandomLineBundle
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
	    setRandomSeed 0
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
	    setRandomSeed 0
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
    	F : Complex
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
	    setRandomSeed 0
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
	(searchUlrich, CliffordModule, Ring, ZZ)
    Headline
    	searching for an Ulrich module of smallest possible rank, or an Ulrich module of given rank
    Usage
    	Ulr = searchUlrich(M,S)
	Ulr = searchUlrich(M,S,r)
    Inputs
    	M : CliffordModule
	S : Ring
	    a polynomial ring in x_0..y_(g-1),z_1,z_2,s,t
	r : ZZ
	    an integer greater than 1, either g or r is even
    Outputs
    	Ulr : Module
    	    a module over the polynomial ring with variables x_0..y_(g-1),z_1,z_2
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

	    When r is indicated, searchUlrich looks for a candidate G of rank r on E and returns a module on S
	    supported on a CI V(q_1,q_2) \subset PP^{2g+1}.

	Example
	    kk=ZZ/101;
	    setRandomSeed 0
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
	    ring Ulr
	    betti freeResolution Ulr
	Text
	    Note that the function searchUlrich returns a module over the polynomial ring
	    with variables x_0..y_(g-1),z_1,z_2.
	Example
	    ring Ulr, gens ring ideal qs, support ideal qs
	    ann Ulr == sub(ideal qs,ring Ulr)

	    elapsedTime Ulr3 = searchUlrich(M,S,3);
	    betti freeResolution Ulr3
    Caveat
    	searchUlrich uses the method randomLineBundle, so the ground field kk has to be finite.
    SeeAlso
    	cliffordModule
	LabBookProtocol
///


doc ///
    Key
    	LabBookProtocol
    	(LabBookProtocol,ZZ)
	(LabBookProtocol,ZZ,ZZ)
    Headline
        returns commands that lead to a construction of an Ulrich bundle
    Usage
    	str = LabBookProtocol(g)
	str = LabBookProtocol(g,r)
    Inputs
        g:ZZ
	   genus of the associated hyperelliptic curve E.
	r:ZZ
	   rank of the vector bundle E
    Outputs
    	S:String
	   of commands which would produce an Ulrich bundle on X
    Description
    	Text
            Our function searchUlrich produces Ulrich bundles of rank r
	    in principle. However some of the computations take lot of time.
	    We break this approach for small (g,r) into parts
	    and protocol the command and the timings.
	CannedExample
	    i1 : g=3

	    o1 = 3
	    i2 : LabBookProtocol(g)
            o2 =
              g=3
              kk=ZZ/101;
              elapsedTime (S,qq,R,u,M1,M2,Mu1,Mu2)=randomNicePencil(kk,g);
              -- .0778377s elapsed
              P=kk[drop(gens S,-2)]
              M=cliffordModule(Mu1,Mu2,R);
              Mor = vectorBundleOnE M.evenCenter;
              Mor1= vectorBundleOnE M.oddCenter;
              f = M.hyperellipticBranchEquation;
              assert(dim ideal jacobian ideal f ==0);
              elapsedTime while (
                  m1=randomLineBundle(g+(g%2),f);
                  m2=randomLineBundle(g%2,f);
                  m12=randomExtension(m1.yAction,m2.yAction);
                  V = vectorBundleOnE m12;
                  Ul=tensor(Mor,V);
                  Ul1=tensor(Mor1,V);
                  d0=unique degrees target Ul.yAction;
                  d1=unique degrees target Ul1.yAction;
                  #d1 >=3 or #d0 >=3) do ();
              -- .159068s elapsed
              betti Ul.yAction, betti Ul1.yAction
              --further commands
              elapsedTime Ul = tensor(M,V); -- the heaviest part computing the actions of generators
              -- 5.5923s elapsed
              M1Ul=sum(#Ul.oddOperators,i->S_i*sub(Ul.oddOperators_i,S));
              r=2
              Ulrich := M1Ul^{r*2^g..(2*r)*2^g-1};
              Ulr=coker map(S^(r*2^g),,Ulrich);
              minimalBetti Ulr
              -- will give an Ulrich bundle, with betti table
              -- 16 32 16
	    i3 : g=3

	    o3 = 3
	    i4 : kk= ZZ/101;
	    i5 : elapsedTime (S,qq,R,u,M1,M2,Mu1,Mu2)=randomNicePencil(kk,g);
	    -- .0760222s elapsed
	    i6 : P=kk[drop(gens S,-2)]

	    o6 = P

	    o6 : PolynomialRing
	    i7 : M=cliffordModule(Mu1,Mu2,R);
	    i8 : Mor = vectorBundleOnE M.evenCenter;
	    i9 : Mor1= vectorBundleOnE M.oddCenter;
	    i10 : f = M.hyperellipticBranchEquation;
	    i11 : assert(dim ideal jacobian ideal f ==0);
	    i12 : elapsedTime while (
		m1=randomLineBundle(g+(g%2),f);
		m2=randomLineBundle(g%2,f);
		m12=randomExtension(m1.yAction,m2.yAction);
		V = vectorBundleOnE m12;
		Ul=tensor(Mor,V);
		Ul1=tensor(Mor1,V);
		d0=unique degrees target Ul.yAction;
		d1=unique degrees target Ul1.yAction;
		#d1 >=3 or #d0 >=3) do ();
	    -- .176025s elapsed
	    i13 : betti Ul.yAction, betti Ul1.yAction

	                   0  1          0  1
	    o13 = (total: 32 32, total: 32 32)
	              -4: 16  .     -2: 32  .
		      -3: 16  .     -1:  .  .
		      -2:  .  .      0:  .  .
		      -1:  . 16      1:  . 32
		       0:  . 16

	    o13 : Sequence
	    i14 : elapsedTime Ul = tensor(M,V); -- the heaviest part computing the actions of generators
	    -- 5.61593s elapsed
	    i15 : M1Ul=sum(#Ul.oddOperators,i->S_i*sub(Ul.oddOperators_i,S));

	                  32      32
	    o15 : Matrix S   <-- S
	    i16 : r=2

	    o16 = 2
	    i17 : Ulrich := M1Ul^{r*2^g..(2*r)*2^g-1};

	                  16      32
	    o17 : Matrix S   <-- S
	    i18 : Ulr=coker map(S^(r*2^g),,Ulrich);
	    i19 : minimalBetti Ulr

	                  0  1  2
	    o19 = total: 16 32 16
	              0: 16 32 16

	    o19 : BettiTally
	    i20 : (g,r)=(3,4)

	    o20 = (3, 4)

	    o20 : Sequence
	    i21 : LabBookProtocol(g,r)

	          g=3
		  r=4
		  kk= ZZ/101;
		  elapsedTime (S,qq,R,u,M1,M2,Mu1,Mu2)=randomNicePencil(kk,g);
		  -- 0.623928 seconds elapsed
		  P=kk[drop(gens S,-2)]
		  gens P
		  M=cliffordModule(Mu1,Mu2,R);
		  Mor = vectorBundleOnE M.evenCenter;
		  Mor1= vectorBundleOnE M.oddCenter;
		  f = M.hyperellipticBranchEquation;
		  assert(dim ideal jacobian ideal f ==0);
		  degSeq={1,2,3,4}
		  elapsedTime while (
		      -- build a vector bundle V as extensions of line bundles of degrees in degSeq
		      V=randomLineBundle(degSeq#0,f);
		      for i from 1 to r-1 do(
			  m1=randomLineBundle(degSeq#i,f);
			  m12=randomExtension(m1.yAction,V.yAction);
			  V=vectorBundleOnE m12;
			  );
		      Ul=tensor(Mor,V);
		      Ul1=tensor(Mor1,V);
		      d0=unique degrees target Ul.yAction;
		      d1=unique degrees target Ul1.yAction;
		      #d1 >=3 or #d0 >=3) do ();
		  -- 1.15312 seconds elapsed
		  betti Ul.yAction,betti Ul1.yAction
		  --further commands
		  elapsedTime Ul = tensor(M,V); -- the heaviest part computing the actions of generators
		  -- 87.5896 seconds elapsed
		  M1Ul=sum(#Ul.oddOperators,i->S_i*sub(Ul.oddOperators_i,S));
		  Ulrich = coker map(P^(r*2^g),,sub(M1Ul^{r*2^g..(2*r)*2^g-1},P));
		  minimalBetti Ulrich
		  -- Is an Ulrich bundle, with betti numbers
		  -- r*2^g,(2*r)*2^g,r*2^g
		  elapsedTime qs=ann Ulrich
		  -- 25.3661 seconds elapsed
		  ideal sub(diff((vars S)_{2*g+2,2*g+3},qq),P)==qs

    SeeAlso
    	searchUlrich
	RandomNicePencil
	tensorProduct
	randomLineBundle
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
	    a polynomial ring kk[x_0..y_{(g-1)},z_1,z_2,s,t] in (2g+4) variables
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
	    setRandomSeed 0
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
	    a polynomial ring kk[x_0..y_{(g-1)},z_1,z_2,s,t] in (2g+4) variables
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
	    setRandomSeed 0
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


--TEST SECTION--
---------------
---tests-------
---------------

TEST/// --test of preRandomLineBundle and randomLineBundle
setRandomSeed 0
kk = ZZ/nextPrime(10^3)
R = kk[ s,t]
g = 1
f = random(2*g+2, R)
assert(dim ideal(jacobian ideal f)== 0)
-- preRandomLineBundle(d,f) is a degree-d line bundle on the curve y^2-(-1)^g*f
L2 = preRandomLineBundle(2,f)
assert(class L2 === VectorBundleOnE)
assert(degOnE L2 == 2)
assert((L2.yAction)^2_(0,0) + (-1)^g * f == 0)
assert(degOnE preRandomLineBundle(0,f) == 0)
-- randomLineBundle(d,f) likewise produces a degree-d line bundle, more balanced
assert(degOnE randomLineBundle(3,f) == 3)
assert(degOnE randomLineBundle(0,f) == 0)
///

TEST///
setRandomSeed 0
kk = ZZ/101;g=1
(S,qq,R,u, M1,M2, Mu1, Mu2) = randomNicePencil(kk,g);
diff(matrix{{S_(2*g+2), S_(2*g+3)}}, qq)
P = kk[drop(gens S, -2)]
qs = sub(diff(matrix{{S_(2*g+2), S_(2*g+3)}}, qq), P)
CI = P/ideal qs

F = freeResolution( coker (sub(u, CI)), LengthLimit => 3)
betti (FF = freeResolution( coker transpose F.dd_3, LengthLimit => 5))
M = cliffordModule(Mu1, Mu2, R)
betti (F1=cliffordModuleToCIResolution(M,S,CI))
betti (FFF = freeResolution(coker FF.dd_5, LengthLimit => 5))
q1 = diff(S_(2*g+2),qq)
q2 = diff(S_(2*g+3),qq)
N = (S^1/(ideal(q1,q2))**coker sub(F.dd_2,S))
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

F = freeResolution( coker (sub(u, CI)), LengthLimit => 3)
betti (FF = freeResolution( coker transpose F.dd_3, LengthLimit => 5))
M = cliffordModule(Mu1, Mu2, R)
betti (F1=cliffordModuleToCIResolution(M,S,CI))
assert(ideal F1.dd_(g+3-(g%2))_{0} ==ideal sub(u,CI))
M = cliffordModule(M1,M2,R)
betti (F2=cliffordModuleToCIResolution(M,S,CI))
assert(ideal F2.dd_(2*g+3)_{0}^{0..2*g+1} == ideal gens CI)
///


TEST///-* test of randomIsotropicSubspace*-
setRandomSeed 0
kk=ZZ/101;g=2
(S,qq,R,u, M1,M2, Mu1, Mu2) = randomNicePencil(kk,g);

M=cliffordModule (Mu1, Mu2, R);
f=M.hyperellipticBranchEquation;
L=randomLineBundle(0,f)
uL=translateIsotropicSubspace(M,L,S)

rU=randomIsotropicSubspace(M,S)
assert (betti rU == betti u)
///

TEST///-*test of cliffordModuleToMatrixFactorization*-
setRandomSeed 0
kk = ZZ/101;g= 1
(S,qq,R,u, M1,M2, Mu1, Mu2) = randomNicePencil(kk,g);
M = cliffordModule(Mu1, Mu2, R)
(Mu1', Mu2') = cliffordModuleToMatrixFactorization (M,S);
assert(Mu1'-Mu1 == 0 and Mu2'-Mu2 ==0)
assert(Mu1'==Mu1)
assert(Mu2'==Mu2)
M = cliffordModule(M1, M2, R);
(M1', M2') = cliffordModuleToMatrixFactorization (M,S);
assert(M1'-M1 == 0 and M2'-M2 ==0)
assert(M1'==M1)
assert(M2'==M2)

--g=2
kk = ZZ/101;g=2
(S,qq,R,u, M1,M2, Mu1, Mu2) = randomNicePencil(kk,g);
M = cliffordModule(Mu1, Mu2, R)
(Mu1', Mu2') = cliffordModuleToMatrixFactorization (M,S);
assert(Mu1'-Mu1 == 0 and Mu2'-Mu2 ==0)
assert(Mu1'==Mu1)
assert(Mu2'==Mu2)
M = cliffordModule(M1, M2, R);
(M1', M2') = cliffordModuleToMatrixFactorization (M,S);
assert(M1'-M1 == 0 and M2'-M2 ==0)
assert(M1'==M1)
assert(M2'==M2)
///

TEST///-*test of centers*-
setRandomSeed 0
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
assert ((cMu.evenCenter, cMu.oddCenter)==centers(cMu.oddOperators, cMu.evenOperators))
///

TEST///-*test of tensorProduct of randomLineBundle and degreeOnE and orderInPic*-
setRandomSeed 4
kk = ZZ/101
R = kk[s,t]
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
assert(orderInPic randomLineBundle(0,f) == 2)
///

TEST///-*test of the Pfaffian formula *-
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
assert(all(n,i->eOdd_i*c1+c0*eOdd_i==0))
assert(all(n,i->eEv_i*c0+c1*eEv_i==0))
assert(target eEv_0 == target c1)
assert(target eOdd_0 == target c0)
determ=det symMatrix(eOdd,eEv)
-- Note the factor (-1)^d occurs in the test below
assert(c0^2-(-1)^d*determ*id_(target c0)==0)
assert(c1^2-(-1)^d*determ*id_(source c1)==0)
///

TEST///-*test of randomNicePencil*-
setRandomSeed 0
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
///

TEST/// -*test of matrixFactorizationK*-
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

TEST/// -*test of randNicePencil and its RandomNicePencil accessors*-
setRandomSeed 0
rNP = randNicePencil(ZZ/101, 1)
assert(class rNP === RandomNicePencil)
-- the eight stored keys (baseRing reuses the Core symbol of that name)
assert(sort apply(keys rNP, toString) === {"baseRing","isotropicSpace","matFact1","matFact2","matFactu1","matFactu2","qqRing","quadraticForm"})
S = rNP.qqRing
qq = rNP.quadraticForm
assert(ring qq === S)
assert(class rNP.baseRing === PolynomialRing)
assert(class rNP.isotropicSpace === Matrix)
-- matFact1, matFact2 are a matrix factorization of the pencil qq
assert((rNP.matFact1 * rNP.matFact2) - qq ** id_(source rNP.matFact1) == 0)
assert((rNP.matFact2 * rNP.matFact1) - qq ** id_(source rNP.matFact1) == 0)
-- matFactu1, matFactu2 are the matrix factorization for the isotropic subspace
assert((rNP.matFactu1 * rNP.matFactu2) - qq ** id_(source rNP.matFactu1) == 0)
assert((rNP.matFactu2 * rNP.matFactu1) - qq ** id_(source rNP.matFactu1) == 0)
///

TEST/// -*test of ciModuleToMatrixFactorization and ciModuleToCliffordModule*-
setRandomSeed 0
kk = ZZ/101
U = kk[x_0..x_3]
qq = random(U^1, U^{2:-2})
Ubar = U/ideal qq
M = coker vars Ubar
-- ciModuleToMatrixFactorization returns a matrix factorization (e1,e0)
(e1,e0) = ciModuleToMatrixFactorization M
assert(isHomogeneous e0 and isHomogeneous e1)
assert(source e0 == target e1)
assert(e0*e1 - diagonalMatrix(ring e0, apply(numcols e0, i->(e0*e1)_(0,0))) == 0)
assert(e1*e0 - diagonalMatrix(ring e1, apply(numcols e1, i->(e1*e0)_(0,0))) == 0)
-- ciModuleToCliffordModule packages the same data as a CliffordModule
C = ciModuleToCliffordModule M
assert(class C === CliffordModule)
assert(sort apply(keys C, toString) === {"evenCenter","evenOperators","hyperellipticBranchEquation","oddCenter","oddOperators","symmetricM"})
assert(class C.symmetricM === Matrix)
-- the branch equation is the determinant of the symmetric matrix symmetricM
assert(det C.symmetricM == C.hyperellipticBranchEquation)
///

TEST/// -*test of vectorBundleOnE, VectorBundleOnE and randomExtension*-
setRandomSeed 0
kk = ZZ/101
R = kk[s,t]
f = (s+2*t)*(s+t)*(s-t)*(s-2*t)
-- a degree-0 line bundle, re-wrapped as a VectorBundleOnE
L0 = randomLineBundle(0,f)
V = vectorBundleOnE(L0.yAction)
assert(class V === VectorBundleOnE)
assert(V.yAction === L0.yAction)
-- randomExtension produces a VectorBundleOnE whose degree on E is additive
L1 = randomLineBundle(-1,f)
E = randomExtension(L1,L0)
assert(class E === VectorBundleOnE)
assert(degOnE E == degOnE L1 + degOnE L0)
-- the (Matrix,Matrix) overload returns the yAction matrix of such an extension
E2 = randomExtension(L1.yAction, L0.yAction)
assert(class E2 === Matrix)
assert(degOnE vectorBundleOnE E2 == degOnE L1 + degOnE L0)
///

TEST/// -*test of searchUlrich and LabBookProtocol*-
setRandomSeed 0
kk = ZZ/101
(S,qq,R,u,M1,M2,Mu1,Mu2) = randomNicePencil(kk,1)
Mu = cliffordModule(Mu1,Mu2,R)
-- searchUlrich finds an Ulrich module: rank 2^(g+1) over S, with a linear presentation
ulr = searchUlrich(Mu,S)
assert(class ulr === Module)
assert(ulr != 0)
assert(#support presentation ulr ==4)
assert(numgens target presentation ulr == 2^2)
assert(isHomogeneous presentation ulr)
-- LabBookProtocol prints lines of codes to search for an Ulrich bundle; it returns a String
assert(class LabBookProtocol 3 === String)
///

end--
