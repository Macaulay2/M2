newPackage( "RationalMaps",
    Version => "1.0", Date => "March 11th, 2022", Authors => {
        {Name => "Karl Schwede",
        Email=> "kschwede@gmail.com",
        HomePage=> "http://www.math.utah.edu/~schwede"
        }, --Karl Schwede was partially supported by  NSF FRG Grant DMS #1265261/1501115, NSF CAREER Grant DMS #1252860/1501102, and NSF grant #1801849
        {Name => "Daniel Smolkin",
        Email=> "smolkind@umich.edu",
        HomePage=> "http://dan.smolk.in"
        },--Dan Smolkin was partially supported by  NSF FRG Grant DMS #1265261/1501115, NSF CAREER Grant DMS #1252860/1501102
        {Name => "S. Hamid Hassanzadeh",
        Email => "hassanzadeh.ufrj@gmail.com",
        HomePage=>"https://www.researchgate.net/profile/Seyed_Hassanzadeh"
        }, --S. Hamid Hassanzadeh was supported by CNPq-bolsa de Produtividade
        {Name => "C.J. Bott",
        Email => "cjamesbott@gmail.com",
        HomePage=>"https://www.math.tamu.edu/directory/formalpg.php?user=cbott2"}
    }, --this file is in the public domain
    Keywords => {"Commutative Algebra"},
    Headline => "rational maps between varieties", 
    PackageExports => {"FastMinors", "Varieties"},
    Certification => {
	 "journal name" => "The Journal of Software for Algebra and Geometry",
	 "journal URI" => "https://msp.org/jsag/",
	 "article title" => "RationalMaps, a package for Macaulay2",
	 "acceptance date" => "17 July 2022",
	 "published article URI" => "https://msp.org/jsag/2022/12-1/p03.xhtml",
	 "published article DOI" => "10.2140/jsag.2022.12.17",
	 "published code URI" => "https://msp.org/jsag/2022/12-1/jsag-v12-n1-x03-RationalMaps.m2",
	 "release at publication" => "0cee3a5ae1e3fbd3dfa8407a4c8d6ad6a13dffd3",	    -- git commit number in hex
	 "version at publication" => "1.0",
	 "volume number" => "12",
	 "volume URI" => "https://msp.org/jsag/2022/12-1/"
	 }
)
--Hassanzadeh was supported by CNPq-bolsa de Produtividade and by the MathAmSud project ``ALGEO''
--Schwede was supported in part by the NSF FRG Grant DMS \#1265261/1501115, NSF CAREER Grant DMS \#1252860/1501102, NSF Grants DMS \#1840190 and DMS \#2101800.
--Smolkin was supported in part by the NSF FRG Grant DMS \#1265261/1501115, NSF CAREER Grant DMS \#1252860/1501102 and NSF Grant DMS \#1801849.
export{
    "RationalMapping", --a new type
    "rationalMapping", --constructor
    "isBirationalMap",
    "idealOfImageOfMap",
    "baseLocusOfMap",
    "isRegularMap",
    "isEmbedding",
--	"relationType",
    "jacobianDualMatrix",
    "isBirationalOntoImage",
    "inverseOfMap",
	"mapOntoImage",
    "QuickRank",
	--"blowUpIdeals", --at some point we should document this and expose it to the user
	--"nonZeroMinor",-- it is internal because the answer is probabilistic (either it finds one or it doesn't) and it is controlled by MinorsLimit option
    "isSameMap",
    "sourceInversionFactor",
        --"simisAlgebra", --at some point we should document this and expose it to the user
    --**********************************
    --*************OPTIONS**************
    --**********************************
    "SaturationStrategy", --an option for controlling how inversion of maps is run.
    "ReesStrategy", --an option for controlling how inversion of maps is run.
    "SimisStrategy", --an option for controlling how inversion of maps is run.
    "HybridStrategy", --an option for controlling how inversion of maps is run. (This is the default)
    "MinorsLimit", --an option for how many times we should randomly look for a minor before calling syz in inverseOfMap
    "HybridLimit", --an option for controlling inversion of maps (whether to do more Simis or more Rees strategies)
    "CheckBirational", --an option for inverseOfMap, whether or not to check whether something is birational
    "SaturateOutput",  --option to turn off saturation of the output
    "AssumeDominant" --option to assume's that the map is dominant (ie, don't compute the kernel)
}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isSameDegree:=method();
isSameDegree(BasicList):=(L)->(
--    n:=#L;
--    flag := true;
--    if n!=0 then (
--        d:=max(apply(L,zz->degree zz));
--        i := 0;
--        while ((i < n) and (flag == true)) do(
--	        if (isHomogeneous(L#i) == false) then flag = false;
--            if ((L#i) != sub(0, ring(L#i))) then (
--	            if (degree(L#i) != d) then flag = false;
--	        );
--       	    i = i+1;
--        );
--    );
--    flag
--);
--***the following code is modified from what was provided by the referee.  
    if #L == 0 then return true; --it is true vacuously
    R:=ring(first L);
    d:=degree(first L);
    all(drop(L, 1), z -> ((z == 0_R) or (degree z == d) ))
);
--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

RationalMapping = new Type of HashTable;
--note Cremona already has a class called RationalMap, I'm trying to avoid conflicts
--a RationalMapping has the following entries
--map, a RingMap, corresponding to the rational map
--cache, a CacheTable, storing anything that happens to be cached

rationalMapping = method(Options=>{}); --constructor for RationalMapping

rationalMapping(RingMap) := o->(phi) -> (
    if not isHomogeneous target phi then error "rationalMapping: the target should be homogeneous";
    if not isHomogeneous source phi then error "rationalMapping: the source should be homogeneous";
    if not isSameDegree(first entries matrix phi) then error "rationalMapping:  expected all terms to have the same degree";
    new RationalMapping from {map=>phi, cache => new CacheTable from {}}
);

rationalMapping(Ring, Ring, BasicList) := o->(R1, R2, L1) -> (
    rationalMapping(map(R1, R2, L1))
);

rationalMapping(Ring, Ring, Matrix) := o->(R1, R2, M1) -> (
    rationalMapping(map(R1, R2, M1))
);

rationalMapping(ProjectiveVariety, ProjectiveVariety, BasicList) := o->(X1, X2, L1) -> (
    rationalMapping(map(ring X2, ring X1, L1))
);

rationalMapping(ProjectiveVariety, ProjectiveVariety, Matrix) := o->(X1, X2, M1) -> (
    rationalMapping(map(ring X2, ring X1, M1))
);

target(RationalMapping) := myMap ->(
    Proj source (myMap#map)
)

source(RationalMapping) := myMap ->(
    Proj target (myMap#map)
)

net RationalMapping := t -> (
    net(source t) | " - - - > " | net(target t) | "   " | net(first entries matrix map t)
)

RationalMapping * RationalMapping := RationalMapping => (phi, psi) -> (
    rationalMapping( (psi#map)*(phi#map))
);

RationalMapping == RationalMapping := Boolean => (phi, psi) -> (
    isSameMap(phi, psi)
);

map(RationalMapping) := o -> phi -> (
    phi#map
);

RationalMapping ^ ZZ := RationalMapping => (myphi, n1) -> (
    if (n1 == 1) then return myphi;
    if (n1 == -1) then return inverseOfMap(myphi, Verbosity => 0);
    if not (source myphi === target myphi) then error "RationalMapping^ZZ : expected a RationalMapping with the same target and source.";
    if (n1 == 0) then (
        --map to the zero should be the identity
        return rationalMapping map(ring source myphi, ring source myphi);
    )
    else if (n1 > 0) then (
        return fold((a,b)->a*b, apply(n1, i->myphi));
    )
    else if (n1 < 0) then (
        inverseOfMyPhi := inverseOfMap(myphi, Verbosity=>0);
        m1 := abs(n1);
        return fold((a,b)->a*b, apply(m1, i->inverseOfMyPhi));
    );    
);
-------------------------------------------------------



StrategyGRevLexSmallestTerm = new HashTable from {LexLargest=>0, LexSmallestTerm => 0, LexSmallest=>0, GRevLexSmallestTerm => 100, GRevLexSmallest => 0, GRevLexLargest=>0,Random=>0,RandomNonzero=>0,Points => 0};
----------------------------------------------------------------
--************************************************************--
-------------------- Function Definitions ----------------------
--************************************************************--
----------------------------------------------------------------

idealOfImageOfMap = method(Options=>{Verbosity=>0, QuickRank=>true});

idealOfImageOfMap(RationalMapping) := o-> (phi) -> (
    if (phi#cache#?idealOfImageOfMap) then return phi#cache#idealOfImageOfMap;
    J := idealOfImageOfMap(phi#map, o);
    phi#cache#idealOfImageOfMap = J;
    J
);



idealOfImageOfMap(RingMap) := o -> (p) -> (
        --h := map(target p, ambient source p,p);
        --do a quick check to see if the map is injective
        if (instance(target p, PolynomialRing)) then(
            if (o.Verbosity >= 2) then print "idealOfImageOfMap: checking if map is zero using rank of the jacobian";
            jac := jacobian matrix p;
            if (o.QuickRank == true) then (
               if (isRankAtLeast(dim source p, jac, Strategy => StrategyGRevLexSmallestTerm, MaxMinors=>2)) then return ideal(sub(0, source p));
            )
            else (
                if (rank jac >= dim source p) then return ideal(sub(0, source p));
            );            
            if (o.Verbosity >= 2) then print "idealOfImageOfMap: map not injective, computing the kernel.";
        );
        im := ker p;
        im
);



-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


baseLocusOfMap = method(Options=>{Verbosity=>0, SaturateOutput=>true});

internalBaseLocusOfMap = method(Options=>{SaturateOutput=>true, Verbosity=>0});
internalBaseLocusOfMap(Matrix) := o->(L1) -> ( --L1 is a row matrix
    if (o.Verbosity >= 2) then print "baseLocusOfMap:  starting";
    if numRows L1 > 1 then error "baseLocsOfMap: Expected a row matrix";
    if isSameDegree( first entries L1  )==false then error "baseLocsOfMap: Expected a matrix of homogeneous elements of the same degree";
    if (o.Verbosity >= 2) then print "baseLocusOfMap:  about to compute ways to write the map";
    
    M:= gens ker transpose presentation image L1;
    -- this matrix gives all the "equivalent"
    -- ways to write the map in question (e.g. (xy : xz) is
    -- equivalent to (y : z) ). So we do this to get the
    -- representation of our map that's defined on the biggest
    -- set of points (e.g. (y : z) extends (xy : xz) to the locus where
    -- x is zero). C.f. proposition 1.1 of the paper
    -- "Cremona Transformations and some Related Algebras" by Aron Simis,
    -- J. Algebra 280 (2004)


    L:= apply(entries M, ll->ideal(ll));
    if (o.SaturateOutput == true) then (
        if (o.Verbosity >= 1) then print "baseLocusOfMap:  saturating output, if this is slow set SaturateOutput => false";
        saturate sum L
    )
    else (
        sum L
    )

    -- the "apply" statement makes a list of ideals; each element of the
    -- list is the ideal generated by elements in a given row of the matrix M
    -- the fold statement adds all of these ideals together into one ideal

    -- each column of M gives a representation of the map with the smallest possible
    -- base locus. So the output of these two commands is the intersection of the base loci
    -- of all the representatives of your map.

);


baseLocusOfMap(RingMap) := o->(ff) ->(
    mm := sub(matrix ff, target ff);
    internalBaseLocusOfMap(mm, o)
);

baseLocusOfMap(RationalMapping) := o->(phi) -> (
    if phi#cache#?baseLocusOfMap then return phi#cache#baseLocusOfMap;
    J := baseLocusOfMap(phi#map, o);
    phi#cache#baseLocusOfMap = J;
    J    
)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- isRegularMap returns true if a map is regular (ie, has no base locus)
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isRegularMap = method(Options=>{Verbosity=>0});


isRegularMap(RingMap) := o->(ff) ->(
    I:=baseLocusOfMap(ff, SaturateOutput=>false, Verbosity=>o.Verbosity);
    --(dim I <= 0)
    if (o.Verbosity >= 2) then print "isRegularMap: computed base locus, now checking its dimension.";
    b := null;
    b = isDimAtMost(0, I);
    if (b === null) then b = dim I <= 0;
    b
);

isRegularMap(RationalMapping) := o->(phi) ->(
    isRegularMap(map phi, o)
);

--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 blowUpIdeals:=method(Options => {Strategy=>ReesStrategy});
 blowUpIdealsSaturation := method();
 blowUpIdealsRees := method();

  --this is to compute the ideal definition of the blowup of a subvariety Z
  --in the projective variety X
  --the defining ideal of the variety X=a
  --the subvariety Z = L list of element in the ring of X

  blowUpIdeals(Ideal, BasicList):=o->(a,L)->(
    if (o.Strategy == ReesStrategy) then (
        blowUpIdealsRees(a,L)
    )
    else if (o.Strategy == SaturationStrategy) then (
        blowUpIdealsSaturation(a,L)
    )
  );


  --the Rees algebra computation below is too slow, we need to modify it
  --Hamid: we may add all of the strategies and options which are applied in saturate
 blowUpIdealsSaturation(Ideal, BasicList):=(a,L)->(
    r:=length  L;
    SS:=ring a;
    RRR:= ring L#0;
    LL:=apply(L,uu->sub(uu, SS));
    n:=numgens ambient  SS;
    K:=coefficientRing SS;
    yyy:=local yyy;
    elt := 0;
    i := 0;
    while ( (i < r) and (sub(L#i,RRR) == sub(0, RRR))) do (i = i+1;);
    if (i == r) then error "blowUpIdealsSaturation: Map is zero map";
    nzd1 := sub(L#i, RRR);
    Rs:=RRR[ toList(yyy_0..yyy_(r-1))];
    M1:=syz(matrix{L},Algorithm =>Homogeneous);
    M2:=sub(M1,Rs);
    N:=matrix{{yyy_0..yyy_(r-1)}};
    symIdeal:=ideal(N*M2);
    --print"symideal ok";
     mymon := monoid[gens ambient SS | toList(yyy_0..yyy_(r-1)), Degrees=>{n:{1,0},r:{0,1}}];
    flatAmbRees:= K(mymon);
    symIdeal2:=sub(a, flatAmbRees)+sub(symIdeal, flatAmbRees);
    nzele:=sub(nzd1,flatAmbRees);
    myReesIdeal:=saturate(symIdeal2,nzele, MinimalGenerators=>false);
   myReesIdeal
  );




  blowUpIdealsRees(Ideal, BasicList):=(a,L)->(
    r:=length  L;
    SS:=ring a;
    LL:=apply(L,uu->sub(uu, SS));
    n:=numgens ambient  SS;
    K:=coefficientRing SS;
    yyy:=local yyy;
    ttt:=local ttt;
    mymon:=monoid[({ttt}|gens ambient SS|toList(yyy_0..yyy_(r-1))), MonomialOrder=>Eliminate 1];
    tR:=K(mymon);
   -- tR:=K[t,gens ambient SS,vars(0..r-1),   MonomialOrder=>Eliminate 1];
    f:=map(tR,SS,submatrix(vars tR,{1..n}));
    F:=f(matrix{LL});
    myt:=(gens tR)#0;
    J:=sub(a,tR)+ideal apply(1..r,j->(gens tR)_(n+j)-myt*F_(0,(j-1)));
    L2:=ideal selectInSubring(1,gens gb J);
    W:=local W;
    nextmon:=monoid[(gens ambient  SS|toList(W_0..W_(r-1))), Degrees=>{n:{1,0},r:{0,1}}];
    RR:=K(nextmon);
    g:=map(RR,tR,0|vars RR);
    trim g(L2));

blowUpIdeals(Ideal, Ideal):=o->(a,b)->(
    blowUpIdeals(a, first entries gens b,Strategy=>o.Strategy)
    );

--Matrix M consists of elements in the ring of a
blowUpIdeals(Ideal, Matrix):=o->(a,M)->(
    blowUpIdeals(a, first entries gens M,Strategy=>o.Strategy)
    );

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- the following is basically a variant of the blowUpIdeals strategy, used for more speed
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simisAlgebra := method();
simisAlgebra(Ideal, BasicList, ZZ):=(a,L,m)->(
    r:=length  L;
    SS:=ring a;
    LL:=apply(L,uu->sub(uu, SS));
    n:=numgens ambient  SS;
    K:=coefficientRing SS;
    yyy:=local yyy;
    ttt:=local ttt;
    if r!=0 then (d:=max(apply(L,zz->degree zz)));--the degree of the elements of linear sys
     mymon:=monoid[({ttt}|gens ambient SS|toList(yyy_0..yyy_(r-1))), MonomialOrder=>Eliminate 1,
	Degrees=>{{(-d_0),1},n:{1,0},r:{0,1}}];
    tR:=K(mymon);  --ambient ring of Rees algebra with weights
    f:=map(tR,SS,submatrix(vars tR,{1..n}));
    F:=f(matrix{LL});
    myt:=(gens tR)#0;
    J:=sub(a,tR)+ideal apply(1..r,j->(gens tR)_(n+j)-myt*F_(0,(j-1)));
    M:=gb(J,DegreeLimit=>{1,m}); --instead of computing the whole Grob. Basis of J we only compute the parts of degree (1,m) or less,
    gM:=selectInSubring(1,gens M);
    L2:=ideal mingens ideal gM;
    W:=local W;
    nextmon:=monoid[(gens ambient  SS|toList(W_0..W_(r-1))), Degrees=>{n:{1,0},r:{0,1}}];
    RR:=K(nextmon);
    g:=map(RR,tR,0|vars RR);
    trim g(L2));




simisAlgebra(Ideal, Ideal,ZZ):=(a,b,m)->(
    simisAlgebra(a, first entries gens b,m)
    );

--Matrix M consists of elements in the ring of a
simisAlgebra(Ideal, Matrix,ZZ):=(a,M,m)->(
    simisAlgebra(a, first entries gens M)
    );



-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 relationType=method(Options => {Strategy=>ReesStrategy, Verbosity=>1});
 --this function computes the "relation type" of an ideal in a ring R.
 --Let R be the ring given bythe  ideal a and L be a list of elements in R.
 --the relation type is the biggest degree in terms of new variables in the
 --defining ideal of the Rees algebra of I over R.
 --

 relationType(Ideal,BasicList):=o->(a,L)->(
     S:=ring L_0;
     J:=blowUpIdeals(a,L,Strategy=>o.Strategy);
     if (o.Verbosity >= 2) then print"blowUpIdeals computed.";
     n:=numgens J;
     L2:={};
     for i from 0 to n-1 do L2=append(L2,(degree J_i)_1);
     max L2);

 relationType(Ideal,Ideal):=o->(a,b)->(
     relationType(ideal,first entries gens b, Strategy=>o.Strategy)
     );

 relationType(Ring,Ideal):=o->(R1,b)->(
     relationType(ideal R1,first entries gens b,Strategy=>o.Strategy)
     );



 --%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- dgi:=method();
 --this function computes the degeneration index of an ideal a which is the
 --number of t linear generators among the generators of a.
 --dgi measures the number of hyperPlanes which cut the variety
 -- defined by a.


 --dgi(Ideal):=(a)->(
 --    S := ring a;
 --    n:=numgens a;
 --    d:=0;
 --    for i from 0 to n-1 do (
 --        if (a_i != sub(0, S)) then (
 --            if (degree a_i)=={1} then d=d+1
 --        );
 --    );
 --);

--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isBirationalMap = method(Options => {AssumeDominant=>false, Strategy=>HybridStrategy,MinorsLimit=>null, Verbosity=>1, HybridLimit=>15, Verbosity=>1, QuickRank=>true});
isBirationalMapInternal = method(Options => {AssumeDominant=>false, Strategy=>HybridStrategy,MinorsLimit=>null, Verbosity=>1, HybridLimit=>15, Verbosity=>1, QuickRank=>true});


--this checks whether a map X -> Y is birational.

--X = Proj R
--Y = Proj S
--This map is given by a list of elements in R, all homogeneous
--of the same degree.
--Below we have defining ideal of X = di
--defining ideal of Y = im
--list of elements = bm
--Strategy => HybridStrategy, ReesStrategy, SimisStrategy, SaturationStrategy


isBirationalMap(RationalMapping) := o-> (phi) -> (
    if (phi#cache#?isBirationalMap) then return phi#cache#isBirationalMap;
    b := isBirationalMapInternal(phi, o);
    phi#cache#isBirationalMap = b;
    b
);

isBirationalMap(RingMap) :=o->(f)->(
    isBirationalMapInternal(rationalMapping f, o)
);

isBirationalMapInternal(RationalMapping) :=o->(phi1)->(
    f := map phi1;
    di := ideal target f;
    R := ring di;
    im := ideal source f;
    S := ring im;
    bm := first entries matrix f; 
    local im1;
    --isBirationalMap(target f, source f, first entries matrix f,AssumeDominant=>o.AssumeDominant,Strategy=>o.Strategy,Verbosity=>o.Verbosity, HybridLimit=>o.HybridLimit,QuickRank=>o.QuickRank)
    if (o.AssumeDominant==false) then (
        if (o.Verbosity >= 1) then (
            print "isBirationalMap: About to find the image of the map.  If you know the image, ";
            print "        you may want to use the AssumeDominant option if this is slow.";
        );
        if (instance(target f, PolynomialRing)) then(
            if (o.Verbosity >= 2) then print "isBirationalMap: initial birationality via rank of the jacobian";
            jac := jacobian matrix f;
            local rk;
            fSourceDim := dim source f;
            if (o.QuickRank == true) then (
                l1 := getSubmatrixOfRank(fSourceDim, jac, Strategy => LexSmallest, MaxMinors=>1);                
                if (l1 === null) then l1 = getSubmatrixOfRank(fSourceDim, jac, Strategy => GRevLexSmallest, MaxMinors=>1, Verbose=>(o.Verbosity >= 2));
                if (l1 === null) then l1 = getSubmatrixOfRank(fSourceDim, jac, Strategy => GRevLexSmallestTerm, MaxMinors=>1, Verbose=>(o.Verbosity >= 2));
                if (l1 === null) then (
                    rk = rank jac;
                )
                else (
                    rk = fSourceDim;
                );
            )
            else (
                rk = rank jac;
            );
            if (o.Verbosity >= 2) then print("isBirationalMap: jac rank = " | toString(rk) | ".  sourceDim = " | fSourceDim);
            if (rk == fSourceDim) then (im1 = im) else (
                if (char S == 0) then print (
                    if (o.Verbosity >= 2) then print "isBirationalMap: the dimension is wrong, not birational.";
                    return false;)
                else (im1 = im + sub(ker f, S));
            ); 
        )
        else (
            im1 = idealOfImageOfMap( map((ring di)/di, ring im, bm), QuickRank=>o.QuickRank);
        );
        if (o.Verbosity >= 2) then print "isBirationalMap: Found the image of the map.";

        if (dim (S^1/im1) >= dim (source f)) then( --first check if the image is the closure of the image is even the right thing
            if (o.Strategy==ReesStrategy or o.Strategy==SaturationStrategy ) then (isBirationalOntoImageInternal(phi1,AssumeDominant=>true, MinorsLimit=>o.MinorsLimit, Strategy=>o.Strategy, Verbosity=>o.Verbosity, QuickRank=>o.QuickRank))
            else if (o.Strategy==HybridStrategy) then ( isBirationalOntoImageInternal(phi1,AssumeDominant=>true, MinorsLimit=>o.MinorsLimit, Strategy=>HybridStrategy, HybridLimit=>o.HybridLimit,Verbosity=>o.Verbosity, QuickRank=>o.QuickRank))
            else if (o.Strategy==SimisStrategy) then (isBirationalOntoImageInternal(phi1,AssumeDominant=>true, MinorsLimit=>o.MinorsLimit, Strategy=>SimisStrategy, Verbosity=>o.Verbosity, QuickRank=>o.QuickRank))
        )
        else(
            if (o.Verbosity >= 2) then print "isBirationalMap: the dimension is really wrong, not birational.";
            false
        )
    )
    else(
        isBirationalOntoImageInternal(phi1,AssumeDominant=>true,Strategy=>o.Strategy,Verbosity=>o.Verbosity, MinorsLimit=>o.MinorsLimit, HybridLimit=>o.HybridLimit, QuickRank=>o.QuickRank)
    )
);

  --%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isBirationalOntoImage = method(Options => {AssumeDominant=>false, MinorsLimit => null, Strategy=>HybridStrategy,Verbosity=>1, HybridLimit=>15, QuickRank=>true});
--if AssumeDominant is true, it doesn't form the kernel.

--*********************************************
--*************the actual functions that do the work

isBirationalOntoImageRees := method(Options => {AssumeDominant=>false, MinorsLimit => null, Strategy=>ReesStrategy,Verbosity=>1, QuickRank=>true});
 isBirationalOntoImageSimis := method(Options => {AssumeDominant=>false, MinorsLimit=> null, HybridLimit=>15,Verbosity=>1, QuickRank=>true});

--*****************************Strategies
--the following method controls how strategies are chosen
isBirationalOntoImageInternal = method(Options => {AssumeDominant=>false, MinorsLimit => null, Strategy=>HybridStrategy,Verbosity=>1, HybridLimit=>15, QuickRank=>true});

isBirationalOntoImageInternal(RationalMapping) :=o->(phi1)->(
--isBirationalOntoImageInternal(Ideal,Ideal, BasicList) :=o->(di,im,bm)->(
    if (o.Verbosity >= 2) then (print "Starting isBirationalOntoImage" );
    if ((o.Strategy == ReesStrategy) or (o.Strategy == SaturationStrategy)) then (
        isBirationalOntoImageRees(phi1, AssumeDominant=>o.AssumeDominant,  Strategy=>o.Strategy,Verbosity=>o.Verbosity, QuickRank=>o.QuickRank)
    )
    else if (o.Strategy == SimisStrategy) then (
        isBirationalOntoImageSimis(phi1, AssumeDominant=>o.AssumeDominant,  HybridLimit=>infinity,Verbosity=>o.Verbosity, QuickRank=>o.QuickRank)
    )
    else if (o.Strategy == HybridStrategy) then(
        isBirationalOntoImageSimis(phi1, AssumeDominant=>o.AssumeDominant, HybridLimit=>o.HybridLimit,Verbosity=>o.Verbosity, QuickRank=>o.QuickRank)
    )
  );



isBirationalOntoImage(RingMap) :=o->(f)->(
    --isBirationalOntoImageInternal(ideal target f, ideal source f, first entries matrix f, o)
    isBirationalOntoImageInternal(rationalMapping f, o)
);

isBirationalOntoImage(RationalMapping) := o->(phi)->(
    if (phi#cache#?isBirationalOntoImage) then return phi#cache#isBirationalOntoImage;
    b := isBirationalOntoImageInternal(phi, o);
    phi#cache#isBirationalOntoImage = b;
    b
);
-*
isBirationalOntoImageRees(Ring,Ring,BasicList) := o->(R1, S1, bm)->(
   isBirationalOntoImageRees(ideal R1, ideal S1, bm, o)
);

isBirationalOntoImageRees(RingMap) :=o->(f)->(
    --isBirationalOntoImageRees(target f, source f, first entries matrix f, o)
    isBirationalOntoImageRees(rationalMapping f)
);
*-

-*
isBirationalOntoImageSimis(Ring,Ring,BasicList) := o->(R1, S1, bm)->(
    isBirationalOntoImageSimis(ideal R1, ideal S1, bm, o)
);

isBirationalOntoImageSimis(RingMap) :=o->(f)->(
    isBirationalOntoImageSimis(target f, source f, first entries matrix f, o)
);
*-

--*************main part of function



isBirationalOntoImageRees(RationalMapping) := o -> (phi1) -> (
    f := map phi1;
    di := ideal target f;
    im := ideal source f;
    bm := first entries matrix f;
    --isBirationalOntoImageRees(Ideal,Ideal, BasicList) :=o->(di,im,bm)->(
    
    if isSameDegree(bm)==false then error "isBirationalOntoImageRees: Expected a list of homogeneous elements of the same degree";
    R:=ring di;
    S:=ring im;
    im1 := im;
    if (o.AssumeDominant == true) then (
        im1 =  im;
    )
    else (
        if (o.Verbosity >= 1) then (
            print "isBirationalOntoImageRees: About to find the image of the map.  If you know the image, ";
            print "        you may want to use the AssumeDominant option if this is slow.";
        );

        im1 = idealOfImageOfMap( map((ring di)/di, ring im, bm), QuickRank=>o.QuickRank);
    );

    K:=coefficientRing R;
--In the following lines we remove the linear parts of the ideal di and
--modify our map bm
    Rlin:=(ambient ring di)/di;
    Rlin2 := minimalPresentation(Rlin);
    phi:=Rlin.minimalPresentationMap;
    Rlin1:=target phi;
    di1:=ideal Rlin1;
    bm0:=phi(matrix{bm});
    bm1:=flatten first entries bm0;
 --From here the situation is under the assumption that the variety is not contained in any hyperplane.
    r:=numgens ambient Rlin1;
    if (o.Verbosity >= 1) then print "isBirationalOntoImageRees:  About to compute the Jacobian Dual Matrix,";
    if (o.Verbosity >= 1) then print "         if it is slow, run again and  set Strategy=>HybridStrategy or SimisStrategy.";
    local barJD;
    if phi1#cache#?jacobianDualMatrix then (
        barJD = phi1#cache#jacobianDualMatrix;
    )
    else (
        barJD=jacobianDualMatrix(phi1,AssumeDominant=>true, Strategy=>o.Strategy);
    );
    --barJD:=jacobianDualMatrix(di1,im1,bm1,AssumeDominant=>true);--JacobianDual Matrix is another function in this package
      nc:=numColumns(transpose barJD);
     nr:=numRows(transpose barJD);
    if (o.Verbosity >= 1) then print "isBirationalOntoImageRees: computed Jacobian dual matrix";
    if (o.Verbosity >= 2) then(
        print ( "Jacobian dual matrix has  " |nc|" columns  and   "|nr|" rows.");
    );
    jdd:=(numgens ambient Rlin1)-1;
    if (o.Verbosity >= 2) then print "isBirationalOntoImageRees: is computing the rank of the  Jacobian dual matrix- barJD";

    --not(isSubset(minors(jdd,barJD),im1))
    if (o.QuickRank == true) then (
        isRankAtLeast(jdd, barJD, Strategy => StrategyDefault, MaxMinors=>2)
    )
    else (
        rank barJD >= jdd
    )
);



--isBirationalOntoImageSimis
---***************************************************

isBirationalOntoImageSimis(RationalMapping) := o-> (phi1) -> (
    f := map phi1;
    di := ideal target f;
    im := ideal source f;
    bm := first entries matrix f;
--isBirationalOntoImageSimis(Ideal,Ideal, BasicList) :=o->(di,im,bm)->(
   -- invList := inverseOfMap(target f, source f, first entries matrix f);
--    map(source f, target f, invList)
--    inverseOfMap(target f, source f, first entries matrix f, AssumeDominant=>o.AssumeDominant)
---*******************
    if (o.Verbosity >= 2) then print "Starting inverseOfMapOntoImageSimis(SimisStrategy or HybridStrategy)";

    im1 := im;
    if (o.AssumeDominant == true) then (
        im1 =  im;
    )
    else (
        if (o.Verbosity >= 1) then (
            print "isBirationalOntoImageSimis: About to find the image of the map.  If you know the image, ";
            print "        you may want to use the AssumeDominant option if this is slow.";
        );

        im1 = idealOfImageOfMap( map((ring di)/di, ring im, bm), QuickRank=>o.QuickRank);
        if (o.Verbosity >= 2) then print "isBirationalOntoImageSimis: Found the image of the map.";
    );
    if isSameDegree(bm)==false then error "isBirationalOntoImageSimis: Expected a list of homogeneous elements of the same degree";
    R:=ring di;
    K:=coefficientRing R;
    S:=ring im;

    --In the following lines we remove the linear parts of the ideal di and
--modify our map bm
    Rlin:=(ambient ring di)/di;
    Rlin2 := minimalPresentation(Rlin);
    phi:=Rlin.minimalPresentationMap;
    Rlin1:=target phi;
    di1:=ideal Rlin1;
    bm0:=phi(matrix{bm});
    bm1:=flatten first entries bm0;
    --From here the situation is under the assumption that the variety is not contained in any hyperplane.
    r:=numgens ambient Rlin1;
    jdd:=(numgens ambient Rlin1)-1;
    --THe following is a part of simisAlgebra
    minorsCt := o.MinorsLimit;
    if (o.MinorsLimit === null) then ( --if the user didn't specify MinorsLimit, we make some educated guesses
        if (jdd < 6) then(
            minorsCt = 3;
        )
        else if (jdd < 9) then (
            minorsCt = 2;
        )
        else if (jdd < 12) then (
            minorsCt = 1;
        )
        else (
            minorsCt = 0;
        );
    );
    rs:=length  bm1;
    SS:=ring di1;
    LL:=apply(bm1,uu->sub(uu, SS));
    n:=numgens ambient  SS;
    Kf:=coefficientRing SS;
    yyy:=local yyy;
    ttt:=local ttt;
    if rs!=0 then (d:=max(apply(bm1,zz->degree zz)));--the degree of the elements of linear sys
    degList := {{(-d_0),1}} | toList(n:{1,0}) | toList(rs:{0,1});
    mymon:=monoid[({ttt}|gens ambient SS|toList(yyy_0..yyy_(rs-1))),  Degrees=>degList, MonomialOrder=>Eliminate 1];
    tR:=Kf(mymon);  --ambient ring of Rees algebra with weights
    f1:=map(tR,SS,submatrix(vars tR,{1..n}));
    F:=f1(matrix{LL});
    myt:=(gens tR)#0;
    J:=sub(di1,tR)+ideal apply(1..rs,j->(gens tR)_(n+j)-myt*F_(0,(j-1)));

    flag := false;   --this boolean checks whether it is birational
    giveUp := false;  --this checks whether we give up checkin birationality or not yet
    secdeg:=1;        --the second degree of Rees equations
    jj := 1;
    M := null;
    while (giveUp == false) do (
	    if (o.Verbosity >= 2) then print("isBirationalOntoImageSimis:  About to compute partial Groebner basis of Rees ideal up to degree " | toString({1, secdeg}) | "." );

        if (secdeg < o.HybridLimit) then (
            M=gb(J,DegreeLimit=>{1,secdeg}); --instead of computing the whole Grob.
                                            --Basis of J we only compute the parts of degree (1,m) or less,
        )
        else(
        if (o.Verbosity >= 1) then print("isBirationalOntoImageSimis:  gave up, it will just compute the whole Groebner basis of the Rees ideal.  Increase HybridLimit and rerun to avoid this." );
            M=gb(J);

            giveUp = true;
        );
        gM:=selectInSubring(1,gens M);
        L2:=ideal mingens ideal gM;
        W:=local W;
        nextmon:=monoid[(gens ambient  SS|toList(W_0..W_(rs-1))), Degrees=>{n:{1,0},rs:{0,1}}];
        RR:=Kf(nextmon);
        g1:=map(RR,tR,0|vars RR);
        Jr:= g1(L2);
        JD:=diff(transpose ((vars ambient ring Jr)_{0..(r-1)}) ,gens Jr);
        vS:=gens ambient S;
        g:=map(S/im1,ring Jr, toList(apply(0..r-1,z->0))|vS);
        barJD:=g(JD);
        nc:=numColumns(transpose barJD);
        nr:=numRows(transpose barJD);
        if (o.Verbosity >= 2) then( print ( "isBirationalOntoImageSimis: Found Jacobian dual matrix (or a weak form of it), it has  " |nc|" columns  and about  "|nr|" rows.");
                            );
        if (giveUp == false) then(
            if (o.Verbosity >= 2) then print "isBirationalOntoImageSimis: is computing the rank of the  Jacobian Dual Matrix- barJD";
            if (o.QuickRank == true) then (
                if (isRankAtLeast(jdd, barJD, Strategy => StrategyDefault, MaxMinors=>minorsCt, Verbose=>(o.Verbosity >= 2))) then (
                    flag=true;
                    giveUp=true;
                );
            )
            else (
                if (rank barJD >= jdd) then (
                    flag=true;
                    giveUp=true;
                )
            );
        )
        else (
            if (o.Verbosity >= 2 ) then( print ( "isBirationalOntoImageSimis: Found Jacobian dual matrix (or a weak form of it), it has  " |nc|" columns  and   "|nr|" rows.");
                            );
            if (o.Verbosity >= 2) then print "isBirationalOntoImageSimis: is computing the rank of the  Jacobian Dual Matrix- barJD";
            if (o.QuickRank == true) then (
                if (isRankAtLeast(jdd, barJD, Strategy => StrategyDefault, MaxMinors=>minorsCt, Verbose=>(o.Verbosity >= 2))) then (
                    flag = true;
                    giveUp=true;
                );
            )
            else (
                if (rank barJD >= jdd) then (
                    flag=true;
                    giveUp=true;
                )
            );
        );
        secdeg=secdeg + jj;
        jj = jj + 1; --we are basically growing secdeg in a quadratic way now, but we could grow it faster or slower...
    );
	flag
);

 --%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 inverseOfMap = method(Options => {AssumeDominant=>false, CheckBirational=>true, Strategy=>HybridStrategy, HybridLimit=>15, Verbosity=>1, MinorsLimit=>null, QuickRank=>true});
 inverseOfMapRees := method(Options => {AssumeDominant=>false, CheckBirational=>true, Strategy=>ReesStrategy, Verbosity=>1,MinorsLimit=>null, QuickRank=>true, RationalMapping=>null});
 inverseOfMapSimis := method(Options => {AssumeDominant=>false, CheckBirational=>true,  HybridLimit=>15, Verbosity=>1,MinorsLimit=>null, QuickRank=>true});
--this checks whether a map X -> Y is birational.

--X = Proj R
--Y = Proj S
----This map is given by a list of elements in R, all homogeneous
--of the same degree.
--Below we have defining ideal of X = di
--defining ideal of Y = im
--list of elements = bm
------*****************************Strategies
inverseOfMap(RingMap):=o->(f)->(
    inverseOfMap(rationalMapping f, o)
);

inverseOfMap(RationalMapping) := o -> (phi) ->(
    f := map phi;
    minorsCt := o.MinorsLimit;
    if (o.MinorsLimit === null) then ( --if the user didn't specify MinorsLimit, we make some educated guesses
        nn := #(gens ambient target f);
        if (nn < 6) then(
            minorsCt = 10;
        )
        else if (nn < 9) then (
            minorsCt = 6;
        )
        else if (nn < 12) then (
            minorsCt = 2;
        )
        else (
            minorsCt = 0;
        );
    );
    if ((o.Strategy == ReesStrategy) or (o.Strategy == SaturationStrategy)) then (
        rationalMapping inverseOfMapRees(phi, QuickRank=>o.QuickRank, AssumeDominant=>o.AssumeDominant, CheckBirational=>o.CheckBirational, Strategy=>o.Strategy,Verbosity=>o.Verbosity, MinorsLimit=>minorsCt)
    )
    else if (o.Strategy == SimisStrategy) then (
        rationalMapping inverseOfMapSimis(phi, QuickRank=>o.QuickRank, AssumeDominant=>o.AssumeDominant, CheckBirational=>o.CheckBirational, HybridLimit=>infinity,Verbosity=>o.Verbosity, MinorsLimit=>minorsCt)
    )
    else if (o.Strategy == HybridStrategy) then(
        rationalMapping inverseOfMapSimis(phi, QuickRank=>o.QuickRank, AssumeDominant=>o.AssumeDominant, CheckBirational=>o.CheckBirational, HybridLimit=>o.HybridLimit,Verbosity=>o.Verbosity, MinorsLimit=>minorsCt)
    )    
);



-*
inverseOfMapRees(Ideal,Ideal,BasicList) :=o->(di,im,bm)->(
    inverseOfMapRees( (ring di)/di, (ring im)/im, bm, o)
);

inverseOfMapRees(Ring,Ring,BasicList) := o->(R1, S1, bm)->(
    inverseOfMapRees(map(R1, S1, bm), o)
    );
*-

--********************************main part Rees
inverseOfMapRees(RationalMapping) := o->(phi1)->(
   -- invList := inverseOfMap(target f, source f, first entries matrix f);
--    map(source f, target f, invList)
--    inverseOfMap(target f, source f, first entries matrix f, AssumeDominant=>o.AssumeDominant)
---*******************
    f := map phi1;
    if (o.Verbosity >= 2) then print "Starting inverseOfMapRees(ReesStrategy or SaturationStrategy)";
    if (o.AssumeDominant == false) then (
        if (o.Verbosity >= 1) then (
            print "inverseOfMapRees: About to find the image of the map.  If you know the image, ";
            print "        you may want to use the AssumeDominant option if this is slow.";
        );
        f = mapOntoImage(f);
        if (o.Verbosity >= 2) then print "inverseOfMapRees: Found the image of the map.";
    );
    di := ideal target f;
    im := ideal source f;
    bm := first entries matrix f;
    if isSameDegree(bm)==false then error "inverseOfMapRees: Expected a list of homogeneous elements of the same degree";
    R:=ring di;
    K:=coefficientRing R;
    S:=ring im;
    im1 := im;

    --In the following lines we remove the linear parts of the ideal di and
--modify our map bm
    Rlin:=(ambient ring di)/di;
    Rlin2 := minimalPresentation(Rlin);
    phi:=Rlin.minimalPresentationMap;
    Rlin1:=target phi;
    di1:=ideal Rlin1;
    bm0:=phi(matrix{bm});
    bm1:=flatten first entries bm0;
    --From here the situation is under the assumption that the variety is not contained in any hyperplane.
    r:=numgens ambient Rlin1;
     if (o.Verbosity >= 2) then print "inverseOfMapRees: About to compute the Jacobian dual matrix";
    local barJD;
    if phi1#cache#?jacobianDualMatrix then (
        barJD = phi1#cache#jacobianDualMatrix;
    )
    else (
        barJD=jacobianDualMatrix(phi1,AssumeDominant=>true, Strategy=>o.Strategy);
    );
    --JacobianDual Matrix is another function in this package
     if (o.Verbosity >= 2) then print "inverseOfMapRees: Computed Jacobian dual matrix";
    --print "JD computed";
    jdd:=(numgens ambient Rlin1)-1;
    if (o.CheckBirational== true) then (
        if (o.QuickRank) then (
            if not (isRankAtLeast(jdd, barJD, Strategy => StrategyDefault, MaxMinors=>2, Verbose=>(o.Verbosity >= 2))) then error "inverseOfMapRees: The map is not birational onto its image";
        )
        else (
            if not (rank barJD >= jdd) then error "inverseOfMapRees: The map is not birational onto its image";
        )
    );
    Inv:={};
     psi:=null;
     Col:={};
     SbarJD:=null;
     nc:=numColumns(transpose barJD);
     nr:=numRows(transpose barJD);
    if (o.Verbosity >= 2 ) then(
        print ( "Jacobian dual matrix has  " |nc|" columns  and about  "|nr|" rows.");
    );
    nonZMinor := null;
    if (o.MinorsLimit > 0) then (
        if (o.Verbosity >= 1) then print ("inverseOfMapRees: Looking for a nonzero minor. \r\n       If this fails, you may increase the attempts with MinorsLimit => #");
        --nonZMinor = getSubmatrixOfRank(jdd, barJD, MaxMinors => o.MinorsLimit, Verbosity=>o.Verbosity);
        nonZMinor = getSubmatrixOfRank(jdd, barJD, Strategy=>LexSmallest, MaxMinors => 1, Verbose=>(o.Verbosity >= 2));
        if (nonZMinor === null) then nonZMinor = getSubmatrixOfRank(jdd, barJD, Strategy=>GRevLexSmallest, MaxMinors => 1, Verbose=>(o.Verbosity >= 2));
        if (nonZMinor === null) then nonZMinor = getSubmatrixOfRank(jdd, barJD, Strategy=>GRevLexSmallestTerm, MaxMinors => 1, Verbose=>(o.Verbosity >= 2));
        if (nonZMinor === null) and (o.MinorsLimit > 3) then nonZMinor = getSubmatrixOfRank(jdd, barJD, MaxMinors => o.MinorsLimit-3, Verbose=>(o.Verbosity >= 2));        
        --nonZeroMinor(barJD,jdd,o.MinorsLimit, Verbosity=>o.Verbosity);
    );
    if (nonZMinor === null) then (
        if (o.Verbosity >= 2) then (
            if (o.MinorsLimit > 0) then print "inverseOfMapRees: Failed to find a nonzero minor.  We now compute syzygies instead.";
            if (o.MinorsLimit == 0) then print "inverseOfMapRees: MinorsLimit => 0, so we now compute syzygies instead.";
            print "                   If this doesn't terminate quickly, you may want to try increasing the option MinorsLimit.";
        );
        Inv =syz(transpose barJD,SyzygyLimit =>1);
        psi = map(source f, Rlin1, sub(transpose Inv, source f));
    )
    else (
        if (o.Verbosity >= 1) then print "inverseOfMapRees: We found a nonzero minor.  If this doesn't terminate quickly, rerun with MinorsLimit=>0.";
        Col = (nonZMinor)#1;
        SbarJD=submatrix(barJD,,Col);
        for i from 0 to jdd do Inv=append(Inv,(-1)^i*det(submatrix'(SbarJD,{i},)));
        psi=map(source f,Rlin1,matrix{Inv});
    );
       psi*phi
);

--**********************************
--inverse of map using simis algebra
--**********************************


inverseOfMapSimis(RationalMapping) :=o->(phi1)->(
   -- invList := inverseOfMap(target f, source f, first entries matrix f);
--    map(source f, target f, invList)
--    inverseOfMap(target f, source f, first entries matrix f, AssumeDominant=>o.AssumeDominant)
---*******************    
    f := map phi1;
    if (o.Verbosity >= 2) then print "Starting inverseOfMapSimis(SimisStrategy or HybridStrategy)";
    if ((o.CheckBirational == true) and (o.HybridLimit == infinity)) then print "Warning:  when using the current default SimisStrategy, the map must be birational.  If the map is not birational, this function will never terminate.";

    if (o.AssumeDominant == false) then (
        if (o.Verbosity >= 1) then (
            print "inverseOfMapSimis: About to find the image of the map.  If you know the image, ";
            print "        you may want to use the AssumeDominant option if this is slow.";
        );
        f = mapOntoImage(f);
        if (o.Verbosity >= 2) then print "inverseOfMapSimis: Found the image of the map.";
    );
    di := ideal target f; -- the defining ideal of the source variety
    im := ideal source f; -- the defining ideal of the target variety
    bm := first entries matrix f;     --the list defining the map from the source to target variety
    if isSameDegree(bm)==false then error "inverseOfMapSimis: Expected a list of homogeneous elements of the same degree";
    R:=ring di;
    K:=coefficientRing R;
    S:=ring im;
    im1 := im;

    --In the following lines we remove the linear parts of the ideal di and
--modify our map bm
    Rlin:=target f;
    Rlin2 := minimalPresentation(Rlin);
    phi:=Rlin.minimalPresentationMap;
    Rlin1:=target phi;
    di1:=ideal Rlin1;
    bm0:=phi(matrix{bm});
    bm1:=flatten first entries bm0;
    --From here the situation is under the assumption that the variety is not contained in any hyperplane.
    r:=numgens ambient Rlin1;
    jdd:=(numgens ambient Rlin1)-1;
    --THe following is a part of simisAlgebra
    rs:=length  bm1;
    SS:=ring di1;
    LL:=apply(bm1,uu->sub(uu, SS));
    n:=numgens ambient  SS;
    Kf:=coefficientRing SS;
    yyy:=local yyy;
    ttt:=local ttt;
    if rs!=0 then (d:=max(apply(bm1,zz->degree zz)));--the degree of the elements of linear sys
    degList := {{(-d_0),1}} | toList(n:{1,0}) | toList(rs:{0,1});
    mymon:=monoid[({ttt}|gens ambient SS|toList(yyy_0..yyy_(rs-1))),  Degrees=>degList, MonomialOrder=>Eliminate 1];
    tR:=Kf(mymon);  --ambient ring of Rees algebra with weights
    f1:=map(tR,SS,submatrix(vars tR,{1..n}));
    F:=f1(matrix{LL});
    myt:=(gens tR)#0;
    J:=sub(di1,tR)+ideal apply(1..rs,j->(gens tR)_(n+j)-myt*F_(0,(j-1)));

    flag := false;
    giveUp := false;
    secdeg:=1;
    jj := 1;
    M := null;
    while (flag == false) do (
--  Jr:= simisAlgebra(di1,bm1,secdeg);
 --THe following is substituting simisAlgebra, we don't call that because we want to save the stored groebner basis
        if (o.Verbosity >= 2) then print("inverseOfMapSimis:  About to compute partial Groebner basis of Rees ideal up to degree " | toString({1, secdeg}) | "." );
        if (secdeg < o.HybridLimit) then (
            M=gb(J,DegreeLimit=>{1,secdeg}); --instead of computing the whole Grob.
                                               --Basis of J we only compute the parts of degree (1,m) or less,
        )
        else( --we are running the hybrid strategy, so we compute the whole gb
            if (o.Verbosity >= 1) then print("inverseOfMapSimis:  We give up.  Using the previous computations, we compute the whole \r\n        Groebner basis of the Rees ideal.  Increase HybridLimit and rerun to avoid this." );
            M=gb(J); -- probably this should be DegreeLimit=>{1,infinity}, not sure if that works or not
            giveUp = true;
        );
        gM:=selectInSubring(1,gens M);
        L2:=ideal mingens ideal gM;
        W:=local W;
        nextmon:=monoid[(gens ambient  SS|toList(W_0..W_(rs-1))), Degrees=>{n:{1,0},rs:{0,1}}];
        RR:=Kf(nextmon);
        g1:=map(RR,tR,0|vars RR);
        Jr:= g1(L2);
        JD:=diff(transpose ((vars ambient ring Jr)_{0..(r-1)}) ,gens Jr);
        vS:=gens ambient S;
        g:=map(source f,ring Jr, toList(apply(0..r-1,z->0))|vS);
        barJD:=g(JD);
        if (o.Verbosity >= 2) then print("inverseOfMapSimis: computed barJD.");

        if (giveUp == false) then(            
            --if (rank barJD >= jdd) then (
            if (o.QuickRank == true) then (
                if (o.Verbosity >= 1) then print("inverseOfMapSimis: About to check rank, if this is very slow, you may want to try turning QuickRank=>false." );
                if (isRankAtLeast(jdd, barJD, MaxMinors=>1, Strategy=>LexSmallest) or 
                    isRankAtLeast(jdd, barJD, MaxMinors=>1, Strategy=>GRevLexSmallest) or 
                    isRankAtLeast(jdd, barJD, MaxMinors=>1, Strategy=>GRevLexSmallestTerm) ) then (
                    if (o.Verbosity >= 1) then print("inverseOfMapSimis: rank found, we computed enough of the Groebner basis." );
                    flag = true;
                );
            )
            else (
                if (rank barJD >= jdd) then (
                    flag = true;
                );
            );
        )
        else (
            flag = true;
            if (o.CheckBirational == true) then (
                if (o.QuickRank) then (
                    if (not isRankAtLeast(jdd, barJD, Strategy => StrategyDefault, MaxMinors=>min(2, o.MinorsLimit), Verbose=>(o.Verbosity >= 2))) then error "inverseOfMapSimis: The map is not birational onto its image";
                )
                else(
                    if (not (rank barJD >= jdd)) then error "inverseOfMapSimis: The map is not birational onto its image";
                )
            );
        );
        secdeg=secdeg + jj;
        jj = jj + 1; --we are basically growing secdeg in a quadratic way now, but we could grow it faster or slower...
                    --maybe the user should control it with an option
    );
    Inv:={};
    psi:=null;
    Col:={};
    SbarJD:=null;
    nc:=numColumns(transpose barJD);
    nr:=numRows(transpose barJD);
    if (o.Verbosity >= 2 ) then(
        print ( "inverseOfMapSimis: Found Jacobian dual matrix (or a weak form of it), it has  " |nc|" columns  and about  "|nr|" rows.");
    );

    nonZMinor := null;
    if (o.MinorsLimit > 0) then (
        if (o.Verbosity >= 1) then print "inverseOfMapSimis: Looking for a nonzero minor.\r\n        If this fails, you may increase the attempts with MinorsLimit => #";
        nonZMinor = getSubmatrixOfRank(jdd, barJD, Strategy=>LexSmallest, MaxMinors => 1, Verbose=>(o.Verbosity >= 2));
        if (nonZMinor === null) then nonZMinor = getSubmatrixOfRank(jdd, barJD, Strategy=>GRevLexSmallest, MaxMinors => 1, Verbose=>(o.Verbosity >= 2));
        if (nonZMinor === null) then nonZMinor = getSubmatrixOfRank(jdd, barJD, Strategy=>GRevLexSmallestTerm, MaxMinors => 1, Verbose=>(o.Verbosity >= 2));
        if (nonZMinor === null) and (o.MinorsLimit > 3) then nonZMinor = getSubmatrixOfRank(jdd, barJD, MaxMinors => o.MinorsLimit-3, Verbose=>(o.Verbosity >= 2));        
        --nonZeroMinor(barJD,jdd,o.MinorsLimit, Verbosity=>o.Verbosity);
    );

    if (nonZMinor === null) then (
        if (o.Verbosity >= 2) then (
            if (o.MinorsLimit >  0) then print "inverseOfMapSimis: Failed to find a nonzero minor.  We now compute syzygies instead.";
            if (o.MinorsLimit == 0) then print "inverseOfMapSimis: MinorsLimit => 0, so we now compute syzygies instead.";
            print "                   If this doesn't terminate quickly, you may want to try increasing the option MinorsLimit.";
        );
        Inv =syz(transpose barJD,SyzygyLimit =>1);
        psi = map(source f, Rlin1, sub(transpose Inv, source f));
    )
    else (
        if (o.Verbosity >= 1) then print "inverseOfMapSimis: We found a nonzero minor.";
        Col = (nonZMinor)#1;
        SbarJD=submatrix(barJD,,Col);        
        for i from 0 to jdd do Inv=append(Inv,(-1)^i*det(submatrix'(SbarJD,{i},)));
        psi=map(source f,Rlin1,matrix{Inv});
    );
    psi*phi
);

-*
inverseOfMapSimis(Ideal,Ideal,BasicList) :=o->(di,im,bm)->(
    inverseOfMapSimis( (ring di)/di, (ring im)/im, bm, AssumeDominant=>o.AssumeDominant, CheckBirational=>o.CheckBirational,Verbosity=>o.Verbosity, MinorsLimit=>o.MinorsLimit)
);
*-

inverseOfMapSimis(Ring,Ring,BasicList) := o->(R1, S1, bm)->(
    inverseOfMapSimis(map(R1, S1, bm), AssumeDominant=>o.AssumeDominant, CheckBirational=>o.CheckBirational,Verbosity=>o.Verbosity, MinorsLimit=>o.MinorsLimit)
    );


--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mapOntoImage = method(Options=>{QuickRank=>true}); --given a map f : X -> Y, this creates the map f : X -> f(X).

mapOntoImage(RingMap) := o -> (f)->(
    S1 := ambient source f;
    R1 := source f;
    I1 := ideal source f;
    local kk;
    JJ := idealOfImageOfMap(f, QuickRank=>o.QuickRank);
    if ( JJ == ideal(sub(0, R1)) ) then (
        return f;
    )
    else (
        kk = sub(JJ, S1) + I1;
    );
--        newMap := map(target f, ambient source f, matrix f);        
    map(target f, (S1)/kk, matrix f)
);

mapOntoImage(RationalMapping) := o -> (phi) -> (
    rationalMapping mapOntoImage(map phi, o)
)

--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isEmbedding = method(Options => {AssumeDominant=>false, Strategy=>HybridStrategy,
	 HybridLimit=>15, Verbosity=>1, MinorsLimit=>0, QuickRank=>true});
 --checks whether a map is a closed embedding.

 isEmbedding(RationalMapping) := o -> (phi1) -> (
     isEmbedding(map phi1, o)
 )

isEmbedding(RingMap):= o-> (f1)->(
    f2:=null;
    if (o.AssumeDominant==false) then(
        if (o.Verbosity >= 1) then (
            print "isEmbedding: About to find the image of the map.  If you know the image, ";
            print "        you may want to use the AssumeDominant option if this is slow.";
        );        
        f2 = mapOntoImage(f1);
	)
    else (
	    f2=f1;
	);
    if (o.Verbosity >= 2) then print "isEmbedding: Checking to see whether the map is a regular map";
        flag := isRegularMap(f2, Verbosity => o.Verbosity);
        if (flag == true) then (
	        if (o.Verbosity >= 2) then (print "isEmbedding: computing the inverse  map");

            try(h := (inverseOfMap(f2, AssumeDominant=>true, CheckBirational=>true, QuickRank=>o.QuickRank, Strategy=>o.Strategy,HybridLimit=>o.HybridLimit, Verbosity=>o.Verbosity, MinorsLimit=>o.MinorsLimit))#map; ) then
            (
	            if (o.Verbosity >= 2) then print "isEmbedding: checking whether the inverse map is a regular map";
        	    flag = isRegularMap(h, Verbosity=>o.Verbosity);
	        )
            else(
	            flag=false
	        );
       );
       flag
);

--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 isSameMap = method(); --checks whether two rational maps are the same. Assumes domain is irreducible
 isSameMapInternal = method();

 isSameMapInternal(List, List, Ring) := (L1, L2, R1) -> (
    rank matrix(frac(R1), {L1, L2}) == 1
 );

 isSameMap(RationalMapping, RationalMapping) := (phi, psi) -> (
     isSameMap(map phi, map psi)
 );


 isSameMap(RingMap, RingMap) := (f1, f2) -> (
    if (not (target f1 === target f2)) then (
        error "isSameMap: The ring maps should have the same target.";
    );
    if (not (source f1 === source f2)) then (
        error "isSameMap: The ring maps should have the same source.";
    );
    theRing := target f1;
--    rank matrix(frac(theRing), entries ((matrix f1) || (matrix f2))) == 1
    isSameMapInternal(first entries matrix f1, first entries matrix f2, theRing)
--    isSameMapToPn( first entries matrix f1, first entries matrix f2)
 );



--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 jacobianDualMatrix = method(Options => {AssumeDominant=>false, Strategy=>ReesStrategy, QuickRank=>true});
 internalJacobianDualMatrix = method(Options => {AssumeDominant=>false, Strategy=>ReesStrategy, QuickRank=>true});
--this the jacobian dual matrix of  a  rational map X -> Y.

--X = Proj R
--Y = Proj S
--This map is given by a list of elements in R, all homogeneous
--of the same degree.
--Below we have defining ideal of X = di
--defining ideal of Y = im
--list of elements = bm

internalJacobianDualMatrix(Ideal,Ideal,BasicList) :=o->(di,im,bm)->(
    if isSameDegree(bm)==false then error "jacobianDualMatrix: Expected a list of homogeneous elements of the same degree";
    R:=ring di;
    K:=coefficientRing R;
    S:=ring im;
    im1 := im;
    if (o.AssumeDominant == true) then (
        im1 =  im;
    )
    else (
        im1 = idealOfImageOfMap( map( (ring di)/di, ring im, bm), QuickRank=>o.QuickRank);
    );
    --In the following lines we remove the linear parts of the ideal di and
--modify our map bm
    Rlin:=(ambient ring di)/di;
    Rlin2 := minimalPresentation(Rlin);
    phi:=Rlin.minimalPresentationMap;
    Rlin1:=target phi;
    di1:=ideal Rlin1;
    bm0:=phi(matrix{bm});
    bm1:=flatten first entries bm0;
    --From here the situation is under the assumption that the variety is not contained in any hyperplane.
    r:=numgens ambient Rlin1;
    Jr:= blowUpIdeals(di1,bm1, Strategy=>o.Strategy);
    n:=numgens Jr;
    L:={};
    for i from 0 to (n-1) do(
	 if  (degree Jr_i)_0==1 then  L=append(L, Jr_i);
    );
   JD:=diff(transpose ((vars ambient ring Jr)_{0..(r-1)}) ,gens ideal L);
   vS:=gens ambient S;
   g:=map(S/im1,ring Jr, toList(apply(0..r-1,z->0))|vS);
   barJD:=g(JD);
   barJD
   );


jacobianDualMatrix(RingMap) := o->(f)->(
    internalJacobianDualMatrix(ideal target f, ideal source f, first entries matrix f, o)
);

jacobianDualMatrix(RationalMapping) := o->(phi)->(
    if (phi#cache#?jacobianDualMatrix) then return phi#cache#jacobianDualMatrix;
    myDual := jacobianDualMatrix(map phi, o);
    phi#cache#jacobianDualMatrix = myDual;
    myDual
);


--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--SourceInversionFactor  is an invariant associated to a rational map which is useful in computation of symbolic powers
sourceInversionFactor=method(Options => {AssumeDominant=>false, CheckBirational=>false, Strategy=>HybridStrategy,
	 HybridLimit=>15, Verbosity=>1, MinorsLimit=>0, QuickRank=>true});

sourceInversionFactor(RingMap):=o->(f)->(
    R:=  target f;
    x:=R_0;
    f2:=f;
    if (o.AssumeDominant==false) then(
    if (o.Verbosity >= 1) then (
        print "sourceInversionFactor: About to find the image of the map.  If you know the image, ";
        print "        you may want to use the AssumeDominant option if this is slow.";
    );
        f2 = mapOntoImage(f);
    );

    invf2:=(inverseOfMap(f2, AssumeDominant=>o.AssumeDominant, CheckBirational=>o.CheckBirational, Strategy=>o.Strategy,
    HybridLimit=>o.HybridLimit, Verbosity=>o.Verbosity, MinorsLimit=>o.MinorsLimit, QuickRank=>o.QuickRank))#map;
    I:=ideal(matrix((f2)*invf2));
    s:=quotient(ideal(I_0),ideal(x));
    s_0
);








--****************************************************--
--*****************Documentation**********************--
--****************************************************--
--needsPackage "Parametrization";
--needsPackage "Cremona";

beginDocumentation();

document {
    Key => RationalMaps,
    Headline => "rational maps between projective varieties",
    EM "RationalMaps", " is a package for computing things related to maps between projective varieties.",
    BR{},BR{},
    "It focuses on finding where a birational map is undefined, checking whether a map is a closed embedding, checking birationality and computing inverse maps",
    BR{},BR{},
    BOLD "Mathematical background:",BR{},
    UL {
	  {"A. V. Dória, S. H. Hassanzadeh, A. Simis,  ",EM "  A characteristic free criterion of birationality", ", Advances in Mathematics, Volume 230, Issue 1, 1 May 2012, Pages 390-413."},
	  {"A. Simis, ",EM "  Cremona Transformations and some Related Algebras", ", Journal of Algebra, Volume 280, Issue 1, 1 October 2004, Pages 162--179"},
	},
    BOLD "Functionality overlap with other packages:\n\n",BR{},BR{},
    EM  TO "Parametrization",
      ":  While the package ", TO "Parametrization", " focuses mostly on curves, it also includes a function ", TO "Parametrization::invertBirationalMap", "
      that has the same functionality as ", TO "inverseOfMap", ".  On the other hand, these two functions were implemented differently and so sometimes one function can be substantially faster than the other.\n", BR{}, BR{},
    EM TO "Cremona::Cremona",
    ":  The package ", TO "Cremona::Cremona", " focuses on  fast probabilistic computations in general cases and  deterministic computations for special
     kinds of maps from projective space.  More precisely, ",BR{},
    UL {
        {TO "Cremona::isBirational", " gives a probabilistic answer to the question of whether a map between varieties is birational.  Furthermore, if the
	     source is projective space, then ", TT "degreeOfRationalMap", " with ", TT   "MathMode=>true", " gives a deterministic correct answer.
	      In some cases, the speed of the latter  is comparable with ", TO "isBirationalMap", " with ", TT   "AssumeDominant=>true." },
        {TO "Cremona::inverseMap", " gives a  fast computation of the inverse of a birational map if the source is projective space ", EM " and ",
	     "the map has maximal linear rank.   In some cases, even if the map has maximal linear rank, our function ", TO "inverseOfMap",
	       " appears to be competitive however.  If you pass inverseMap a map not from projective space, then it calls a modified and improved version of ",
	      TO "Parametrization::invertBirationalMap", " from ", TO "Parametrization", "."},
    },
}


--***************************************************************

document{
    Key=>{CheckBirational},
    Headline=> "whether to check birationality",
    Usage =>"  CheckBirational=>b",
      "If true, inverseOfMap, isEmbedding and sourceInversionFactor  will check whether the passed map is birational.
      If it is not birational, it will throw an error.",
    SeeAlso =>{
        "inverseOfMap",
        "isEmbedding",
        "sourceInversionFactor"
    }
}
--***************************************************************

document{
    Key=>{HybridLimit},
    Headline=>"an option to control HybridStrategy",
       "This controls behavior when using ", TT "Strategy=>HybridStrategy", ".  ", "By increasing the HybridLimit value (default 15), 
       HybridStrategy will execute SimisStrategy longer. 
	     Infinity will behave exactly like SimisStrategy.",
    SeeAlso=>{
        "HybridStrategy"        
    }
}
--***************************************************************

--***************************************************************

document{
    Key=>{MinorsLimit},
    Headline=>"an option to limit the number of random minors computed",
            "One of the ways to invert a map is to find a nonzero minor of a variant of the jacobianDualMatrix.
	     This function controls how many minors (heuristically chosen via ", TO "FastMinors", ") to check before switching to another strategy (involving computing a syzygy).
	     Setting it to zero will mean no minors are checked.
	     If it is left as null (the default), these functions will determine a value using a heuristic that depends on the varieties involved.",
    SeeAlso=>
        inverseOfMap
}

document{
    Key=>{QuickRank},
    Headline=>" an option for controlling how rank is computed",
            "If set to true, then checking whether rank is at least a certain number will be computed via the package ", TO "FastMinors", ".",
    SeeAlso=>
        inverseOfMap
}

--***************************************************************
document{
    Key=>{ SaturateOutput},
    Headline =>"whether the value returned should be saturated",
    Usage =>"SaturateOutput=>b",
    "  If ", TT "SaturateOutput"," is ", TT "true"," (the default), then functions will saturate their output.
    Otherwise they will not.  It may be beneficial not to saturate in certain circumstances as saturation may slow computation.",
}
--***************************************************************
document{
    Key=>{ AssumeDominant},
    Headline =>"whether to assume a rational map between projective varieties is dominant",
    Usage =>"AssumeDominant=>b",
    "  If ", TT "AssumeDominant"," is ", TT "true",", it can speed up computation as a kernel will not be computed.",
}

--***************************************************************

doc ///
    Key
        HybridStrategy
    Headline
        A strategy for determining whether a map is birational and computing its inverse
    Description
    	Text
            HybridStrategy is a valid value for the Strategy Option for inverseOfMap, isBirationalMap, and isEmbedding.
	    This is currently the default strategy.  It is a combination of ReesStrategy and SimisStrategy.
	    Increasing the HybridLimit value (default 15) will force SimisStrategy to be executed longer.
    SeeAlso
        ReesStrategy
        SaturationStrategy
        SimisStrategy
        HybridLimit
///

--***************************************************************

--relationType used to be valid
doc ///
    Key
        ReesStrategy
    Headline
        a strategy for determining whether a map is birational and computing its inverse 
    Description
    	Text
            ReesStrategy is a valid value for the Strategy Option for inverseOfMap, isBirationalMap, and isEmbedding. By choosing Strategy=>ReesStrategy, the ideal of
            definition of the Rees algebra is computed by the known elimination technique. 
            This technique is described in Proposition 1.5 on page 21 in the book
        Text
            @UL{
                {"Vasconcelos, Wolmer.  ", BOLD "Integral closure.", " Springer Monographs in Mathematics. ", EM "Springer-Verlag, Berlin,", "2005. xii+519 pp."}
            }@
    SeeAlso
        SaturationStrategy
        SimisStrategy
        HybridStrategy

///
--***************************************************************
--relationType used to be documented here
doc ///
    Key
        SaturationStrategy
    Headline
        a strategy for determining whether a map is birational and computing its inverse
    Description
    	Text
            SaturationStrategy is a valid value for the Strategy Option for inverseOfMap, isBirationalMap, and isEmbedding. By choosing Strategy=>SaturationStrategy,
	        the equations of the ideal of definition of the Rees algebra are generated by saturating the ideal of definition of the symmetric algebra into a non-zero element.
	        Notice that in this package (and in particular, in this Strategy option) the rings are assumed to be integral domains.
	        This Strategy appears to be slower in some examples.
    SeeAlso
        ReesStrategy
        SimisStrategy
        HybridStrategy
///
--***************************************************************

doc ///
    Key
        SimisStrategy
    Headline
        a strategy for determining whether a map is birational and computing its inverse
    Description
    	Text
            SimisStrategy is a valid value for the Strategy Option of inverseOfMap, isBirationalMap, and isEmbedding. Considering the bigraded structure of the
            equations of the ideal of definition of a Rees algebra, SimisStrategy looks for all Gröbner bases where the degree is (1, n) for some natural number n. The advantage
            of this restriction is that this part of the Rees ideal is enough to decide birationality and to compute the inverse map; this strategy reduces 
            computation time. A disadvantage of this Strategy is that if the given map is not birational this Strategy may never
            end because the jacobianDualMatrix will not attain its maximum rank. To circumvent this problem we implemented HybridStrategy.
    SeeAlso
        ReesStrategy
        SaturationStrategy
        HybridStrategy
///

--***************************************************************

doc ///
    Key
        RationalMapping
        rationalMapping
        (rationalMapping, RingMap)
        (rationalMapping, Ring, Ring, BasicList)
        (rationalMapping, Ring, Ring, Matrix)
        (rationalMapping, ProjectiveVariety, ProjectiveVariety, BasicList)
        (rationalMapping, ProjectiveVariety, ProjectiveVariety, Matrix)
    Headline
        a rational mapping between projective varieties
    Usage        
        phi = rationalMapping(f)        
        phi = rationalMapping(targetRing, sourceRing, l)        
        phi = rationalMapping(targetRing, sourceRing, m)
        phi = rationalMapping(targetVariety, sourceVariety, l)
        phi = rationalMapping(targetVariety, sourceVariety, m)
    Inputs
        f:RingMap
            a ring map corresponding to the rational map between varieties
        targetRing:Ring
            the ring corresponding to the source variety
        sourceRing:Ring
            the ring corresponding to the target variety
        targetVariety:ProjectiveVariety
            the target variety
        sourceVariety:ProjectiveVariety
            the source variety
        l:BasicList
            the list of elements describing the map
        m:Matrix
            the matrix describing the map
    Description
        Text
            A {\tt RationalMapping} is a Type that is used to treat maps between projective varieties  geometrically.  It stores essentially equivalent data to the corresponding map between the homogeneous coordinate rings.  The way to construct the object is to use the function {\tt rationalMapping}.  
        Text             
            For example, the following is a Cremona transformation on $P^2$ constructed in multiple ways (in this case, the entries describing the map all have degree 2).
        Example
            R = QQ[x,y,z]
            P2 = Proj(R)
            phi1 = rationalMapping(P2, P2, {y*z,x*z,x*y})
            phi2 = rationalMapping(R, R, matrix{{y*z,x*z,x*y}})
            phi3 = rationalMapping(map(R, R, {y*z,x*z,x*y}))
        Text
            The source and target can also be different.  For example, consider the following map from $P^1$ to a nodal cubic in $P^2$.
        Example
            S = QQ[x,y,z];
            P2 = Proj S;
            R = QQ[a,b];
            P1 = Proj R;
            phi = rationalMapping(P2, P1, {b*a*(a-b), a^2*(a-b), b^3})  
            h = map(R, S, {b*a*(a-b), a^2*(a-b), b^3})
            psi = rationalMapping h
            phi == psi
        Text
            Notice that when defining a map between projective varieties, we keep the target then source input convention.
        Text
            Warning, the list or matrix describing the map needs every entry to have the same degree. 
    SeeAlso
        (symbol *, RationalMapping, RationalMapping)
        (symbol ==, RationalMapping, RationalMapping)
///

doc ///
    Key
        (symbol *, RationalMapping, RationalMapping)
        (symbol ^, RationalMapping, ZZ)
    Headline
        compose rational maps between projective varieties
    Description
        Text
            This allows one to compose two rational maps between projective varieties.  
        Example
            R = QQ[x,y,z]
            P2 = Proj(R)
            phi = rationalMapping (P2, P2, {y*z,x*z,x*y})
            ident = rationalMapping (P2, P2, {x,y,z})
            phi*phi == ident        
        Text
            Raising a map to the negative first power means computing the inverse birational map.  Raising a map to the first power simply returns the map itself.  In the next example we compute the blowup of a point on $P^2$ and its inverse.
        Example
            P5ring = ZZ/103[a..f];
            R = ZZ/103[x,y,z];        
            P2 = Proj R;
            identP2 = rationalMapping(P2, P2, {x,y,z});
            M = matrix{{a,b,c},{d,e,f}};
            blowUp = Proj(P5ring/(minors(2, M)+ideal(b - d)));
            identBlowUp = rationalMapping(blowUp, blowUp, {a,b,c,d,e,f});
            tau = rationalMapping(P2, blowUp,{a, b, c});
            tauInverse = tau^-1;
            tau*tauInverse == identP2 --a map composed with its inverse is the identity
            tauInverse*tau == identBlowUp
        Text
            Note that one can only raise maps to powers (with the exception of 1 and -1) if the source and target are the same.  In that case, raising a map to a negative power means compose the inverse of a map with itself.  We illustrate this with the quadratic transformation on $P^2$ that we started with (an transformation of order 2 in the Cremona group).
        Example
            phi^3 == phi^-1 
            phi^-2 == ident
            phi^1 == ident
    SeeAlso
        isSameMap
///

doc ///
    Key
        (map, RationalMapping)
    Headline    
        the ring map associated to a RationalMapping between projective varieties
    Description
        Text
            Given a {\tt RationalMapping} between projective varieties, this returns the associated map between projective varieties.
///

doc ///
    Key
        (source, RationalMapping)
        (target, RationalMapping)
    Headline
        returns the source or target of a RationalMapping between projective varieties.
    Description
        Text
            Given a {\tt RationalMapping} between projective varieties these functions can be used to return the source or target.  
        Example
            R = QQ[a,b];
            S = QQ[x,y,z];
            P2 = Proj R;
            P3 = Proj S;
            f = map(R, S, {a,b,0});
            phi = rationalMapping f;
            source phi
            target phi
            source f
            target f
        Text
            Note that the source of phi corresponds to the target of f and the target of phi corresponds to the source of f.
///

--***************************************************************



doc ///
    Key
        isBirationalMap
        (isBirationalMap, RingMap)
        (isBirationalMap, RationalMapping)
        [isBirationalMap, AssumeDominant]
        [isBirationalMap, Strategy]
	    [isBirationalMap,Verbosity]
	    [isBirationalMap,HybridLimit]
        [isBirationalMap,MinorsLimit]
        [isBirationalMap, QuickRank]
    Headline
        whether a map between projective varieties is birational
    Usage        
        val = isBirationalMap(Pi)        
        val = isBirationalMap(phi)        
    Inputs        
        Pi:RingMap
            a ring map S to R corresponding to X mapping to Y    
        phi:RationalMapping
            a rational map between projective varieties X to Y
        Verbosity => ZZ
            if 0 then silence the function, if 1 then generate informative output which can be used to adjust strategies, if > 1 then generate a detailed description of the execution
        AssumeDominant => Boolean
            whether to assume the provided rational map of projective varieties is dominant, if set to true it can speed up computation
        Strategy=>Symbol
            choose the strategy to use: HybridStrategy, SimisStrategy, or ReesStrategy
        HybridLimit => ZZ
            within HybridStrategy, within HybridStrategy, the option HybridLimit controls how often SimisStrategy and ReesStrategy are used
        MinorsLimit => ZZ
            how many submatrices of a variant of the Jacobian dual matrix to consider before switching to a different strategy       
        QuickRank => Boolean
            whether to compute rank via the package @TO2(FastMinors, "FastMinors")@
    Outputs
        val:Boolean
            true if the map is birational, false if otherwise
    Description
        Text
            The function {\tt isBirationalMap} computes whether a map between projective varieties is birational.   The option {\tt AssumeDominant} being true will cause the function to assume that the kernel of the associated ring map is zero (default value is false).  The target and source must be varieties; their defining ideals must be prime.  Let's check that the plane quadratic Cremona transformation is birational.
        Example
            R=QQ[x,y,z];
            S=QQ[a,b,c];
            Pi = map(R, S, {x*y, x*z, y*z});
            isBirationalMap(Pi, Verbosity=>0, Strategy=>SimisStrategy )
        Text
            We can also verify that a cover of $P^1$ by an elliptic curve is not birational.
        Example
            R=QQ[x,y,z]/(x^3+y^3-z^3);
            S=QQ[s,t];
            Pi = map(R, S, {x, y-z});
            isBirationalMap(Pi, Verbosity=>0)
        Text
            Note that the Frobenius map is not birational.
        Example
            R = ZZ/5[x,y,z]/(x^3+y^3-z^3);
            S = ZZ/5[a,b,c]/(a^3+b^3-b^3);
            h = map(R, S, {x^5, y^5, z^5});
            isBirationalMap(h, Strategy=>SaturationStrategy)
    SeeAlso
        isBirationalOntoImage
        HybridStrategy
        SimisStrategy
        ReesStrategy
    Caveat
        Also see the very fast probabilistic birationality checking of the @TO "Cremona::Cremona"@ package: @TO "Cremona::isBirational"@.
///
--***************************************************************

doc ///
        Key
            isBirationalOntoImage
            (isBirationalOntoImage, RingMap)        
            (isBirationalOntoImage, RationalMapping)
            [isBirationalOntoImage,Verbosity]
            [isBirationalOntoImage, AssumeDominant]
            [isBirationalOntoImage, Strategy]
            [isBirationalOntoImage, HybridLimit]
            [isBirationalOntoImage, MinorsLimit]
            [isBirationalOntoImage, QuickRank]
        Headline
                whether a map between projective varieties is birational onto its image
        Usage
                val = isBirationalOntoImage(Pi)
                val = isBirationalOntoImage(phi)
        Inputs
                Pi:RingMap
                        A ring map S to R corresponding to a rational map between projective varieties
                phi:RationalMapping
                        A rational map between projective varieties
                Verbosity => ZZ
                    if 0 then silence the function, if 1 then generate informative output which can be used to adjust strategies, if > 1 then generate a detailed description of the execution
                AssumeDominant => Boolean
                    whether to assume the provided rational map of projective varieties is dominant, if true it can speed up computation as a kernel will not be computed                  
                Strategy=>Symbol
                    choose the strategy to use: HybridStrategy, SimisStrategy, or ReesStrategy  
                HybridLimit => ZZ
                    within HybridStrategy, the option HybridLimit controls how often SimisStrategy and ReesStrategy are used, larger numbers means SimisStrategy will be executed longer
                MinorsLimit => ZZ
                    how many submatrices of a variant of the Jacobian dual matrix to consider before switching to a different strategy
                QuickRank => Boolean
                    whether to compute rank via the package @TO2(FastMinors, "FastMinors")@
        Outputs
                val:Boolean
                        true if the map is birational onto its image, false if otherwise
        Description
            Text
                The function {\tt isBirationalOntoImage} computes whether $f : X \to Y$ is birational onto its image.  It is essentially a combination of {\tt mapOntoImage} with {\tt isBirationalOntoImage}.  Setting option {\tt AssumeDominant} to true will cause the function to assume that the kernel of the associated ring map is zero (default value is false).  The source must be a variety; its defining ideal must be prime.  In the following example, the map is not birational, but it is birational onto its image.
            Example
                R=QQ[x,y];
                S=QQ[a,b,c,d];
                Pi = map(R, S, {x^3, x^2*y, x*y^2, y^3});
                isBirationalOntoImage(Pi, Verbosity=>0)
                isBirationalMap(Pi,  Verbosity=>0)
            Text
                Sub-Hankel matrices (matrices whose ascending skew-diagonal entries are constant) have homaloidal determinants (the associated partial derivatives define a Cremona map).
                For more discussion see:
            Text
                @UL{
                    { "Mostafazadehfard, Maral; Simis, Aron.  Homaloidal determinants.", EM " J. Algebra ", " 450 (2016), 59--101."}
                }@
            Text
                Consider the following example illustrating this.
            Example
                A = QQ[z_0..z_6];
                H=map(A^4,4,(i,j)->A_(i+j));
                SH=sub(H,{z_5=>0,z_6=>0})
                sh=map(A, A, transpose jacobian ideal det SH );
                isBirationalOntoImage(sh, Verbosity=>0)
                B=QQ[t_0..t_4];
                li=map(B,A,matrix{{t_0..t_4,0,0}});
                phi=li*sh;
                isBirationalOntoImage(phi, HybridLimit=>2)
        SeeAlso
            isBirationalMap
///
--***************************************************************


doc ///
    Key
        idealOfImageOfMap        
        (idealOfImageOfMap, RingMap)
        (idealOfImageOfMap, RationalMapping)
        [idealOfImageOfMap, Verbosity]
        [idealOfImageOfMap, QuickRank]
    Headline
        finds defining equations for the image of a rational map between varieties or schemes
    Usage
        im = idealOfImageOfMap(p)
        im = idealOfImageOfMap(phi)
    Inputs
        p:RingMap
            corresponding to a rational map of projective varieties
        phi:RationalMapping
            a rational map between projective varieties
        QuickRank => Boolean
            whether to compute rank via the package @TO2(FastMinors, "FastMinors")@
    Outputs
        im:Ideal
            defining equations for the image
    Description
        Text
            Given a rational map $f : X \to Y \subset P^N$, {\tt idealOfImageOfMap} returns the defining ideal of the image of $f$ in $P^N$. The rings provided implicitly in the inputs should be polynomial rings or quotients of polynomial rings. In particular, {\tt idealOfImageOfMap} function returns an ideal defining a subset of the ambient projective space of the image.  In the following example we consider the image of $P^1$ inside $P^1 \times P^1$.
        Example
            S = QQ[x,y,z,w];
            b = ideal(x*y-z*w);
            R = QQ[u,v];
            a = ideal(sub(0,R));
            f = matrix {{u,0,v,0}};
            phi = rationalMapping(R/a, S/b, f)
            idealOfImageOfMap(phi)
            psi = rationalMapping(Proj(S/b), Proj(R/a), f)
            idealOfImageOfMap(psi)
        Text
            This function frequently just calls @TO2((kernel, RingMap), "ker")@ from Macaulay2.  However, if the target of the ring map is a polynomial ring, then it first tries to verify whether the ring map is injective.  This is done by computing the rank of an appropriate Jacobian matrix.
///
--***************************************************************

doc ///
    Key
        jacobianDualMatrix        
        (jacobianDualMatrix, RingMap)
        (jacobianDualMatrix, RationalMapping)
        [jacobianDualMatrix,AssumeDominant]
        [jacobianDualMatrix,Strategy]
        [jacobianDualMatrix, QuickRank]
    Headline
        computes the Jacobian dual matrix
    Usage
        M = jacobianDualMatrix(p)
        M = jacobianDualMatrix(phi)
    Inputs
        phi:RationalMapping
            a rational map between projective varieties
        p:RingMap
            ring map corresponding to a rational map between projective varieties
        Strategy=>Symbol
                choose the strategy to use: ReesStrategy or SaturationStrategy
        AssumeDominant => Boolean
            whether to assume the provided rational map of projective varieties is dominant, if set to true it can speed up computation
        QuickRank => Boolean
            whether to compute rank via the package @TO2(FastMinors, "FastMinors")@
    Outputs
        M:Matrix
            a matrix $M$ over the coordinate ring of the image, the kernel of $M$
            describes the syzygies of the inverse map, if it exists.
    Description
        Text
            The Jacobian dual matrix is a matrix whose kernel describes the syzygies of the matrix corresponding to the inverse map.
            For more information, see 
        Text
            @UL{
                {"Doria, A. V.; Hassanzadeh, S. H.; Simis, A.", " A characteristic-free criterion of birationality.", EM " Adv. Math.", " 230 (2012), no. 1, 390--413."}
            }@
        Text
            This is mostly an internal function. It is used when checking whether a map is birational and when computing the inverse map.  If the {\tt AssumeDominant} option is set to {\tt true}, it assumes that the kernel of the associated ring map is zero (default value is false).  Valid values for the {\tt Strategy} option are {\tt ReesStrategy} and {\tt SaturationStrategy}.  
        Example
            R=QQ[x,y];
            S=QQ[a,b,c,d];
            Pi = map(R, S, {x^3, x^2*y, x*y^2, y^3});
            jacobianDualMatrix(Pi, Strategy=>SaturationStrategy)
    SeeAlso
        HybridStrategy
        SimisStrategy
        ReesStrategy
///
--***************************************************************
--***************************************************************


doc ///
        Key
                mapOntoImage
                (mapOntoImage, RingMap)
                (mapOntoImage, RationalMapping)
                [mapOntoImage, QuickRank]
        Headline
                the induced map from a variety to the closure of its image under a rational map
        Usage
                h = mapOntoImage(f)        
                psi = mapOntoImage(phi)        
        Inputs                
                f:RingMap
                        the ring map corresponding to a rational map $\phi$ of projective varieties
                phi:RationalMapping
                        a rational map $\phi$ of projective varieties              
                QuickRank => Boolean
                        whether to compute rank via the package @TO2(FastMinors, "FastMinors")@
        Outputs
                h:RingMap
                    the map of rings corresponding to $X \to \overline{\phi(X)}$.
                psi:RationalMapping
                    the rational map 
        Description
                Text
                        Given $f : X \to Y$ {\tt mapOntoImage} returns $X \to \overline{\phi(X)}$.  Alternately, given $f: S \to R$, {\tt mapOntoImage} just returns $S/(kernel f) \to R$.  {\tt mapOntoImage} first computes whether the kernel is $0$ without calling @TO2((kernel, RingMap), "ker")@, which can have speed advantages.
                Example
                        R = QQ[x,y];
                        S = QQ[a,b,c];
                        f = map(R, S, {x^2, x*y, y^2});
                        mapOntoImage(f)	     
                        phi = rationalMapping f
                        mapOntoImage(phi)           
///
--***************************************************************

doc ///
        Key
                isEmbedding
                (isEmbedding, RingMap)
                (isEmbedding, RationalMapping)
                [isEmbedding, AssumeDominant]                
                [isEmbedding, HybridLimit]
                [isEmbedding, Strategy]
                [isEmbedding, MinorsLimit]
                [isEmbedding, Verbosity]
                [isEmbedding, QuickRank]
        Headline
                whether a rational map of projective varieties is a closed embedding
        Usage
                val = isEmbedding(f)
                val = isEmbedding(phi)
        Inputs
                f:RingMap
                    the ring map corresponding to $f : X \to Y$
                phi:RationalMapping
                    a rational map of projective varieties, $f : X \to Y$.
                Verbosity => ZZ
                    if 0 then silence the function, if 1 then generate informative output which can be used to adjust strategies, if > 1 then generate a detailed description of the execution
                AssumeDominant => Boolean
                    whether to assume the provided rational map of projective varieties is dominant, if set to true it can speed up computation               
                Strategy=>Symbol
                    choose the strategy to use: HybridStrategy, SimisStrategy, or ReesStrategy
                HybridLimit => ZZ
                    within HybridStrategy, the option HybridLimit controls how often SimisStrategy and ReesStrategy are used,   larger numbers means SimisStrategy will be executed longer
                MinorsLimit => ZZ
                    how many submatrices of a variant of the Jacobian dual matrix to consider before switching to a different strategy                
                QuickRank => Boolean
                    whether to compute rank via the package @TO2(FastMinors, "FastMinors")@
        Outputs
                val:Boolean
                    true if the map is an embedding, otherwise false.
        Description
                Text
                        Given a map of rings, corresponding to a rational map $f : X \to Y$, {\tt isEmbedding} determines whether $f$ map embeds $X$ as a closed subscheme into $Y$.  The target and source must be varieties; their defining ideals must be prime.  Consider the Veronese embedding.
                Example
                        R = ZZ/7[x,y];
                        S = ZZ/7[a,b,c];
                        h = map(R, S, {x^2, x*y, y^2});
                        isEmbedding(h, Verbosity=>1)
                Text                        
                        If the option {\tt Verbosity} is set to {\tt 2}, the function will produce very detailed output.  Setting it to {\tt 0} will suppress output such output.
                        Now consider the projection from a point on the plane to the line at infinity.
                Example
                        R=QQ[x,y,z];
                        S=QQ[a,b];
                        h=rationalMapping(R, S, {y,z});
                        isEmbedding(h, Verbosity=>0)
                Text
                        That is obviously not an embedding.  It is even not an embedding when we restrict to a quadratic curve, even though it is a regular map.
                Example
                        R=QQ[x,y,z]/(x^2+y^2-z^2);
                        S=QQ[a,b];
                        h=map(R,S, {y,z});
                        isRegularMap(h)
                        isEmbedding(h, Verbosity=>0)                
                Text
                        If the option {\tt AssumeDominant} is set to {\tt true}, the function won't compute the kernel of the ring map.  Otherwise it will.
                Text
                        The remaining options, {\tt Strategy}, {\tt HybridLimit}, {\tt MinorsLimit}, and {\tt CheckBirational} are simply passed when {\tt isEmbedding} calls {\tt inverseOfMap}.  Note, this function, {\tt isEmbedding}, will only behave properly if {\tt CheckBirational} is set to {\tt true}.
                Text
                        We conclude by considering the map from $P^1$ to a cuspidal curve in $P^2$.  This is not an embedding, but if we take the strict transform in the blowup of $P^2$, it is an embedding.
                Example
                        R = ZZ/103[x,y,z];    
                        T = ZZ/103[u,v];
                        P2 = Proj R;    
                        P1 = Proj T;
                        phi = rationalMapping(P2, P1, {u^3, u^2*v, v^3});                            
                        isEmbedding(phi, Verbosity=>0)
                        P5ring = ZZ/103[a..f];
                        M = matrix{{a,b,c},{d,e,f}};
                        blowUpSubvar = Proj(P5ring/(minors(2, M)+ideal(b - d)));
                        tau = rationalMapping(P2, blowUpSubvar,{a, b, c}); --the blowup
                        tauInverse = tau^-1; --the inverse blowup
                        isEmbedding(tauInverse*phi, Verbosity => 0)
        SeeAlso
                HybridStrategy
                SimisStrategy
                ReesStrategy
///

--***************************************************************


doc ///
    Key
        baseLocusOfMap        
        (baseLocusOfMap, RingMap)
        (baseLocusOfMap, RationalMapping)
        [baseLocusOfMap, SaturateOutput]
        [baseLocusOfMap, Verbosity]
    Headline
        the base locus of a map from a projective variety to projective space
    Usage        
        I = baseLocusOfMap(h)
        I = baseLocusOfMap(phi)
    Inputs
        h: RingMap
            a ring map corresponding to a rational map of projective varieties
        phi: RationalMapping
            a rational map between projective varieties
        SaturateOutput => Boolean
            if set to true then the output will be saturated
        Verbosity => ZZ
            if 0 then silence the function, if 1 then generate informative output which can be used to adjust strategies, if > 1 then generate a detailed description of the execution
    Outputs
        I: Ideal
            the saturated defining ideal of the base locus of the corresponding maps
    Description
        Text
            This defines the locus where a given map of projective varieties is not defined.  If the option {\tt SaturateOutput} is set to {\tt false}, the output will not be saturated.  The default value is true.  Consider the following rational map from $P^2$ to $P^1$.
        Example
            R = QQ[x,y,z];
            S = QQ[a,b];
            f = map(R, S, {x,y});
            baseLocusOfMap(f)
        Text
            Observe it is not defined at the point [0:0:1], which is exactly what one expects.  However, we can restrict the map to a curve in $P^2$ and then it will be defined everywhere.
        Example
            R=QQ[x,y,z]/(y^2*z-x*(x-z)*(x+z));
            S=QQ[a,b];
            f=rationalMapping(R,S,{x,y});
            baseLocusOfMap(f)
        Text
            Let us next consider the quadratic Cremona transformation.
        Example
            R=QQ[x,y,z];
            S=QQ[a,b,c];
            f=map(R,S,{y*z,x*z,x*y});
            J=baseLocusOfMap(f)
            minimalPrimes J
        Text
            The base locus is exactly the three points one expects.
///
--***************************************************************

--doc ///
--    Key
--        relationType
--        (relationType, Ideal,BasicList)
--        (relationType, Ideal,Ideal)
--        (relationType, Ring,Ideal)
--	[relationType,Strategy]
--	[relationType,Verbosity]
--    Headline
--        Given an ideal in a ring this computes the maximum degree, of the new variables, of the minimal generators of the defining ideal of the associated Rees algebra.
--    Usage
--        n = relationType(I, L)
--        n = relationType(I, J)
--        n = relationType(R,J)
--    Inputs
--        I: Ideal
--            The ideal defining the base ring $R$.
--        L: List
--            The list of generators of the ideal $J$ we are forming the Rees algebra of.
--        R: Ring
--            The base ring.
--        J: Ideal
--            The ideal we are forming the Rees algebra of.
--    Outputs
--        n: ZZ
--            The maximum degree of the generators of the defining ideal of the Rees algebra.
--    Description
--        Text
--            Suppose $( g_1, \ldots, g_m ) = J \subseteq R$ is an ideal in a ring $R$.  We form the Rees algebra $R[Jt] = R[Y_1, \ldots, Y_m]/K$ where the $Y_i$ map to the $g_i$.  This function returns the maximum $Y$-degree of the generators of $K$.  For more information, see page 22 of Vasconcelos, Rees algebras, multiplicities, algorithms. Springer Monographs in Mathematics. Springer-Verlag, Berlin, 2005.
--        Example
--            R = QQ[x_0..x_8];
--            M = genericMatrix(R,x_0,3,3)
--            J = minors (2,M)
--            relationType(R,J)
--///
--***************************************************************

doc ///
    Key
        isSameMap
        (symbol ==, RationalMapping, RationalMapping)
        (isSameMap, RingMap,RingMap)
        (isSameMap, RationalMapping, RationalMapping)
    Headline
        whether two rational maps to between projective varieties are really the same
    Usage
        b = isSameMap(f1, f2)
        phi == psi
        b = isSameMap(phi, psi)
    Inputs
        f1: RingMap
            a map of rings corresponding to a rational map between projective varieties
        f2: RingMap
            a map of rings corresponding to a rational map between projective varieties
        phi: RationalMapping
            a map between projective varieties
        psi: RationalMapping
            a rational map between projective varieties
    Outputs
        b: Boolean
            true if the rational maps are the same, false otherwise.
    Description
        Text
            Checks whether two rational maps between projective varieties are really the same (that is, agree on a dense open set).
        Example
            R=QQ[x,y,z];
            S=QQ[a,b,c];
            f1=map(R, S, {y*z,x*z,x*y});
            f2=map(R, S, {x*y*z,x^2*z,x^2*y});
            isSameMap(f1,f2)        
        Text
            The Cremona transformation is not the identity, but its square is.
        Example
            R = ZZ/7[x,y,z]
            phi = rationalMapping(R, R, {y*z,x*z,x*y})
            ident = rationalMapping(R, R, {x,y,z})
            phi == ident
            phi^2 == ident
--        Example
--            R = QQ[x_0..x_8];
--            M = genericMatrix(R,x_0,3,3);
--            A = submatrix'(M,{2},)
--            B = submatrix'(M,{0},)
--            L1 = first entries gens minors(2,A)
--            L2 = first entries gens minors(2,B)
--            isSameMapToPn(L1,L2)
///
--***************************************************************

doc ///
    Key
        isRegularMap        
        (isRegularMap, RingMap)
        (isRegularMap, RationalMapping)
        [isRegularMap, Verbosity]
    Headline
        whether a map to projective space is regular
    Usage        
        b = isRegularMap(f)
    Inputs
        f: RingMap
            a ring map corresponding to a rational map of projective varieties
        Verbosity => ZZ
            if 0 then silence the function, if 1 then generate informative output which can be used to adjust strategies, if > 1 then generate a detailed description of the execution
    Outputs
        b: Boolean
    Description
        Text
            This function runs baseLocusOfMap(f) and checks whether the ideal defining the base locus is the whole ring.
        Example
            P5 = QQ[a..f];
            P2 = QQ[x,y,z];
            M = matrix{{a,b,c},{d,e,f}};
            blowUpSubvar = P5/(minors(2, M) + ideal(b - d));
            f = map(blowUpSubvar, P2, {a, b, c});
            isRegularMap(f)
///
--***************************************************************

doc ///
    Key
        inverseOfMap
        (inverseOfMap, RingMap)
        (inverseOfMap, RationalMapping)
        [inverseOfMap, AssumeDominant]
        [inverseOfMap, Strategy]
        [inverseOfMap, CheckBirational]
        [inverseOfMap, HybridLimit]
        [inverseOfMap, Verbosity]
        [inverseOfMap, MinorsLimit]
        [inverseOfMap, QuickRank]
    Headline
        inverse of a birational map between projective varieties
    Usage
        psi = inverseOfMap(g)
        psi = inverseOfMap(phi)
    Inputs
        g: RingMap
            corresponding to a birational map $f : X \to Y$
        phi: RationalMapping
            a rational map between projective varieties $f : X \to Y$
        Verbosity => ZZ
            if 0 then silence the function, if 1 then generate informative output which can be used to adjust strategies, if > 1 then generate a detailed description of the execution
        CheckBirational => Boolean
            whether to check birationality (if it is not birational, and CheckBirational is set to true, then an error will be thrown)
        AssumeDominant => Boolean
            whether to assume a rational map of schemes is dominant, if set to true it can speed up computation
        Strategy=>Symbol
                choose the strategy to use: HybridStrategy, SimisStrategy, or ReesStrategy
        HybridLimit => ZZ
            within HybridStrategy, the option HybridLimit controls how often SimisStrategy and ReesStrategy are used, larger numbers means SimisStrategy will be executed longer
        MinorsLimit => ZZ
            how many submatrices of a variant of the Jacobian dual matrix to consider before switching to a different strategy
        QuickRank => Boolean
            whether to compute rank via the package @TO2(FastMinors, "FastMinors")@
    Outputs
        psi: RationalMapping
            inverse function of the birational map
    Description
        Text
            Given a rational map $f : X \to Y$, {\tt inverseOfMap} computes the inverse of the induced map $X \to \overline{f(X)}$, provided it is birational."
            The target and source must be varieties; their defining ideals must be prime.
        Text
            If {\tt AssumeDominant} is set to {\tt true} (default is {\tt false}) then it assumes that the rational map of projective varieties is dominant, otherwise the function will compute the image by finding the kernel of $f$.
        Text
            The {\tt Strategy} option can be set to {\tt HybridStrategy} (default), {\tt SimisStrategy}, {\tt ReesStrategy}, or {\tt SaturationStrategy}.  Note that {\tt SimisStrategy} will never terminate for non-birational maps. If {\tt CheckBirational} is set to {\tt false} (default is {\tt true}), then no check for birationality will be done.  If it is set to {\tt true} and the map is not birational, then an error will be thrown if you are not using {\tt SimisStrategy}. The option {\tt HybridLimit} controls {\tt HybridStrategy}.  Larger values of {\tt HybridLimit} (the default value is 15) will mean that {\tt SimisStrategy} is executed longer, smaller values will mean that {\tt ReesStrategy} will be switched to sooner.
        Example
            R = ZZ/7[x,y,z];
            S = ZZ/7[a,b,c];
            h = map(R, S, {y*z, x*z, x*y});
            inverseOfMap (h, Verbosity=>0)
        Text
            Notice that removal of the leading minus signs would not change the projective map.  Next let us compute the inverse of the blowup of $P^2$ at a point.
        Example
            P5 = QQ[a..f];
            M = matrix{{a,b,c},{d,e,f}};
            blowUpSubvar = P5/(minors(2, M)+ideal(b - d));
            h = map(blowUpSubvar, QQ[x,y,z],{a, b, c});
            g = inverseOfMap(h, Verbosity=>0)
            baseLocusOfMap(g)
            baseLocusOfMap(h)
        Text
            The next example is a birational map on $\mathbb{P}^4$. 
        Example
            Q=QQ[x,y,z,t,u];
            phi=map(Q,Q,matrix{{x^5,y*x^4,z*x^4+y^5,t*x^4+z^5,u*x^4+t^5}});
            time inverseOfMap(phi,CheckBirational=>false, Verbosity=>0)
        Text
            Finally, we do an example of plane Cremona maps whose source is not minimally embedded.
        Example
            R=QQ[x,y,z,t]/(z-2*t);
            F = {y*z*(x-z)*(x-2*y), x*z*(y-z)*(x-2*y),y*x*(y-z)*(x-z)};
            S = QQ[u,v,w];
            ident = rationalMapping map(S, S)
            h = rationalMapping(R, S, F);
            g = inverseOfMap(h, Verbosity=>0) 
            h*g == ident        
    SeeAlso
        HybridStrategy
        SimisStrategy
        ReesStrategy
    Caveat
        The current implementation of this function works only for irreducible varieties.  Also see the function @TO "Cremona::inverseMap"@ in the package @TO "Cremona::Cremona"@, which for some maps from projective space is faster.  Additionally, also compare with the function @TO "Parametrization::invertBirationalMap"@ of the package @TO "Parametrization"@.
///
--***************************************************************

doc ///
    Key
        sourceInversionFactor
        (sourceInversionFactor, RingMap)
        [sourceInversionFactor, AssumeDominant]
        [sourceInversionFactor, Strategy]
        [sourceInversionFactor, CheckBirational]
        [sourceInversionFactor, HybridLimit]
        [sourceInversionFactor, Verbosity]
        [sourceInversionFactor, MinorsLimit]
        [sourceInversionFactor, QuickRank]
    Headline
        computes the common factor among the components of the composition of the inverse map and the original map
    Usage
         s = sourceInversionFactor(g)
    Inputs
        g: RingMap
            a birational map $f : X \to Y$.
        Verbosity => ZZ
            if 0 then silence the function, if 1 then generate informative output which can be used to adjust strategies, if > 1 then generate a detailed description of the execution
        CheckBirational => Boolean
            whether to check birationality (if it is not birational, and CheckBirational is set to true, then an error will be thrown)
        Strategy=>Symbol
            choose the strategy to use: HybridStrategy, SimisStrategy, or ReesStrategy
        HybridLimit => ZZ
            within HybridStrategy, the option HybridLimit controls how often SimisStrategy and ReesStrategy are used, larger numbers means SimisStrategy will be executed longer
        MinorsLimit => ZZ
            how many submatrices of a variant of the Jacobian dual matrix to consider before switching to a different strategy
        AssumeDominant => Boolean
            whether to assume a rational map of schemes is dominant, if set to true it can speed up computation
        QuickRank => Boolean
            whether to compute rank via the package @TO2(FastMinors, "FastMinors")@
    Outputs
        s: RingElement
             an element of the coordinate ring of $X$ .
    Description
        Text
            Given a map $f : X \to Y$, sourceInversionFactor computes the common factor among the components of, $f^{(-1)}$ composed with $f$, which is an element of the coordinate ring of $X$.
        Text
            If {\tt AssumeDominant} is set to {\tt true} (default is {\tt false}) then it assumes that the rational map of projective varieties is dominant, otherwise the function will compute the image by finding the kernel of $f$.
        Text
            The {\tt Strategy} option can be set to {\tt HybridStrategy} (default), {\tt SimisStrategy}, {\tt ReesStrategy}, or {\tt SaturationStrategy}.  Note {\tt SimisStrategy} will never terminate for non-birational maps. If {\tt CheckBirational} is set to {\tt false} (default is {\tt true}), then no check for birationality will be done.  If it is set to {\tt true} and the map is not birational, then an error will be thrown if you are not using {\tt SimisStrategy}. The option {\tt HybridLimit} controls {\tt HybridStrategy}.  Larger values of {\tt HybridLimit} (the default value is 15) will mean that {\tt SimisStrategy} is executed longer, smaller values will mean that {\tt ReesStrategy} will be switched to sooner.
        Example
            R = ZZ/7[x,y,z];
            S = ZZ/7[a,b,c];
            h = map(R, S, {y*z, x*z, x*y});
            sourceInversionFactor h
        Example
             S=QQ[a,b,c,d];
             g=(b^2-a*c)*c*d;
             phi=map(S,S,transpose jacobian ideal g);
             sourceInversionFactor(phi, Verbosity=>0)
    Caveat
        The current implementation of this function works only for irreducible varieties..  Also see the function @TO "Cremona::inverseMap"@ in the package @TO "Cremona::Cremona"@, which for some maps from projective space is faster.  Additionally, also compare with the function @TO "Parametrization::invertBirationalMap"@ of the package @TO"Parametrization"@.
    SeeAlso
        HybridStrategy
        SimisStrategy
        ReesStrategy
///



--******************************************
--******************************************
--******TESTS TESTS TESTS TESTS TESTS*******
--******************************************
--******************************************

TEST /// --test #0
	------------------------------------
	------- Tests for idealOfImageOfMap -------
	------------------------------------
	S = QQ[x,y,z,w];
	b = ideal(x*y-z*w);
	R = QQ[u,v];
	a = ideal(sub(0,R));
	f = matrix {{u,0,v,0}};
    psi = rationalMapping(R/a, S/b, f)
	im = idealOfImageOfMap(psi);
	assert (im == sub(ideal(y,w), S/b));
    T = QQ[x,y,z];
    phi = map(T, T, {y*z, x*z, x*y});
    assert(ideal(0_T) == idealOfImageOfMap(phi));
///

TEST /// --test #1
	S = QQ[x0,x1];
	T = QQ[y0,y1,y2];
	f = map(S,T,{x0^4,x0^2*x1^2,x1^4});
	im = idealOfImageOfMap(f);
	assert(im == ideal(y1^2-y0*y2))
///

TEST /// --test #2
	-- Since in Projective Space, check to make sure different representations give the same result
	S = QQ[x,y];
	T = QQ[u,v];
	f1 = map(S,T,{x,y});
	f2 = map(S,T,{x^3*y^2,x^2*y^3});
	assert(idealOfImageOfMap(f1)==idealOfImageOfMap(f2))
///


	-------------------------------------
	-- Tests for baseLocusOfMap ---------
	-------------------------------------
TEST ///	--test #3
    R = QQ[x,y,z]
	f = map(R, R, matrix{{x^2*y, x^2*z, x*y*z}})
	I = ideal(x*y, y*z, x*z)
	assert(I == baseLocusOfMap(f))
///

TEST ///	--test #4
    R = QQ[x,y,z]
	f = map(R, R, {x^2*y, x^2*z, x*y*z})
	I = ideal(x*y, y*z, x*z)
	assert(I == baseLocusOfMap(f))
///

TEST /// --test #5
	-- reducible source

	R = QQ[x,y,z]/(x*y)
	f = map(R, R, matrix{{x^2, x*y, y^2}})
	I = ideal(x,y)
	assert(I == baseLocusOfMap(f))

    -- we should have a test for when that kernel is not a cyclic module
///


	-------------------------------------
	----- isRegularMap -----------------
	-------------------------------------
TEST /// --test #6
    R = QQ[x,y,z,w]/(x*y - z*w)
    S = QQ[a,b,c]
    f = map(R, S, matrix{{sub(1,R), 0, 0}})
    assert(isRegularMap(f))
///

TEST /// --test #7
    R = QQ[x,y]/(x*y)
    f = map(R, R, matrix{{x,y}})
    assert(isRegularMap(f))
///

TEST /// --test #8
    R = QQ[x,y,z]/(x^3 + y^3 - z^3)
    S = QQ[a,b]
    f = map(R,S,matrix{{(y-z)*x, x^2}})
    assert(isRegularMap(f))
///

TEST /// --test #9
        R=QQ[x,y,z];
        S=QQ[a,b];
        h = map(R, S, {x,y});
        assert(isRegularMap(h) == false)
///

TEST /// --test #10
        R=QQ[x,y,z];
        S=QQ[a,b,c];
        h = map(R,S,{y*z,x*z,x*y});
        assert(isRegularMap(h) == false)
///

TEST /// -- test #11
    -- projection from the blow up of P2 to P2

    P5 = QQ[a..f];
    M = matrix{{a,b,c},{d,e,f}};
    P2 = QQ[x,y,z];
    blowUpSubvar = P5/(minors(2,M) + ideal(b - d));
    f = map(blowUpSubvar, P2, {a, b, c});
    assert(isRegularMap(f))
///

	-------------------------------------
	----- isBirationalOntoImage  --------
	-------------------------------------

TEST /// --test #12 (a map from the blowup of P^2 at a point back down to P^2)
    P5 = QQ[a..f];
    M = matrix{{a,b,c},{d,e,f}};
    blowUpSubvar = P5/(minors(2, M) + ideal(b-d));
    f = map(blowUpSubvar, QQ[x,y,z], {a, b, c});
    assert(isBirationalOntoImage(f, Verbosity=>0) == true)
///

TEST /// --test #13 (quadratic cremona transformation)
    R = QQ[x,y,z];
    S = QQ[a,b,c];
    f = map(R, S, {y*z, x*z, x*y});
    assert(isBirationalOntoImage(f) == true)
///

TEST /// --test #14 (map P^1 to P^2)
    R = QQ[x,y];
    S = QQ[a,b,c];
    f = map(R, S, {x,y,0});
    assert(isBirationalOntoImage(f) == true)
///

TEST /// --test #15 (let's map an elliptic curve onto P^1)
    R = QQ[x,y,z]/(x^3+y^3-z^3);
    S = QQ[a,b];
    f = map(R, S, {x, y-z});
    assert(isBirationalOntoImage(f) == false)
///

TEST /// --test #16 (map P^2\pt -> P^1)
    R = QQ[x,y,z];
    S = QQ[a,b];
    f = map(R, S, {x,y});
    assert(isBirationalOntoImage(f) == false)
///

TEST /// --test #17 (3rd veronese embedding of P^1)
    R = QQ[x,y];
    S = QQ[a,b,c,d];
    f = map(R, S, {x^3,x^2*y,x*y^2,x^3});
    assert(isBirationalOntoImage(f) == true)
///

	-------------------------------------
	----- isBirationalMap  --------------
	-------------------------------------

TEST /// --test #18 (quadratic cremona)
    R = QQ[x,y,z];
    S = QQ[a,b,c];
    f = map(R, S, {y*z, x*z, x*y});
    assert(isBirationalMap(f) == true)
///

TEST /// --test #19 (map P^1 to P^2)
    R = QQ[x,y];
    S = QQ[a,b,c];
    f = map(R, S, {x,y,0});
    assert(isBirationalMap(f) == false)
///

TEST /// --test #20 (let's map an elliptic curve onto P^1)
    R = QQ[x,y,z]/(x^3+y^3-z^3);
    S = QQ[a,b];
    f = map(R, S, {x, y-z});
    assert(isBirationalOntoImage(f, Strategy=>SaturationStrategy, QuickRank=>true) == false)
    assert(isBirationalOntoImage(f, Strategy=>SaturationStrategy, QuickRank=>false) == false)    
///

TEST /// --test #21 (3rd veronese embedding of P^1)
    R = QQ[x,y];
    S = QQ[a,b,c,d];
    f = map(R, S, {x^3,x^2*y,x*y^2,x^3});
    assert(isBirationalOntoImage(f, QuickRank=>true, HybridLimit=>20) == true);
    assert(isBirationalOntoImage(f, QuickRank=>false, HybridLimit=>20) == true);   
///

TEST /// --test #22 (Frobenius on an elliptic curve)
    R = ZZ/5[x,y,z]/(x^3+y^3-z^3);
    S = ZZ/5[a,b,c]/(a^3+b^3-b^3);
    h = map(R, S, {x^5, y^5, z^5});
    assert(isBirationalMap(h, QuickRank=>false) == false);
    assert(isBirationalMap(h, QuickRank=>true) == false);
///
	-------------------------------------
	----- Tests for inverseOfMap  -------
	-------------------------------------

TEST /// --test #23
    -- Let's find the inverse of the projection map from
    -- the blow up of P^2 to P^2

    -- the blow up of P^2 is a projective variety in P^5:
    P5 = QQ[a..f];
    M = matrix{{a,b,c},{d,e,f}};
    blowUpSubvar = P5/(minors(2, M)+ideal(b - d));
    T = QQ[x,y,z];
    h = map(blowUpSubvar, T, {a, b, c});
    assert( (baseLocusOfMap(inverseOfMap(h, QuickRank=>true)) == sub(ideal(x,y), T)) and (baseLocusOfMap(inverseOfMap(h, QuickRank=>true, Strategy=>ReesStrategy)) == sub(ideal(x,y), T)) );
    assert( (baseLocusOfMap(inverseOfMap(h, QuickRank=>false)) == sub(ideal(x,y), T)) and (baseLocusOfMap(inverseOfMap(h, QuickRank=>false, Strategy=>ReesStrategy)) == sub(ideal(x,y), T)) )
///

TEST /// --test #24
    R =  QQ[a..d]/(a*d - b*c);
    S = QQ[x,y,z];
    h = rationalMapping(S, R, {x^2, x*y, x*z, y*z});
    f = inverseOfMap(map(R, S, {a,b,c}));
    g = inverseOfMap(map(R, S, {a,b,c}),Strategy=>ReesStrategy);    
    assert( (isSameMap(f, h)) and (isSameMap(g, h)) )
///

TEST /// --test #25 (quadratic cremona)
    R = ZZ/11[x,y,z];
    S = ZZ/11[a,b,c];
    h = rationalMapping(R, S, {y*z, x*z, x*y});
    phi = rationalMapping(S, R, {b*c,a*c,a*b});
    g = inverseOfMap(h,AssumeDominant=>true);
    f = inverseOfMap(h,AssumeDominant=>true, Strategy=>ReesStrategy);
    assert((g == phi) and (f == phi))
///

-----------------------------------
------- isEmbedding ---------------
-----------------------------------

TEST /// --test #26
    -- Consider the projection map from
    -- the blow up of P^2 to P^2

    -- the blow up of P^2 is a projective variety in P^5:
    P5 = QQ[a..f]
    M = matrix{{a,b,c},{d,e,f}}
    blowUpSubvar = P5/(minors(2, M)+ideal(b - d))
    h = map(blowUpSubvar, QQ[x,y,z],{a, b, c})
    assert(isEmbedding(h)==false)
///

TEST /// --test #27
    --Let's do the twisted cubic curve
    P3 = ZZ/101[x,y,z,w];
    C = ZZ/101[a,b];
    h = map(C, P3, {a^3, (a^2)*b, a*b^2, b^3});
    assert(isEmbedding(h) == true)
///

TEST /// --test #28
     --let's parametrize the nodal plane cubic
     P2 = QQ[x,y,z]
     C = QQ[a,b]
     h = map(C, P2, {b*a*(a-b), a^2*(a-b), b^3})
     assert((isBirationalMap h == false) and (isBirationalOntoImage h == true) and (isEmbedding(h) == false) and (isRegularMap inverseOfMap h == false))
///

TEST /// --test #29, map from genus 3 curve to projective space
    needsPackage "Divisor";
    C = ZZ/103[x,y,z]/(x^4+x^2*y*z+y^4+z^3*x);
    Q = ideal(y,x+z); --a point on our curve
    f2 = mapToProjectiveSpace(7*divisor(Q)); --a divisor of degree 7 (this is degree 7, so should induce an embedding)
    assert( (isEmbedding(f2) == true)) --note for this example, 6*divisor(Q) is not an embedding, indeed it appears the image is singular for 6*D.
///


-----------------------------------
------- Further Tests -------------
-----------------------------------

--finally we test a map between non-rational varieties
TEST /// --test #30, maps between cones over elliptic curves and their blowups
    --the cone over an elliptic curve lies in P3, the blowup lives in P11
    P3 = QQ[x,y,z,w];
    P11 = QQ[a_{0,0}..a_{2,3}];
    M = matrix{{a_{0,0}..a_{0,3}},{a_{1,0}..a_{1,3}},{a_{2,0}..a_{2,3}}};
    blowUpP3 = P11/(minors(2,M) + ideal(a_{1,0}-a_{0,1}, a_{2,0}-a_{0,2}, a_{2,1}-a_{1,2}));
    h = map(blowUpP3, P3, {a_{0,0}..a_{0,3}}); -- map from blowup of P3 back down to P3
    J = ideal(h(x^3+y^3+z^3)) --x^3+y^3+z^3 defines the projective cone over an elliptic curve
    I = saturate(J, ideal(h(x), h(y), h(z))); -- strict transform of the projective cone over an elliptic curve
    S = P11/(ideal(blowUpP3) + sub(I, P11));
    T = P3/ideal(x^3+y^3+z^3);
    g = map(S, T, toList(a_{0,0}..a_{0,3}));
    b = isRegularMap g;
    gg = inverseOfMap g;
    assert( b and ( isRegularMap gg == false))
///


------------------------------------------
------- Testing RationalMapping Type------
------------------------------------------

TEST /// --test #31, constructor testing, equality testing, source target testing
    R = ZZ/101[x,y,z];
    S = ZZ/101[u,v,w];
    PR = Proj R;
    PS = Proj S;
    L = {random(3, R), random(3,R), random(3,R)};    
    M = matrix{L};
    p1 = rationalMapping map(R, S, L);
    p2 = rationalMapping map(R, S, M);
    p3 = rationalMapping(R, S, L);
    p4 = rationalMapping(R, S, M);    
    p5 = rationalMapping(PS, PR, L);
    p6 = rationalMapping(PS, PR, M);
    assert (source p1 === PR and source p2 === PR and source p3 === PR and source p4 === PR and source p5 === PR and source p6 === PR)
    assert (target p1 === PS and target p2 === PS and target p3 === PS and target p4 === PS and target p5 === PS and target p6 === PS)
    assert( p1 == p2 and p1 == p3 and p1 == p4 and p1 == p5 and p1 == p6);
    L1 = {L#0 + x^3, L#1, L#2}
    M1 = matrix{L1};
    q1 = rationalMapping map(R, S, L1);
    q2 = rationalMapping map(R, S, M1);
    q3 = rationalMapping(R, S, L1);
    q4 = rationalMapping(R, S, M1);
    q5 = rationalMapping(PS, PR, L1);
    q6 = rationalMapping(PS, PR, M1);
    assert( p1 != q1 and p2 != q2 and p3 != q3 and p4 != q4 and p5 != q5 and p6 != q6)
///

TEST /// --test #32, composition testing, birational and embedding testing
    R = ZZ/103[x,y,z];    
    T = ZZ/103[u,v];
    P2 = Proj R;    
    P1 = Proj T;
    phi = rationalMapping(P2, P1, {u^3, u^2*v, v^3});
    psi = rationalMapping(P2, P2, {y*z,x*z,x*y});
    assert isBirationalOntoImage(phi);
    assert not isEmbedding(phi);
    assert not isBirationalMap(phi);    
    assert not isRegularMap(inverseOfMap phi);
    rho = psi*phi;
    assert isBirationalOntoImage(rho);
    assert not isBirationalMap(rho);    
    S = ZZ/103[m,n,l]/ideal(m^3 - n^2*l); --a cusp
    cusp = Proj S;
    kappa = rationalMapping(P2, cusp, {m,n,l});
    assert isBirationalOntoImage(kappa);
    assert isEmbedding(kappa); --this should be an embedding by definition
    assert isBirationalOntoImage(psi*kappa);
    P5 = ZZ/103[a..f];
    M = matrix{{a,b,c},{d,e,f}};
    blowUpSubvar = Proj(P5/(minors(2, M)+ideal(b - d)));
    tau = rationalMapping(P2, blowUpSubvar,{a, b, c});
    tauI = inverseOfMap tau;
    assert isEmbedding(tauI*phi);  --the map of P1 to a cusp was not an embedding before but after we blow up the origin, it's fine.
///

TEST /// --test #33, self composition testing
    R = ZZ/59[x,y,z];
    P2 = Proj R;
    phi = rationalMapping(P2, P2, {y*z, x*z, x*y});
    ident = rationalMapping(P2, P2, {x,y,z});
    assert(phi^2 == ident and phi^-1 == phi and ident^-1 == ident and phi^-2 == ident and phi^3 == ident*phi^-1 and phi^0 == ident)
///

TEST /// --test #34, an interesting example based on a question of Abbas Nasrolanejad.
    A=QQ[x,y,z,w];
    S={y*z*w^2,x*z*w^2,x*y*w^2,y*z^2*w,x*z^2*w,y^2*z*w,x^2*z*w,x*y^2*w,x^2*y*w,x*y*z*w,x*y*z^2,x*y^2*z,x^2*y*z};
    R=QQ[t_0..t_12];
    phi=map(A,R,S);
    J=ker phi;
    S=QQ[s_0..s_5];
    psi=map(A,S, {x*y,x*z,x*w,y*z,y*w,z*w});
    I=ker psi;
    R1=R/J;
    identR1 = rationalMapping(map(R1, R1));
    S1=S/I;
    identS1 = rationalMapping(map(S1, S1));
    rat=rationalMapping(S1,R1,{s_4*s_5,s_2*s_5,s_2*s_4,s_3*s_5,s_1*s_5,s_3*s_4,s_1*s_2,s_0*s_4,s_0*s_2,s_0*s_5,s_1*s_3,s_0*s_3,s_0*s_1});
   assert( isBirationalMap(rat, AssumeDominant=>true) )
   ratI = rat^-1;
   assert(rat*ratI == identR1 and ratI*rat == identS1)
///


----Version information----
--0.1  First version.
--0.2  Substantial improvements in speed and documentation.
--0.21 Minor changes especially to documentation.
--1.0  added new Type RationalMapping, various rewrites, numerous small improvements, improved documentation


----FUTURE PLANS------
--1.  Handle multi-graded rings (multi-graded maps etc.)
--2.  Find generic degree of a map (generic rank).
--3.  Degree of inverse map as well.
--4.  Make faster.
------a) maybe add multi-core support?
------b) find the relevant low degree part of the blowup ideal
------c) be smarter when looking at ranks of matrices, in particular
---------when trying to show that the rank is at least x, we should evaluate
---------the variables appropriately to some (large) field randomly, and then
---------check the rank there.
--5.  Check for smoothness/flatness of map (find loci)?
