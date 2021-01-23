newPackage( "RationalMaps",
Version => "0.3", Date => "August 11th, 2019", Authors => {
     {Name => "Karl Schwede",
     Email=> "kschwede@gmail.com",
     HomePage=> "http://www.math.utah.edu/~schwede"
     }, --Karl Schwede was partially supported by  NSF FRG Grant DMS #1265261/1501115, NSF CAREER Grant DMS #1252860/1501102, and NSF grant #1801849
     {Name => "Daniel Smolkin",
     Email=> "smolkin@math.utah.edu",
     HomePage=> "http://www.math.utah.edu/~smolkin"
     },--Dan Smolkin was partially supported by  NSF FRG Grant DMS #1265261/1501115, NSF CAREER Grant DMS #1252860/1501102
     {Name => "S. Hamid Hassanzadeh",
     Email => "hassanzadeh.ufrj@gmail.com",
     HomePage=>"https://www.researchgate.net/profile/Seyed_Hassanzadeh"
     }, --S. Hamid Hassanzadeh was supported by CNPq-bolsa de Produtividade
     {Name => "C.J. Bott",
     Email => "cjamesbott@gmail.com"}
}, --this file is in the public domain
Keywords => {"Commutative Algebra"},
Headline => "rational maps between varieties", PackageImports => {"FastLinAlg"})
export{
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
	--"nonZeroMinor",-- it is internal because the answer is probobalistic (either it finds one or it doesn't) and it is controlled by MinorsCount option
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
    "MinorsCount", --an option for how many times we should randomly look for a minor before calling syz in inverseOfMap
    "HybridLimit", --an option for controlling inversion of maps (whether to do more simis or more rees strategies)
    "CheckBirational", --an option for inverseOfMap, whether or not to check if something is birational
    "SaturateOutput",  --option to turn off saturation of the output
    "AssumeDominant" --option to assume's that the map is dominant (ie, don't compute the kernel)
}


StrategyGRevLexSmallestTerm = new HashTable from {LexLargest=>0, LexSmallestTerm => 0, LexSmallest=>0, GRevLexSmallestTerm => 100, GRevLexSmallest => 0, GRevLexLargest=>0,Random=>0,RandomNonzero=>0,Points => 0};
----------------------------------------------------------------
--************************************************************--
-------------------- Function Definitions ----------------------
--************************************************************--
----------------------------------------------------------------

idealOfImageOfMap = method(Options=>{Verbose=>false, QuickRank=>true});

idealOfImageOfMap(Ideal,Ideal,Matrix) := o -> (a,b,f) -> (
	h := map((ring a)/a, ring b ,f);
	-- the image of f is the same as the kernel of its pullback on the
	-- coordinate rings. h is this pullback
        idealOfImageOfMap(h, o)
	);

idealOfImageOfMap(Ideal,Ideal,BasicList) := o -> (a,b,g) -> (
	h:=map((ring a)/a, ring b ,g);
	idealOfImageOfMap(h, o)
	);

idealOfImageOfMap(Ring,Ring,Matrix) := o -> (R,S,f) -> (
	h := map(R,S,f);
	idealOfImageOfMap(h, o)
	);

idealOfImageOfMap(Ring,Ring,BasicList) := o -> (R,S,g) -> (
        h := map(R,S,g);
        idealOfImageOfMap(h, o)
);

idealOfImageOfMap(RingMap) := o -> (p) -> (
        --h := map(target p, ambient source p,p);
        --do a quick check to see if the map is injective
        if (instance(target p, PolynomialRing)) then(
            if (o.Verbose == true) then print "idealOfImageOfMap: checking if map is zero using rank of the jacobian";
            jac := jacobian matrix p;
            if (o.QuickRank == true) then (
               if (isRankAtLeast(dim source p, jac, Strategy => StrategyGRevLexSmallestTerm, MaxMinors=>2)) then return ideal(sub(0, source p));
            )
            else (
                if (rank jac >= dim source p) then return ideal(sub(0, source p));
            );            
            if (o.Verbose == true) then print "idealOfImageOfMap: map not injective, computing the kernel.";
        );
        im := ker p;
        im
);

dimImage = method();

dimImage(Ideal,Ideal,Matrix) := (a,b,f) -> (
        h := map( (ring a)/a, (ring b)/b, f);
        dimImage(h)
        	-- subtract 1 from the dimension of the image since in projective space
	);

dimImage(Ideal,Ideal,BasicList) := (a,b,g) -> (
        h := map( (ring a)/a, (ring b)/b, g);
        dimImage(h)
       	);

dimImage(Ring,Ring,Matrix) := (R,S,f) -> (
        h := map( R, S, f);
        dimImage(h)
	);

dimImage(Ring,Ring,BasicList) := (R,S,g) -> (
        h := map( R, S, g);
        dimImage(h)
        );

dimImage(RingMap) := (p) -> (
	I := idealOfImageOfMap(p);
	(dim I) - 1
);

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
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


baseLocusOfMap = method(Options=>{SaturateOutput=>true});

baseLocusOfMap(Matrix) := o->(L1) -> ( --L1 is a row matrix
    if numRows L1 > 1 then error "baseLocsOfMap: Expected a row matrix";
    if isSameDegree( first entries L1  )==false then error "baseLocsOfMap: Expected a matrix of homogeneous elements of the same degree";

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

baseLocusOfMap(List) := o->(L) ->(
    baseLocusOfMap(matrix{L}, SaturateOutput=>o.SaturateOutput)
);

baseLocusOfMap(RingMap) := o->(ff) ->(
    mm := sub(matrix ff, target ff);
    baseLocusOfMap(mm, SaturateOutput=>o.SaturateOutput)
);

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- isRegularMap returns true if a map is regular (ie, has no base locus)
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isRegularMap = method();

isRegularMap(Matrix) := (L1) -> ( --L1 is a row matrix
    I:= baseLocusOfMap(L1, SaturateOutput=>false);
    (dim I <= 0)
);

isRegularMap(List) := (L1) -> (
    I:= baseLocusOfMap(L1, SaturateOutput=>false);
    (dim I <= 0)
);

isRegularMap(RingMap) := (ff) ->(
    I:=baseLocusOfMap(ff, SaturateOutput=>false);
    (dim I <= 0)
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


  --the rees algebra computation below is too slow, we need to modify it
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
    M:=gb(J,DegreeLimit=>{1,m}); --instead of computing the whole Grob. Baisis of J we only compute the parts of degree (1,m) or less,
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

 relationType=method(Options => {Strategy=>ReesStrategy, Verbose=>true});
 --this function computes the "relation type" of an ideal in a ring R.
 --Let R be the ring given bythe  ideal a and L be a list of elements in R.
 --the relation type is the biggest degree in terms of new variables in the
 --defining ideal of the rees algebra of I over R.
 --

 relationType(Ideal,BasicList):=o->(a,L)->(
     S:=ring L_0;
     J:=blowUpIdeals(a,L,Strategy=>o.Strategy);
     if (o.Verbose) then print"blowUpIdeals computed.";
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
isBirationalMap = method(Options => {AssumeDominant=>false, Strategy=>HybridStrategy,MinorsCount=>null, Verbose=>true, HybridLimit=>15, Verbose=>true, QuickRank=>true});

--this checks whether a map X -> Y is birational.

--X = Proj R
--Y = Proj S
--This map is given by a list of elements in R, all homogeneous
--of the same degree.
--Below we have defining ideal of X = di
--defining ideal of Y = im
--list of elements = bm
isBirationalMap(Ideal,Ideal,BasicList) :=o->(di,im,bm)->(
    R:=ring di;
    R2 := R/di;
    S:=ring im;
    ff := map(R2, S/di, apply(bm, z -> sub(z, R2)));
    isBirationalMap(ff)
);

isBirationalMap(Ring,Ring,BasicList) := o->(R1, S1, bm)->(
    isBirationalMap(ideal R1, ideal S1, bm,AssumeDominant=>o.AssumeDominant, Strategy=>o.Strategy,Verbose=>o.Verbose, HybridLimit=>o.HybridLimit,QuickRank=>o.QuickRank)
    );


isBirationalMap(RingMap) :=o->(f)->(
    di := ideal target f;
    R := ring di;
    im := ideal source f;
    S := ring im;
    bm := first entries matrix f; 
    local im1;
    --isBirationalMap(target f, source f, first entries matrix f,AssumeDominant=>o.AssumeDominant,Strategy=>o.Strategy,Verbose=>o.Verbose, HybridLimit=>o.HybridLimit,QuickRank=>o.QuickRank)
    if (o.AssumeDominant==false) then (
        if (o.Verbose) then (
            print "isBirationalMap: About to find the image of the map.  If you know the image, ";
            print "        you may want to use the AssumeDominant option if this is slow.";
        );
--        im1 = idealOfImageOfMap(di, im, bm, QuickRank=>o.QuickRank);
        if (instance(target f, PolynomialRing)) then(
            if (o.Verbose == true) then print "isBirationalMap: initial birationality via rank of the jacobian";
            jac := jacobian matrix f;
            local rk;
            fSourceDim := dim source f;
            if (o.QuickRank == true) then (
                l1 := getSubmatrixOfRank(fSourceDim, jac, Strategy => StrategyGRevLexSmallestTerm, MaxMinors=>2);
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
            if (o.Verbose == true) then print("isBirationalMap: jac rank = " | toString(rk) | ".  sourceDim = " | fSourceDim);
            if (rk == fSourceDim) then (im1 = im) else (
                if (char S == 0) then print (
                    if (o.Verbose === true) then print "isBirationalMap: the dimension is wrong, not birational.";
                    return false;)
                else (im1 = im + sub(ker f, S));
            ); 
        )
        else (
            im1 = idealOfImageOfMap(di, im, bm, QuickRank=>o.QuickRank);
        );
        if (o.Verbose === true) then print "isBirationalMap: Found the image of the map.";

        if (dim (S^1/im1) >= dim (source f)) then( --first check if the image is the closure of the image is even the right thing
            if (o.Strategy==ReesStrategy or o.Strategy==SaturationStrategy ) then (isBirationalOntoImage(di,im1,bm,AssumeDominant=>true, MinorsCount=>o.MinorsCount, Strategy=>o.Strategy, Verbose=>o.Verbose, QuickRank=>o.QuickRank))
            else if (o.Strategy==HybridStrategy) then ( isBirationalOntoImage(di,im1,bm,AssumeDominant=>true, MinorsCount=>o.MinorsCount, Strategy=>HybridStrategy, HybridLimit=>o.HybridLimit,Verbose=>o.Verbose, QuickRank=>o.QuickRank))
            else if (o.Strategy==SimisStrategy) then (isBirationalOntoImage(di,im1,bm,AssumeDominant=>true, MinorsCount=>o.MinorsCount, Strategy=>SimisStrategy, Verbose=>o.Verbose, QuickRank=>o.QuickRank))
        )
        else(
            if (o.Verbose === true) then print "isBirationalMap: the dimension is really wrong, not birational.";
            false
        )
    )
    else(
        isBirationalOntoImage(di,im1,bm,AssumeDominant=>true,Strategy=>o.Strategy,Verbose=>o.Verbose, MinorsCount=>o.MinorsCount, HybridLimit=>o.HybridLimit, QuickRank=>o.QuickRank)
    )
);

  --%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isBirationalOntoImage = method(Options => {AssumeDominant=>false, MinorsCount => null, Strategy=>HybridStrategy,Verbose=>true, HybridLimit=>15, QuickRank=>true});
--if AssumeDominant is true, it doesn't form the kernel.
isBirationalOntoImageRees := method(Options => {AssumeDominant=>false, MinorsCount => null, Strategy=>ReesStrategy,Verbose=>true, QuickRank=>true});
 isBirationalOntoImageSimis := method(Options => {AssumeDominant=>false, MinorsCount=> null, HybridLimit=>15,Verbose=>true, QuickRank=>true});


--*****************************Strategies
isBirationalOntoImage(Ideal,Ideal, BasicList) :=o->(di,im,bm)->(
    if (o.Verbose) then (print "Starting isBirationalOntoImage" );
    if ((o.Strategy == ReesStrategy) or (o.Strategy == SaturationStrategy)) then (
        isBirationalOntoImageRees(di,im,bm, AssumeDominant=>o.AssumeDominant,  Strategy=>o.Strategy,Verbose=>o.Verbose, QuickRank=>o.QuickRank)
    )
    else if (o.Strategy == SimisStrategy) then (
        isBirationalOntoImageSimis(di,im,bm, AssumeDominant=>o.AssumeDominant,  HybridLimit=>infinity,Verbose=>o.Verbose, QuickRank=>o.QuickRank)
    )
    else if (o.Strategy == HybridStrategy) then(
        isBirationalOntoImageSimis(di,im,bm, AssumeDominant=>o.AssumeDominant, HybridLimit=>o.HybridLimit,Verbose=>o.Verbose, QuickRank=>o.QuickRank)
    )
  );
--*********************************************
--*************other modes
isBirationalOntoImage(Ring,Ring,BasicList) := o->(R1, S1, bm)->(
    isBirationalOntoImage(ideal R1, ideal S1, bm, AssumeDominant=>o.AssumeDominant,Strategy=>o.Strategy,Verbose=>o.Verbose,HybridLimit=>o.HybridLimit, QuickRank=>o.QuickRank)
);

isBirationalOntoImage(RingMap) :=o->(f)->(
    isBirationalOntoImage(target f, source f, first entries matrix f, AssumeDominant=>o.AssumeDominant,Strategy=>o.Strategy,Verbose=>o.Verbose,HybridLimit=>o.HybridLimit, QuickRank=>o.QuickRank)
);

isBirationalOntoImageRees(Ring,Ring,BasicList) := o->(R1, S1, bm)->(
   isBirationalOntoImageRees(ideal R1, ideal S1, bm, AssumeDominant=>o.AssumeDominant,Strategy=>o.Strategy,Verbose=>o.Verbose, QuickRank=>o.QuickRank)
);

isBirationalOntoImageRees(RingMap) :=o->(f)->(
    isBirationalOntoImageRees(target f, source f, first entries matrix f, AssumeDominant=>o.AssumeDominant,Strategy=>o.Strategy,Verbose=>o.Verbose, QuickRank=>o.QuickRank)
);
isBirationalOntoImageSimis(Ring,Ring,BasicList) := o->(R1, S1, bm)->(
    isBirationalOntoImageSimis(ideal R1, ideal S1, bm, AssumeDominant=>o.AssumeDominant,Strategy=>o.Strategy,HybridLimit=>o.HybridLimit,Verbose=>o.Verbose, QuickRank=>o.QuickRank)
);

isBirationalOntoImageSimis(RingMap) :=o->(f)->(
    isBirationalOntoImageSimis(target f, source f, first entries matrix f, AssumeDominant=>o.AssumeDominant,Strategy=>o.Strategy,HybridLimit=>o.HybridLimit,Verbose=>o.Verbose, QuickRank=>o.QuickRank)
);


--*************main part of function




isBirationalOntoImageRees(Ideal,Ideal, BasicList) :=o->(di,im,bm)->(
    if isSameDegree(bm)==false then error "isBirationalOntoImageRees: Expected a list of homogeneous elements of the same degree";
    R:=ring di;
    S:=ring im;
    im1 := im;
    if (o.AssumeDominant == true) then (
        im1 =  im;
    )
    else (
        if (o.Verbose) then (
            print "isBirationalOntoImageRees: About to find the image of the map.  If you know the image, ";
            print "        you may want to use the AssumeDominant option if this is slow.";
        );

        im1 = idealOfImageOfMap(di, im, bm, QuickRank=>o.QuickRank);
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
     if (o.Verbose) then print "isBirationalOntoImageRees:  About to compute the Jacobian Dual Matrix,";
      if (o.Verbose) then print "if it is slow, run again and  set Strategy=>HybridStrategy or SimisStrategy.";
    --1/0;
    barJD:=jacobianDualMatrix(di1,im1,bm1,AssumeDominant=>true);--JacobianDual Matrix is another function in this package
      nc:=numColumns(transpose barJD);
     nr:=numRows(transpose barJD);
    if (o.Verbose) then print "isBirationalOntoImageRees: computed Jacobian Dual Matrix- barJD";
    if (o#Verbose ) then(
        print ( "Jacobain dual matrix has  " |nc|" columns  and   "|nr|" rows.");
    );
    jdd:=(numgens ambient Rlin1)-1;
    if (o.Verbose) then print "isBirationalOntoImageRees: is computing the rank of the  Jacobian Dual Matrix- barJD";

    --not(isSubset(minors(jdd,barJD),im1))
    if (o.QuickRank == true) then (
        isRankAtLeast(jdd, barJD, Strategy => StrategyGRevLexSmallestTerm, MaxMinors=>2)
    )
    else (
        rank barJD >= jdd
    )
);



--isBirationalOntoImageSimis
---***************************************************

isBirationalOntoImageSimis(Ideal,Ideal, BasicList) :=o->(di,im,bm)->(
   -- invList := inverseOfMap(target f, source f, first entries matrix f);
--    map(source f, target f, invList)
--    inverseOfMap(target f, source f, first entries matrix f, AssumeDominant=>o.AssumeDominant)
---*******************
    if (o.Verbose == true) then print "Starting inverseOfMapOntoImageSimis(SimisStrategy or HybridStrategy)";

    im1 := im;
    if (o.AssumeDominant == true) then (
        im1 =  im;
    )
    else (
        if (o.Verbose) then (
            print "isBirationalOntoImageSimis: About to find the image of the map.  If you know the image, ";
            print "        you may want to use the AssumeDominant option if this is slow.";
        );

        im1 = idealOfImageOfMap(di, im, bm, QuickRank=>o.QuickRank);
        if (o.Verbose === true) then print "isBirationalOntoImageSimis: Found the image of the map.";
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
    minorsCt := o.MinorsCount;
    if (o.MinorsCount === null) then ( --if the user didn't specify MinorsCount, we make some educated guesses
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

    flag := false;   --this boolian checks if it is birational
    giveUp := false;  --this checks if we giveup checkin birationality or not yet
    secdeg:=1;        --the second degree of rees equations
    jj := 1;
    M := null;
    while (giveUp == false) do (
	    if (o.Verbose === true) then print("isBirationalOntoImageSimis:  About to compute partial Groebner basis of rees ideal up to degree " | toString({1, secdeg}) | "." );

        if (secdeg < o.HybridLimit) then (
            M=gb(J,DegreeLimit=>{1,secdeg}); --instead of computing the whole Grob.
                                            --Baisis of J we only compute the parts of degree (1,m) or less,
        )
        else(
        if (o.Verbose === true) then print("isBirationalOntoImageSimis:  gave up, it will just compute the whole Groebner basis of the rees ideal.  Increase HybridLimit and rerun to avoid this." );
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
        if (o#Verbose ) then( print ( "isBirationalOntoImageSimis: Found Jacobian dual matrix (or a weak form of it), it has  " |nc|" columns  and about  "|nr|" rows.");
                            );
        if (giveUp == false) then(
            if (o.Verbose === true) then print "isBirationalOntoImageSimis: is computing the rank of the  Jacobian Dual Matrix- barJD";
            if (o.QuickRank == true) then (
                if (isRankAtLeast(jdd, barJD, Strategy => StrategyGRevLexSmallestTerm, MaxMinors=>minorsCt, Verbose=>o.Verbose)) then (
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
            if (o#Verbose ) then( print ( "isBirationalOntoImageSimis: Found Jacobian dual matrix (or a weak form of it), it has  " |nc|" columns  and   "|nr|" rows.");
                            );
            if (o.Verbose === true) then print "isBirationalOntoImageSimis: is computing the rank of the  Jacobian Dual Matrix- barJD";
            if (o.QuickRank == true) then (
                if (isRankAtLeast(jdd, barJD, Strategy => StrategyGRevLexSmallestTerm, MaxMinors=>minorsCt, Verbose=>o.Verbose)) then (
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

 inverseOfMap = method(Options => {AssumeDominant=>false, CheckBirational=>true, Strategy=>HybridStrategy, HybridLimit=>15, Verbose=>true, MinorsCount=>null, QuickRank=>true});
 inverseOfMapRees := method(Options => {AssumeDominant=>false, CheckBirational=>true, Strategy=>ReesStrategy, Verbose=>true,MinorsCount=>null, QuickRank=>true});
 inverseOfMapSimis := method(Options => {AssumeDominant=>false, CheckBirational=>true,  HybridLimit=>15, Verbose=>true,MinorsCount=>null, QuickRank=>true});
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
    minorsCt := o.MinorsCount;
    if (o.MinorsCount === null) then ( --if the user didn't specify MinorsCount, we make some educated guesses
        nn := #(gens ambient target f);
        if (nn < 6) then(
            minorsCt = 10;
        )
        else if (nn < 9) then (
            minorsCt = 6;
        )
        else if (nn < 12) then (
            minorsCt = 3;
        )
        else (
            minorsCt = 0;
        );
    );
    if ((o.Strategy == ReesStrategy) or (o.Strategy == SaturationStrategy)) then (
        inverseOfMapRees(f, QuickRank=>o.QuickRank, AssumeDominant=>o.AssumeDominant, CheckBirational=>o.CheckBirational, Strategy=>o.Strategy,Verbose=>o.Verbose, MinorsCount=>minorsCt)
    )
    else if (o.Strategy == SimisStrategy) then (
        inverseOfMapSimis(f, QuickRank=>o.QuickRank, AssumeDominant=>o.AssumeDominant, CheckBirational=>o.CheckBirational, HybridLimit=>infinity,Verbose=>o.Verbose, MinorsCount=>minorsCt)
    )
    else if (o.Strategy == HybridStrategy) then(
        inverseOfMapSimis(f, QuickRank=>o.QuickRank, AssumeDominant=>o.AssumeDominant, CheckBirational=>o.CheckBirational, HybridLimit=>o.HybridLimit,Verbose=>o.Verbose, MinorsCount=>minorsCt)
    )
  );

--**************other modes
inverseOfMap(Ideal,Ideal,BasicList) :=o->(di,im,bm)->(
    inverseOfMap( (ring di)/di, (ring im)/im, bm, o)
);

inverseOfMap(Ring,Ring,BasicList) := o->(R1, S1, bm)->(
    inverseOfMap(map(R1, S1, bm), o)
    );


inverseOfMapRees(Ideal,Ideal,BasicList) :=o->(di,im,bm)->(
    inverseOfMapRees( (ring di)/di, (ring im)/im, bm, o)
);

inverseOfMapRees(Ring,Ring,BasicList) := o->(R1, S1, bm)->(
    inverseOfMapRees(map(R1, S1, bm), o)
    );

--********************************main part Rees
inverseOfMapRees(RingMap) := o->(f)->(
   -- invList := inverseOfMap(target f, source f, first entries matrix f);
--    map(source f, target f, invList)
--    inverseOfMap(target f, source f, first entries matrix f, AssumeDominant=>o.AssumeDominant)
---*******************
    if (o.Verbose == true) then print "Starting inverseOfMapRees(ReesStrategy or SaturationStrategy)";
    if (o.AssumeDominant == false) then (
        if (o.Verbose) then (
            print "inverseOfMapRees: About to find the image of the map.  If you know the image, ";
            print "        you may want to use the AssumeDominant option if this is slow.";
        );
        f = mapOntoImage(f);
        if (o.Verbose === true) then print "inverseOfMapRees: Found the image of the map.";
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
     if (o.Verbose === true) then print "inverseOfMapRees: About to compute the Jacobian Dual Matrix";
    barJD:=jacobianDualMatrix(di1,im1,bm1,AssumeDominant=>true, Strategy=>o.Strategy);--JacobianDual Matrix is another function in this package
     if (o.Verbose === true) then print "inverseOfMapRees: Computed Jacobian Dual Matrix";
    --print "JD computed";
    jdd:=(numgens ambient Rlin1)-1;
    if (o.CheckBirational== true) then (
        if (o.QuickRank) then (
            if not (isRankAtLeast(jdd, barJD, Strategy => StrategyGRevLexSmallestTerm, MaxMinors=>2, Verbose=>o.Verbose)) then error "inverseOfMapRees: The map is not birational onto its image";
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
    if (o#Verbose ) then(
        print ( "Jacobain dual matrix has  " |nc|" columns  and about  "|nr|" rows.");
    );
    nonZMinor := null;
    if (o.MinorsCount > 0) then (
        if (o.Verbose == true) then print ("inverseOfMapRees: Looking for a nonzero minor. \r\n       If this fails, you may increase the attempts with MinorsCount => #");
        nonZMinor = getSubmatrixOfRank(jdd, barJD, MaxMinors => o.MinorsCount, Verbose=>o.Verbose);
        --nonZeroMinor(barJD,jdd,o.MinorsCount, Verbose=>o.Verbose);
    );
    if (nonZMinor === null) then (
        if (o.Verbose==true) then (
            if (o.MinorsCount > 0) then print "inverseOfMapRees: Failed to find a nonzero minor.  We now compute syzygies instead.";
            if (o.MinorsCount == 0) then print "inverseOfMapRees: MinorsCount => 0, so we now compute syzygies instead.";
            print "                   If this doesn't terminate quickly, you may want to try increasing the option MinorsCount.";
        );
        Inv =syz(transpose barJD,SyzygyLimit =>1);
        psi = map(source f, Rlin1, sub(transpose Inv, source f));
    )
    else (
        if (o.Verbose==true) then print "inverseOfMapRees: We found a nonzero minor.  If this doesn't terminate quickly, rerun with MinorsCount=>0.";
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


inverseOfMapSimis(RingMap) :=o->(f)->(
   -- invList := inverseOfMap(target f, source f, first entries matrix f);
--    map(source f, target f, invList)
--    inverseOfMap(target f, source f, first entries matrix f, AssumeDominant=>o.AssumeDominant)
---*******************
    if (o.Verbose == true) then print "Starting inverseOfMapSimis(SimisStrategy or HybridStrategy)";
    if ((o.CheckBirational == true) and (o.HybridLimit == infinity)) then print "Warning:  when using the current default SimisStrategy, the map must be birational.  If the map is not birational, this function will never terminate.";

    if (o.AssumeDominant == false) then (
        if (o.Verbose) then (
            print "inverseOfMapSimis: About to find the image of the map.  If you know the image, ";
            print "        you may want to use the AssumeDominant option if this is slow.";
        );
        f = mapOntoImage(f);
        if (o.Verbose === true) then print "inverseOfMapSimis: Found the image of the map.";
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
        if (o.Verbose === true) then print("inverseOfMapSimis:  About to compute partial Groebner basis of rees ideal up to degree " | toString({1, secdeg}) | "." );
        if (secdeg < o.HybridLimit) then (
            M=gb(J,DegreeLimit=>{1,secdeg}); --instead of computing the whole Grob.
                                               --Baisis of J we only compute the parts of degree (1,m) or less,
        )
        else( --we are running the hybrid strategy, so we compute the whole gb
            if (o.Verbose === true) then print("inverseOfMapSimis:  We give up.  Using the previous computations, we compute the whole \r\n        Groebner basis of the rees ideal.  Increase HybridLimit and rerun to avoid this." );
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
        --if (o.Verbose === true) then print("inverseOfMapSimis: computed barJD.");

        if (giveUp == false) then(            
            --if (rank barJD >= jdd) then (
            if (o.QuickRank == true) then (
                if (o.Verbose === true) then print("inverseOfMapSimis: About to check rank, if this is very slow, you may try turning QuickRank=>false." );
                if (isRankAtLeast(jdd, barJD, MaxMinors=>min(2, o.MinorsCount), Strategy=>StrategyGRevLexSmallestTerm)) then (
                    if (o.Verbose === true) then print("inverseOfMapSimis: We computed enough of the Groebner basis." );
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
                    if (not isRankAtLeast(jdd, barJD, Strategy => StrategyGRevLexSmallestTerm, MaxMinors=>min(2, o.MinorsCount), Verbose=>o.Verbose)) then error "inverseOfMapSimis: The map is not birational onto its image";
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
    if (o#Verbose ) then(
        print ( "inverseOfMapSimis: Found Jacobian dual matrix (or a weak form of it), it has  " |nc|" columns  and about  "|nr|" rows.");
    );

    nonZMinor := null;
    if (o.MinorsCount > 0) then (
        if (o.Verbose==true) then print "inverseOfMapSimis: Looking for a nonzero minor.\r\n        If this fails, you may increase the attempts with MinorsCount => #";
        nonZMinor = getSubmatrixOfRank(jdd, barJD, MaxMinors => o.MinorsCount, Verbose=>o.Verbose);
        --nonZeroMinor(barJD,jdd,o.MinorsCount, Verbose=>o.Verbose);
    );

    if (nonZMinor === null) then (
        if (o.Verbose==true) then (
            if (o.MinorsCount >  0) then print "inverseOfMapSimis: Failed to find a nonzero minor.  We now compute syzygies instead.";
            if (o.MinorsCount == 0) then print "inverseOfMapSimis: MinorsCount => 0, so we now compute syzygies instead.";
            print "                   If this doesn't terminate quickly, you may want to try increasing the option MinorsCount.";
        );
        Inv =syz(transpose barJD,SyzygyLimit =>1);
        psi = map(source f, Rlin1, sub(transpose Inv, source f));
    )
    else (
        if (o.Verbose==true) then print "inverseOfMapSimis: We found a nonzero minor.";
        Col = (nonZMinor)#1;
        SbarJD=submatrix(barJD,,Col);
        for i from 0 to jdd do Inv=append(Inv,(-1)^i*det(submatrix'(SbarJD,{i},)));
        psi=map(source f,Rlin1,matrix{Inv});
    );
    psi*phi
);


inverseOfMapSimis(Ideal,Ideal,BasicList) :=o->(di,im,bm)->(
    inverseOfMapSimis( (ring di)/di, (ring im)/im, bm, AssumeDominant=>o.AssumeDominant, CheckBirational=>o.CheckBirational,Verbose=>o.Verbose, MinorsCount=>o.MinorsCount)
);

inverseOfMapSimis(Ring,Ring,BasicList) := o->(R1, S1, bm)->(
    inverseOfMapSimis(map(R1, S1, bm), AssumeDominant=>o.AssumeDominant, CheckBirational=>o.CheckBirational,Verbose=>o.Verbose, MinorsCount=>o.MinorsCount)
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
        kk = JJ + I1;
    );
--        newMap := map(target f, ambient source f, matrix f);        
    map(target f, (S1)/kk, matrix f)
);

mapOntoImage(Ring, Ring, BasicList) := o -> (R,S,l)->(
        newMap := map(R, ambient S, l);
        mapOntoImage(newMap, o)
);

mapOntoImage(Ideal, Ideal, BasicList) := o -> (a,b,l)->(
        newMap := map((ring a)/a, (ring b)/b, l);
        mapOntoImage(newMap, o)
);

--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isEmbedding = method(Options => {AssumeDominant=>false, CheckBirational=>true, Strategy=>HybridStrategy,
	 HybridLimit=>15, Verbose=>true, MinorsCount=>0, QuickRank=>true});
 --checks whether a map is a closed embedding.

isEmbedding(Ideal, Ideal, BasicList) := o-> (a1, b1, f1)->(
        newMap := map((ring a1)/a1, (ring b1)/b1, f1);
        isEmbedding(newMap, o)
);

isEmbedding(Ring, Ring, BasicList) := o-> (R1, S1, f1)->(
        newMap:=map(R1,S1,f1);
        isEmbedding(newMap, o)
);

isEmbedding(RingMap):= o-> (f1)->(
    f2:=null;
    if (o.AssumeDominant==false) then(
        if (o.Verbose) then (
            print "isEmbedding: About to find the image of the map.  If you know the image, ";
            print "        you may want to use the AssumeDominant option if this is slow.";
        );        
        f2 = mapOntoImage(f1);
	)
    else (
	    f2=f1;
	);
    if (o.Verbose === true) then print "isEmbedding: Checking to see if the map is a regular map";
        flag := isRegularMap(f2);
        if (flag == true) then (
	        if (o.Verbose === true) then (print "isEmbedding: computing the inverse  map");

            try(h := inverseOfMap(f2, AssumeDominant=>true, QuickRank=>o.QuickRank, Strategy=>o.Strategy,HybridLimit=>o.HybridLimit, Verbose=>o.Verbose, MinorsCount=>o.MinorsCount); ) then
            (
	            if (o.Verbose === true) then print "isEmbedding: checking if the inverse map is a regular map";
        	    flag = isRegularMap(h);
	        )
            else(
	            flag=false
	        );
       );
       flag
);

--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 isSameMap = method(); --checks whether two rational maps are the same. Assumes domain is irreducible

 isSameMap(List, List) := (L1, L2) -> (
    theRing := ring first L1;
--    rank matrix(frac(theRing), {L1, L2}) == 1
    isSameMap(L1, L1, theRing)
 );

 isSameMap(List, List, Ring) := (L1, L2, R1) -> (
    rank matrix(frac(R1), {L1, L2}) == 1
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
    isSameMap(first entries matrix f1, first entries matrix f2, theRing)
--    isSameMapToPn( first entries matrix f1, first entries matrix f2)
 );



--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 jacobianDualMatrix = method(Options => {AssumeDominant=>false, Strategy=>ReesStrategy, QuickRank=>true});
--this the jacobian dual matrix of  a  rational map X -> Y.

--X = Proj R
--Y = Proj S
--This map is given by a list of elements in R, all homogeneous
--of the same degree.
--Below we have defining ideal of X = di
--defining ideal of Y = im
--list of elements = bm

jacobianDualMatrix(Ideal,Ideal,BasicList) :=o->(di,im,bm)->(
    if isSameDegree(bm)==false then error "jacobianDualMatrix: Expected a list of homogeneous elements of the same degree";
    R:=ring di;
    K:=coefficientRing R;
    S:=ring im;
    im1 := im;
    if (o.AssumeDominant == true) then (
        im1 =  im;
    )
    else (
        im1 = idealOfImageOfMap(di, im, bm, QuickRank=>o.QuickRank);
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

jacobianDualMatrix(Ring,Ring,BasicList) := o->(R1, S1, bm)->(
   jacobianDualMatrix(ideal R1, ideal S1, bm, AssumeDominant=>o.AssumeDominant)
    );

jacobianDualMatrix(RingMap) := o->(f)->(
   -- invList := inverseOfMap(target f, source f, first entries matrix f);
--    map(source f, target f, invList)
    jacobianDualMatrix(target f, source f, first entries matrix f, AssumeDominant=>o.AssumeDominant)
    );


--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--SourceInversionFactor  is an invariant associated to a rational map which is useful in computation of symbolic powers
sourceInversionFactor=method(Options => {AssumeDominant=>false, CheckBirational=>false, Strategy=>HybridStrategy,
	 HybridLimit=>15, Verbose=>true, MinorsCount=>0, QuickRank=>true});

sourceInversionFactor(RingMap):=o->(f)->(
    R:=  target f;
    x:=R_0;
    f2:=f;
    if (o.AssumeDominant==false) then(
    if (o.Verbose) then (
        print "sourceInversionFactor: About to find the image of the map.  If you know the image, ";
        print "        you may want to use the AssumeDominant option if this is slow.";
    );
        f2 = mapOntoImage(f);
    );

    invf2:=inverseOfMap(f2, AssumeDominant=>o.AssumeDominant, CheckBirational=>o.CheckBirational, Strategy=>o.Strategy,
    HybridLimit=>o.HybridLimit, Verbose=>o.Verbose, MinorsCount=>o.MinorsCount, QuickRank=>o.QuickRank);
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
    Headline => "rational maps between varieties",
    EM "RationalMaps", " is a package for computing things related to maps between projective varieties.",
    BR{},BR{},
    "It focuses on finding where a birational map is undefined, checking whether a map is a closed embedding, checking birationality and computing inverse maps",
    BR{},BR{},
    BOLD "Mathematical background:",BR{},
    UL {
	  {"A. V. Dória, S. H. Hassanzadeh, A. Simis,  ",EM "  A characteristic free criterion of birationality", ", Advances in Mathematics, Volume 230, Issue 1, 1 May 2012, Pages 390-413."},
	  {"A. Simis, ",EM "  Cremona Transformations and some Related Algebras", ", Journal of Algebra, Volume 280, Issue 1, 1 October 2004, Pages 162–179"},
	},
    BOLD "Functionality overlap with other packages:\n\n",BR{},BR{},
    BOLD "Parametrization.m2",
      ":  While the package", TT "Parametrization", " focuses on mostly on curves, it also includes a function ", TT "invertBirationalMap", "
      which has the same functionality as ", TO "inverseOfMap", ".  On the other hand, these two functions were implemented somewhat differently and so sometimes one function can be substantially faster than the other.\n", BR{}, BR{},
    BOLD "Cremona.m2",
    ":  The package ", TT "Cremona", " focuses on  fast probabilistic computations in general cases and  deterministic computations for special
     kinds of maps from projective space.  More precisely, ",BR{},
    UL {
        {TT "isBirational", " gives a probabilisitc answer to the question of whether a map between varieties is birational.  Furthermore, if the
	     source is projective space, then ", TT "degreeOfRationalMap", " with ", TT   "MathMode=>true", " can give a deterministic answer.
	      In some cases, the speed of the latter  is comparable with ", TO "isBirationalMap", " with ", TT   "AssumeDominant=>true." },
        {TT "inverseMap", " gives a  fast computation of the inverse of a birational map if the source is projective space ", EM " and ",
	     "the map has maximal linear rank.   In some cases, even if the map has maximal linear rank, our function ", TO "inverseOfMap",
	       " appears to be competitive however.  If you pass inverseMap a map not from projective space, then it calls a modified version ",
	      TT "invertBirationalMap", " from ", TT "Parametrization", "."},
    },
}

--***************************************************************
document {
  Key => {[isBirationalMap,Verbose],
	  [isBirationalOntoImage,Verbose],
	   [isEmbedding, Verbose],
--	  [relationType,Verbose],
	  [inverseOfMap, Verbose],
	  [sourceInversionFactor, Verbose]},
    Headline => "generate informative output",
    "If this option is set, functions will generate additional output. Default is true. "}

--***************************************************************

document{
    Key=>{CheckBirational, [isEmbedding, CheckBirational],
	[inverseOfMap, CheckBirational],
	[sourceInversionFactor, CheckBirational]},
    Headline=> "If true, functions will check birationality.",
    Usage =>"  CheckBirational=>b",
      "If true, inverseOfMap, isEmbedding and sourceInversionFactor  will check whether the passed map is birational.
      If it is not birational, it will throw an error."

}
--***************************************************************

document{
    Key=>{HybridLimit, [isBirationalMap,HybridLimit],
       	  [inverseOfMap, HybridLimit],
          [isBirationalOntoImage,HybridLimit],
	  [isEmbedding, HybridLimit],
	  [sourceInversionFactor, HybridLimit]},
    Headline=>"An option to control HybridStrategy",
       "By increasing the HybridLimit value (default 15), you can weight
       HybridStrategy it more towards SimisStrategy.
	     Infinity will behave just like SimisStrategy.",
    SeeAlso=>
        HybridStrategy
}
--***************************************************************
document{
    Key=>{AssumeDominant, [isBirationalMap, AssumeDominant],
	[isBirationalOntoImage, AssumeDominant],
	[isEmbedding, AssumeDominant],
	[jacobianDualMatrix,AssumeDominant],
	[inverseOfMap, AssumeDominant],
	[sourceInversionFactor, AssumeDominant] },
    Headline=>"
        If true, certain functions assume that the map from X to Y is dominant.",
    Usage=>"AssumeDominant=>b",
       " If true, certain functions assume that $f : X \to Y$ is dominant.
         In other words that the closure of $f(X)$ is equal to $Y$.
	  In practice, this means that a kernel of a ring map will
	   not be computed.",
}
--***************************************************************

document{
    Key=>{MinorsCount, [isEmbedding, MinorsCount],
	[inverseOfMap, MinorsCount],
	[sourceInversionFactor,MinorsCount], 
    [isBirationalOntoImage, MinorsCount],
    [isBirationalMap, MinorsCount] },
    Headline=>" An option controlling the behavior of isBirational and inverseOfMap
     (and other functions which call those).",
            "One of the ways to invert a map is to find a nonzero minor of a variant of the jacobialDualMatrix.
	     This function controls how many (randomly chosen) minors to check before switching to another strategy (invovling computing a syzygy).
	     Setting it to zero will mean no minors are checked.
	     If it is left as null (the default), the functions will try to make an educated guess as to how big to make this,
	     depending on varieties you are working with.",
    SeeAlso=>
        inverseOfMap
}

document{
    Key=>{QuickRank, [isEmbedding, QuickRank],
	[inverseOfMap, QuickRank],
	[isBirationalMap,QuickRank],
    [isBirationalOntoImage, QuickRank],
    [sourceInversionFactor, QuickRank],
    [idealOfImageOfMap, QuickRank],
    [jacobianDualMatrix, QuickRank],
    [mapOntoImage, QuickRank]
    },
    Headline=>" An option for computing how rank is computed",
            "If set to true, then checking if rank is at least a certain number will be computed via the package", TT "FastLinAlg",
    SeeAlso=>
        inverseOfMap
}
--***************************************************************
document{
    Key=>{[sourceInversionFactor, Strategy],
	[isBirationalMap, Strategy],
	[isBirationalOntoImage,Strategy],
	[jacobianDualMatrix,Strategy],
	[isEmbedding, Strategy],
--	[relationType,Strategy],
	[inverseOfMap, Strategy]
	 },
    Headline=>" Determines the desired Strategy in each function.",
       "In sourceInversionFactor, isBirationalMap, isBirationalOntoImage,
	    isEmbeddinga and inverseOfMap, Strategy may assumed any of three options
	    ReesStrategy, SimisStrategy or  HybridStrategy (default). These functions as well as 
	    jacobianDualMatrix may also attain the Strategy=>SaturationStrategy or ReesStrategy (default).  ",

}
--***************************************************************
document{
    Key=>{ SaturateOutput, [baseLocusOfMap, SaturateOutput]},
    Headline =>"If false, certain functions will not saturate their output.",
    Usage =>"SaturateOutput=>b",
    "  If ", TT "SaturateOutput"," is ", TT "true"," (the default), then functions will saturate their output.
    Otherwise they will not.  It may be beneficial not to saturate in certain circumstances.",
}
--***************************************************************


--***************************************************************

doc ///
    Key
        HybridStrategy
    Headline
        A strategy for inverseOfMap,  isBirationalMap and isEmbedding.
    Description
    	Text
            It is a valid value for the Strategy Option for inverseOfMap.
	    This is currently the default strategy.  It is a combination of ReesStrategy and SimisStrategy.
	    By increasing the HybridLimit value (default 15), you can weight it more towards SimisStrategy.
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
        A strategy for inverseOfMap, isBirationalMap, and is Embedding.
    Description
    	Text
            It is a valid value for the Strategy Option for inverseOfMap (and other functions). By choosing Strategy=>ReesStrategy, the equation of the
	    ideal of definition of the Rees algebra are generated by the known elimination technique.
	    E.g. Vasconcelos, Rees algebras, multiplicities, algorithms. Springer Monographs in Mathematics. Springer-Verlag, Berlin, 2005.
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
        A strategy for inverseOfMap, isBirationalMap, isEmbedding.
    Description
    	Text
            It is a valid value for the Strategy Option for inverseOfMap (and other functions). By choosing Strategy=>SaturationStrategy,
	    the equation of the ideal of definition of the Rees algebra are generated by saturating the ideal of definition of the symmetric algebra into a non-zero element.
	    Notice that in this package (and in particular in this Strategy Option) the rings are assumed to be integral domains.
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
        A strategy for inverseOfMap, isBirationalMap and isEmbedding.
    Description
    	Text
            It is a valid value for the Strategy Option for inverseOfMap (and other functions). Considering the bigraded structure of the
	    equations of the ideal of definition of Rees algerba, SimisStrategy is looking for all Grobner basis with first degree 1. The advantage
	    of this restriction is that only this part of the Rees ideal is enough to decide Birationality and to compute the inverse map; so that
	    this Strategy reduces the number of computations. A disadvatage of this Strategy is that if the given map is not birational this Strategy may never
	    end because the jacobianDualMatrix will not attain its maximum rank. To circumvent this problem we consider  HybridStrategy.
    SeeAlso
        ReesStrategy
        SaturationStrategy
        HybridStrategy
///

--***************************************************************



doc ///
    Key
        isBirationalMap
        (isBirationalMap, Ideal, Ideal, BasicList)
        (isBirationalMap, Ring, Ring, BasicList)
        (isBirationalMap, RingMap)
       -- [isBirationalMap, AssumeDominant]
       --[isBirationalMap, Strategy]
	--[isBirationalMap,Verbose]
	--[isBirationalMap,HybridLimit]
    Headline
        Checks if a map between projective varieties is birational.
    Usage
        val = isBirationalMap(a,b,f)
        val = isBirationalMap(R,S,f)
        val = isBirationalMap(Pi)
    Inputs
        a:Ideal
            defining equations for X
        b:Ideal
            defining equations for Y
        f:BasicList
            A list of where to send the variables in the ring of b, to in the ring of a.
        R:Ring
            the homogeneous coordinate ring of X
        S:Ring
            the homogeneous coordinate ring of Y
        Pi:RingMap
            A ring map S to R corresponding to X mapping to Y
    Outputs
        val:Boolean
            true if the map is birational, false if otherwise
    Description
        Text
            This checks if a map between projective varieties is birational.  There are a number of ways to call this.  A simple one is to pass the function a map between two graded rings.  In this case, the variables should be sent to elements of a single fixed degree.  The option {\tt AssumeDominant} being true will cause the function to assume that the kernel of the associated ring map is zero (default value is false).  The target and source must be varieties, in particular their defining ideals must be prime.  Let's check that the plane quadratic cremona transformation is birational.
        Example
            R=QQ[x,y,z];
            S=QQ[a,b,c];
            Pi = map(R, S, {x*y, x*z, y*z});
            isBirationalMap(Pi, Verbose=>false, Strategy=>SimisStrategy )
        Text
            We can also verify that a cover of $P^1$ by an elliptic curve is not birational.
        Example
            R=QQ[x,y,z]/(x^3+y^3-z^3);
            S=QQ[s,t];
            Pi = map(R, S, {x, y-z});
            isBirationalMap(Pi, Verbose=>false)
        Text
            Note the Frobenius map is not birational.
        Example
            R = ZZ/5[x,y,z]/(x^3+y^3-z^3);
            S = ZZ/5[a,b,c]/(a^3+b^3-b^3);
            h = map(R, S, {x^5, y^5, z^5});
            isBirationalMap(h, Strategy=>SaturationStrategy)
    SeeAlso
        isBirationalOntoImage
    Caveat
        Also see the very fast probabilisitc birationality checking of the Cremona package: isBirational
///
--***************************************************************

doc ///
        Key
		isBirationalOntoImage
		(isBirationalOntoImage, Ideal, Ideal, BasicList)
		(isBirationalOntoImage, Ring, Ring, BasicList)
		(isBirationalOntoImage, RingMap)
        Headline
                Checks if a map between projective varieties is birational onto its image.
        Usage
                val = isBirationalOntoImage(a,b,f)
                val = isBirationalOntoImage(R,S,f)
                val = isBirationalOntoImage(Pi)
        Inputs
                a:Ideal
                        defining equations for X
                b:Ideal
                        defining equations for Y
                f:BasicList
                        A list of where to send the variables in the ring of b, to in the ring of a.
                R:Ring
                        the homogeneous coordinate ring of X
                S:Ring
                        the homogeneous coordinate ring of Y
                Pi:RingMap
                        A ring map S to R corresponding to X mapping to Y
        Outputs
                val:Boolean
                        true if the map is birational onto its image, false if otherwise
        Description
            Text
                This checks whether $f : X \to Y$ is birational onto its image.  We do this by computing the image and then calling {\tt isBirationalOntoImage}.  The option {\tt AssumeDominant} being true will cause the function to assume that the kernel of the associated ring map is zero (default value is false).  The source must be a variety, in particular its defining ideals must be prime.  In the following example, the map is not birational, but it is birational onto its image.
            Example
                R=QQ[x,y];
                S=QQ[a,b,c,d];
                Pi = map(R, S, {x^3, x^2*y, x*y^2, y^3});
                isBirationalOntoImage(Pi, Verbose=>false)
                isBirationalMap(Pi,  Verbose=>false)
            Text
                Sub-Hankel matrices have homaloidal determinants.
            Example
                A = QQ[z_0..z_6];
                H=map(A^4,4,(i,j)->A_(i+j));
                SH=sub(H,{z_5=>0,z_6=>0})
                sh=map(A, A, transpose jacobian ideal det SH );
                isBirationalOntoImage(sh, Verbose=>false)
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
		(idealOfImageOfMap,Ideal,Ideal,Matrix)
		(idealOfImageOfMap,Ideal,Ideal,BasicList)
		(idealOfImageOfMap,Ring,Ring,Matrix)
		(idealOfImageOfMap,Ring,Ring,BasicList)
		(idealOfImageOfMap,RingMap)
		[idealOfImageOfMap, Verbose]
	Headline
		Finds defining equations for the image of a rational map between varieties or schemes
	Usage
		im = idealOfImageOfMap(a,b,f)
		im = idealOfImageOfMap(a,b,g)
		im = idealOfImageOfMap(R,S,f)
		im = idealOfImageOfMap(R,S,g)
		im = idealOfImageOfMap(p)
	Inputs
		a:Ideal
			defining equations for X
		b:Ideal
			defining equations for Y
		f:Matrix
                        projective rational map given by polynomial representatives
		g:BasicList
			projective rational map given by polynomial representatives
		R:Ring
			coordinate ring of X
		S:Ring
			coordinate ring of Y
		p:RingMap
			projective rational map given by polynomial representatives
	Outputs
		im:Ideal
			defining equations for the image of f
	Description
		Text
			Given $f : X \to Y \subset P^N$, this returns the defining ideal of $f(x) \subseteq P^N$. It should be noted for inputs that all rings are quotients of polynomial rings, and all ideals and ring maps are of these.  In particular, this function returns an ideal defining a subset of the  the ambient projective space of the image.  In the following example we consider the image of $P^1$ inside $P^1 \times P^1$.
		Example
			S = QQ[x,y,z,w];
			b = ideal(x*y-z*w);
			R = QQ[u,v];
			a = ideal(sub(0,R));
			f = matrix {{u,0,v,0}};
			idealOfImageOfMap(a,b,f)
		Text
			This function frequently just calls {\tt ker} from Macaulay2.  However, if the target of the ring map is a polynomial ring, then it first tries to verify if the ring map is injective.  This is done by computing the rank of an appropriate jacobian matrix.
///
--***************************************************************

doc ///
	Key
		jacobianDualMatrix
		(jacobianDualMatrix,Ideal,Ideal,BasicList)
		(jacobianDualMatrix,Ring,Ring,BasicList)
		(jacobianDualMatrix,RingMap)
	--	[jacobianDualMatrix,AssumeDominant]
	--	[jacobianDualMatrix,Strategy]
	Headline
		Computes the Jacobian Dual Matrix, a matrix whose kernel describing the syzygies of the inverse map.
	Usage
		M = jacobianDualMatrix(a,b,g)
		M = jacobianDualMatrix(R,S,g)
		M = jacobianDualMatrix(p)
	Inputs
		a:Ideal
			defining equations for X
		b:Ideal
			defining equations for Y
		g:BasicList
			projective rational map given by polynomial representatives
		R:Ring
			coordinate ring of X
		S:Ring
			coordinate ring of Y
		p:RingMap
			projective rational map given by polynomial representatives
	Outputs
		M:Matrix
			Returns a matrix over the coordinate ring of the image, the kernel of this matrix
			 describing the syzygies of the inverse map, if it exists.
	Description
		Text
			This is mostly an internal function which is used when checking if a map is birational and when computing the inverse map.  If the {\tt AssumeDominant} option is set to {\tt true}, it assumes that the kernel of the associated ring map is zero (default value is false).  Valid values for the {\tt Strategy} option are {\tt ReesStrategy} and {\tt SaturationStrategy}.  For more information, see Doria, Hassanzadeh, Simis, A characteristic-free criterion of birationality.  Adv. Math. 230 (2012), no. 1, 390–413.
		Example
                       R=QQ[x,y];
                       S=QQ[a,b,c,d];
                       Pi = map(R, S, {x^3, x^2*y, x*y^2, y^3});
		       jacobianDualMatrix(Pi, Strategy=>SaturationStrategy)
///
--***************************************************************
--***************************************************************


doc ///
        Key
                mapOntoImage
                (mapOntoImage, RingMap)
                (mapOntoImage, Ideal, Ideal, BasicList)
                (mapOntoImage, Ring, Ring, BasicList)
        Headline
                Given a map of rings, correspoing to X mapping to Y, this returns the map of rings corresponding to X mapping to f(X).
        Usage
                h = mapOntoImage(f)
                h = mapOntoImage(a,b,l)
                h = mapOntoImage(R,S,l)
        Inputs
                a:Ideal
                        defining equations for X
                b:Ideal
                        defining equations for Y
		l:BasicList
                        projective rational map given by polynomial represenatives of the same degree
                f:RingMap
                        the ring map corresponding to $f : X \to Y$
                R:Ring
                        coordinate ring for X
                S:Ring
                        coordinate ring for Y

        Outputs
                h:RingMap
			the map of rings corresponding to $f : X \to f(X)$.
	Description
	        Text
	                This function is really simple, given $S \to R$, this just returns $S/kernel \to R$.
	        Example
	                R = QQ[x,y];
	                S = QQ[a,b,c];
	                f = map(R, S, {x^2, x*y, y^2});
	                mapOntoImage(f)
	                mapOntoImage(R,S,{x^2,x*y,y^2})
///
--***************************************************************

doc ///
        Key
                isEmbedding
                (isEmbedding, RingMap)
                (isEmbedding, Ideal, Ideal, BasicList)
                (isEmbedding, Ring, Ring, BasicList)
             --   [isEmbedding, AssumeDominant]
                --[isEmbedding, CheckBirational]
                --[isEmbedding, HybridLimit]
               -- [isEmbedding, Strategy]
               -- [isEmbedding, MinorsCount]
                --[isEmbedding, Verbose]
        Headline
                Checks whether a map of projective varieties is a closed embedding.
        Usage
                val = isEmbedding(f)
                val = isEmbedding(a,b,l)
                val = isEmbedding(R,S,l)
        Inputs
                a:Ideal
                        defining equations for X
                b:Ideal
                        defining equations for Y
		l:BasicList
                        projective rational map given by polynomial represenatives of the same degree
                f:RingMap
                        the ring map corresponding to $f : X \to Y$
                R:Ring
                        coordinate ring for X
                S:Ring
                        coordinate ring for Y

        Outputs
                val:Boolean
			true if the map is an embedding, otherwise false.
	Description
	        Text
	                Given a map of rings, correspoing to $f : X \to Y$, this determines if this map embeds $X$ as a closed subscheme into $Y$.  The target and source must be varieties, in particular their defining ideals must be prime.  Consider the Veronese embedding.
	        Example
	                R = ZZ/7[x,y];
	                S = ZZ/7[a,b,c];
	                f = map(R, S, {x^2, x*y, y^2});
	                isEmbedding(f, Verbose=>false)
	        Text
	                Now consider the projection from a point on the plane to the line at infinity.
	        Example
	                R=QQ[x,y,z];
	                S=QQ[a,b];
	                f=map(R, S, {y,z});
	                isEmbedding(f, Verbose=>false)
	        Text
	                That is obviously not an embedding.  It is even not an embedding when we restrict to a quadratic curve, even though it is a regular map.
	        Example
	                R=QQ[x,y,z]/(x^2+y^2-z^2);
	                S=QQ[a,b];
	                f=map(R,S, {y,z});
	                isRegularMap(f)
	                isEmbedding(f)
	        Text
	                If the option {\tt Verbose} is set to {\tt true}, the function will describe what it is doing at each step.
	        Text
	                If the option {\tt AssumeDominant} is set to {\tt true}, the function won't compute the kernel of the ring map.  Otherwise it will.
	        Text
	                The remaining options, {\tt Strategy}, {\tt HybridLimit}, {\tt MinorsCount}, and {\tt CheckBirational} are simply passed when this function calls {\tt inverseOfMap}.  Note, this function, {\tt isEmbedding}, will only behave properly if {\tt CheckBirational} is set to {\tt true}.
///

--***************************************************************


doc ///
    Key
        baseLocusOfMap
        (baseLocusOfMap, Matrix)
        (baseLocusOfMap, List)
        (baseLocusOfMap, RingMap)
--        [baseLocusOfMap, SaturateOutput]
    Headline
        Computes base locus of a map from a projective variety to projective space
    Usage
        I = baseLocusOfMap(M)
        I = baseLocusOfMap(L)
        I = baseLocusOfMap(h)
    Inputs
        M: Matrix
            Row matrix whose entries correspond to the coordinates of your map to projective space.
        L: List
            A list whose entries correspond to the coordinates of your map to projective space.
        h: RingMap
            A ring map corresponding to a map of projective varieties.
    Outputs
        I: Ideal
            The saturated defining ideal of the baselocus of the corresponding maps.
    Description
        Text
            This defines the locus where a given map of projective varieties is not defined.  If the option {\tt SaturateOutput} is set to {\tt false}, the output will not be saturated.  The default value is true.  Consider the following rational map from $P^2$ to $P^1$
        Example
            R = QQ[x,y,z];
            S = QQ[a,b];
            f = map(R, S, {x,y});
            baseLocusOfMap(f)
        Text
            Observe it is not defined at the point [0:0:1], which is exactly what one expects.  However, we can restrict the map to a curve on $P^2$ and then it will be defined everywhere.
        Example
            R=QQ[x,y,z]/(y^2*z-x*(x-z)*(x+z));
            S=QQ[a,b];
            f=map(R,S,{x,y});
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
--	[relationType,Verbose]
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
        (isSameMap, List,List)
        (isSameMap, List,List,Ring)
        (isSameMap, RingMap,RingMap)
    Headline
        Checks whether two maps to projective space are really the same
    Usage
        b = isSameMap(L1,L2)
        b = isSameMap(L1,L2, R1)
        b = isSameMap(f1, f2)
    Inputs
        L1: List
            The homogeneous forms that define the first map.
        L2: List
            The homogeneous forms that define the second map.
        R1: Ring
            The ring in which the homogeneous forms should live.
        f1: RingMap
            The first map.
        f2: RingMap
            The second map.
    Outputs
        b: Boolean
            True if the maps are the same, false otherwise.
    Description
        Text
            Checks whether two maps, from the same variety, to projective space are really the same. If you pass it two ring maps, it will check whether the source and targets are really the same.
        Example
            R=QQ[x,y,z];
            S=QQ[a,b,c];
            L1={y*z,x*z,x*y};
            L2={x*y*z,x^2*z,x^2*y};
            isSameMap(L1,L2)
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
        (isRegularMap, Matrix)
        (isRegularMap, List)
        (isRegularMap, RingMap)

    Headline
        Checks whether a map to projective space is regular
    Usage
        b = isRegularMap(M)
        b = isRegularMap(L)
        b = isRegularMap(f)
    Inputs
        M: Matrix
            Row matrix whose entries correspond to the coordinates of your map to projective space
        L: List
            A list whose entries correspond to the coordinates of your map to projective space
        f: RingMap
            A ring map corresponding to a map of projective varieties.
    Outputs
        b: Boolean
    Description
        Text
            This function just runs baseLocusOfMap(M) and checks if the ideal defining the base locus is the whole ring.
        Example
            P5 = QQ[a..f];
            M = matrix{{a,b,c},{d,e,f}};
            segreProduct = P5/minors(2, M);
            blowUpSubvar = segreProduct/ideal(b - d);
            f = {a, b, c};
            isRegularMap({a,b,c})
///
--***************************************************************

doc ///
    Key
        inverseOfMap
		(inverseOfMap, Ideal, Ideal, BasicList)
		(inverseOfMap, Ring, Ring, BasicList)
		(inverseOfMap, RingMap)
--		[inverseOfMap, AssumeDominant]
--		[inverseOfMap, Strategy]
--               [inverseOfMap, CheckBirational]
--               [inverseOfMap, HybridLimit, MinorsCount]
--		[inverseOfMap, Verbose]
--		[inverseOfMap, MinorsCount]
    Headline
        Computes the inverse map of a given birational map between projective varieties. Returns an error if the map is not birational onto its image.
    Usage
        f = inverseOfMap(I, J, L)
        f = inverseOfMap(R, S, L)
        f = inverseOfMap(g)
    Inputs
        I: Ideal
            Defining ideal of source
        J: Ideal
            Defining ideal of target
        L: List
            List of polynomials that define the coordinates of your birational map
        g: RingMap
            Your birational map $f : X \to Y$.
    Outputs
        f: RingMap
            Inverse function of your birational map, $f(X) \to X$.
    Description
        Text
            Given a map $f : X \to Y$, this finds the inverse of your birational map $f(X) \to X$ (if it is birational onto its image).  The target and source must be varieties, in particular their defining ideals must be prime.
        Text
            If {\tt AssumeDominant} is set to {\tt true} (default is {\tt false}) then it assumes that the map of varieties is dominant, otherwise the function will compute the image by finding the kernel of $f$.
        Text
            The {\tt Strategy} option can be set to {\tt HybridStrategy} (default), {\tt SimisStrategy}, {\tt ReesStrategy}, or {\tt SaturationStrategy}.  Note {\tt SimisStrategy} will never terminate for non-birational maps. If {\tt CheckBirational} is set to {\tt false} (default is {\tt true}), then no check for birationality will be done.  If it is set to {\tt true} and the map is not birational, an error will be thrown if you are not using {\tt SimisStrategy}. The option {\tt HybridLimit} can weight the {\tt HybridStrategy} between {\tt ReesStrategy} and {\tt SimisStrategy}, the default value is {\tt 15} and increasing it will weight towards {\tt SimisStrategy}.
        Example
            R = ZZ/7[x,y,z];
            S = ZZ/7[a,b,c];
            h = map(R, S, {y*z, x*z, x*y});
            inverseOfMap (h, Verbose=>false)
        Text
            Notice that the leading minus signs do not change the projective map.  Next let us compute the inverse of the blowup of $P^2$ at a point.
        Example
            P5 = QQ[a..f];
            M = matrix{{a,b,c},{d,e,f}};
            blowUpSubvar = P5/(minors(2, M)+ideal(b - d));
            h = map(blowUpSubvar, QQ[x,y,z],{a, b, c});
            g = inverseOfMap(h, Verbose=>false)
            baseLocusOfMap(g)
            baseLocusOfMap(h)
        Text
            The next example, is a Birational map on $\mathbb{P}^4$. 
        Example
            Q=QQ[x,y,z,t,u];
            phi=map(Q,Q,matrix{{x^5,y*x^4,z*x^4+y^5,t*x^4+z^5,u*x^4+t^5}});
            time inverseOfMap(phi,CheckBirational=>false)
        Text
            Finally, we do an example of plane Cremona maps whose source is not minimally embedded.
        Example
            R=QQ[x,y,z,t]/(z-2*t);
            F = {y*z*(x-z)*(x-2*y), x*z*(y-z)*(x-2*y),y*x*(y-z)*(x-z)};
            S = QQ[u,v,w];
            h = map(R, S, F);
            g = inverseOfMap h
            use S;
            (g*h)(u)*v==(g*h)(v)*u
            (g*h)(u)*w==(g*h)(w)*u
            (g*h)(v)*w==(g*h)(w)*v
        Text
            Notice the last checks are just verifying that the composition g*h agrees with the identity.
    Caveat
        Only works for irreducible varieties right now.  Also see the function inverseMap in the package Cremona, which for certain types of maps from projective space is sometimes faster.  Additionally, also compare with the function invertBirationalMap of the package Parametrization.
///
--***************************************************************

doc ///
    Key
        sourceInversionFactor
		(sourceInversionFactor, RingMap)
--     	[sourceInversionFactor, AssumeDominant]
--       [sourceInversionFactor, Strategy]
--       [sourceInversionFactor, CheckBirational]
--       [sourceInversionFactor, HybridLimit]
--       [sourceInversionFactor, Verbose]
--	 [sourceInversionFactor,MinorsCount]
    Headline
        Computes the the common factor among the the components of the composition of the inverse map and the original map.
    Usage
         s = sourceInversionFactor(g)
    Inputs
        g: RingMap
            Your birational map $f : X \to Y$.
    Outputs
        s: RingElement
             an element of the coordinate ring of $X$ .
    Description
        Text
            Given a map $f : X \to Y$, this finds common factor among the the components of, $f^{(-1)}$ composed with $f$, which is an element of the coordinate ring of $X$ .
        Text
            If {\tt AssumeDominant} is set to {\tt true} (default is {\tt false}) then it assumes that the map of varieties is dominant, otherwise the function will compute the image by finding the kernel of $f$.
        Text
            The {\tt Strategy} option can be set to {\tt HybridStrategy} (default), {\tt SimisStrategy}, {\tt ReesStrategy}, or {\tt SaturationStrategy}.  Note {\tt SimisStrategy} will never terminate for non-birational maps. If {\tt CheckBirational} is set to {\tt false} (default is {\tt true}), then no check for birationality will be done.  If it is set to {\tt true} and the map is not birational, an error will be thrown if you are not using {\tt SimisStrategy}. The option {\tt HybridLimit} can weight the {\tt HybridStrategy} between {\tt ReesStrategy} and {\tt SimisStrategy}, the default value is {\tt 15} and increasing it will weight towards {\tt SimisStrategy}.
        Example
            R = ZZ/7[x,y,z];
            S = ZZ/7[a,b,c];
            h = map(R, S, {y*z, x*z, x*y});
            sourceInversionFactor h
        Example
             S=QQ[a,b,c,d];
             g=(b^2-a*c)*c*d;
             phi=map(S,S,transpose jacobian ideal g);
             sourceInversionFactor(phi, Verbose=>false)


    Caveat
        Only works for irreducible varieties right now.  Also see the function inverseMap in the package Cremona, which for certain types of maps from projective space is sometimes faster.  Additionally, also compare with the function invertBirationalMap of the package Parametrization.
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
	im = idealOfImageOfMap(a,b,f);
	assert (im == ideal(y,w));
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
	M = matrix{{x^2*y, x^2*z, x*y*z}}
	I = ideal(x*y, y*z, x*z)
	assert(I == baseLocusOfMap(M))
///

TEST ///	--test #4
    R = QQ[x,y,z]
	L = {x^2*y, x^2*z, x*y*z}
	I = ideal(x*y, y*z, x*z)
	assert(I == baseLocusOfMap(L))
///

TEST /// --test #5
	-- reducible source

	R = QQ[x,y,z]/(x*y)
	M = matrix{{x^2, x*y, y^2}}
	I = ideal(x,y)
	assert(I == baseLocusOfMap(M))

    -- we should have a test for when that kernel is not a cyclic module
///


	-------------------------------------
	----- isRegularMap -----------------
	-------------------------------------
TEST /// --test #6
	R = QQ[x,y,z,w]/(x*y - z*w)
	M = matrix{{sub(1,R), 0, 0}}
	assert(isRegularMap(M))
///

TEST /// --test #7
    R = QQ[x,y]/(x*y)
    M = matrix{{x,y}}
    assert(isRegularMap(M))
///

TEST /// --test #8
    R = QQ[x,y,z]/(x^3 + y^3 - z^3)
    M = matrix{{(y-z)*x, x^2}}
    assert(isRegularMap(M) == true)
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
    segreProduct = P5/minors(2, M);
    blowUpSubvar = segreProduct/ideal(b - d);
    f = {a, b, c};
    assert(isRegularMap(matrix{{a,b,c}}) == true)
///

	-------------------------------------
	----- isBirationalOntoImage  --------
	-------------------------------------

TEST /// --test #12 (a map from the blowup of P^2 at a point back down to P^2)
    P5 = QQ[a..f];
    M = matrix{{a,b,c},{d,e,f}};
    blowUpSubvar = P5/(minors(2, M) + ideal(b-d));
    f = {a, b, c};
    assert(isBirationalOntoImage(blowUpSubvar, QQ[x,y,z], f, Verbose=>false) == true)
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
    f = inverseOfMap(R, S, {a,b,c});
    g = inverseOfMap(R, S, {a,b,c},Strategy=>ReesStrategy);
    assert( (isSameMap(first entries matrix f, {x^2, x*y, x*z, y*z})) and (isSameMap(first entries matrix g, {x^2, x*y, x*z, y*z})) )
///

TEST /// --test #25 (quadratic cremona)
    R = ZZ/11[x,y,z];
    S = ZZ/11[a,b,c];
    h = map(R, S, {y*z, x*z, x*y});
    g = inverseOfMap(h,AssumeDominant=>true);
    f = inverseOfMap(h,AssumeDominant=>true, Strategy=>ReesStrategy);
    assert( (isSameMap(first entries matrix g, {b*c, a*c, a*b})) and (isSameMap(first entries matrix f, {b*c, a*c, a*b})) )
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
     --let's parameterize the nodal plane cubic
     P2 = QQ[x,y,z]
     C = QQ[a,b]
     h = map(C, P2, {b*a*(a-b), a^2*(a-b), b^3})
     assert((isBirationalMap h == false) and (isBirationalOntoImage h == true) and (isEmbedding(h) == false) and (isRegularMap inverseOfMap h == false))
///

TEST /// --test #29, map from genus 3 curve to projective space
    needsPackage "Divisor";
    C = QQ[x,y,z]/(x^4+x^2*y*z+y^4+z^3*x);
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
----Version information----
--0.1  First version.
--0.2  Substantial improvements in speed and documentation.
--0.21 Minor changes especially to documentation.

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
