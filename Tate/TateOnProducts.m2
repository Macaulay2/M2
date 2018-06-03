///
restart
uninstallPackage"TateOnProducts"
restart
installPackage("TateOnProducts")--,FileName=>schreyer/Dropbox/SVDComplexes/from-git/TateOnProducts.m2)
loadPackage("TateOnProducts",Reload=>true)
viewHelp "TateOnProducts"
peek loadedFiles
check "TateOnProducts"
///
newPackage(
	"TateOnProducts",
    	Version => "0.3", 
    	Date => "Mai 31, 2018",
    	Authors => { {Name => "Daniel Erman", 
		  Email => "derman@math.wisc.edu", 
		  HomePage => "http://www.math.wisc.edu/~derman/"},
	         {Name => "David Eisenbud", 
		  Email => "de@msri.org", 
		  HomePage => "http://www.msri.org/~de/"},
	         {Name => "Frank-Olaf Schreyer", 
		  Email => "schreyer@math.uni-sb.de", 
		  HomePage => "http://www.math.uni-sb.de/ag/schreyer/"},
	          {Name => "Michael E. Stillman", 
		   Email => "mike@math.cornell.edu", 
		  HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"}
                    },          
    	Headline => "Tate resolutions on products of projective spaces",
    	DebuggingMode => true
    	)

export {
    "symExt",
    "cohomologyMatrix",
    "cohomologyHashTable",    
    "cohomologyPolynomialTable",
    "tallyDegrees",
    "lowerCorner",
    "upperCorner",
    "beilinsonWindow",
    "tateExtension",  -- actually doesn't exist!!
    "sloppyTateExtension",
    "pushAboveWindow",
    "firstQuadrantComplex",
    "lastQuadrantComplex",
    "cornerComplex",
    "regionComplex",
    "strand",
    -- beilinson functor
    "beilinsonContraction",
    "beilinsonBundle",
    "beilinson",
    "ContractionData",
    "tateData",
    "productOfProjectiveSpaces",
    "contractionData", -- probably doesn't need to be exported
    "BundleType",
    "PrunedQuotient", "QuotientBundle", "SubBundle",
    --
    "cornerCohomologyTablesOfUa",
    "coarseMultigradedRegularity",
    "Characteristic",
    "VariableName",
    "CoefficientField",
    "CohomologyVariables",
    "Rings",
    "CohomRing",
    "TateRingData",
    "TateData",
    "bgg",
    --the following could all be part of ChainComplexExtras
    "prependZeroMap",
    "appendZeroMap",
    "removeZeroTrailingTerms",
    "trivialHomologicalTruncation",
    "isChainComplex",
    "nonzeroMin", 
    "nonzeroMax",
    "minimize",
    "isMinimalChainComplex",
    "resolutionOfChainComplex",
    "chainComplexMap",
    "InitialDegree",
    "isQuism"
    --    Check
    }

protect TateData
protect CohomRing
protect Rings
protect TateRingData
protect BeilinsonBundles
protect LargeBases
protect ChangeBases

--needsPackage "ChainComplexExtras"
----------------------------------------------
-- from graded modules to Tate resolutions  --
----------------------------------------------
coarseMultigradedRegularity = method(Options =>
               {Strategy =>"MinimalResolution"})
coarseMultigradedRegularity ChainComplex := o-> F -> (
    --we assume F starts in homol degree 0.
    el := length F;
    r := degreeLength ring F;
    D := apply((min F..max F), i-> degrees F_i);
    --replace with D = hashTable
    L := flatten apply(length D, i-> apply(D_i, s -> s-toList(r:i)));
    regs := apply(r, p-> max(apply(L, q-> q_p)));
    d := regularity F;
    e := d-sum regs;
    e' := floor(e/r);
    f := e-r*e';
    regs + toList(#regs:e') + (toList(f:1)|toList((#regs-f):0))
    )

coarseMultigradedRegularity Module := o-> M -> (
    if o.Strategy == "MinimalResolution" then F :=res M  else
    if o.Strategy == "FastNonminimal" then (
    S := ring M;
    S' := coefficientRing S[gens S];
    m := presentation M;
    Tm := target m;
    Tm':= S'^(degrees Tm/sum);
    M' := coker map(Tm',,sub(presentation M, S'));
    assert(isHomogeneous M');
    F' := res(M', FastNonminimal=>true);
    F = allGradings(F',Tm, S));
    coarseMultigradedRegularity F
    ) 

allGradings=method()
allGradings (ChainComplex,Module, Ring) := (fJ,F0,Sall) -> (
    fJall := new ChainComplex;
    fJall.Ring = Sall;
    fJall_0 = F0;
    for i from 1 to length fJ do (
	m := map(fJall_(i-1),,sub(fJ.dd_i,Sall));
	fJall_i = source m;
	fJall.dd_i=m);
    chainComplex apply(length fJ,i->fJall.dd_(i+1))
    )



cornerComplex=method()
cornerComplex(ChainComplex,List) := (C,c) -> 
       (d:=c-toList(#c:1);cornerComplex1(C,d))
    
cornerComplex1=method()
cornerComplex1(ChainComplex,List) := (C,c) -> (
    -- addded this line to make the function  work for the zero complex
    if C==0 then return C;
    --
    t:= numFactors ring C;
--    if max C -min C < #t then error " need a complex of length at least t";
    C':= C[min C+1]; 
    Cge := firstQuadrantComplex1(C'[-#t+1],c);
    Cle := lastQuadrantComplex1(C',c);
--    <<(betti Cge, betti Cle) <<endl;
    A:=0;B:=0;AB:=0;d:=0;
    Ccorner:= chainComplex apply(max C- min C - #t-1,e-> (d:=e+#t; A=Cge.dd_(d);B= Cle.dd_(d); AB = cornerMap(C',c,d);
--	   print((betti A,betti AB,betti B));
	    (A|AB)||(map(target B, source A,0)|B)));
    return Ccorner[-min C-1])


cornerComplex(Module, List, List) := (M,low, high) ->(
    --high, low are lists of length = degreeLength ring M
    (S,E) := (tateData ring M)#Rings;
    regs := coarseMultigradedRegularity M; --regs
    hi := apply(#regs, i->max(regs_i, high_i+1)); --hi
    N := presentation truncate(hi, M)**S^{hi};-- betti N
    Q := symExt(N,E); --betti Q   
    (res (coker Q,LengthLimit=>(sum hi-sum low)))**E^{hi}[sum hi]
    )

productOfProjectiveSpaces = method(Options=>
    {CoefficientField=>ZZ/32003,
    Variables=>{getSymbol "x", getSymbol "e"},
    CohomologyVariables => {getSymbol "h", getSymbol "k"}})
productOfProjectiveSpaces(List) := opts -> n -> (
     kk := opts.CoefficientField;
     x:= opts.Variables#0; -- symbol x;
     e:= opts.Variables#1; -- symbol e;
     h := opts.CohomologyVariables#0; -- symbol h
     k := opts.CohomologyVariables#1; -- symbol k
     t:= #n;
     xx:=flatten apply(t,i->apply(n_i+1,j->x_(i,j)));
     degs:=flatten apply(t,i->apply(n_i+1,k->apply(t,j->if i==j then 1 else 0)));
     S:=kk[xx,Degrees=>degs];
     ee:=flatten apply(t,i->apply(n_i+1,j->e_(i,j)));
     E:=kk[ee,Degrees=>degs,SkewCommutative=>true];
     CR := ZZ[h,k];
     tateData := new MutableHashTable;
     tateData#Rings = (S,E);
     tateData#CohomRing = CR;
     tateData#BeilinsonBundles = new MutableHashTable;
     S.TateData = tateData;
     E.TateData = tateData;
     (S,E))

productOfProjectiveSpaces ZZ := opt -> n -> (productOfProjectiveSpaces(toList(n:1)))




///
restart
loadPackage ("TateOnProducts", Reload =>true)
peek loadedFiles
(P,E) = productOfProjectiveSpaces{2,2}
M = coker random(P^1, P^{{-1,-2},{-2,-1},{-1,-1}})
rowdegs = {{0,0}, {-1,1},{-1,-1},{-1,-2},{-2,-2}}
coldegs = apply(rowdegs, r->{-3,-3}-r)
m1 = random(P^rowdegs, P^coldegs)
M = coker gens pfaffians(4, m1-transpose m1)
R = coarseMultigradedRegularity M
R ={4,4}
netList apply(1+ length G, i-> tally degrees G_i)
betti (G =res M)
R = {} %{1,4} also works.
betti (G = res truncate(R, M))
netList apply(1+ length G, i-> tally degrees G_i)

regularity G

(P,E) = productOfProjectiveSpaces{5}
M = coker random(P^1, P^{-3,-4,-5})
M = P^1/ideal(P_0^3,P_1^4,P_2^5)
R = coarseMultigradedRegularity M
minimalBetti truncate(R, M)
apply(1+ length G, i-> tally degrees G_i)
///

-*
setupRings=method(Options=>{Variables=>{getSymbol "x", getSymbol "e"}})
setupRings(Ring,List) := opts -> (kk,n) -> (
     t:= #n;
     x:= symbol x;
     xx:=flatten apply(t,i->apply(n_i+1,j->x_(i,j)));
     degs:=flatten apply(t,i->apply(n_i+1,k->apply(t,j->if i==j then 1 else 0)));
     Sloc:=kk[xx,Degrees=>degs];
     e:= symbol e;
     ee:=flatten apply(t,i->apply(n_i+1,j->e_(i,j)));
     Eloc:=kk[ee,Degrees=>degs,SkewCommutative=>true];
--     h:=symbol h;
--     H:=ZZ[h];
     return(Sloc,Eloc))

setupRings(Ring,List) := opts -> (kk,n) -> (
     x:= opts.Variables#0; -- symbol x;
     e:= opts.Variables#1; -- symbol e;
     h := getSymbol "h";
     k := getSymbol "k";
     t:= #n;
     xx:=flatten apply(t,i->apply(n_i+1,j->x_(i,j)));
     degs:=flatten apply(t,i->apply(n_i+1,k->apply(t,j->if i==j then 1 else 0)));
     S:=kk[xx,Degrees=>degs];
     ee:=flatten apply(t,i->apply(n_i+1,j->e_(i,j)));
     E:=kk[ee,Degrees=>degs,SkewCommutative=>true];
     CohomRing := ZZ[h,k];
     tateData := new MutableHashTable;
     tateData.Rings = (S,E);
     tateData.CohomRing = CohomRing;
     tateData.BeilinsonBundles = new MutableHashTable;
     S.TateData = tateData;
     E.TateData = tateData;
     (S,E)
     )
 *-
 
tateData = method()
tateData Ring := (S) -> if not S.?TateData then 
   error "expected ring created with
   'productOfProjectiveSpaces'" else S.TateData

ringData = method()
ringData Ring := E -> if not E.?TateRingData then E.TateRingData = (
  differentDegrees := unique last degrees vars E;
  varsLists := apply(differentDegrees, deg -> select (gens E, x-> degree x == deg));
  t := #varsLists;
  irrList := apply(varsLists, L -> ideal(L));
  v := varsLists/(L->#L);
  n := apply(v, i-> i-1);
  (t,v,n,varsLists,irrList)
  ) else E.TateRingData

ringData Module := M -> ringData ring M

///
(S,E) = productOfProjectiveSpaces {1,2}
ringData E
ringData S
v = {2,3}
E = kk[e_0..e_1, f_0..f_2, Degrees => {v_0:{1,0},v_1:{0,1}}, SkewCommutative => true]
ringData E
(S,E) = productOfProjectiveSpaces{2,2}
ringData S
///


symExt=method()
symExt(Matrix,Ring) := (m,E) -> (
     ev := map(E,ring m,vars E);
     mt := transpose jacobian m;
     jn := ev(syz mt);
     a:=(vars E**E^(rank target m));
--betti a,tally degrees source a, isHomogeneous a     
--betti jn, tally degrees target jn, tally degrees source jn, isHomogeneous jn
--tally(     degrees target jn+degrees source a)
     b:=a*jn;
--betti b, tally degrees target b, tally degrees source b
     c:=map(target b,E^(degrees source jn),b);
     transpose c)

    

subMatrix=(m,d,e) -> (
     columns:=select(rank source m, i-> degree m_i==e);
     rows:=select(rank target m, i-> (degrees target m)_i ==d);
     transpose (transpose m_columns)_rows) 

upperCorner=method()
--  needs update if we work with negative grading on exterior algebra.
upperCorner(ChainComplex,List) := (F,deg) ->(
     E:=ring F;
     degsE:= unique degrees E;
     n:=apply(degsE,dege->#select(degrees E,d->d==dege)-1);
     assert(
	  #(degrees E)_0 == # deg
	  );
     -- sign change for k
     k:=sum deg;
     degsa:=degrees F_(-k-1);
     -- sign change -deg_j three times and inequalities 
     L1:=select(#degsa,i->#select(#deg,j->degsa_i_j <= -deg_j and degsa_i_j >= -(deg_j+n_j+1) )==#deg);
     degsb:=degrees F_(-k);
     L2:=select(#degsb,i->#select(#deg,j->degsb_i_j==-deg_j)==#deg);
     ((F.dd_(-k))^L1)_L2     
     )


lowerCorner=method()
lowerCorner(ChainComplex,List) := (F,deg) ->(
     E:=ring F;
     degsE:= unique degrees E;
     n:=apply(degsE,dege->#select(degrees E,d->d==dege)-1);
     assert(
	  #(degrees E)_0 == # deg
	  );
     -- sign change for k
     k:= sum deg;
     degsa:=degrees F_(-k);
     -- sign change -deg_j three times and inequalities    
     L1:=select(#degsa,i->#select(#deg,j->degsa_i_j==-deg_j)==#deg);
     degsb:=degrees F_(-k+1);
     L2:=select(#degsb,i->#select(#deg,j->degsb_i_j<=-(deg_j-n_j-1) and degsb_i_j>=-deg_j)==#deg);
     ((F.dd_(-k+1))^L1)_L2
     )

-*
corner=method()
corner(ChainComplex,List) := (F,deg) ->(
     E:=ring F;
     degsE:= unique degrees E;
     assert(
	  #(degrees E)_0 == # deg
	  );
     box:=boxDegrees E;
     k:=-sum deg;
     degsa:=degrees F_k;     
     L1:=flatten apply(box,boxdeg->select(#degsa,i->degsa_i==-deg+boxdeg));
     degsb:=degrees F_(k-1);
     L2:=unique flatten apply(degsE,degE-> flatten apply(box,boxdeg->select(#degsb,i->degsb_i==-deg+boxdeg-degE)));
     transpose (transpose F.dd_k_L1)_L2
     )

corner(ChainComplex,ZZ,List) := (F,k,deg) ->(
    --Frank: I do not understand why we want this function, most likely for a bit more flexibility
     E:=ring F;
     degsE:= unique degrees E;
     n:=apply(degsE,dege->#select(degrees E,d->d==dege)-1);
     assert(
	  #(degrees E)_0 == # deg
	  );
     degsa:=degrees F_k;     
     L1:=select(#degsa,i->#select(#deg,j->degsa_i_j<=-deg_j+n_j)==#deg);
     degsb:=degrees F_(k-1);
     L2:=select(#degsb,i->#select(#deg,j->degsb_i_j<=-deg_j+n_j)==#deg);
     transpose (transpose F.dd_k_L1)_L2
     )
*-




---------------------------------------------------------
-- numerical information                               --
---------------------------------------------------------

cohomologyMatrix=method()


cohomologyMatrix(ChainComplex,List,List) := (F,da,db) -> (
       --Under the assumption that T is part of a Tate resolution of a sheaf F on a product of
       --two projective space P^{n_1} x P^{n_2}, the function returns a matrix of cohomology polynomials 
       --$$\sum_{i=0}^{|n|} \, dim H^i(\mathbb P^{n_1}\times \mathbb P^{n_2},\mathcal F(c_1,c_2)) * h^i \in \, \mathbb Z[h,k]$$
       --for every c=(c_1,c_2) with $a_1 \le c_1 \le b_1$ and $a_2 \le c_2 \le b_2$.
       --In case T corresponds to an object in the derived category D^b(P^{n_1}x P^{n_2}), then
       --hypercohomology polynomials are returned, with the convention that k stands for k=h^{ -1}.
       
       --If T is not a large enough part of the Tate resolution, such as W below, 
       --then the function collects only
       --the contribution of T to the cohomology table of the Tate resolution, according to the formula in
       --Corollary 0.2 of @ HREF("http://arxiv.org/abs/","Tate Resolutions on Products of Projective Spaces") @.
        
       --The polynomial for
       --(b_1,b_2) sits in the north-east corner, the one corresponding to (a_1,a_2) in the south-west
       --corner.       
     E:= ring F; 
     if not #unique degrees E==2 then error "works only for two factors";
     L:=flatten apply(toList(min F..max F), k->
	 apply(degrees F_k, deg->
	     sum deg-k));
     minL:=min L;maxL := max L;
     h:=symbol h; k:=symbol k;p:=0;
     H:=ZZ[h,k];
     C:=matrix  apply(toList(-db_1..-da_1),j->
	 apply(toList(da_0..db_0),i->
	     sum(min F..max F,d-> (p=d+i-j;
		 if p<=0 then h^(-p) else k^(p))*(tally degrees F_(d))_({ -i,j}))));  
     C
     )
cohomologyMatrix(Module, List, List) := (M, low, high) -> (
    if degreeLength M != 2 then error"this version works only with a product of two projective spaces.";
    if #low !=2 or #high !=2 then error"expected degree lists of length 2";
    if not all(#low, i-> low_i<=high_i) then error"low should be less than high";
    C := cornerComplex(M, low, high);
    cohomologyMatrix(C, low , high))


cohomologyPolynomialTable = method()
cohomologyPolynomialTable HashTable := H ->(
    nonzeros := unique ((keys H)/first);
    low := {min (nonzeros/first), min(nonzeros/last)};
    high := {max (nonzeros/first), max(nonzeros/last)};    
    h := getSymbol "h";
    k := getSymbol "k";
    coh := ZZ[h,k];
    p:=0;
    hashTable apply(nonzeros,c->
	     (c, sum(select(keys H,cp-> cp_0==c),cp->
		     (p=cp_1;
		if p>=0 then (H#cp)*(coh_0)^p else (H#cp)*(coh_1)^(-p)
		     ))))
    )
cohomologyPolynomialTable(Module, List, List) := (M,low,high) ->
    cohomologyPolynomialTable cohomologyHashTable(M,low,high)
cohomologyPolynomialTable(ChainComplex, List, List) := (T,low,high) ->
    cohomologyPolynomialTable cohomologyHashTable(T,low,high)

cohomologyHashTable=method()

cohomologyHashTable(ChainComplex,List,List) := (F,low,high) -> (
       --Under the assumption that T is part of a Tate resolution of a sheaf F on a product of
       --projective spaces P^{n_1} x ... x P^{n_t}, the function returns a hashTable
       --In case T corresponds to an object in the derived category D^b(P^{n_1}x P^{n_2}), then
       --hypercohomology is returned.
       
       --If T is not a large enough part of the Tate resolution, such as W below, 
       --then the function collects only
       --the contribution of T to the cohomology table of the Tate resolution, according to the formula in
       --Corollary 0.2 of @ HREF("http://arxiv.org/abs/","Tate Resolutions on Products of Projective Spaces") @.
     E:= ring F; 
     deglen := degreeLength E;
     minF := min F;
     maxF := max F;
     if #low != deglen or #high != deglen then error"Expected list of length the number of factors of the projective product.";
     keylist := toList(low..high);
     hashTable flatten apply(keylist, a ->
	 (suma := sum a;
	     apply(toList(minF..maxF), d-> 
		     ({a,-d-suma},#select(degrees F_d, c->c==-a)))))
     )	 

cohomologyHashTable(Module, List, List) := (M, low, high) -> (
    if not all(#low, i-> low_i<=high_i) then error"low should be less than high";
    C := cornerComplex(M, low, high);
    cohomologyHashTable(C, low , high))
    
    

tallyDegrees=method()
tallyDegrees(ChainComplex) := C -> (
    apply(min C..max C,k->tally degrees C_k))



boxDegrees=method()
boxDegrees(Ring) := E -> (
     degs:= unique degrees E;
     t:=#degs;
     n:=apply(t,k->#select(degrees E,d->d==degs_k)-1);
     deg:=0*n;
     box:={deg};
     scan(#n,k->
	  box=flatten apply(box,deg-> apply(n_k+1,i->deg+i*degs_k))
	  );
     box)
 	    




beilinsonWindow=method()
beilinsonWindow ChainComplex := (C)-> (
    tD := tateData ring C;
    (S,E) := tD.Rings;
    (minC, maxC, mapsC) := toSequence chainComplexData C;
    windows := for i from minC to maxC list (
        degs := degrees C_i;
        positions(degs, a -> inBeilinsonWindow(a,E))
        );
    maps := for i from minC + 1 to maxC list (
        submatrix(C.dd_i, windows_(i-minC-1), windows_(i-minC))
        );
    removeZeroTrailingTerms chainComplexFromData{minC, maxC, maps}
    )


isChainComplex=method()
isChainComplex(ChainComplex) := W -> (
     lengthW:= max W- min W;
     #select(min W+1..max W-1,i->( if (source W.dd_i==0 or W.dd_(i+1)==0) then  true else W.dd_i*W.dd_(i+1)==0)) ==lengthW-1)


outsideBeilinsonRange=method()
outsideBeilinsonRange(Matrix) :=  m -> (
     E:= ring m;
     t:=#unique degrees E;
     n:=apply(unique degrees E,d-> (#select( degrees  E, e-> e==d)-1));
     d:=0;
	  --source indices not in the Beilison window
     sourcem := select(rank source m,i-> (d=degree (source m)_i;#select(#d,i->d_i<0 or d_i>n_i)>0));
     m_sourcem)	


-*
-- is still needed in one of the examples in TateExtension -- should go eventually
truncateInE=method()
truncateInE(List,Module):= (d,M) -> (
    base:=basis(M);
    degs:=degrees source base;
    m:=base_(select(#degs,k->#select(#d,i->degs_k_i >= d_i)==#d));
    image m 	
    )    
*-


-----------------
--  The corner Complex
-------------------------
numFactors=method(TypicalValue=>List)
    -- given the symmetric or exterior Cox ring E of the product pf projective spaces
    -- compute the number t of factors and return the List {0,...,t-1} 
numFactors(Ring) := E-> (
    t:= #unique degrees E;
    return toList(0..(t-1)))

quadrantMap=method()
quadrantMap(Matrix,List,List,List) := (M,c,I,J) -> (
    degSource:=degrees source M;
    degTarget:=degrees target M;
    t:= numFactors ring M; --the list {0,...,t-1}
    I':=select(t,j-> not member(j,I));
    J':=select(t,j-> not member(j,J));
    --sign change for c ??
    cc:=c;
    goodColumns:=select(#degSource,k -> (#select(I,i-> degSource_k_i >= cc_i)==#I and #select(I',i->degSource_k_i < cc_i)==#I')); 
    goodRows:=select(#degTarget,k -> (#select(J,i-> degTarget_k_i >= cc_i)==#J and #select(J',i->degTarget_k_i < cc_i)==#J')); 	
    return ((M^goodRows)_goodColumns)) 

firstQuadrantComplex=method()
     
firstQuadrantComplex(ChainComplex,List) := (C,c) -> ( 
     -- c index of the lower corner of the first quadrant
     firstQuadrantComplex1(C,c-toList(#c:1)) )

firstQuadrantComplex1=method()
firstQuadrantComplex1(ChainComplex,List) := (C,c) -> (
    -- c index of upper corner of the complementary last quadrant
    -- addded this line to make the function  work for the zero complex
    if C==0 then return C;
    --
    s:=min C;
    C':=C[s];
    -- I:= numFactors ring C;
    -- sign change for c (1x)
    Cge:=chainComplex apply(max C'-1,d -> quadrantMap(C'.dd_(d+1),-c,{},{}));
    return Cge[-s])

lastQuadrantComplex=method()
lastQuadrantComplex(ChainComplex,List) := (C,c) -> (
    -- c index of the lower corner of the complentary first quadrant
    lastQuadrantComplex1(C,c-toList(#c:1)))


lastQuadrantComplex1=method()
lastQuadrantComplex1(ChainComplex,List) := (C,c) -> (
     -- c index of the upper corner of the last quadrant
     -- addded this line to make the function  work for the zero complex
    if C==0 then return C;
    --
    s:=min C;
    C':=C[s];
    I:= numFactors ring C;
    --sign chain for c (1x)
    Cge:=chainComplex apply(max C'-1,d -> quadrantMap(C'.dd_(d+1),-c,I,I));
    return Cge[-s])

cornerMap=method()
cornerMap(ChainComplex,List,ZZ) := (C,c,d) -> (
    -- addded this line to make the function  work for the zero complex
    if C==0 then return C;
    --
    E := ring C;
    t := numFactors E;
    Is:=reverse apply(t,i->select(t,j->j<i));
    --sign change for c (2x)
    M:= quadrantMap(C.dd_d,-c,t,Is_0);
    Ms:=apply(#t-1,j->quadrantMap(C.dd_(d-j-1),-c,Is_j,Is_(j+1))); 
    -- multiplication of empty matrices some times does not work! so there is a work around.
    scan(Ms, N-> if source N == E^0 then M=map(target N, source M,0) else M=N*M); 
    return M)
    


regionComplex=method()
regionComplex(ChainComplex,List,Sequence) := (T,c,IJK) -> (
    T1:=trivialHomologicalTruncation(T,nonzeroMin T, nonzeroMax T);
    T2:=T1[min T1];
    Ls:=apply(toList(min T2..max T2),k->goodColumns(T2_k,c,IJK));
    rT:=chainComplex apply(min T2+1..max T2,k-> ((T2.dd_k))^(Ls_(k-1))_(Ls_(k)));	
    rT[-min T1])

goodColumns=method()
goodColumns(Module,List,Sequence) := (F,c,IJK) -> (
    degF:=degrees F;
    --sensitive to signs
    --select(#degF,g-> goodDegree(degF_g,c,IJK))
    select(#degF,g-> goodDegree(-degF_g,c,IJK))
    )

--Daniel: I'm not exactly sure what the correct notation for this function is.
goodDegree=method()
goodDegree(List,List,Sequence) := (d,c,IJK) -> (
    (I,J,K) := IJK;
    #select(I,i-> d_i<c_i)==#I and #select(J,j->d_j==c_j)==#J and #select(K,k->d_k>=c_k)==#K
    )	



strand=method()
strand(ChainComplex,List,List) := (T,c,I) -> (
    regionComplex(T,c,({},I,{})))




--------------------------------------------------
-- formal ChainComplex manipulations            --
--------------------------------------------------

chainComplexData = C->(
    minC := min C;
    maxC := max C;
    C':=C[minC];
    {minC, maxC, apply(toList(1..maxC-minC), i-> (C').dd_i)}
)
chainComplexFromData = method()
chainComplexFromData List := L ->(
    --format of L is desired min, desired max, list of 
    --shifted maps
    C := chainComplex L_2;
    assert( min C == 0);
    C[-L_0])

chainComplexFromData(ZZ, List) := (minC,L) ->(
    --minC will become the min of the output complex
    C := chainComplex L;
    assert( min C ==0);
    C[-minC])




trivialHomologicalTruncation=method()
trivialHomologicalTruncation(ChainComplex,ZZ,ZZ) := (C,d,e) -> (
    F := C;
    -- given a chain complex 
    -- ... <- C_{k-1} <- C_{k} <- C_{k+1} <- ...
    -- return the trivial truncation
    --   0 <- C_d <- C_{d+1} <- ... < C_e <- 0
    if d>e then error "expect d <= e";
    while min F > d do (F =prependZeroMap F);
    while max F < e do (F=appendZeroMap F);
    G := F[d];
    if d==e then (G= prependZeroMap chainComplex map(G_0,(ring G)^0,0)) else (
	G=prependZeroMap appendZeroMap chainComplex apply(toList(1..e-d),k->G.dd_k));
    G[-d])
///
E=ZZ/101[e_0,e_1,SkewCommutative=>true]
F=res ideal vars E
betti F
C=dual res (coker transpose F.dd_3,LengthLimit=>8)[-3]
betti C
C1=trivialHomologicalTruncation(C,-2,2)
trivialHomologicalTruncation(C1,-3,3)
///
	
    

prependZeroMap= method()
prependZeroMap ChainComplex := C->(
    L := chainComplexData(C[-1]);
    minC := L_0;
    newd := map((ring C)^0, target L_2_0, 0);
    (chainComplexFromData(minC-1,prepend(newd,L_2)))[1]
    )
    
appendZeroMap= method()
appendZeroMap ChainComplex := C->(
    L := chainComplexData(C);
    minC := L_0;
    newd := map(source last L_2,(ring C)^0, 0);
    chainComplexFromData(minC,append(L_2,newd))
    )    
    
    

nonzeroMin = method()
nonzeroMin(ChainComplex) := C -> (
    --assert( not C==0);
    if C==0 then return min C; 
    m:= min C;
    while C_m==0 do (m=m+1);
    m)


nonzeroMax = method()
nonzeroMax(ChainComplex) := C -> (
    --assert( not C==0);
    if C==0 then return max C; 
    m:= max C;
    while C_m==0 do (m=m-1);
    m)
///
symbol tt
R=ZZ[tt]
C=chainComplex {matrix{{R_0}}}
C1=appendZeroMap prependZeroMap C
nonzeroMax C1,max C1
nonzeroMin C1, min C1
///

removeZeroTrailingTerms = method()
removeZeroTrailingTerms(ChainComplex) := W -> (
    E := ring W;
    mi := nonzeroMin W;
    ma := nonzeroMax W;
    W' := W[mi];
    if mi==ma then (return (chainComplex({map(E^0,W'_0,0),map(W'_0,E^0,0)}))[-mi+1]) else
    (chainComplex apply(toList(1..ma-mi),i->W'.dd_i))[-mi]
    )

///
R=ZZ[tt]
C=chainComplex {matrix{{R_0}}}
C1=appendZeroMap prependZeroMap C
removeZeroTrailingTerms C1
///


extendFromMiddle = method()
extendFromMiddle (ChainComplex, ChainComplex, Matrix, ZZ) := (F1, F2, f, i) ->(
    --f is a map to F1_i from F2_0. Output is a ChainComplexMap to F1 from F2e,
    --where F2e is a chain complex obtained from F2 by prepending zeros.
    --CAVEAT the process of making a new ChainComplex seems to destroy
    --the direct sum information in the source and target modules!
    S:= ring F1;
    ind := toList(min F1.. max F1);
    F1List := apply (ind, i->F1.dd_i);
    F1i := chainComplex F1List_{i+1..max F1};
    fi := extend(F1i,F2,f);
    F2e := chainComplex(
	apply(ind, j-> 
	    if j<i-1 then map (S^0,S^0,0) else
	    if j == i-1 then map(S^0, F2_0,0) else 
	    F2.dd_(j-i+1))
	);
    map(F1, F2e, j->
	    if j< i then map(F1_j, F2e_j,0) else fi_(j-i))
    )

chainComplexMap=method(
    Options => {InitialDegree => -infinity}
)
chainComplexMap(ChainComplex,ChainComplex,List):= o -> (D,C,maps) -> (
   --- the code commented out should also work, and is in some sense
   --- more desireable as it uses map in the code.  However, something squirly
   --- happens in the map code.
   ---    startDeg := min C;
   ---    if (o.InitialDegree != -infinity) then startDeg = o.InitialDegree;
   ---    definingSet := set (startDeg..startDeg + length maps - 1);
   ---    map(D,C,i -> (if member(i, definingSet) then maps_(i - startDeg) else 0))
   startDeg := min C;
   if (o.InitialDegree != -infinity) then startDeg = o.InitialDegree;
   F := new ChainComplexMap;
   F.degree = 0;
   F.source = C;
   F.target = D;
   index1 := startDeg;
   scan(maps, x -> (F#index1 = x; index1 = index1 + 1;));
   F
)

resolutionOfChainComplex = method(Options=>{LengthLimit => infinity})
resolutionOfChainComplex ChainComplex := o -> C -> (
    -- computes a (generally non-minimal) resolution of a complex by the method
    -- of iterated mapping cones, and returns the ChainComplexMap from this to C. 
    -- If 
    -- C: 0 -> Cn ->...->Cm ->0
    -- is a chain complex, and Gi is a resolution of
    -- Ci, and [G -> F] denotes the mapping cone of a map of complexes G \to F,
    -- then the resolution of C is Gm if n=m; is [Gn->Gm] if n = m+1
    -- and otherwise is defined inductively  as
    -- Fi = [Gi -> F(i-1)]
    -- where the map Gi -> F(i-1)
    -- is induced by lifing Gi_0 --> G(i-1)_0 to the kernel of the (i-1)-st differential of
    -- F(i-1).
    complete C;
    minC := min C;
    maxC := max C;
    len:= length C; -- =maxC-minC
    n := numgens ring C;
    lengthLimit := max(n+len, len+o.LengthLimit);
    ind := toList(minC..maxC);
    reslist := apply(ind, i-> res(C_i, LengthLimit => lengthLimit-(i-minC)));
    mats := apply(ind, i-> matrix C.dd_i);
    --mats_i is the map from the free cover of C_i to 
    --the free cover of C_(i-1)
    F := reslist_0;
    comp :={id_(F_0)};
    if len >= 1 then(
	G := reslist_1;
    	F = cone extend(F,G, mats_1);
    	comp = comp | {F_1^[1]}
	);
    k := null;
    phi := null;
    for i from 2 to len do(
	G = reslist_i;
	k = syz F.dd_(i-1);
	phi := (mats_i)//(F_(i-1)^[1]*k);
	--note: F_(i-1)^[1] is the projection to the free cover of C_(i-1)
	--so phi is the lifting of mats_i, to the source of k,
	--and k*phi is the induced map to F_(i-1).
	F = cone extendFromMiddle(F,G,k*phi,i-1);
	comp = comp |{F_i^[1]};
	);
--    compMap := chainComplexMap(C[minC],F,comp);
--    compMap := chainComplexMap(C,F[-minC],comp);    
--    Cres := F[-minC];
--    Cres.cache.comparisonMap = compMap[-minC];
--    Cres
--    compMap[-minC]
    chainComplexMap(C,F[-minC],comp)
    )


minimize = method ()
minimize ChainComplex := E ->(
    --To simplify the notation consider the complex C = E[min E] that
    --is shifted so that the first nonzero module is C_0.
    --The algorithm:
    --Set dbar = the reduction of the differential d mod the maximal ideal.
    --choose a complement of ker dbar, and compute the idempotent rho: E -> E.
    -- the map rho is not a chain complex map, but the image of 
    --(rho | d*rho): C ++ C[1] --> C is a subcomplex and 
    --the minimization of  C is the complex C/image(rho|d*rho).
    --The script returns the ChainComplexMap from the minimization to C.
    complete E;
    C:= E[min E]; -- now min C == 0.
    M := max C;
    S := ring C;
    red := map(S,S,toList(numgens S:0_S));
    --make maps g_i: ker(red C.dd_i) -> C_i
    g := hashTable for i from 0 to M+1 list {i,syz red C.dd_i};
    --For each i choose an idempotent rho#i:C_i\to C_i
    --whose image is the complement
    --image g#i, Note that rho#0 = 0.
    rho := hashTable for i from 0 to M+1 list 
	{i,id_(C_i) - g#i*(id_(target g#i)//g#i)};
    minC := coker map(C, C++C[1], i-> rho#i | C.dd_(i+1)*rho#(i+1));
    pmC := prune minC;
    m := map(pmC, C, i-> (pmC_i.cache.pruningMap)^(-1) * inducedMap(minC_i, C_i));
    m[-min E]
    )
--    if o.Check==true then
--      if not isChainComplex minC then 
--           error"didn't produce a chain complex";
--    if o.Check==true then
--      if not isQuism m then 
--           error"didn't produce a quasi-isomorphic complex";
--    E' := pmC[-min E];
--    E'.cache.pruningMap = m[-min E];
--    E'
--    )

isMinimalChainComplex = C -> (
    S := ring C;
    red := map(S,S,toList(numgens S:0_S));
    T :=true;
    scan(toList(1+min C..max C),
	i-> if 0 != red(C.dd_i) then T = false);
    T
    )


{*
minimize = method (
    Options => {Check => false}
    )
minimize ChainComplex := o -> E ->(
    --To simplify the notation consider the complex C = E[nonZeroMin E] that
    --is shifted so that the first nonzer module is C_0.
    --The algorithm:
    --Set dbar = the reduction of the differential d mod the maximal ideal.
    --choose a complement of ker dbar, and compute the idempotent rho: E -> E.
    -- the map rho is not a chain complex map, but the image of 
    --(rho | d*rho): C ++ C[1] --> C is a subcomplex and 
    --minimize C = C/image(rho|d*rho).
    --The script sets (minimize C).cache.pruningMap equal to the map from C to minimize C.
    complete E;
    C:= E[nonzeroMin E]; -- now min C == 0.
    M := max C;
    S := ring C;
    red := map(S,S,toList(numgens S:0_S));
    --make maps g_i: ker(red C.dd_i) -> C_i
    g := hashTable for i from 0 to M+1 list {i,syz red C.dd_i};
    --For each i choose an idempotent rho#i:C_i\to C_i
    --whose image is the complement
    --image g#i, Note that rho#0 = 0.
    rho := hashTable for i from 0 to M+1 list 
	{i,id_(C_i) - g#i*(id_(target g#i)//g#i)};
    minC := coker map(C, C++C[1], i-> rho#i | C.dd_(i+1)*rho#(i+1));
    pmC := prune minC;
    if o.Check==true then
      if not isChainComplex minC then 
           error"didn't produce a chain complex";
    m := map(pmC, C, i-> (pmC_i.cache.pruningMap)^(-1) * inducedMap(minC_i, C_i));
    if o.Check==true then
      if not isQuism m then 
           error"didn't produce a quasi-isomorphic complex";
    E' := pmC[-nonZeroMin E];
    E'.cache.pruningMap = m[-min E];
    E'
    )
*}
isExact=method()
isExact(ChainComplex):=(C) -> (
   if (all((min C,max C), i -> (prune HH_i(C) == 0))) then true else false
)

isQuism=method()
isQuism(ChainComplexMap):=(phi) -> (
   isExact(cone phi)
)


///
restart
loadPackage "TateOnProducts"
S=ZZ/101[x_0..x_2]
m=random(S^{1,0},S^{0,-1})
C=chainComplex{m}
target minimize C
///

-----------------------------------------------
-- Beilinson monads, Tate extensions         --
-----------------------------------------------

inWindow = method()
inWindow(List,List) := (D,n) -> 
    #D == #select(#D, i->(0<=D_i and D_i<=n_i))

inWindow(ChainComplex) := W -> (
    (t,v,n,varsList,irrList) := ringData ring W;
    L:=flatten apply(toList(nonzeroMin W.. nonzeroMax W),d-> degrees W_d);
    #select(L, D-> not inWindow(D,n))==0)    

{*aboveWindow = method()
aboveWindow(List,List) := (D,n) -> #D == #select(#D, i-> D_i>n_i)

gensInWindow = method()
gensInWindow(Module) := M ->(
    rd = ringData ring M; 
    #D == #select(#D, i->(0<=D_i and D_i<=n_i)))
*}
///
n = {3,5,4}
D = { -1,4,3}
inWindow (D,n)
aboveWindow (D,n)
///

powers = (D,mL) -> (
m1 := directSum apply(#D, i -> gens trim((mL_i)^(D_i)));
m2 := matrix{toList(#D:1_(ring m1))};
m2*m1)

pushAboveWindow = method()
pushAboveWindow Module := M -> (
    E:= ring M;
    (t,v,n,varsLists,irrList) := ringData E;
    g := gens M;
    degList := last degrees g;
    if degList == {} then return M;
    directSum apply(degList, D->if inWindow(D,n) 
    then powers(v-D,irrList)**E^{ -D} else id_(E^{ -D}))
    )
///
(S,E) = productOfProjectiveSpaces {1,2}
pushAboveWindow(E^{{0,0},-{ -1,0},-{1,2},-{1,3}})
///


pushAboveWindow Matrix := A ->(
    if A==0 then return A;
    mingens image (A*pushAboveWindow source A)
    )

///
(S,E) = productOfProjectiveSpaces {1,2}
A=matrix{{E_0,E_2}}
pushAboveWindow A
///

pushAboveWindow(Matrix,Matrix) := (A,B) ->(
    assert(A*B == 0);
    C := pushAboveWindow syz A;
    mingens image(C % image B)
    )

pushAboveWindow(Matrix,Matrix,Matrix) := (A,B,C) ->(
    B2 := pushAboveWindow(A,B);
    assert((B|B2)*(C||map(source B2, source C, 0))== 0);
    assert(A*(B|B2) == 0);
    (B|B2, C||map(source B2, source C, 0))

)

pushAboveWindow List := L ->(
    --L = List of matrices that make a complex
    len := #L;
    if len == 1 then return append(L,pushAboveWindow syz L_0);
    if len == 2 then return {L_0, L_1|pushAboveWindow(L_0,L_1)};
    BC := null;
    A := L_0; B := L_1; C:= L_2;
    L':= {A};
    i := 2;
    while(   --<<i<<endl;flush;
    	BC = pushAboveWindow (A,B,C);
    	L' = append(L', BC_0);
    	A = BC_0;
    	B = BC_1;
    	assert(source A == target B);
    	i<len-1)
    do(
     	i = i+1;
     	C = L_i;
     	assert(source B == target C)
       );
   append(L',B)
    )

pushAboveWindow ChainComplex := C -> (
    C':=appendZeroMap appendZeroMap prependZeroMap C;
    L := chainComplexData C';
    M := pushAboveWindow L_2;
    chainComplexFromData(min C', M)
    )


sloppyTateExtension=method()
sloppyTateExtension(ChainComplex) := W -> (
    -- input W : a Beilinson representative of an object in D^b(PP)
    -- output :  an Tate extension in a bounded range
    -- compute the TateExtension in a sloppy way: the Beilinson window of the extension is only
    -- isomorphic, bat not equal W.
    (t,v,n,irrList,idealList) := ringData ring W;
    if not inWindow W then error "expect a complex with terms only in the Beilinson window";
    W1 := removeZeroTrailingTerms W;
    -- W1:= W;    
    TW1:=pushAboveWindow W1;    
    ma:= nonzeroMax TW1; mi:=nonzeroMin TW1; 
    --betti W1,betti TW1
--Bbounds given for the length of the resolution have to be discussed 
--They should come out of the proof of the theorem !!     
    TW1e := res(coker TW1.dd_(ma),LengthLimit=>(3*sum v))[-ma];
    --betti TW1e
    --changed a sign here
    --TW1c := cornerComplex(TW1e,2*v);
    TW1c := cornerComplex(TW1e,-2*v);
    --betti TWc
    TW2 := dual res(coker transpose TW1c.dd_(ma+sum v),LengthLimit =>(ma+3*sum v -mi))[-ma-sum v+1];
    --betti TW2
    TW2

    )

---------------------------------------
-- Construction of Beilinson functor --
---------------------------------------
beilinson = method(Options=>{BundleType=>PrunedQuotient}) -- other options: QuotientBundle, SubBundle.
-- beilinson(free E-Module, BundleType=>PrunedQuotient) returns an S-module
-- beilinson(free E-Matrix, BundleType=>PrunedQuotient) returns a matrix over S
-- beilinson(free E-ChainComplex, BundleType=>PrunedQuotient) returns a chain complex over S.
-- These three functions implement the beilinson functor     
--   from the category of graded free E-modules to the category of graded S-modules
--   (the actual beilinson functor is this followed by sheafification).
-- example uses:
--   (S,E) = --productOfProjectiveSpaces {1,2}
--   beilinson(E^{-1,-1})
--   beilinson random(E^{{-1,0}}, E^{{-2,-1}})

-- The following function should be moved to the Macaulay2 Core.
tensor(Matrix,Matrix) := opts -> (A,B) -> A ** B
tensor List := opts -> (L) -> (
    result := L#0;
    for i from 1 to #L-1 do result = tensor(result,L#i,opts);
    result
    )

-- The following functions are here to facilitate the construction
-- of the beilinson functor.  In particular, they make sure that the
-- generators are in the correct order and the signs are correct.

sortedBases = method()
sortedBases List := (varList) -> hashTable for i from 0 to #varList list i => (
  if i === 0 then matrix{{1_(ring first varList)}} else matrix{(subsets(varList, i))/product}
  )
sortedBases Ring := (cacheValue symbol sortedBases)(E -> (
    (t,v,n,varsList,irrList) := ringData E;
    varsList/sortedBases
    ))

sortedBasis = method()
sortedBasis(List, Ring) := (deg, E) -> (
    sB := sortedBases E;
    if not E.cache.?sortedBasis then E.cache.sortedBasis = new MutableHashTable;
    if not E.cache.sortedBasis#?deg then E.cache.sortedBasis#deg = (
        tensor for i from 0 to #deg-1 list sB#i#(deg#i)
        );
    E.cache.sortedBasis#deg
    )
transposeSortedBasis = method()
transposeSortedBasis(List, Ring) := (deg, E) -> (
    --sB := sortedBases E;
    if not E.?cache then E.cache = new CacheTable;
    if not E.cache.?transposeSortedBasis then E.cache.transposeSortedBasis = new MutableHashTable;
    if not E.cache.transposeSortedBasis#?deg then E.cache.transposeSortedBasis#deg = (
        matrix transpose entries sortedBasis(deg,E)
        );
    E.cache.transposeSortedBasis#deg
    )

koszulmap = (i,sortedB,S) -> (
    --cokernel(koszul(i+2,sortedB#1)) ** S^{i}
    if i == 0 then map(S^0, S^1, 0)
    else (
        d := first degrees source sortedB#1;
        (sub(diff(transpose sortedB#(i-1), sortedB#i), vars S)) ** S^{(i-2)*d}
        )
    )

beilinsonBundle = method(Options=>options beilinson)
-- beilinsonBundle(i, whichblock, E, BundleType => PrunedQuotient)
--   This returns a basic beilinson bundle (i.e. a pullback of a single
--     projective space factor).
--   notes:
--     (1) can use S as well as E, where (S,E) is the result of productOfProjectiveSpaces.
--     (2) BundleType is either PrunedQuotient(default) or QuotientBundle
--       SubBundle is not implemented. 
--     (3) if (S,E) has blocks n = {n0, ..., n(r-1)}
--       then 0 <= whichblock <= r-1
--       and 0 <= i <= n_whichblock
--       If either i or whichblock is outside of this range, the zero module is returned
--       and (TODO) is not stored in E.TateData.BeilinsonBundles
--     (4) These bundles are stashed in E.TateData.BeilinsonBundles, so they are not recomputed.
-- beilinsonBundle({a_0,...,a_(r-1)},E, BundleType => PrunedQuotient)
--   Returns the corresponding tensor product of basic beilinson bundles.
--   notes (1),(2),(4) hold here as well.
--   if a is out of range, the zero module is returned.
-- return value for either version is an S-module.
beilinsonBundle(ZZ, ZZ, Ring) := opts -> (a, whichblock, R) -> (
    -- R can be either S or E.
    tD := tateData R;
    if not tD.BeilinsonBundles#?(a,whichblock,opts.BundleType) then tD.BeilinsonBundles#(a,whichblock,opts.BundleType) = (
        (S,E) := tD.Rings;
        (t,v,n,varsList,irrList) := ringData E;
        sortedB := (sortedBases E)#whichblock;
        if a < 0 or a > n#whichblock then S^0
        else (
            if opts.BundleType === PrunedQuotient then (
                if a === 0 then S^1
                else if a === n#whichblock then (
                    deg := for i from 0 to t-1 list if i === whichblock then -1 else 0;
                    S^{deg}
                    )
                else coker koszulmap(a+2, sortedB, S)
                )
            else if opts.BundleType === QuotientBundle then (
                if a === n#whichblock then (
                    deg = for i from 0 to t-1 list if i === whichblock then -1 else 0;
                    S^{deg}
                    )
                else coker koszulmap(a+2, sortedB, S)
                )
            else if opts.BundleType === SubBundle then (
                error "SubBundle not yet implemented";
                )
            else error "expected BundleType to be one of PrunedQuotient (default) or QuotientBundle"
            -- note: once SubBundle is implemented, change this error message.
            )
        );
    tD.BeilinsonBundles#(a,whichblock,opts.BundleType)
    )

beilinsonBundle(List, Ring) := opts -> (a, R) -> (
    -- R can be either S or E.
    tD := tateData R;
    if not tD.BeilinsonBundles#?(a, opts.BundleType) then tD.BeilinsonBundles#(a, opts.BundleType) = (
        (S,E) := tD.Rings;
        (t,v,n,varsList,irrList) := ringData E;
        result := beilinsonBundle(a#0, 0, S, opts);
        for i from 1 to t-1 do result = result ** beilinsonBundle(a#i, i, S, opts);
        result
        );
    tD.BeilinsonBundles#(a, opts.BundleType)
    )

---Used in some examples in the test section. Probably does
--not need to be exported.
contractionData = method(Options => options beilinson)
contractionData(List, List, Ring) := opts -> (rowdeg, coldeg, E1) -> (
    -- tar, src are multidegrees
    tD := tateData E1;
    if not tD.?ContractionData then tD.ContractionData = new MutableHashTable;
    if not tD.ContractionData#?(rowdeg, coldeg, opts.BundleType) then
        tD.ContractionData#(rowdeg, coldeg, opts.BundleType) = (
            (S,E) := tD.Rings;
            if opts.BundleType === PrunedQuotient then (
                mc := for i from 0 to #rowdeg-1 list (
                    ei := coldeg#i - rowdeg#i;
                    if rowdeg#i ==0 and coldeg#i == 0 then id_(E^1) else (sortedBases E)#i#(coldeg#i+1)
                    );
                mr := for i from 0 to #rowdeg-1 list (
                    ei := coldeg#i - rowdeg#i;
                    -- The following is created in this funny way, because transpose of a matrix over
                    -- the exterior power introduces signs that we don't want here.
                    if rowdeg#i ==0 and coldeg#i == 0 then id_(E^1) else matrix transpose entries (sortedBases E)#i#(rowdeg#i+1)
                    );
                changemat := for i from 0 to #rowdeg-1 list (
                    ei := coldeg#i - rowdeg#i;
                    if rowdeg#i ==0 and coldeg#i == 0 then id_(S^1)
                    else if rowdeg#i == 0 and coldeg#i > 0 then (sortedBases S)#i#1
                    else (
                        ncols := numColumns (sortedBases E)#i#(rowdeg#i+1);
                        id_(S^ncols)
                        )
                    );
                (tensor mc, tensor mr, tensor changemat)
                )
            else if opts.BundleType === QuotientBundle then (
                mc = for i from 0 to #rowdeg-1 list
                    (sortedBases E)#i#(coldeg#i+1);
                mr = for i from 0 to #rowdeg-1 list (
                    -- The following is created in this funny way, because transpose of a matrix over
                    -- the exterior power introduces signs that we don't want here.
                    matrix transpose entries (sortedBases E)#i#(rowdeg#i+1)
                    );
                (tensor mc, tensor mr)
                )
            else if opts.BundleType === SubBundle then (
                error "not done yet";
                )
            else error "..."
            );
    tD.ContractionData#(rowdeg, coldeg, opts.BundleType)
    )

numgensU = method(Options => options beilinson)
numgensU(List, Ring) := opts -> (deg, E) -> (
    (t,v,n,varsList,irrList) := ringData E;
    if opts.BundleType === PrunedQuotient then (
      product for i from 0 to #deg-1 list if deg#i == 0 or deg#i == n#i then 1 else binomial(n#i + 1, deg#i+1)
    ) else if opts.BundleType === QuotientBundle then (
      product for i from 0 to #deg-1 list if deg#i == n#i then 1 else binomial(n#i + 1, deg#i+1)
    ) else if opts.BundleType === SubBundle then (
    ) else
      error("BundleType "|toString opts.BundleType|" unknown")
    )

beilinsonContraction = method(Options => options beilinson)
-- beilinsonContraction(e, rowdeg, coldeg)
--   returns a map between two Beilinson generators
--     (i.e. the beilinson generators beilinsonBundle(a, E)
--      of the derived category).
-- notes: 
--  (1) (S,E) is the result of productOfProjectiveSpaces
--  (2) e is a homogeneous element of E
--     giving a map from E^(-coldeg) --> E^(-rowdeg).
--  (3) note: E is positively graded, in contrast to the paper!
beilinsonContraction(RingElement, List, List) := opts -> (e, rowdeg, coldeg) -> (
    if e != 0 and degree e + rowdeg != coldeg then error "degrees in 'beilinsonContraction' are incorrect";
    E := ring e;
    tD := tateData E;
    (t,v,n,varsList,irrList) := ringData E;
    S := first tD.Rings;
    edeg := coldeg - rowdeg;
    if opts.BundleType === PrunedQuotient then (
        (mc, mr, changemat) := contractionData(rowdeg, coldeg, ring e, opts);
        missing := for i from 0 to t-1 list if rowdeg#i == 0 and coldeg#i == 0 then 1 else 0;
        signchange1 := sum(0..t-2, i -> edeg#i * sum(i+1..t-1, j -> missing#j));
        signchange := if odd signchange1 then -1 else 1;    
        if e == 0 then 
            map(S^(numgensU(rowdeg,E,opts)), S^(numgensU(coldeg,E,opts)), 0)
        else
            changemat * signchange * substitute(contract(diff(e, mc), mr), S)      
        )
    else if opts.BundleType === QuotientBundle then (
        (mc, mr) = contractionData(rowdeg, coldeg, ring e, opts);
        if e == 0 then 
            map(S^(numgensU(rowdeg,E,opts)), S^(numgensU(coldeg,E,opts)), 0)
        else
            substitute(contract(diff(e, mc), mr), S)      
        )
    else if opts.BundleType === SubBundle then (
        error "not done yet"
        )
    else error "what?"
    )

inBeilinsonWindow = method()
-- inBeilinsonWindow(deg, E)
--   returns a boolean value.
--   returns (beilinson(E^{-deg}) != 0),
--    i.e. whether the 'deg' values are 'in range'.
inBeilinsonWindow(List, Ring) := (deg, E) -> (
    (t,v,n,varsList,irrList) := ringData E;
    for i from 0 to #deg-1 do (
        if deg#i < 0 or deg#i > n#i
        then return false;
        );
    true
    )

beilinson Module := Module => opts -> o -> (
    -- TODO (Mike will do this).
    )

beilinson Matrix := Matrix => opts -> o -> (
    if not member(opts#BundleType, set{PrunedQuotient, QuotientBundle, SubBundle})
    then error "expected BundleType to be one of PrunedQuotient, QuotientBundle, SubBundle";
    E1 := ring o;
    tD := tateData E1;
    (S,E) := tD.Rings;
    coldegs := degrees source o;
    rowdegs := degrees target o;
    colpos := positions(coldegs, a -> inBeilinsonWindow(a,E));
    rowpos := positions(rowdegs, a -> inBeilinsonWindow(a,E));
    src := if #colpos == 0 then S^0 else directSum for a in colpos list beilinsonBundle(coldegs#a,S,opts);
    tar := if #rowpos == 0 then S^0 else directSum for a in rowpos list beilinsonBundle(rowdegs#a,S,opts);
    if #colpos == 0 or #rowpos == 0 then return map(tar, src, 0);
    rowdegs = rowdegs_rowpos;
    coldegs = coldegs_colpos;
    elems := entries submatrix(o, rowpos, colpos);
    mats := matrix for r from 0 to #rowpos-1 list
      for c from 0 to #colpos-1 list (
            rdeg := rowdegs#r;
            cdeg := coldegs#c;
            beilinsonContraction(elems#r#c, rdeg, cdeg,opts)
          );
    map(tar,src,mats)    
    )

beilinson ChainComplex := opts -> (BT) -> (
    -- BT should be a complex over E = exterior algebra
    data := chainComplexData BT;
    removeZeroTrailingTerms chainComplexFromData{data#0, data#1,data#2/(m -> (beilinson(m, opts)))}
    )

inContractionWindow = method()
inContractionWindow(List, Ring) := (deg, E) -> (
    (t,v,n,varsList,irrList) := ringData E;
    for i from 0 to #deg-1 do (
        if deg#i < 0 or deg#i > n#i+1
        then return false;
        );
    true
    )

contractionFunctor = method()
-- contractionFunctor implements the functor
--  from free E-modules to free S-modules with degree zero maps
contractionFunctor Module := (F) -> (
    -- F is a free E-module
    -- result is a free S-module, where contractionFunctor(E(-a)) = W^a
    if not isFreeModule F then error "expected free module over an exterior algebra";
    tD := tateData ring F;
    (S,E) := tD.Rings;
    if ring F =!= E then error "expected free module over an exterior algebra";
    zero := degree(1_E);
    nc := numColumns basis(zero, dual F);
    S^nc
    )

contractionFunctor Matrix := Matrix => m -> (
    F := source m;
    G := target m;
    if not isFreeModule F or not isFreeModule G then error "expected a map of free E-modules";
    tD := tateData ring m;
    (S,E) := tD.Rings;
    if ring m =!= E then error "expected free module over an exterior algebra";
    coldegs := degrees F;
    rowdegs := degrees G;
    colpos := positions(coldegs, a -> inContractionWindow(a,E));
    rowpos := positions(rowdegs, a -> inContractionWindow(a,E));
    mwindow := submatrix(m, rowpos, colpos);
    src := contractionFunctor source mwindow;
    tar := contractionFunctor target mwindow;
    if #colpos == 0 or #rowpos == 0 then return map(tar, src, 0);
    rowdegs = rowdegs_rowpos;
    coldegs = coldegs_colpos;
    elems := entries mwindow;
    mats := matrix for r from 0 to #rowpos-1 list
      for c from 0 to #colpos-1 list (
            rdeg := rowdegs#r;
            cdeg := coldegs#c;
            mr := transposeSortedBasis(rdeg, E);
            mc := sortedBasis(cdeg, E);
            e := elems#r#c;
            if e == 0 then
              map(S^(numRows mr), S^(numColumns mc), 0)
            else
              substitute(contract(diff(elems#r#c, mc), mr), S)
          );
    map(tar,src,mats)    
    )

contractionFunctor ChainComplex := (C) -> (
    -- C should be a complex over E = exterior algebra
    data := chainComplexData contractionWindow C;
    removeZeroTrailingTerms chainComplexFromData{data#0, data#1,data#2/(m -> contractionFunctor m)}
    )

contractionWindow=method()
contractionWindow ChainComplex := (C)-> (
    tD := tateData ring C;
    (S,E) := tD.Rings;
    (minC, maxC, mapsC) := toSequence chainComplexData C;
    windows := for i from minC to maxC list (
        degs := degrees C_i;
        positions(degs, a -> inContractionWindow(a,E))
        );
    maps := for i from minC + 1 to maxC list (
        submatrix(C.dd_i, windows_(i-minC-1), windows_(i-minC))
        );
    removeZeroTrailingTerms chainComplexFromData{minC, maxC, maps}
    )

contractionSequence = method()
contractionSequence ChainComplex := (T) -> (
    -- given a complex of E-modules, forms two
    -- exact sequences, 
    --   C: beilinson(T, BundleType=>QuotientBundle)
    --   D: contractionComplex (T ** E^{{-1,...,-1}})
    -- and the natural map
    --   phi : C --> D
    -- between them.  Returns phi.
    tD := tateData ring T;
    (S,E) := tD.Rings;
    CT := beilinsonWindow T;
    ones := toList((numgens degreesRing S):1);
    T1 := T ** E^{-ones};
    DT := contractionWindow T1;
    Cdegrees := for i from min CT to max CT list i => degrees CT_i;
    Ddegrees := for i from min DT to max DT list i => degrees DT_i;
    C := beilinson(T, BundleType=>QuotientBundle);
    D := contractionFunctor T1;
    (hashTable Cdegrees, hashTable Ddegrees, C, D)
    )
///
-- XX
restart
debug needsPackage "TateOnProducts"
  n = {2,1}
(S,E) = productOfProjectiveSpaces n
  contractionFunctor(E^{{-1,-1}})
  contractionFunctor(E^{{1,1}})
  contractionFunctor(E^{{3,1}})
  m = map(E^{{0,-1}}, E^{{-2,-1}}, {{e_(0,1)*e_(0,2)}})
  contractionFunctor m

  T1 = (dual res trim (ideal vars E)^2 [1]);
  cohomologyMatrix(T1,-3*n,3*n)
  T2 = res(coker lowerCorner(T1, {2,2}), LengthLimit=>14)[4]
  cohomologyMatrix(T2,-3*n,3*n)
  contractionWindow T2
  cT = contractionFunctor T2
  cT.dd^2
  prune HH cT

  a = {3,3}
  T4 = ((T2 ** E^{a})[sum a])
  cohomologyMatrix(oo, -5*n,5*n)
  contractionWindow T4
  elapsedTime cT4 = contractionFunctor T4;
  cT4
  prune HH cT4
  cT4.dd^2 == 0  

  -- now we compute the chain complex map beilinson T2 --> contractionFunctor T2
  BT2 = beilinson(T2, BundleType=>QuotientBundle)
  tallyDegrees beilinsonWindow T2
  for i from min BT2 to max BT2 list i => numgens BT2_i
  contractionFunctor (T2 ** E^{{-1,-1}})
  tallyDegrees contractionWindow (T2 ** E^{{-1,-1}})
  --TODO: want the map of block Koszul matrices.
  
  (h1,h2,C,D) = contractionSequence T2
  for r in h1#-1 list for c in h2#-1 list (
      << "doing " << r << " and " << c << endl;
      koszulm(n, r, c, S)
      )
  matrix oo
      tensor for i from 0 to #a-1 list koszulmap(a#i, (sortedBases S)#i, S)

        -- XXXX

  koszulsize = (n,a) -> product for i from 0 to #n-1 list binomial(n#i,a#i)  
  koszulsize({2,1},{2,0})
  koszulsize({3,2},{2,1})
  koszulm = (n,rowdeg,coldeg,S) -> (
      deg := coldeg-rowdeg;
      for i from 0 to #deg-1 do if deg#i < 0 or deg#i > n#i+1 then return map(S^(koszulsize(n,rowdeg)), S^(koszulsize(n,coldeg)), 0);
      tensor for i from 0 to #rowdeg-1 list koszulmap(deg#i,(sortedBases S)#i,S)
      )  

  koszulm(n,{0,0},{1,1},S)
  koszulm(n,{0,0},{2,1},S)
  koszulmap(0, (sortedBases E)#0, S)
  beilinsonBundle({0,0}, S, BundleType=>QuotientBundle)
///


  -- This function checks that beilinson is functorial, by creating random matrices in all possible degrees,
  -- and 
  testBeilinson = method(Options => options beilinson)
  testBeilinson List := opts -> n -> (
      (S,E) := productOfProjectiveSpaces n;
      zeros := toList(#n:0);
      degs := reverse toList(-n..zeros);
      m1 := random(E^degs, E^degs);
      m2 := random(E^degs, E^degs);
      shouldBeZero := beilinson(m1*m2,opts) - beilinson(m1,opts) * beilinson(m2,opts);
      if shouldBeZero == 0 then return "OK";
      map(E^degs, E^degs, for tar in degs list for src in degs list (
          p1 := random(E^{tar}, E^degs);
          p2 := random(E^degs, E^{src});
          if beilinson(p1 * p2, opts) - beilinson(p1, opts) * beilinson(p2, opts) == 0
          then 0_E
          else 1_E
          ))
      )

  testBeilinson1 = method(Options => options beilinson)
  testBeilinson1 List := opts -> (n) -> (
      (S,E) := productOfProjectiveSpaces n;
      zeros := toList(#n:0);
      degs := reverse toList(zeros..n);
      triples := flatten flatten for d1 in degs list 
        for d2 in degs list 
          for d3 in degs list 
            if all(d2-d1, i -> i >= 0) and all(d3-d2, i -> i >= 0) 
            then (d1,d2,d3) 
            else continue;
      H := hashTable for x in triples list (
          p1 := random(E^{-x#0}, E^{-x#1});
          p2 := random(E^{-x#1}, E^{-x#2});
          if beilinson(p1 * p2, opts) - beilinson(p1, opts) * beilinson(p2, opts) == 0 then continue;
          << "beilinson functoriality fails for " << x << endl;
          x => (p1, p2)
          );
      if #keys H == 0 then null else H
      )
  
----------------------------------------
-- Examples in the Paper                                   --
----------------------------------------
cornerCohomologyTablesOfUa = method()
cornerCohomologyTablesOfUa(List) := n-> (
    if not #n ==2 then error "expect product with two factors only";
    (S,E) := productOfProjectiveSpaces n;    
    a:=0;U:=0;W:=0;T:=0;cTa:=0;cTb:=0;cTb1:=0;
    Us:=flatten apply(n_0+1,a0->apply(n_1+1,a1->(
	    a={a0,a1};
            U=E^{ -a};
            W=(chainComplex {map(E^0,U,0),map(U,E^0,0)})[1];
	    {a,W})
	));
        Ts:=apply(Us,aW->( 
	T=sloppyTateExtension aW_1;
	T=trivialHomologicalTruncation(T,-2*sum n,2*sum n);
	append(aW,T)));
    apply(Ts,aT-> (
	cTa=cornerComplex(aT_2,-aT_0);
	--cTb=dual cornerComplex(dual aT_2,aT_0-{1,1});
	cTb1=cornerComplex(aT_2,{1,1})[-1];
        (cohomologyMatrix(aT_2,-2*n,2*n),
        cohomologyMatrix(cTa,-2*n,2*n),
        cohomologyMatrix(aT_1,-2*n,2*n),
        --cohomologyMatrix(cTb,-2*n,2*n),
	cohomologyMatrix(cTb1,-2*n,2*n),
--	betti trivialHomologicalTruncation(aT_1,1-sum n,-1+ sum n),
	betti trivialHomologicalTruncation(cTa,1-sum n,-1+ sum n),
	betti trivialHomologicalTruncation(cTb1,1-sum n,-1+ sum n)
	)
    ))
)

cornerCohomologyTablesOfUa(List,List) :=(n,a)-> (
    if not #n ==2 then error "expect product with two factors only";
    if not (a_0 <=n_0 and 0 <= a_0 and a_1 <=n_1 and 0 <= a_1) then error "expected 0 <= a <=n"; 
    (S,E) := productOfProjectiveSpaces n;
    U:=0;W:=0;T:=0;cTa:=0;cTb:=0;cTb1:=0;
            U=E^{ -a};
            W=(chainComplex {map(E^0,U,0),map(U,E^0,0)})[1];
	T=sloppyTateExtension W;
	T=trivialHomologicalTruncation(T,-2*sum n,2*sum n);
	cTa=cornerComplex(T,-a);
	--cTb=dual cornerComplex(dual T,a-{1,1});
	cTb1=cornerComplex(T,{1,1})[-1];
        {cohomologyMatrix(T,-2*n,2*n),
        cohomologyMatrix(cTa,-2*n,2*n),
        cohomologyMatrix(W,-2*n,2*n),
        --cohomologyMatrix(cTb,-2*n,2*n),
	cohomologyMatrix(cTb1,-2*n,2*n),
--	betti W,
	betti trivialHomologicalTruncation(cTa,1-sum n,-1+ sum n),
	betti trivialHomologicalTruncation(cTb1,1-sum n,-1+ sum n)}
    )
///
time  cornerCohomologyTablesOfUa({1,2},{1,1})

tally degrees source M
tally degrees target M
M' = submatrixByDegrees(M, {10,11},{11,11});
betti M'
///

--preliminaries for bgg:
multMap = method()

-*
multMap(Module,List,List):= (P,a',a) ->(
    --produces a map from 
    --a sum of copies of S^a to a sum of copies of S^a'.
    --If the grading is changed to the "correct" one for E,
    --this code will need fixing!
    if sum a' - sum a != 1 then error"Sums must differ by 1";
    (S,E) := (tateData ring P)#Rings;
    pos := positions(gens E, e-> degree e == a'-a);
    Ba := matrix basis(a,P);
    Ba' := matrix basis(a',P);
    map(S^{(numcols Ba'):a'}, S^{(numcols Ba):a},
	 sum(pos,i-> 
	     S_i*
	   (sum(numrows Ba, ell-> 
		   sub(contract((transpose Ba')_{ell},E_i*Ba^{ell}),S))))
       )
)
*-

multMap(Module,List,List):= (P,a',a) ->(
    --produces a map from 
    --a sum of copies of S^a to a sum of copies of S^a'.
    --If the grading is changed to the "correct" one for E,
    --this code will need fixing!
    if sum a' - sum a != 1 then error"Sums must differ by 1";
    (S,E) := (tateData ring P)#Rings;
    pos := positions(gens E, e-> degree e == a'-a);
    ee := apply(gens E, e->map(P,P**E^{degree e},e));
    Ba := basis(a,P)**E^{a'-a};
    Ba' :=basis(a',P);
    map(S^{(numcols Ba'):a'}, S^{(numcols Ba):a},
	 sum(pos, p->S_p*sub(ee_p*Ba//Ba', S))
	 )
       )

///

(S,E) = productOfProjectiveSpaces{1,2}
P = truncate({1,1},E^1)
a = {1,2}
a' = {2,2}
m = multMap(P,a',a)
degrees source m
degrees target m
betti m
///

bgg = method()
bgg Module := P -> (
    (S,E) := (tateData ring P)#Rings;
    D := (degrees basis P)_1;
    Ds := sort apply(D, d->(sum d,d));
    minP := min(Ds/first);
    maxP := max(Ds/first);
    freeModuleDegs := hashTable apply(toList(minP..maxP), i-> 
	    (-i=>select(Ds,d-> d_0 == i)/last)
	    );
    LP := new ChainComplex;
    LP.ring = S;
--define the modules as direct sums, with one degree per summand
    scan(toList(minP..maxP), i->
	LP#(-i) = directSum apply(unique freeModuleDegs#(-i), d -> 
	    S^(select(freeModuleDegs#(-i), k-> d ==k))));
--define the maps
    tar:=S^0; sour := S^0; utar := {};usour := {}; a:= 0;a':=0;
    u := L->unique degrees L;
    scan(toList(min LP+1..max LP), k->(
	    tar = LP_(k-1);
	    sour = LP_k;
	    utar = u tar;
	    usour = u sour;
	    LP.dd#k = sum(#utar, i-> 
		      sum(#usour, j->(
	              a' = -utar_i;
	              a = -usour_j;
                      map(tar,sour,
    		         tar_[i]*multMap(P,a',a)*(sour^[j])
		         )
		     )))
               )
	);
   LP)


///
restart
loadPackage("TateOnProducts", Reload=>true)
///


--------------------------
-- Begin of the documentation
------------------------
beginDocumentation()


document { 
  Key => TateOnProducts,
  Headline => "Computation of parts of the Tate resolution on products",
  "This package contains implementations of the algorithm from our paper ",
  HREF("https://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces"),
  ". It allows to compute the direct image complexes of a coherent sheaf along the projection onto a product 
  of any of the factors.",
  PARA{},
     "In the moment the function tateExtension is not completed, a version sloppyTateExtension works 
     however nicely in examples. 

     The documentation and comments in the code are in a preliminary shape.
     Some function have to be removed, other wait for their implementation ",
   PARA{},"The main differences from the paper are:",
   UL{  "the exterior algebra E is positively graded ",
        "we use E instead of omega_E ",
	" all complexes are chain complexes instead of cochain complexes"
	},

   PARA{},
   SUBSECTION "From graded modules to Tate resolutions",  
   UL{   TO productOfProjectiveSpaces,
	 TO symExt,
	 TO lowerCorner,
	 TO upperCorner
      },
   SUBSECTION "Numerical Information",
   UL{ 
      TO cohomologyMatrix,
      TO cohomologyPolynomialTable,
      TO cohomologyHashTable,
      TO tallyDegrees
     },
    SUBSECTION "Subcomplexes",
    UL{
       TO cornerComplex,
       TO regionComplex,
       TO strand,
       TO firstQuadrantComplex,
       TO lastQuadrantComplex
      },
    SUBSECTION "formal ChainComplex manipulations",
    UL{
	TO prependZeroMap,
        TO appendZeroMap,
        TO removeZeroTrailingTerms,
	TO trivialHomologicalTruncation,
	TO nonzeroMin,
	TO nonzeroMax,
        TO isChainComplex,
	TO minimize
	},      
    SUBSECTION "Beilinson monads",
    UL{ 
	TO beilinsonWindow,
	TO sloppyTateExtension,
	TO tateExtension,
	TO pushAboveWindow,
	TO beilinsonBundle,
	TO beilinsonContraction,
	TO beilinson,
        },
    
    SUBSECTION "Examples from the papers",
    UL{ 
	TO cornerCohomologyTablesOfUa,
       },
    
    SUBSECTION "Missing pieces",
    UL{ "BGG functor R for complexes of S-modules",
	"projective resolutions of complexes",
	"BGG functor L for complexes of E-modules",
	"various composition of functions",
	TO cornerCohomologyTablesOfUa,
	"Example section:  examples from the paper, jumping lines, 
	$R pi_* sO_X$ for a resolution of singularities, $Rf_*sF$ for a coherent sheaf
	$sF$ on $X subset P^{n_1}$ and a morphism $f:X -> P^{n_2}$.",
	} 
         
   }

doc ///
   Key
    coarseMultigradedRegularity
    (coarseMultigradedRegularity, Module)
    (coarseMultigradedRegularity, ChainComplex)
    [coarseMultigradedRegularity, Strategy]
   Headline
    A truncation that has linear resolution
   Usage
    R = coarseMultigradedRegularity M
   Inputs
    M:Module
     multi-graded module over a multi-graded polynomomial ring
    M:ChainComplex
     multi-graded module over a multi-graded polynomomial ring
    Strategy => String
   Outputs
    R:List
     degree such that truncate(R,M) has linear resolution
   Description
    Text
     Uses a free resolution and takes the maximum degree of a term
     minus the homological position in each component. Then adjusts
     so that the sum of the degrees is at least the ordinary
     regularity.
    Example
     (S,E) = productOfProjectiveSpaces{1,1,2}
     I = ideal(x_(0,0)^2,x_(1,0)^3,x_(2,0)^4)
     R = coarseMultigradedRegularity(S^1/I)
     N = truncate(R,S^1/I);
     betti res N
     netList toList tallyDegrees res N
   Caveat
    We haven't yet proven that this is right.
   SeeAlso
    productOfProjectiveSpaces
    tallyDegrees
///

-*
doc ///
  Key
    setupRings
    (setupRings,Ring,List)
  Headline
    setup the Cox ring of a product of t projective space, and its exterior dual 
  Usage
    (S,E)=setup(kk,n)
  Inputs
    kk: Ring
       the ground field 
    n: List
       the list \{n_1,...,n_t\} \, of the dimensions of the factors
  Outputs
    S: PolynomialRing
       the homogeneous coordinate of  P^{n_1}x ... x P^{n_t} of t 
       projective spaces
    E: PolynomialRing
       the corresponding exterior algebra   
  Description
     Text
     Example
        n={1,1}
	kk=ZZ/101 -- the ground field
        (S,E)=setupRings(ZZ/101,n)
	(coefficientRing S) === (coefficientRing E)
	trim (ideal vars S)^2
        trim (ideal vars E)^2	
///
*-
doc ///
  Key
    productOfProjectiveSpaces
    (productOfProjectiveSpaces,List)
    (productOfProjectiveSpaces, ZZ)
    [productOfProjectiveSpaces, CoefficientField]
    [productOfProjectiveSpaces, Variables]
    [productOfProjectiveSpaces, CohomologyVariables]
    
  Headline
    Cox ring of a product of projective spaces and it Koszul dual exterior algebra
  Usage
    (S,E)=productOfProjectiveSpaces N
    (S,E)=productOfProjectiveSpaces n    
  Inputs
    N: List
       the list \{n_1,...,n_t\} \, of the dimensions of the factors
    n: ZZ
       Gives n copies of P^1
    CoefficientField => Ring
       ground field of S,E
    Variables => List
       list of 2 symbols
    CohomologyVariables => List
       list of 2 symbols
  Outputs
    S: PolynomialRing
       homogeneous coordinate ring of P^{n_1}x ... x P^{n_t}
    E: PolynomialRing
       the corresponding exterior algebra   
  Description
     Text
      The degrees of the variables for the i-th projective space are indexed
      x_(i,0),..,x_(i,n_i-1), and have degree (0..0,1,0,..0) with a 1 in the i-th place.
      The script also caches some values in S.TateData and
      E.TateData, so that S and E can subsequently find eachother
      and also their cohomology ring.
     Example
        (S,E)=productOfProjectiveSpaces{1,2}
	vars S
	vars E
	(S,E) = productOfProjectiveSpaces({1,1},
	    Variables =>{getSymbol "u",getSymbol"v"},
	    CohomologyVariables =>{getSymbol "p",getSymbol "q"},
	    CoefficientField => QQ)
	(coefficientRing S) === (coefficientRing E)
	trim (ideal vars S)^2
        trim (ideal vars E)^2	
	peek S.TateData
	S.TateData#CohomRing
///

doc ///
   Key
    CoefficientField
   Headline
    Option for productOfProjectiveSpaces
   Description
    Text
     Base field for the two polynomial rings
///


doc ///
   Key
    Rings
   Headline
    Option for productOfProjectiveSpaces
   Description
    Text
     Base field for the two polynomial rings
///


doc ///
   Key
    CohomologyVariables
   Headline
    Option for productOfProjectiveSpaces
   Description
    Text
     names of the variables in cohomRing, the "cohomology ring"
///


doc///
  Key
    symExt
    (symExt,Matrix,Ring)
  Headline
    from linear presentation matrices over S to linear presentation matrices over E and conversely 
  Usage
    symExt(m,E)
  Inputs
    m: Matrix
       a linear presentation matrix over S 
    E: Ring
       the Koszul dual ring of S
  Outputs
     : Matrix
       the corresponding linear presentation matrix over E
  Description
     Text
       Same method as in the single factor case
     Example
        n={1,2}
	(S,E) = productOfProjectiveSpaces n
	vars S, vars E
        m=map(S^4,S^{{ -1,0},{0,-1}}, transpose matrix{{S_0,S_1,0,0},{S_2,0,S_3,S_4}})
        mE=symExt(m,E)

///



-*
doc ///
  Key
    ringData
    (ringData,Ring)
    (ringData,Module)
  Headline
    collect basic data of a ZZ^t graded ring
  Usage
    (t,v,n,varsList,irrList) = ringData E
  Inputs
    E: PolynomialRing
       a ZZ^t graded polynomial ring
  Outputs
    t: ZZ
       the number of factors
    v: List
       the number of generators of E in each degree
    n: List
       the dimensions of the factors 
    varsList: List
       List of List of generators in the various degrees
    irrList: List of corresponding (geometrically irrelevant prime) ideals   
  Description
     Text
       E is an ZZ^t graded polynomial ring. The functions returns basic data
       needed in many subroutines of the package TateOnProducts
     Example
        (S,E)=setupRings(ZZ/101,{1,2})
	(t,v,n,varsList,irrList)=ringData E;
	t==2
	(v,n)
	v==n+toList(t:1)
	varsList
	irrList       		
///
*-

doc ///
  Key
    upperCorner
    (upperCorner,ChainComplex,List)
  Headline
    compute the upper corner 
  Usage
     m=upperCorner(F,d)
  Inputs
   F: ChainComplex
      over the exterior algebra
   d: List
      a (multi)-degree 
  Outputs
     : Matrix
       a submatrix of the differential $F_k -> F_{k-1}$
  Description
     Text
       Let $k = -|d|$ be the total degree and $G \subset F_k$ the summand spanned by the generators of $F_k$ in degree d, 
       $H \subset F_{k-1}$ the summand spanned by generators of degree d' with $0 \le d'-d \le n$. The function returns
       the corresponding submatrix $m: G -> H$ of the differential.
       
       So the source will be generated in a single degree, and the target will be generated
       in multiple degrees.  The names comes from the fact that when we resolve this map,
       this map creates the "upper corner" in the corner complex.
     Example
        n={1,2};
        (S,E) = productOfProjectiveSpaces n
        
        F=dual res((ker transpose vars E)**E^{{ 2,3}},LengthLimit=>4)
	cohomologyMatrix(F,-{3,3},{4,4})
        betti F
	tallyDegrees F
        deg={2,1} 
        m=upperCorner(F,deg);
        tally degrees target m, tally degrees source m
        Fm=(res(coker m,LengthLimit=>4))[sum deg+1]
        betti Fm
        cohomologyMatrix(Fm,-{3,3},{4,4})
///



doc ///
  Key
    lowerCorner
    (lowerCorner,ChainComplex,List)
  Headline
    compute the lower corner 
  Usage
     m=lowerCorner(F,d)
  Inputs
   F: ChainComplex
      over the exterior algebra
   d: List
      a (multi)-degree 
  Outputs
     : Matrix
       a submatrix of the differential $F_{k+1} -> F_{k}$
  Description
     Text
       Let $k = |deg|$ be the total degree and $G \subset F_k$ the summand spanned by the generators of $F_k$ in degree d, 
       $H \subset F_{k+1}$ the summand spanned by generators of degree d' with $0 \le d-d' \le n$. The function returns
       the corresponding submatrix $m: H -> G$ of the differential.
     Example
        n={1,2}; 
	(S,E) = productOfProjectiveSpaces n
        F=dual res((ker transpose vars E)**E^{{ 2,3}},LengthLimit=>4)
        betti F
	tallyDegrees F
        deg={2,1} 
        m=lowerCorner(F,deg);
        tally degrees target m, tally degrees source m
        Fm=(res(coker m,LengthLimit=>7))[sum deg]
        betti Fm
        cohomologyMatrix(Fm,-{3,3},{4,4})
///



----------------------------
-- numerical information  --
----------------------------
doc ///
  Key
    cohomologyMatrix
    (cohomologyMatrix, Module, List, List)
    (cohomologyMatrix,ChainComplex,List,List)
  Headline
    cohomology groups of a sheaf on P^{n_1}xP^{n_2}, or of (part) of a Tate resolution
  Usage
    H=cohomologyMatrix(M,low,high)    
    H=cohomologyMatrix(T,low,high)
  Inputs
    T: ChainComplex
       free complex over the exterior algebra 
    M: Module
       graded module representing a sheaf on a product of projective spaces
    low: List
    high: List
       two lists low=\{a_1,a_2\}, high=\{b_1,b_2\} representing bidegrees
  Outputs
    H: Matrix
       a (1+b1-a1)x(1+b2-a2) matrix of ring elements in $\mathbb Z[h,k]$
  Description
     Text
       If M is a bigraded module over a bigraded polynomial ring representing a sheaf F on
       P^{n_1} x P^{n_2}, the script returns a block of the cohomology table, represented
       as a table of "cohomology polynomials" in $\mathbb Z[h,k]$ of the form
       $$\sum_{i=0}^{|n|} \, dim H^i(\mathcal F(c_1,c_2)) * h^i$$
       in each position \{c_1,c_2\}
       for $a_1 \le c_1 \le b_1$ and $a_2 \le c_2 \le b_2$.
       In case M corresponds to an object in the derived category D^b(P^{n_1}x P^{n_2}), then
       hypercohomology polynomials are returned, with the convention that k stands for k=h^{ -1}.

       The polynomial for
       \{b_1,b_2\} sits in the north-east corner, the one corresponding to (a_1,a_2) in the south-west
       corner.       
       
       In the case of a product of more (or fewer) projective spaces, or if a hash table
       output is desired, use
       cohomologyHashTable or cohomologyPolynomialTable instead.
              
       The script computes a sufficient part of the Tate resolution for F, and then
       calls itself in the version for a Tate resolution. More generally,
       If T is part of a Tate resolution of F
       the function returns a matrix of cohomology polynomials corresponding to T.
       
       If T is not a large enough part of the Tate resolution, such as W below, 
       then the function collects only
       the contribution of T to the cohomology table of the Tate resolution, according to the formula in
       Corollary 0.2 of
       @ HREF("https://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces")@.
        
     Example
    	(S,E) = productOfProjectiveSpaces{1,2}	
	M = S^1
	low = {-3,-3};high={0,0};
	cohomologyMatrix(M,low,high)
     Text
        As a second example, consider
	the structure sheaf
	$\mathcal O_E$ of a nonsingular cubic contained in (point)xP^2.
	The corresponding graded module is
     Example
	M = S^1/ideal(x_(0,0), x_(1,0)^3+x_(1,1)^3+x_(1,2)^3)
	low = {-3,-3};high={0,0};
	cohomologyMatrix(M,low,high)
     Text	
	and the "1+h" in the Northeast (= upper right) corner signifies that
	that $h^0(\mathcal O_E) = h^1(\mathcal O_E) = 1.$
///

doc ///
  Key
    cohomologyHashTable
    (cohomologyHashTable,Module,List,List)
    (cohomologyHashTable,ChainComplex,List,List)
  Headline
    cohomology groups of a sheaf on a product of projective spaces, or of (part) of a Tate resolution
  Usage
    H=cohomologyHashTable(M,low,high)      
    H=cohomologyHashTable(T,low,high)
  Inputs
    M: Module
       graded module representing a sheaf on a product of projective spaces
    T: ChainComplex
       free complex over the exterior algebra 
    low: List
    high: List
       two lists representing multi-degrees, the range for computation.
  Outputs
    H: HashTable
       values are dimensions of (hyper)cohomology groups
  Description
     Text
       If M is a multi-graded module representing a coherent sheaf F on $P^n := P^{n_0} x .. x P^{n_{t-1}}$, 
       the script returns a hash table with entries 
       {a,i} => h^i(F(a))
       where a is a multi-index, low<=a<=high in the partial order 
       (thus the value is 0 when i is not in the range 0..sum n.)
       In case T is a Tate resolution corresponding to an object F in D^b(P^n), then
       the values returned are the dimensions of the hypercohomology groups of twists of F, and
       the values can be nonzero in a wider range.
       
       In case the number of factors t is 2, the output of @ TO cohomologyMatrix @ is 
       easier to parse.
              
       The script computes a sufficient part of the Tate resolution for F, and then
       calls itself in the version for a Tate resolution. 
       
       If T is not a large enough part of the Tate resolution, such as W below, 
       then the function collects only
       the contribution of T to the cohomology table of the Tate resolution, according to the formula in
       Corollary 0.2 of 
       @ HREF("https://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces")@.
        
     Example
    	(S,E) = productOfProjectiveSpaces{1,2}	
	M = S^1
	low = {-3,-3};high = {3,3};
	H = cohomologyHashTable(M, low,high);
     Text
        We can print just the entries representing nonzero cohomology
	groups:
     Example
	H' = hashTable(select(pairs H, p-> p_1!=0))
     Text
        In the case of two factors (t=2), the same 
	information can be read conveniently from a matrix
     Example
        cohomologyMatrix(M, low, high)
     Text
        where the entry in the a= \{a_0,a_1\} place is
	sum_i h^i(F(a)*h^i \in ZZ[h].
        In the case of more factors, the same format is available
	through the command
     Example
	cohomologyPolynomialTable H'
  Caveat
        In case of hypercohomology, we write k 
	instead of h^{-1}, and use the cohomology ring
	ZZ[h,k].
  SeeAlso
        productOfProjectiveSpaces
     	cohomologyMatrix
        cohomologyPolynomialTable
	cornerComplex
///

doc ///
  Key
    cohomologyPolynomialTable
    (cohomologyPolynomialTable,Module,List,List)
    (cohomologyPolynomialTable,ChainComplex,List,List)
    (cohomologyPolynomialTable,HashTable)    
  Headline
    cohomology groups of a sheaf on a product of projective spaces, or of (part) of a Tate resolution
  Usage
    H=cohomologyPolynomialTable H'      
    H=cohomologyPolynomialTable(M,low,high)      
    H=cohomologyPolynomialTable(T,low,high)
  Inputs
    H': HashTable
       output of cohomologyHashTable
    M: Module
       graded module representing a sheaf on a product of projective spaces
    T: ChainComplex
       free complex over the exterior algebra 
    low: List
    high: List
       two lists representing multi-degrees, the range for computation.
  Outputs
    H: HashTable
       values are hypercohomology polynomials
  Description
     Text
       If M is a multi-graded module representing a coherent sheaf F on $P^n := P^{n_0} x .. x P^{n_{t-1}}$, 
       the script returns a hash table with entries 
       a => sum_h^i(F(a))*h^i \in ZZ[h,k], 
       where k represents h^{-1},
       where a is a multi-index, low<=a<=high in the partial order 
       (thus the value is 0 when i is not in the range 0..sum n.)
       In case T is a Tate resolution corresponding to an object F in D^b(P^n), then
       the values returned are the polyomials of the hypercohomology groups of twists of F, and
       the values can be nonzero in a wider range.
       
       In case the number of factors t is 2, the output of @ TO cohomologyMatrix @ is 
       easier to parse.
              
       The script computes a sufficient part of the Tate resolution for F, and then
       calls itself in the version for a Tate resolution. 
       
       If T is not a large enough part of the Tate resolution, such as W below, 
       then the function collects only
       the contribution of T to the cohomology table of the Tate resolution, according to the formula in
       Corollary 0.2 of 
       @ HREF("https://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces")@.
        
     Example
    	(S,E) = productOfProjectiveSpaces{1,2}	
	M = S^1
	low = {-3,-3};high = {3,3};
	H' = cohomologyHashTable(M, low,high);
	H = cohomologyPolynomialTable H'
	H = cohomologyPolynomialTable (M, low, high)
     Text
        We can print just the entries representing nonzero cohomology
	groups:
     Example
	trimH = hashTable(select(pairs H, p-> p_1!=0))
     Text
        In the case of two factors (t=2), the same 
	information can be read conveniently from a matrix
     Example
        cohomologyMatrix(M, low, high)
     Text
        where the entry in the a= \{a_0,a_1\} place is
	sum_i h^i(F(a)*h^i \in ZZ[h].
  Caveat
        In case of hypercohomology, we write k 
	instead of h^{-1}, and use the cohomology ring
	ZZ[h,k].
  SeeAlso
        productOfProjectiveSpaces
     	cohomologyMatrix
        cohomologyHashTable
///


doc ///
  Key
    tallyDegrees
    (tallyDegrees,ChainComplex)
  Headline
    collect the degrees of the generators of the terms in a free complex
  Usage
    tallyDegrees C
  Inputs
    C: ChainComplex
        a complex of graded free modules
  Outputs
     : Sequence
       a sequence of tallies of the degrees of the generators of the free module
  Description
     Text
       Returns for each free module C_d in the complex the result of @ TO tally @ @ TO degrees @ C_d
     Example
        S=ZZ/101[x_0..x_1,y_0,z_0,Degrees=>{2:{2,0,0},1:{0,1,0},{0,0,1}}]
	C =res ideal vars S
	betti C
	tallyDegrees C	
///


-----------------------------------------
--- formal chain complex manipulations --
-----------------------------------------
doc ///
  Key
    nonzeroMax  
    (nonzeroMax,ChainComplex)
  Headline
    computes the homological position of the last non-zero module in a ChainComplex 
  Usage
    nonzeroMin C
    nonzeroMax C
  Inputs
    C: ChainComplex
  Outputs
     : ZZ
  Description
     Text
       The function @ TO  max @ applied to a chain complex returns the largest 
       position of a defined term in a 
       chain complex, which very well might be the zero module. The function nonzeroMax returns
       the largest positions of a non-zero module.  
     Example
       S=ZZ/101[x,y]/ideal(x*y)
       C=chainComplex(matrix{{x}},matrix{{y}}**S^{ -1},matrix{{x}}**S^{ -2})[1] 
       isChainComplex C
       C'=prependZeroMap appendZeroMap C
       min C', nonzeroMin C'
       max C', nonzeroMax C'
///




doc ///
  Key
    nonzeroMin
    (nonzeroMin,ChainComplex)    
  Headline
    computes the homological position of the first non-zero module in a ChainComplex 
  Usage
    nonzeroMin C
  Inputs
    C: ChainComplex
  Outputs
     : ZZ
  Description
     Text
       The function @ TO  min @ applied to a chain complex returns the smallest 
       position of a defined term in a 
       chain complex, which very well might be the zero module. The function nonzeroMin return
       the smallest positions of a non-zero module.  
     Example
       S=ZZ/101[x,y]/ideal(x*y)
       C=chainComplex(matrix{{x}},matrix{{y}}**S^{ -1},matrix{{x}}**S^{ -2})[1] 
       isChainComplex C
       C'=prependZeroMap appendZeroMap C
       min C', nonzeroMin C'
       max C', nonzeroMax C'
///

doc ///
  Key
    appendZeroMap
    (appendZeroMap,ChainComplex)    
  Headline
    append a zero map to chain complex 
  Usage
    appendZeroMap C
  Inputs
    C: ChainComplex
  Outputs
     : ChainComplex
  Description
     Text
       Add a zero map after the last differential in a chain complex.
     Example
       S=ZZ/101[x,y]/ideal(x*y)
       C=chainComplex(matrix{{x}},matrix{{y}}**S^{ -1},matrix{{x}}**S^{ -2})[1] 
       appendZeroMap C
       prependZeroMap C
///

doc ///
  Key
    prependZeroMap
    (prependZeroMap,ChainComplex)    
  Headline
    prepend a zero map to chain complex 
  Usage
    prependZeroMap C
  Inputs
    C: ChainComplex
  Outputs
     : ChainComplex
  Description
     Text
       Add a zero map before the first differential in a chain complex.
     Example
       S=ZZ/101[x,y]/ideal(x*y)
       C=chainComplex(matrix{{x}},matrix{{y}}**S^{ -1},matrix{{x}}**S^{ -2})[1] 
       prependZeroMap C
       appendZeroMap C       
///

doc ///
  Key
    removeZeroTrailingTerms
    (removeZeroTrailingTerms,ChainComplex)    
  Headline
    remove trailing zero terms of a chain complex 
  Usage
    removeZeroTrailingTerms C
  Inputs
    C: ChainComplex
  Outputs
     : ChainComplex
  Description
     Text
       Remove trailing zero terms in a complex
     Example
       S=ZZ/101[x,y]/ideal(x*y)
       C=prependZeroMap appendZeroMap chainComplex(matrix{{x}},matrix{{y}}**S^{ -1},matrix{{x}}**S^{ -2})[1] 
       removeZeroTrailingTerms C
     Text
       If C has only one nonzero term, then the functions returns two zero maps.
     Example
       S=ZZ
       C=prependZeroMap  chainComplex( map(S^0,S^1,0))[3]
       removeZeroTrailingTerms C      
///

doc ///
  Key
    trivialHomologicalTruncation
    (trivialHomologicalTruncation,ChainComplex,ZZ,ZZ)    
  Headline
    return the trivial truncation of a chain complex 
  Usage
    trivialHomologicalTruncation(ChainComplex,d,e)
  Inputs
    C: ChainComplex
    d: ZZ
    e: ZZ
       homological indices
  Outputs
     : ChainComplex
  Description
     Text
       Given a chain complex
        
        ... <- C_{k-1} <- C_k <- C_{k+1} <- ...
	
       return the trivial truncation
       
       0 <- C_d <- C_{d+1} <- ... < C_e <- 0
     Example
       E=ZZ/101[e_0,e_1,SkewCommutative=>true];F=res ideal vars E;
       C=dual res (coker transpose F.dd_3,LengthLimit=>8)[-3]
       C1=trivialHomologicalTruncation(C,-2,2)
       C2=trivialHomologicalTruncation(C1,-3,3)
       C3=removeZeroTrailingTerms C2
       C4=trivialHomologicalTruncation(C3,2,2)             
///


doc ///
  Key
    isChainComplex
    (isChainComplex,ChainComplex)
  Headline
    checks whether the differentials compose to zero 
  Usage
    isChainComplex C
  Inputs
    C: ChainComplex
  Outputs
     : Boolean
  Description
     Text
       checks that all differentials compose to zero.
     Example
       S=ZZ/101[x,y]
       C=res ideal vars S, C'=chainComplex(matrix{{x}},matrix{{y}}) 
       isChainComplex C, isChainComplex C'       
     Text
       The buildin function @ TO dual @
       for chainComplexes over the exterior algebra
       does not return a complex, because the dual of a left module is a right module. 
     Example
        kk=ZZ/101;n=4;
	E=kk[e_0..e_n,SkewCommutative =>true]
	m=map(E^{0,1},,matrix{{ e_0,e_1*e_2},{e_3*e_4,e_0*e_1*e_4}})
	fm=res coker m
	isChainComplex fm
	dualfm = dual fm	
	isChainComplex dualfm
	f2=res( coker dualfm.dd_(-5),LengthLimit=> 6)[6]
	betti f2
	betti dual fm
///


doc ///
   Key
    minimize
    (minimize, ChainComplex)
   Headline
    minimal quotient complex of a free ChainComplex 
   Usage
    m = minimize F
   Inputs
    F:ChainComplex
     chain complex of free modules
   Outputs
    m:ChainComplexMap
     quasi-isomorphism F -> F', where F' is a minimal free complex
   Description
    Text
     For the quasi-isomorphism from a minimal subcomplex use 
     
     dual minimize dual F
    
     To simplify the notation consider the complex C = E[min E] that
     is shifted so that the first module is C_0.
     The algorithm:
     Set dbar = the reduction of the differential d mod the maximal ideal.
     a complement of ker dbar, and compute the idempotent rho: E -> E.
     the map rho is not a chain complex map, but the image of 
     (rho | d*rho): C ++ C[1] --> C is a subcomplex and 
     the minimization of  C is the complex C/image(rho|d*rho).
     The script returns the ChainComplexMap from the minimization to C.
     Example
       S=ZZ/101[x,y];
       m= map(S^{0,1},S^{0,-1}, matrix{{1,x},{y,x^2}})
       C=chainComplex{m} 
       Cmin=target minimize C
       betti C, betti Cmin
       m, Cmin.dd_1  
    Text     
     For a more interesting illustration we first make a nonminimal complex by adding 
     trivial complexes to a minimal complex and then mixing things up
     by conjugating with general isomorphisms:
    Example
     S = ZZ/32003[a,b,c]
     red = map(S,S,toList(numgens S:0_S))
     C = koszul gens (ideal vars S)^2
     G = S^{0,-1,-2,-3,-4,-5,-6}
     D = apply(length C+1, i-> C_i++G++G)
     zG = map(G,G,0)
     difs0 = apply(length C, i-> (map(D_i, D_(i+1), matrix{{C.dd_(i+1), map(C_i,G,0), map(C_i,G,0)},{map(G,C_(i+1),0), zG, zG},{map(G,C_(i+1),0), id_G, zG}})));
     len = #difs0
     Q = apply(len, i-> random(target difs0_i, target difs0_i))|
       {random(source difs0_(len-1), source difs0_(len-1))};
     difs1 = apply(len, i-> Q_i*difs0_i*Q_(i+1)^(-1));
     E = chainComplex difs1
     isMinimalChainComplex E
    Text
     Now we minimize the result. The free summand we added to the end
     maps to zero, and thus is part of the minimization.
    Example
     time m = minimize (E[1]);
     isQuism m
     E[1] == source m
     E' = target m
     isChainComplex E'
     isMinimalChainComplex E'
///



--------------------------
-- subcomplexes         --
--------------------------
{*
doc ///
  Key
    regionComplex
    (regionComplex,ChainComplex,List,List,List,List)
  Headline
    form the region complex 
  Usage
    regionComplex(T,c,I,J,K)
  Inputs
    T: ChainComplex
       a (part of a) Tate resolution on a product of t projective spaces
    c: List
       a degree
    I: List
    J: List
    K: List 
       disjoint subsets of the List 1..t  
  Outputs
     : ChainComplex
  Description
     Text
       Forms the region complex T_c(I,J,K) as defined in section 3 of 
       @  HREF("http://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces") @. 
     Example
       (S,E) = 	productOfProjectiveSpaces {1,2}
///

doc ///
  Key
    strand
    (strand,ChainComplex,List,List)
  Headline
    form the strand 
  Usage
    strand(T,c,I)
  Inputs
    T: ChainComplex
       a (part of a) Tate resolution on a product of t projective spaces
    c: List
       a degree
    I: List
    subsets of the List 1..t  
  Outputs
     : ChainComplex
  Description
     Text
       Form the I-th strand through c as defined  
       @  HREF("http://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces") @. 
     Example
        (S,E) = productOfProjectiveSpaces {1,2}     
        ///
*}

doc ///
  Key
    regionComplex
    (regionComplex,ChainComplex,List,Sequence)
  Headline
    region complex
  Usage
    regionComplex(T,c,IJK)
  Inputs
    T: ChainComplex
       over the exterior algebra
    c: List
       a (multi) degree 
    IJK: Sequence
       a sequence (I,J,K) of disjoint subsets of \{0..t-1\}       
  Outputs
     : ChainComplex
       a region complex of T
  Description
     Text
        We compute the region complex of T as defined in
        @ HREF("http://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces") @
	section 3. Note that different from the paper I,J,K are sublists of 0...t-1 and not subsets of 1..t. 
	In the examples below, only rT2 and rT3 are proper region complexes. 
     Example
        n={1,1};
        (S,E) = productOfProjectiveSpaces n;
	T1 = (dual res trim (ideal vars E)^2)[1];
	a=-{2,2};T2=T1**E^{a}[sum a];
	W=beilinsonWindow T2,cohomologyMatrix(W,-2*n,2*n)
        T=sloppyTateExtension W;
	cohomologyMatrix(T,-{3,3},{3,3})
	c={1,0}
	rT0=regionComplex(T,c,({},{0,1},{})); --a single position
	cohomologyMatrix(rT0,-{3,3},{3,3})
    	rT1=regionComplex(T,c,({0},{1},{})); --a horizontal half line
	cohomologyMatrix(rT1,-{3,3},{3,3})
       	rT2=regionComplex(T,c,({},{0},{})); -- a vertical line
    	cohomologyMatrix(rT2,-{3,3},{3,3})
	rT3=regionComplex(T,c,({},{},{1})); -- a upper half plane
    	cohomologyMatrix(rT3,-{3,3},{3,3})
	rT4=regionComplex(T,c,({0},{},{1})); --a north east quadrant
    	cohomologyMatrix(rT4,-{3,3},{3,3})
	rT5=regionComplex(T,c,({1},{},{0})); --a south west quadrant
    	cohomologyMatrix(rT5,-{3,3},{3,3})
///


doc ///
  Key
    strand
    (strand,ChainComplex,List,List)
  Headline
    take the strand
  Usage
    strand(T,c,I)
  Inputs
    T: ChainComplex
       over the exterior algebra
    c: List
       a (multi) degree 
    I: List
       a sublist of \{0..t-1\} , where t denotes the number of factors      
  Outputs
     : ChainComplex
       the I-strand of T through c
  Description
     Text
        We compute the strand of T as defined in @
        HREF("http://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces") @
	Theorem 0.4. If T is (part of) the Tate resolution of a sheaf $F$, then the I-strand of $T$ through $c$
	correponds to the Tate resolution $R{\pi_J}_*(F(c))$ where $J =\{0,\ldots,t-1\} - I$ is the complement and $\pi_J: \mathbb PP \to \prod_{j \in J} \mathbb P^{n_j}$ 
	denotes the projection. 
     Example
        n={1,1};
        (S,E) = productOfProjectiveSpaces n;
	T1 = (dual res trim (ideal vars E)^2)[1];
	a=-{2,2};T2=T1**E^{a}[sum a];
	W=beilinsonWindow T2,cohomologyMatrix(W,-2*n,2*n)
        T=sloppyTateExtension W;
	cohomologyMatrix(T,-{3,3},{3,3})
	sT1=strand(T,-{1,1},{1});
	cohomologyMatrix(sT1,-{3,3},{3,3})
	sT2=strand(T,{1,1},{0});
	cohomologyMatrix(sT2,-{3,3},{3,3})
	sT3=removeZeroTrailingTerms strand(T,{1,-1},{0,1})
	cohomologyMatrix(sT3,-{3,3},{3,3})
	
///



doc ///
  Key
    firstQuadrantComplex
    (firstQuadrantComplex,ChainComplex,List)
  Headline
    form the first quadrant complex 
  Usage
    firstQuadrantComplex(T,c)
  Inputs
    T: ChainComplex
       a (part of a) Tate resolution on a product of t projective spaces
    c: List
       cohomological degree of the lower corner of the first complex 
  Outputs
     : ChainComplex
  Description
     Text
       Form the first quadrant complex with corner c of a (part of a) Tate resolution T as defined in
       @  HREF("http://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces") @. 
     Example
        (S,E) = productOfProjectiveSpaces {1,1};
	T1= (dual res( trim (ideal vars E)^2,LengthLimit=>8))[1];
        T=trivialHomologicalTruncation(T2=res(coker upperCorner(T1,{4,3}),LengthLimit=>13)[7],-5,6);
    	betti T
	cohomologyMatrix(T,-{4,4},{3,2})
    	fqT=firstQuadrantComplex(T,-{2,1});
    	betti fqT	
	cohomologyMatrix(fqT,-{4,4},{3,2})
	cohomologyMatrix(fqT,-{2,1},-{1,0})
	lqT=lastQuadrantComplex(T,-{2,1});
    	betti lqT	
	cohomologyMatrix(lqT,-{4,4},{3,2})
	cohomologyMatrix(lqT,-{3,2},-{2,1})
	cT=cornerComplex(T,-{2,1});
	betti cT	
	cohomologyMatrix(cT,-{4,4},{3,2})
///

doc ///
  Key
    lastQuadrantComplex
    (lastQuadrantComplex,ChainComplex,List)
  Headline
    form the last quadrant complex
  Usage
    lastQuadrantComplex(T,c)
  Inputs
    T: ChainComplex
       a (part of a) Tate resolution on a product of t projective spaces
    c: List
       cohomological degree of the lower corner of the complementary first quadrant complex 
  Outputs
     : ChainComplex
  Description
     Text
       Form the last quadrant complex with corner c of a (part of a) Tate resolution T as defined in
       @  HREF("http://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces") @. 
     Example
        (S,E) = productOfProjectiveSpaces {1,1};
	T1= (dual res( trim (ideal vars E)^2,LengthLimit=>8))[1];
        T=trivialHomologicalTruncation(T2=res(coker upperCorner(T1,{4,3}),LengthLimit=>13)[7],-5,6);
    	betti T
	cohomologyMatrix(T,-{4,4},{3,2})
    	fqT=firstQuadrantComplex(T,-{2,1});
    	betti fqT	
	cohomologyMatrix(fqT,-{4,4},{3,2})
	cohomologyMatrix(fqT,-{2,1},-{1,0})
	lqT=lastQuadrantComplex(T,-{2,1});
    	betti lqT	
	cohomologyMatrix(lqT,-{4,4},{3,2})
	cohomologyMatrix(lqT,-{3,2},-{2,1})
	cT=cornerComplex(T,-{2,1});
	betti cT	
	cohomologyMatrix(cT,-{4,4},{3,2})	
///

doc ///
  Key
    cornerComplex
    (cornerComplex,ChainComplex,List)
    (cornerComplex,Module,List,List)    
  Headline
    form the corner complex
  Usage
    C = cornerComplex(T,c)
    C = cornerComplex(M,low,high)
  Inputs
    T: ChainComplex
       a (part of a) Tate resolution on a product of t projective spaces
    c: List
       cohomological degree of upper corner of the  last quadrant complex which is part of the corner complex   
    M: Module
       multi-graded module representing a sheaf F
  Outputs
    C : ChainComplex
       The corner complex
  Description
     Text
       The call
       
       cornerComplex(M,low,high)
       
       forms the corner complex of the sheaf F represented by M,
       computed in such a way that all the cohomology groups of
       twists F(a) of F can be computed for low <= a <= high.

       The call
       
       cornerComplex(T,c)
       
       forms the corner complex with corner c of a (part of a) Tate resolution T as defined in
       @  HREF("http://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces") @. 
       
     Example
        (S,E) = productOfProjectiveSpaces{1,1}
	T1= (dual res( trim (ideal vars E)^2,LengthLimit=>8))[1];
        T=trivialHomologicalTruncation(T2=res(coker upperCorner(T1,{4,3}),LengthLimit=>13)[7],-5,6);
    	betti T
	cohomologyMatrix(T,-{4,4},{3,2})
    	fqT=firstQuadrantComplex(T,-{2,1});
    	betti fqT	
	cohomologyMatrix(fqT,-{4,4},{3,2})
	cohomologyMatrix(fqT,-{2,1},-{1,0})
	lqT=lastQuadrantComplex(T,-{2,1});
    	betti lqT	
	cohomologyMatrix(lqT,-{4,4},{3,2})
	cohomologyMatrix(lqT,-{3,2},-{2,1})
	cT=cornerComplex(T,-{2,1});
	betti cT	
	cohomologyMatrix(cT,-{4,4},{3,2})
///
{*     Example
        (S,E) = productOfProjectiveSpaces {1,2};
	T1= (dual res( trim (ideal vars E)^2,LengthLimit=>4))
	isChainComplex T1
	tallyDegrees T1
	cohomologyMatrix(T1,-{10,10},{10,10})
	cohomologyMatrix(T1[-3],-{10,10},{10,10})
	T2=T1++T1**E^{{1,1}}[2]
    	T3=firstQuadrantComplex(T2,-{1,1})
	d= nonzeroMax T3;
	T4=res(coker T3.dd_d,LengthLimit=>13)[-d];
	T=removeZeroTrailingTerms lastQuadrantComplex(T4,-{3,3})
	betti T
        cT=cornerComplex(T,-{1,1})
	betti cT
	cohomologyMatrix(cT,-{4,4},{3,3})
	cohomologyMatrix(T,-{4,4},{3,3})	
*}


-------------------------------------------------
-- Beinson monads, Tate extension              --
-------------------------------------------------
doc ///
  Key
    beilinsonWindow
    (beilinsonWindow,ChainComplex)
  Headline
    extract the subquotient complex which contributes to the Beilinson window
  Usage
    W=beilinsonWindow T
  Inputs
    T: ChainComplex
       a (part of a) Tate resolution on a product of t projective spaces
  Outputs
    W: ChainComplex
  Description
     Text
       Extract the terms which under the U-functor defined in 
       @  HREF("http://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces") @
       contributed to the Beilinson complex U(T) of T, i.e. W is the smallest free subquotient complex of T
       such that U(W) = U(T) 
     Example
        n={1,1};
        (S,E) = productOfProjectiveSpaces n;
        W=(chainComplex {map(E^0,E^1,0),map(E^1,E^0,0)})[1]
        time T=sloppyTateExtension W;
        cohomologyMatrix(T,-{3,3},{3,3})
	W=beilinsonWindow T
	cohomologyMatrix(W,-{2,2},{2,2})
        a={2,-3}
        W2=removeZeroTrailingTerms beilinsonWindow (T**E^{a}[sum a])
        cohomologyMatrix(W2,-{2,2},{2,2})
        cohomologyMatrix(sloppyTateExtension W2,-{2,2},{2,2})
///

doc ///
  Key
    sloppyTateExtension
    (sloppyTateExtension,ChainComplex)
  Headline
    extend the terms in the Beilinson window to a part of a corner complex of the corresponding Tate resolution
  Usage
    T=sloppyTateExtension W
  Inputs
    W: ChainComplex
       terms in the Beilinson window of a Tate resolution 
  Outputs
    T: ChainComplex
       a corner complex of the corresponding Tate resolution 
  Description
     Text
       Every object F in in the derived category D^d(P) 
       of coherent sheaves on a product P=P^{n_1}x..xP^{n_t} of t projective space is of the 
       form U(W) with W a complex with terms in the 
       Beilinson range only. The function computes with the algorithm (not!) described in section 4 of 
       @ HREF("http://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces") @
       computes part of a suitable choosen corner complex of the Tate resolution T(F). 
       
       The phrase sloppy refers to the fact that the Beilinson window of T is not equal, 
       but only isomorphic to W. Moreover the bounds in the computation are only a guess and certainly not optimal.
     Example
        n={1,1};
        (S,E) = productOfProjectiveSpaces n;
	T1 = (dual res trim (ideal vars E)^2)[1];
    	isChainComplex T1
	a=-{2,2};
	T2=T1**E^{a}[sum a];
	W=beilinsonWindow T2
	cohomologyMatrix(W,-2*n,2*n)
        T=sloppyTateExtension W
	cohomologyMatrix(T,-3*n,4*n)
	cohomologyMatrix(beilinsonWindow T,-n,n)
	cohomologyMatrix(T,-5*n,4*n) -- the view including the corner
///

doc ///
  Key
    pushAboveWindow
    (pushAboveWindow,ChainComplex)
    (pushAboveWindow,List)
    (pushAboveWindow,Matrix)
    (pushAboveWindow,Matrix,Matrix)
    (pushAboveWindow,Matrix,Matrix,Matrix)
    (pushAboveWindow,Module)
  Headline
    push a projective resolution of the Beilinson complex out of the window
  Usage
    T=pushAboveWindow W
  Inputs
    W: ChainComplex
       terms in the Beilinson window of a Tate resolution 
  Outputs
    T: ChainComplex
       a non-minimal version of the quadrant complex ?!? qT_{\le 0} of the Tate resolution T=T(W) ?
  Description
     Text
       Every object F in in the derived category D^d(P) 
       of coherent sheaves on a product P=P^{n_1}x..xP^{n_t} of t projective space is of the 
       form U(W) with W a complex with terms in the 
       Beilinson range only. 
       This function is the first step in our computation of the algorithm 
        (not!) described in section 4 of 
       @ HREF("http://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces") @
       that computes part of a suitable choosen corner complex of the Tate resolution T(F).        
     Example
        n={1,1};
        (S,E) = productOfProjectiveSpaces n;
	T1 = (dual res trim (ideal vars E)^2)[1];
    	isChainComplex T1
	a=-{2,2};
	T2=T1**E^{a}[sum a];
	W=beilinsonWindow T2
	cohomologyMatrix(W,-2*n,2*n)
        T=sloppyTateExtension W;
	cohomologyMatrix(T,-5*n,4*n) -- a view with the corner
	puT=trivialHomologicalTruncation(pushAboveWindow W,-1, 6)
	cohomologyMatrix(puT,-3*n,{1,1})
	betti W
	qT=trivialHomologicalTruncation(lastQuadrantComplex(T,{0,0}),-1,6)
	cohomologyMatrix(qT,-3*n,{1,1})
	betti puT
	betti qT
    	betti T	
	puT.dd_3_{0}
///


doc ///
  Key
    beilinson
    (beilinson,Module)
    (beilinson,Matrix)
    (beilinson,ChainComplex)
    [beilinson,BundleType]
  Headline
    apply the beilinson funcor
  Usage
    M=beilinson F
    phi=beilison psi
    C=beilinson T
  Inputs
    F: Module
       a free module over the exterior algebra E
    psi: Matrix
       a map between free modules over E
    T: ChainComplex
       a complex of free modules over E
    BundleType => Symbol
       the possible values are SubBundle or PrunedQuotient    
  Outputs
    M: Module
       a module over the symmetric algebra S
    phi: Matrix
       a map between S-modules
    C: ChainComplex
       a chain complex of S-modules
  Description
     Text
       The Beilinson functor is a functor from the category of free E-modules to the category of coherent sheaves
       which associates to a cyclic free E-module of generated in multidegree a the vector bundle U^a.
       -- w_E or socle degree might be better.
       Note that the U^a for multidegrees a=\{a_1,...,a_t\} with 0 \le a_i \le n_i form a full exceptional series for 
       the derived category of coherent sheaves on the product
       PP = P^{n_1} \times ... \times P^{n_t} of t projective spaces, see e.g.
       @ HREF("http://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces") @.
        
       In the function we compute from a complex of free E-modules the corresponding complex of graded S-modules, whose
       sheafifications are the corresponding sheaves. The corresponding graded S-module are choosen as quotients of  
       free S-modules in case of the default option BundleType=>PrunedQuotient, or as submodules of free S-modules.
       The true Beilinson functor is obtained by the sheafication of resulting the complex.
       
       The Beilinson monad of a coherent sheaf $\mathcal F$ is the the sheafication of 
       beilinson( T($\mathcal F$)) of its Tate resolution T($\mathcal F$).         
     Example
        (S,E) = productOfProjectiveSpaces {2,1}     
        psi=random(E^{{-1,0}}, E^{{-2,-1}})
	phi=beilinson psi
	beilinson(E^{{-1,0}})
	T = chainComplex(psi)
	C = beilinson T
	betti T, betti C
  SeeAlso
    BundleType
    SubBundle
    PrunedQuotient	
///

doc ///
  Key
    beilinsonBundle
    (beilinsonBundle,ZZ,ZZ,Ring)
    (beilinsonBundle,List,Ring)    
    [beilinsonBundle,BundleType]
  Headline
    compute a basic Beilinson bundle
  Usage
    B=beilinsonBundle(i,whichblock,E)
    B=beilinsonBundle(a,E)
  Inputs
    i: ZZ
       0 \le i \le n_{whichblock}
    whichblock: ZZ
       0 \le whichblock \le r-1
    E: Ring
       the exterior algebra or symmetric algebra
    a: List
       of integers \{a_0,...,a_{(r-1)} \} 
    BundleType => Symbol
       the possible values are SubBundle or PrunedQuotient    
  Outputs
    B: Module
       a module over the symmetric algebra S
  Description
     Text
       The first version 
       computes a basic Beilinson bundle, i.e. the pullback of a Beilinson bundle from a single factor of a the product
       PP = P^{n_0} \times ... \times P^{n_{(r-1)}} of r projective spaces. 
       
       The second version computes the tensor product of the basic bundles beilinsonBundle(a_i,i,E) for i from 0 to r-1.
       See also
       @ HREF("http://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces") @.
        
       The vector bundle B is represented by its S-module of global sections, which is either the quotient or
        a submodule of a
       free S-modules depending on the value of the option BundleType.
       
       The results are stashed in E.TateData.BeilinsonBundles, so they are not recomputed.
       
     Example
        (S,E) = productOfProjectiveSpaces {2,3}     
        B1=beilinsonBundle(1,0,E)
	B2=beilinsonBundle(1,1,E)
	B=beilinsonBundle({1,1},E); betti B
	B1**B2 == B
  SeeAlso
    BundleType
    SubBundle
    PrunedQuotient	
///

doc ///
  Key
    beilinsonContraction
    (beilinsonContraction,RingElement,List,List)
  Headline
    compute a Beilinson contraction
  Usage
    beilinsonContraction(e, rowdeg, coldeg)
  Inputs
    e: RingElement
       an element of the exterior algebra E
    rowdeg: List
    coldeg: List 
        two multidegrees
    BundleType => Symbol
        with values PrunedQuotient or SubBundle 
  Outputs
      : Matrix
       a map between modules over the symmetric algebra S
  Description
     Text
       Returns a map between two Beilinson generators
       (i.e. the beilinson generators beilinsonBundle(a, E)
        of the derived category). 
        Note:
	 
        (1) (S,E) is the result of productOfProjectiveSpaces
       
        (2) e is a homogeneous element of E giving a map from E(-coldeg) --> E(-rowdeg).
 

       
     Example
        (S,E) = productOfProjectiveSpaces {2,1}     
	gens S, gens E
        f=e_(0,0)*e_(0,1)*e_(1,0)
	beilinsonContraction(f,{0,0},{2,1})
	m=beilinsonContraction(e_(0,0)*e_(1,0),{0,0},{1,1})
  Caveat 
    E is positively graded, in contrast to the paper!
  SeeAlso
    beilinson
    beilinsonBundle
    BundleType
    SubBundle
    PrunedQuotient	
///



doc ///
  Key
    BundleType
  Headline
    Option in beilinson with values PrunedQuotient or SubBundle 
  Description
     Text
      The Beilinson bundle U^a can be represented either by quotient or sub-bundles
  SeeAlso
    beilinson
///

doc ///
  Key
    SubBundle
  Headline
    value for the option BundleType in beilinson 
  Description
     Text
      The Beilinson bundlse U^a will be represented  by subbundles.
  SeeAlso
    beilinson
    BundleType
    PrunedQuotient
///

doc ///
  Key
    PrunedQuotient
  Headline
    value for the option BundleType in beilinson 
  Description
     Text
      The Beilinson bundles U^a will be represented by quotient bundles.
  SeeAlso
    beilinson
    BundleType
    SubBundle
///
--------------------------------------------------------------
-- Examples of the paper
--------------------------------------------------------------

doc ///
  Key
    cornerCohomologyTablesOfUa
    (cornerCohomologyTablesOfUa,List)
    (cornerCohomologyTablesOfUa,List,List)
  Headline
    cohomology tables of Ua and related complexes - Example 3.6
  Usage
     cornerCohomologyTablesOfUa(n,a)
     cornerCohomologyTablesOfUa(n)
  Inputs 
    n: List
       the list \{n_1,...,n_t\} \, of the dimensions of the factors
    a: List
       the list \{a_1,...,a_t\} \, of the exterior powers
  Outputs
     : List
       a list of four cohomology tables  
  Description
     Text
       Given a degree list  \{a_1,...,a_t\}\, with $0 \le a_i \le n_i$ 
       for $U^a = \Lambda^{a_1} U_1 \otimes \ldots  \otimes \,  \Lambda^{a_t} U_t$
       part of the Tate resolution T=T(U^a) gets computed. Four cohomology tables and two betti tables get returned:
       The cohomology table  
        
       of T,
       
       of the corner complex of T at c=-a, 
       
       of the Beilinson Window of T,
	   	   
       of the shifted corner complex at \{1,..,1\}, shifted by [-1],
       
       and the two betti table with respect to total degree of the two corner complexes above. 

       This illustrates the validity of Example 3.6 of our paper
       @ HREF("http://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces") @.
       Current implementation handles only the case of two factors.      
     Example
        netList cornerCohomologyTablesOfUa({1,2},{1,1})
///




     doc ///
        Key
	 resolutionOfChainComplex
	 (resolutionOfChainComplex, ChainComplex)
        Headline
	 free resolution of a chain complex
        Usage
	 F = resolutionOfChainComplex C
        Inputs
	 C:ChainComplex
        Outputs
	 F:ChainComplex
        Description
         Text
	  Given a chain complex C, the routine returns a surjective ChainComplexMap p:F->C from a free
	  complex. The complex F is constructed from minimal free resolutions of the terms of C
	  by the method of iterated mapping cones. 
	  
	  That is, if 
	  C: 0 -> Cn ->...->Cm ->0
	  is a chain complex, and Gi is a resolution of
	  Ci, and [G -> F] denotes the mapping cone of a map of complexes G \to F,
	  then the resolution of C is Gm if n=m; is [Gn->Gm] if n = m+1
	  and otherwise is defined inductively  as
	  Fi = [Gi -> F(i-1)]
	  where the map Gi -> F(i-1)
	  is induced by lifing Gi_0 --> G(i-1)_0 to the kernel of the (i-1)-st differential of
	  F(i-1).
	  
	  The complex F = source p is not necessarily minimal, but minimize F returns a morphism to a minimal free
	  chain complex quasi-isomorphic to F, and 
	  dual minimimize dual F
	  returns a quasi-isomorphism from a minimal free complex, so
	  
	  p*(dual minimimize dual F)
	  
	  is the quasi-isomorphism from the minimal free resolution of C.
         Example
	  kk= ZZ/101
	  S = kk[a,b,c]
	  R = S/ideal"ab2,a2c3"
	  f = map(R,S,vars R)
	  C = res(R^1/(ideal vars R))**(R^1/(ideal vars R)^5);
	  mods = for i from 0 to max C list pushForward(f, C_i);
	  C = chainComplex for i from min C+1 to max C list map(mods_(i-1),mods_i,substitute(matrix C.dd_i,S));
	  time m = resolutionOfChainComplex C;
	  betti source m
	  betti target minimize source m
        SeeAlso
	 minimize
     ///



document {
     Key => {isQuism, (isQuism,ChainComplexMap)},
     Headline => "Test to see if the ChainComplexMap is a quasiisomorphism.",
     Usage => "isQuism(phi)",
     Inputs => {
	  "phi" => {},
     },
     Outputs => {
	  {"Boolean"}
     },
     "A quasiisomorphism is a chain map that is an isomorphism in homology.",
     "Mapping cones currently do not work properly for complexes concentrated",
     "in one degree, so isQuism could return bad information in that case.",
     EXAMPLE {
	     "R = ZZ/101[a,b,c]",
	     "kRes = res coker vars R",
	     "multBya = extend(kRes,kRes,matrix{{a}})",
	     "isQuism(multBya)",
	     "F = extend(kRes,kRes,matrix{{1_R}})",
	     "isQuism(F)",
	     }
     }

document {
     Key => {chainComplexMap, (chainComplexMap,ChainComplex,ChainComplex,List)},
     Headline => "Defines a ChainComplexMap via a list of matrices.",
     Usage => "chainComplexMap(D,C,mapList)",
     Inputs => {
	  "D" => ChainComplex => {"target of ChainComplexMap"},
	  "C" => ChainComplex => {"source of ChainComplexMap"},
	  "mapList" => List => {"list of maps defining the new ChainComplexMap"},
     },
     Outputs => {
	  ChainComplexMap => {"The desired ChainComplexMap."},
     },
     EXAMPLE {
	     "R = ZZ/101[a,b,c]",
	     "kRes = res coker vars R",
	     "multBya = extend(kRes,kRes,matrix{{a}})",
	     "mapList = apply((min kRes..max kRes), i -> multBya_i)",
	     "multBya2 = chainComplexMap(kRes,kRes,toList mapList)",
	     "multBya2 == multBya",
	     }
     }
doc ///
   Key
    isMinimalChainComplex
   Headline
    tests for minimality
   Usage
    b = isMinimalChainComplex C
   Inputs
    C:ChainComplex
     chain complex of free modules
   Outputs
    b:Boolean
   Description
    Text
     The script tests whether all the differentials of C become zero when
     we substitute 0 for each variable of ring C
///

doc ///
   Key
    tateData
    (tateData, Ring)
   Headline
    reads TateData from the cache of an appropriate ring
   Usage
    T = tateData S
   Inputs
    S:Ring
     such as produced by productOfProjectiveSpaces
   Outputs
    T: HashTable
   Description
    Text
     The function
     productOfProjectiveSpaces 
     creates two rings and store various data in their cache table,
     which tateData reads.
    Example
     (S,E) = productOfProjectiveSpaces{1,2}
     T = tateData S
     peek T
     T === S.TateData
     peek E.TateData
     T === E.TateData
   SeeAlso
    productOfProjectiveSpaces
    BeilinsonBundles
///


doc ///
   Key
    bgg
    (bgg, Module)
   Headline
    make a linear free complex from an exterior module
   Usage
    LP = bgg P
   Inputs
    P: Module
     module over an exterior algebra 
   Outputs
    LP:ChainComplex
     over a symmetric algebra 
   Description
    Text
     Here P is an E-module, and LP is a linear complex of free S-modules,
     where (S,E) is the Koszul pair corresponding to a 
     product of projective spaces. 
     
     The complex LP is that produced from P by the 
     Bernstein-Gel'fand-Gel'fand functor called L in
     our paper
     @ HREF("http://arxiv.org/abs/","Tate Resolutions on Products of Projective Spaces") @.
    Example
     (S,E) = productOfProjectiveSpaces{1,2}
     P = prune truncate({1,2},E^1)**E^{{1,2}};
     LP = bgg P
     netList apply(toList(min LP..max LP), i-> decompose ann HH_i LP)
     M = prune HH_0 LP
     betti res M
     high = {2,2}
     cohomologyMatrix(M, -high, high)
--the corner complex code is slightly off:
     C = cornerComplex(M,-high,high)
     betti cornerComplex (C,{1,1})
     productOfProjectiveSpaces{1,2}
   SeeAlso
    productOfProjectiveSpaces
///

------------------------------------
-----TESTS-----
------------------------------------

TEST///
(S,E) = productOfProjectiveSpaces{1,2}
P = prune truncate({1,2},E^1)
L = bgg P
assert( (betti L) === new BettiTally from {(-3,{-1,-2},-3) => 6, (-5,{-2,-3},-5) => 1, (-4,{-2,-2},-4)
      --------------------------------------------------------------------------------------------------------
      => 3, (-4,{-1,-3},-4) => 2} );
assert all(min L +1..max L, i-> L.dd_(i-1)*L.dd_i == 0)
assert( (prune HH_(-3) L) === cokernel map((S)^{{0,1},{0,1},{0,1}},(S)^1,{{x_(1,0)}, {-x_(1,1)},
       {-x_(1,2)}}) );
///

TEST ///  
(S,E) = productOfProjectiveSpaces{1,1};
C = cornerComplex (S^1,{0,0},{3,3});
assert (cohomRing = ZZ[h,k];
    (sub (cohomologyMatrix (C, {0,0},{3,3}), cohomRing) === 
	map(cohomRing^4,cohomRing^4,{{4, 8, 12, 16}, {3, 6, 9, 12}, {2, 4, 6, 8}, {1, 2, 3, 4}}))
///

TEST ///
(S,E) = productOfProjectiveSpaces{1,2}
M = S^{{-1,2}}
high = {3,3}
C = cornerComplex(M,-high,high)
betti C
BW = beilinsonWindow C
B = beilinson C
netList toList tallyDegrees B
cohomologyMatrix(M, -high,high)
cohomologyMatrix(BW, -high,high)
netList apply(toList(min B..max B), i-> ann HH_(i) B)
M' = HH_0 B
assert(beilinsonWindow cornerComplex(M',-high,high) == BW)
///

TEST ///
(S,E) = productOfProjectiveSpaces{1,2}
M = coker random(S^2, S^{2:{-1,-1}})
high = {3,3}
C = cornerComplex(M,-high,high);
BW = beilinsonWindow C
betti BW
B = beilinson C
tallyDegrees B
netList toList tallyDegrees B
cohomologyMatrix(M, -high,high)
cohomologyMatrix(BW, -high,high)
netList apply(toList(min B..max B), i-> ann HH_(i) B)
M' = HH_0 B
assert isIsomorphic(M',M)
--note: isomorphic, not equal!
cohomologyMatrix(M', -high, high)
beilinsonWindow cornerComplex(M',-high,high)
assert(beilinsonWindow cornerComplex(M',-high,high) == BW)
///

TEST ///
(S,E) = productOfProjectiveSpaces{1,2}
M = coker random(S^2, S^{2:{-1,-1}})
high = {3,3}
C = cornerComplex(M,-high,high);
B = beilinson C
assert isIsomorphic(M',M)
--note: isomorphic, not equal!
///

TEST ///
(S,E) = productOfProjectiveSpaces{1,2}
M = coker random(S^2, S^{2:{-1,-1}})
high = {3,3}
C = cornerComplex(M,-high,high);
B = beilinson C
assert isIsomorphic(M',M)
--note: isomorphic, not equal!
///

------------Tests that aren't necessarily tests yet:
TEST ///
n={1,2}
(S,E) = productOfProjectiveSpaces n
peek tateData S
ringData S
ringData E
-- What are these next two lines doing?
scan(#n,i->scan(n_i+1,j->x_(i,j)=S_(sum(i,k->n_k+1)+j)))
scan(#n,i->scan(n_i+1,j->e_(i,j)=E_(sum(i,k->n_k+1)+j)))

m=matrix{{x_(0,0),x_(1,0)},
       {x_(0,1),0},
       {0,x_(1,1)},
       {0,x_(1,2)}}
betti m
mE=symExt(m,E)
betti (T= res coker mE)
tallyDegrees T
cohomologyMatrix( T, -{2,2},{6,6})
///

TEST ///       
n={1,2}
(S,E) = productOfProjectiveSpaces n
use S
vars S
m=map(S^4,S^{{ -1,0},{0,-1}}, transpose matrix{{S_0,S_1,0,0},{S_2,0,S_3,S_4}})
mE=symExt(m,E)

///


----The next two tests were commented out, along with the "corner" 
--scripts
TEST ///
n={1,2}
(S,E) = productOfProjectiveSpaces n
F=dual (res((ker transpose vars E)**E^{{ 2,3}},LengthLimit=>10))
cohomologyMatrix(F,-2*n,2*n)
tallyDegrees F

deg={2,1} 
m=corner(F,deg);
tally degrees source m, tally degrees target m
Fm=(res(coker m,LengthLimit=>10))[sum deg]
betti Fm
betti F
cohomologyMatrix(Fm,deg-{5,5},deg+{1,1})
///
TEST///
n={1,1}
(S,E) = productOfProjectiveSpaces n

time fB=dual res(coker random(E^7,E^{13:{ -1,0},11:{0,-1}}),LengthLimit=>10);	 	  
cohomologyMatrix(fB,-{1,1},{5,5})
deg={3,3}
m= corner(fB,deg);
f= res( ker  m,LengthLimit=> 4)[3]
tallyDegrees f
betti m, tally degrees target m, tally degrees source m
m1= corner(f,-1,{2,0});
betti m1, tally degrees target m1, tally degrees source m1
///

///
loadPackage ("TateOnProducts", Reload =>true)
///
TEST ///
n={1,2}; (S,E) = productOfProjectiveSpaces n;
	a={1,1}; U=E^{ -a};
	W=(chainComplex {map(E^0,U,0),map(U,E^0,0)})[1]
	tallyDegrees W
	cohomologyMatrix(W,-{3,3},{3,3})
        time T=trivialHomologicalTruncation(sloppyTateExtension W,0,3)
	cohomologyMatrix(T,-{3,3},{3,3})
	cohomologyMatrix(T,-{2,3},{3,3})	
low = {-3,-3};high = {3,3}
F = T

M = S^1
low = {-3,-3};high = {3,3}
F = cornerComplex(M,low,high)
betti F
tallyDegrees F
cohomologyMatrix(M, low,high)
H = cohomologyHashTable(M, low,high)
cohomologyPolynomialTable H


///

TEST///
--
--loadPackage("TateOnProducts", Reload =>true)
low = {-2,-2}; high={3,3}
(S,E) = productOfProjectiveSpaces{1,2}
C = cornerComplex(S^1,low,high)
H = cohomologyHashTable(C, low, high)
--cohomologyMatrix H -- not defined yet.
cohomologyMatrix(S^1,{-3,-3},{3,3})
///

TEST ///
n={4}
(S,E) = productOfProjectiveSpaces n
C=res ideal vars E
C1=C**E^{{ +1}}[0]
W=beilinsonWindow C1
apply(min W+1 ..max W,k->(W.dd_k==C1.dd_k,betti W.dd_k))
W
///

TEST ///
S=ZZ[x,y]/ideal(x*y)
C=(chainComplex(matrix{{x}},matrix{{y^2}},matrix{{x^2}}))[3]
isHomogeneous C
-- chainComplexData is no longer exported by ChainComplexExtras
-- L=chainComplexData C
-- C'=chainComplexFromData L
-- assert(C'== C)
///


TEST ///
-- ZZZZ
restart
  needsPackage "TateOnProducts"
  n={2,1};
  (S,E) = productOfProjectiveSpaces n;
  T1 = (dual res trim (ideal vars E)^2 [1]);
  cohomologyMatrix(T1,-3*n,3*n)
  beilinson removeZeroTrailingTerms beilinsonWindow T1
  beilinson T1
  beilinson(T1, BundleType=>QuotientBundle)
  T2 = res(coker lowerCorner(T1, {2,2}), LengthLimit=>10)[4]
  cohomologyMatrix(T2,-3*n,3*n)
  BW2 = beilinsonWindow T2
  cohomologyMatrix(oo, -5*n,5*n)
  B2 = beilinson T2
  B2 = beilinson(T2, BundleType=>QuotientBundle)
  F2 = (prune HH B2)_0
  -- now another shift
  BW3 = beilinsonWindow ((T2 ** E^{{-2,-2}})[-4])
  B3 = beilinson ((T2 ** E^{{-2,-2}})[-4])
  B3 = beilinson( ((T2 ** E^{{-2,-2}})[-4]), BundleType=>QuotientBundle);
  B3.dd^2 == 0
  F3 = (prune HH B3)_0 ** S^{{-2,-2}}
  -- F2 and F3 should be the same sheaf on P^2 x P^1.
  degrees F2
  degrees F3
  h = homomorphism (Hom(F3,F2))_{0}
  prune ker h  
  decompose ann prune coker h  -- so h is an isomorphism of sheaves
  tdeg = {3,3} -- for QuotientBundle
  tdeg = {2,2} -- for PrunedQuotient
  F3a = truncate(tdeg,F3);
  F2a = truncate(tdeg,F2);
  peek betti Hom(F3a,F2a) -- way too long for tdeg {3,3}
  h = homomorphism (Hom(F3a,F2a))_{0}
  det matrix h == 1
  assert(ker h== 0)
  assert(coker h == 0) -- do h is an isomorphism of modules.

  -- Now shift another time
  a = {3,3}
  T4 = ((T2 ** E^{a})[sum a])
  cohomologyMatrix(oo, -5*n,5*n)
  BW4 = removeZeroTrailingTerms beilinsonWindow T4
  BW4.dd^2 == 0
  B4 = (beilinson BW4) ** S^{a};
  B4.dd^2 == 0
  irrelevant = intersect (last ringData S)
  for i from nonzeroMin B4 to nonzeroMax B4 do if i != 0 then assert(saturate(ann HH_i(B4), irrelevant) == 1)
  M = prune HH_0 B4
  
  -- now let's start with M
  tdeg = {4,4}
  tM = prune truncate(tdeg, M);
  m1 = (presentation tM) ** S^{tdeg};
  corner1 = symExt(m1,E);
  betti corner1
  T5 = ((res(coker corner1, LengthLimit => 10)) ** E^{tdeg})[sum tdeg]
  cohomologyMatrix(oo, -5*n,5*n)
  BW5 = removeZeroTrailingTerms beilinsonWindow T5
  betti BW5
  beilinson BW5 
///

TEST ///
  -- Take a sheaf on P^2 x P^3, e.g. the graph of a rational map
restart
  needsPackage "TateOnProducts"
  n={2,3};
(S,E) = productOfProjectiveSpaces n;
  
  m = random(S^1, S^{4:{-3,0}}) || matrix {{S_3, S_4, S_5, S_6}}
  M = coker m
  tdeg = {6,2}
  tM = truncate(tdeg, M);
  m1 = (presentation tM) ** S^{tdeg};
  betti m1
  corner1 = symExt(m1,E);
  T = ((res(coker corner1, LengthLimit => 7)) ** E^{tdeg})[sum tdeg]
  cohomologyMatrix(T, -5*n,5*n)
  T1 = T ** E^{{-3,0}}[-3]
  BW = removeZeroTrailingTerms beilinsonWindow T1
  cohomologyMatrix(BW, -5*n, 5*n)
  assert(BW.dd^2 == 0)
  assert(isHomogeneous BW)
  betti BW
  B = beilinson BW;
  betti B
  B.dd^2 == 0  -- BUG!!!
  B.dd_1 * B.dd_2 -- not yet 0...!

  tallyDegrees BW
  B000 = positions(degrees BW_0, a -> a == {0,0})
  B002 = positions(degrees BW_0, a -> a == {0,2})
  B101 = positions(degrees BW_1, a -> a == {0,1})
  B103 = positions(degrees BW_1, a -> a == {0,3})
  B110 = positions(degrees BW_1, a -> a == {1,0})
  B112 = positions(degrees BW_1, a -> a == {1,2})
  B213 = positions(degrees BW_2, a -> a == {1,3})
  B220 = positions(degrees BW_2, a -> a == {2,0})
  B222 = positions(degrees BW_2, a -> a == {2,2})

  a1 = submatrix(BW.dd_1, B000, B101) --
  a2 = submatrix(BW.dd_1, B000, B103)
  a3 = submatrix(BW.dd_1, B000, B110) -- 
  a4 = submatrix(BW.dd_1, B000, B112)

  -- 2nd (block) row of BW.dd_1
  b1 = submatrix(BW.dd_1, B002, B101) -- 0
  b2 = submatrix(BW.dd_1, B002, B103)
  b3 = submatrix(BW.dd_1, B002, B110) -- 0
  b4 = submatrix(BW.dd_1, B002, B112)

  -- 1st column of BW.dd_2
  c1 = submatrix(BW.dd_2, B101, B213)    
  c2 = submatrix(BW.dd_2, B103, B213)  
  c3 = submatrix(BW.dd_2, B110, B213)
  c4 = submatrix(BW.dd_2, B112, B213)  
  
  -- 2nd column of BW.dd_2
  d1 = submatrix(BW.dd_2, B101, B220) -- 0
  d2 = submatrix(BW.dd_2, B103, B220) -- 0
  d3 = submatrix(BW.dd_2, B110, B220)
  d4 = submatrix(BW.dd_2, B112, B220) -- 0

  -- 3rd column of BW.dd_2
  f1 = submatrix(BW.dd_2, B101, B222) -- 
  f2 = submatrix(BW.dd_2, B103, B222) -- 0
  f3 = submatrix(BW.dd_2, B110, B222)
  f4 = submatrix(BW.dd_2, B112, B222) -- 

  a1 * c1 + a2 * c2 + a3 * c3 + a4 * c4 == 0 -- true
  beilinson(a1 * c1) + beilinson(a2 * c2) + beilinson(a3 * c3) + beilinson(a4 * c4) == 0 -- true
  beilinson a1 * beilinson c1 == beilinson(a1*c1) -- true
  beilinson a2 * beilinson c2 == beilinson(a2*c2) -- true
  beilinson a3 * beilinson c3 == beilinson(a3*c3) -- true
  beilinson a4 * beilinson c4 + beilinson(a4*c4) -- WRONG SIGN. 

  a3 * d3 == 0
  beilinson a3 * beilinson d3 == 0 -- true
  
  a1 * f1 + a2 * f2 + a3 * f3 + a4 * f4 == 0  
  beilinson a1 * beilinson f1 == beilinson(a1*f1) -- true
  beilinson a2 * beilinson f2 == beilinson(a2*f2) -- true
  beilinson a3 * beilinson f3 + beilinson(a3*f3) -- WRONG SIGN
  beilinson a4 * beilinson f4 == beilinson(a4*f4) -- true

  b2*c2 + b4*c4 == 0
  beilinson(b2*c2) + beilinson(b4*c4) == 0
  beilinson(b2*c2) == beilinson b2 * beilinson c2 -- true
  beilinson(b4*c4) + beilinson b4 * beilinson c4 -- WRONG SIGN.

  beilinson(b2*f2) + beilinson(b4*f4) == 0
  beilinson(b2*f2) == beilinson b2 * beilinson f2 -- true
  beilinson(b4*f4) + beilinson b4 * beilinson f4 -- WRONG SIGN.

  degrees source a4
  degrees target a4
  tally degrees source c4
  tally degrees target c4

  m1 = map(E^{{0,0}}, E^{{-1,-2}}, {{e_(0,1)*e_(1,0)*e_(1,1)}})
  m2 = map(E^{{-1,-2}}, E^{{-1,-3}}, {{e_(1,2)}})
  beilinson(m1*m2) 
  beilinson m1 * beilinson m2
///

TEST ///
  -- test of beilinsonBundle and numgensU
restart
  debug needsPackage "TateOnProducts"

  for n in toList({1,1}..{5,5}) do (
(S,E) = productOfProjectiveSpaces n;
      for x in toList({0,0}..n) do assert((numgens beilinsonBundle(x,S) == numgensU(x,S)))
      )

  n = {2,1,3,3};
  (S,E) = productOfProjectiveSpaces n;
  for x in toList({0,0,0,0}..n) do assert((numgens beilinsonBundle(x,S) == numgensU(x,S)))

  n = {3,4}
  (S,E) = productOfProjectiveSpaces n
  U = for i from 0 to n#0 list beilinsonBundle({i,0},S)
  V = for i from 0 to n#1 list beilinsonBundle({0,i},S)
  for x in toList({0,0}..n) do (
      assert(beilinsonBundle(x,S) == U#(x#0) ** V#(x#1))
      )
///  

TEST ///
  -- test of beilinson, on small examples
  -- XX
restart
  debug needsPackage "TateOnProducts"

  n = {2, 1}
  (S,E) = productOfProjectiveSpaces n
  m = map(E^1, E^0, 0)
  bm = beilinson m
  assert(map(beilinsonBundle({0,0},S), S^0, 0) == bm)

  m = map(E^0, E^0, 0)
  bm = beilinson m
  assert(map(S^0, S^0, 0) == bm)

  m = map(E^0, E^1, 0)
  bm = beilinson m
  assert(map(S^0, beilinsonBundle({0,0},S), 0) == bm)

  n = {1,2}
  testBeilinson {1,1} -- ok
  testBeilinson({1,1}, BundleType=>QuotientBundle)
  testBeilinson {1,2} -- 
  testBeilinson {2,1} -- ok
  testBeilinson {3,1} -- ok
  testBeilinson({3,1}, BundleType => QuotientBundle) -- ok
  testBeilinson {2,2} -- 
  testBeilinson {1,3} --
  testBeilinson {4,1} -- ok
  testBeilinson {3,2} -- 
  testBeilinson {2,3} --
  testBeilinson {1,4} --
  testBeilinson {1} -- ok
  testBeilinson {2} -- ok
  testBeilinson {3} -- ok
  testBeilinson {4} -- ok
  testBeilinson {5} -- ok
  testBeilinson {6} -- ok
  testBeilinson {5,3} -- ok
  testBeilinson {1,3,1,1} -- ok
  testBeilinson {1,3,1,2} -- ok

  testBeilinson1 {1,1} -- ok
  testBeilinson1 {1,2} -- 
  testBeilinson1 {2,1} -- ok
  testBeilinson1 {3,1} -- ok
  testBeilinson1 {2,2} -- 
  testBeilinson1 {1,3} --
  testBeilinson1 {4,1} -- ok
  testBeilinson1 {3,2} -- 
  testBeilinson1 {2,3} --
  testBeilinson1 {1,4} --
  testBeilinson1 {1} -- ok
  testBeilinson1 {2} -- ok
  testBeilinson1 {3} -- ok
  testBeilinson1 {4} -- ok
  testBeilinson1 {5} -- ok
  testBeilinson1 {6} -- ok

  testBeilinson1 {1,1,1} -- ok
  testBeilinson1 {1,1,2}
  testBeilinson1 {1,2,1}
  testBeilinson1 {2,1,1} -- ok
  testBeilinson1 {3,1,1} -- ok

  testBeilinson1 {1,2,2}
  testBeilinson1 {2,1,2}
  testBeilinson1 {2,2,1} -- ok
  
  testBeilinson1 {1,1,1,1} -- ok

  n = {1, 2}
  (S,E) = productOfProjectiveSpaces n
  p1 = map(E^{{0,0}}, E^{{-1,0}}, e_(0,0))
  p2 = map(E^{{-1,0}}, E^{{-1,-2}}, e_(1,0)*e_(1,1))
  isHomogeneous p1          
  isHomogeneous p2
  p1*p2
  beilinson p1
  beilinson p2
  beilinson (p1*p2)

  p1 = map(E^{{0,0}}, E^{{0,-2}}, e_(1,0)*e_(1,1))
  p2 = map(E^{{0,-2}}, E^{{-1,-2}}, e_(0,0))

///


------------------------------------

-- Example of beilinson
TEST ///
restart
  -- XXX
  needsPackage "TateOnProducts"
  n = {3,2}
  (S,E) = productOfProjectiveSpaces n
  assert(degrees beilinsonBundle({0,0},S) == {{0,0}})
  U1 = beilinsonBundle({1,0},S)
  U2 = beilinsonBundle({2,0},S)
  U3 = beilinsonBundle({3,0},S)
  V1 = beilinsonBundle({0,1},S)
  V2 = beilinsonBundle({0,2},S)
  assert(rank sheaf U1 == 3) -- is this computation correct?
  
  assert(U1 ** V1 == beilinsonBundle({1,1},S))
  assert(U1 ** V2 == beilinsonBundle({1,2},S))
  assert(U2 ** V1 == beilinsonBundle({2,1},S))
  assert(U2 ** V2 == beilinsonBundle({2,2},S))
  assert(U3 ** V1 == beilinsonBundle({3,1},S))
  assert(U3 ** V2 == beilinsonBundle({3,2},S))
///

TEST ///
restart
  needsPackage "TateOnProducts"
  n = {2,1}
  (S,E) = productOfProjectiveSpaces n;
  assert(degrees beilinsonBundle({0,0},S) == {{0,0}})
  U1 = beilinsonBundle({1,0},S)
  U2 = beilinsonBundle({2,0},S)
  V1 = beilinsonBundle({0,1},S)
  assert(rank sheaf U1 == n#0) -- is this computation correct?
  
  assert(U1 ** V1 == beilinsonBundle({1,1},S))
  assert(U2 ** V1 == beilinsonBundle({2,1},S))
  
  m1 = numgens U1
  m2 = numgens U2
  p1 = numgens V1

  m = beilinson1(e_(1,1), {0,1}, {0,1}, S)
  assert((numRows m, numColumns m)  == (1, p1))

  m = beilinson1(e_(1,1), {0,1}, {1,1}, S)
  assert((numRows m, numColumns m)  == (3, 3))

///

TEST ///
restart
  needsPackage "TateOnProducts"
  n={2,1};
  (S,E) = productOfProjectiveSpaces n;

  T1 = (dual res trim (ideal vars E)^2)[1];
  a=-{2,2};
  T2=T1**E^{a}[sum a];
  W=removeZeroTrailingTerms beilinsonWindow T2,cohomologyMatrix(W,-2*n,2*n)
  T = sloppyTateExtension W
  cohomologyMatrix(oo,-3*n,3*n)
elapsedTime  beilinsonWindow(T ** E^{{1,1}}[2])
elapsedTime  beilinsonWindow(T ** E^{{1,1}}[2], 1)
  beilinson1(W.dd_1_(0,0), {1,0}, {1,0}, S)
  beilinson1(e_(0,1), {1,0}, {1,0}, S)
  UF = beilinson W
  prune HH UF
  Wt = chainComplex {W.dd_2}
  Wt = chainComplex {W.dd_1}
  UF = beilinson Wt
  
  U0 = beilinsonBundle({0,0},S)  
  U1 = beilinsonBundle({1,0},S)
  U2 = beilinsonBundle({1,0},S)
  V0 = beilinsonBundle({0,0},S)  
  V1 = beilinsonBundle({0,1},S)  
  U1 ** beilinson1(e_(1,1), {0,1}, {0,1}, S) -- ok
  beilinson1(e_(1,1), {0,1}, {1,1}, S) -- error, now ok 
  beilinson1(e_(1,1), {0,1}, {2,1}, S) -- error, now ok 
  beilinson1(e_(1,1), {0,1}, {0,1}, S) -- now error, now ok

  beilinson1(0_E, {2,0}-{0,1}, {2,0}, S) -- how to handle this one ??
  
  makeBasis({0,0},E)
  makeBasis({0,1},E)
  makeBasis({1,0},E)
  makeBasis({1,1},E)  
  makeBasis({2,0},E)
  makeBasis({2,1},E)
  tensor makeChangeBasis({0,0},E)
  tensor makeChangeBasis({0,1},E)
  makeChangeBasis({1,0},E)
  makeChangeBasis({1,1},E)
  makeChangeBasis({2,0},E)
  makeChangeBasis({2,1},E)

  makeBasis({2,2},E) -- error
  

restart
  debug needsPackage "TateOnProducts"
  n={2,1};
  (S,E) = productOfProjectiveSpaces n;
    
  netList toList contractionData({0,0}, {1,0}, E) -- 1x3
  netList toList contractionData({1,0}, {1,0}, E) -- 3x3
  -- {0,1}, {1,0}                                       -- zero matrix of size: xx x xx

  netList toList contractionData({0,0}, {0,1}, E) -- 1x1
  netList toList contractionData({0,1}, {0,1}, E) -- 1x1

  netList toList contractionData({0,0}, {2,0}, E) -- would give 1x1
  netList toList contractionData({1,0}, {2,0}, E) -- would give 3x1

  netList toList contractionData({0,0}, {1,1}, E) -- would give 1x3
  netList toList contractionData({1,0}, {1,1}, E) -- would give 3x3
  netList toList contractionData({0,1}, {1,1}, E) -- would give 1x3

  beilinsonContraction(e_(0,1)+e_(0,2), {0,0}, {1,0})  -- 1x3
  beilinsonContraction(13_E, {1,0}, {1,0})  -- 3x3
  beilinsonContraction(0_E, {0,1}, {1,0}) -- 1x3
  
  beilinsonContraction(e_(1,0)+e_(1,1), {0,0}, {0,1})  -- 1x1
  beilinsonContraction(13_E, {0,1}, {0,1})  -- 1x1
  beilinsonContraction(0_E, {1,0}, {0,1})  -- 3x1

  beilinsonContraction(e_(0,1)*e_(0,0), {0,0}, {2,0})  -- 1x1
  beilinsonContraction(e_(0,1), {1,0}, {2,0})  -- 3x1
  beilinsonContraction(0_E, {0,1}, {2,0})  -- 1x1

  beilinsonContraction(e_(0,1)*e_(1,0), {0,0}, {1,1})  -- 1x3
  beilinsonContraction(e_(1,0), {1,0}, {1,1})  -- 3x3
  beilinsonContraction(e_(0,1), {0,1}, {1,1})  -- 1x3

  assert(numgensU({0,0},E) == 1)
  assert(numgensU({0,1},E) == 1)
  assert(numgensU({1,0},E) == 3)
  assert(numgensU({1,1},E) == 3)
  assert(numgensU({2,0},E) == 1)
  assert(numgensU({2,1},E) == 1)
  
  f1 = e_(0,1)*e_(0,2)
  contract(f1,f1)
  diff(f1,f1)
  contract(matrix{{f1}},matrix{{f1}})
  diff(matrix{{f1}},matrix{{f1}})
  contract(matrix{{f1}},matrix{{f1}})
  diff(matrix{{f1}},matrix{{f1}})
  transpose matrix{{f1}}

  m1 = beilinsonContraction(e_(0,1), {0,0}, {1,0})  
  m2 = beilinsonContraction(e_(0,2), {1,0}, {2,0})  
  m12 = beilinsonContraction(e_(0,1)*e_(0,2), {0,0},{2,0})
  m12 = beilinsonContraction(e_(0,2)*e_(0,1), {0,0},{2,0})
  m1*m2
  
  e1 = map(E^{{0,0}}, E^{{-1,0}}, {{e_(0,1)}})  
  e2 = map(E^{{-1,0}}, E^{{-2,0}}, {{e_(0,2)}})
  assert(beilinson e1 * beilinson e2 == beilinson(e1 * e2))
///

TEST ///
-- YYYY
restart
  needsPackage "TateOnProducts"
  n={1,1};
  (S,E) = productOfProjectiveSpaces n;
  T1 = (dual res trim (ideal (e_(0,1)*e_(1,1)))[1]);
  cohomologyMatrix(T1,-3*n,3*n)
  a=-{2,2};
  T2=T1**E^{a}[sum a];
  cohomologyMatrix(T2,-3*n,3*n)
  W=removeZeroTrailingTerms beilinsonWindow T2,cohomologyMatrix(W,-2*n,2*n)
  T = sloppyTateExtension W 
  cohomologyMatrix(T,-3*n,3*n)
  UF = beilinson W
  UF.dd^2
  UF.dd
  W.dd
  e1 = W.dd_1
  e2 = W.dd_2
  degrees e1
  degrees e2
  beilinson e1
  beilinson e2
  Hs = prune HH UF;
  ann Hs_0
  ann Hs_1
  Wt = chainComplex {W.dd_2}
  Wt = chainComplex {W.dd_1}
  UF = beilinson Wt

///

TEST ///
restart
  needsPackage "TateOnProducts"
  n={2,1};
 (S,E) = productOfProjectiveSpaces n;

  T1 = (dual res trim (ideal vars E)^2)[1];
  a=-{2,2};
  T2=T1**E^{a}[sum a];
  cohomologyMatrix(T2,-3*n,3*n)
  W=removeZeroTrailingTerms beilinsonWindow T2,cohomologyMatrix(W,-2*n,2*n)
  T = sloppyTateExtension W 
  cohomologyMatrix(T,-3*n,3*n)
  UF = beilinson W
  Hs = prune HH UF;
  ann Hs_0
  ann Hs_1
  Wt = chainComplex {W.dd_2}
  Wt = chainComplex {W.dd_1}
  UF = beilinson Wt

///

------------------------------------



end--

restart
uninstallPackage"TateOnProducts"
installPackage"TateOnProducts"
--loadPackage("TateOnProducts",Reload=>true)
viewHelp TateOnProducts
viewHelp
netList cornerCohomologyTablesOfUa({1,2})

-- experiment with the old dual: Question can the wrong dula produce a resolution with wrong betti numbers?

        kk=ZZ/101;n=4;
	E=kk[e_0..e_n,SkewCommutative =>true]
	m=map(E^{0,1},,matrix{{ e_0,e_1*e_2+e_3*e_4},{e_3*e_4-e_1*e_2,e_0*e_1*e_4}})
        isHomogeneous m
        dual m
    	fm=res coker m
	isChainComplex fm
	betti fm
	dualfm = dual fm	
	isChainComplex dualfm
	f2=res( coker dualfm.dd_(-1),LengthLimit=> 5)[2]
	betti f2
	betti dual fm
	
	
        kk=ZZ/101;n=2
	E=kk[e_0..e_n,SkewCommutative =>true]
	m=map(E^1,,matrix{{ e_0+e_1, (e_0-e_1)*e_2}})
        isHomogeneous m
        dual m
	m1 = syz transpose syz transpose m
    	fm=res (coker m, LengthLimit =>10)
    	fm1=res (coker m, LengthLimit =>10)	
	isChainComplex fm
	betti fm
	dualfm = dual fm	
	isChainComplex dualfm
	f2=res( coker dualfm.dd_(-1),LengthLimit=> 10)[2]
	f2.dd_0
	betti f2
	betti dual fm
	
	
---------------------- 
n={1,2}
(S,E) = productOfProjectiveSpaces n
a={0,1}
Ua=E^{ -a}
W=chainComplex(map(E^0,Ua,0),map(Ua,E^0,0))[1] 
time T=sloppyTateExtension(W) 
betti (qT=firstQuadrantComplex(T,{0,0}))
cohomologyMatrix(qT,-n,2*n),cohomologyMatrix(T,-2*n,2*n)
-------------

-- viewHelp res seems that a some point either Dan or Mike thought about installing res(ChainComplex)
methods res
S=ZZ/101[x,y,z]/ideal(x*y)
M0=((S^1/ideal y)**S^{2}), M1=S^1, M2=S^{ -1}
C=chainComplex({map(M0,M1,matrix{{x^2}}),map(M1,M2,matrix{{y}})})
isChainComplex C
isHomogeneous C

	
	

