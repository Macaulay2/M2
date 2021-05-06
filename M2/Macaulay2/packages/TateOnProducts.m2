-*
restart
uninstallPackage"TateOnProducts"
restart
installPackage("TateOnProducts")--,FileName=>schreyer/Dropbox/SVDComplexes/from-git/TateOnProducts.m2)
loadPackage("TateOnProducts",Reload=>true)
viewHelp "TateOnProducts"
peek loadedFiles
check "TateOnProducts" 
*-
newPackage(
    "TateOnProducts",
    Version => "1.2",
    Date => "January 30, 2020",
    Headline => "Tate resolutions on products of projective spaces",
    Authors => {
	{ Name => "Daniel Erman",        Email => "derman@math.wisc.edu",    HomePage => "http://www.math.wisc.edu/~derman/" },
	{ Name => "David Eisenbud",      Email => "de@msri.org",             HomePage => "http://www.msri.org/~de/" },
	{ Name => "Frank-Olaf Schreyer", Email => "schreyer@math.uni-sb.de", HomePage => "http://www.math.uni-sb.de/ag/schreyer/" },
	{ Name => "Michael E. Stillman", Email => "mike@math.cornell.edu",   HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html" },
	{ Name => "Yeongrak Kim",     	 Email => "kim@math.uni-sb.de",      HomePage => "http://sites.google.com/view/yeongrak/"}
	},
    Keywords => {"Commutative Algebra"},
    PackageImports => {"Truncations", "SVDComplexes"},
    PackageExports => {"SVDComplexes"},
    DebuggingMode => false
    )

export {
    "symExt",
    "cohomologyMatrix",
    "cohomologyHashTable",
    "composedFunctions",
    "eulerPolynomialTable",
    "tallyDegrees",
    "lowerCorner",
    "upperCorner",
    "beilinsonWindow",
    "tateResolution",
    "tateExtension",
    "firstQuadrantComplex",
    "lastQuadrantComplex",
    "cornerComplex",
    "regionComplex",
    "strand",
    -- beilinson functor
    "beilinsonContraction",
    "beilinsonBundle",
    "beilinson",
--    "quotientPresentationComplex",
    "ContractionData",
    "tateData",
    "productOfProjectiveSpaces",
    "contractionData", -- probably doesn't need to be exported
    "BundleType",
    "PrunedQuotient",
    "QuotientBundle",
    "SubBundle",
    "FreeBundle",
    "MapsBetweenFreeBundles",
    "DummyQuotientBundle",
    --
--    "cornerCohomologyTablesOfUa",
    "coarseMultigradedRegularity",
    "CoefficientField",
    "CohomologyVariables",
    "Rings",
--    "CohomRing",
--    "TateRingData",
    "TateData",
    "bgg",
    "directImageComplex",
    "actionOnDirectImage",
    --the following could all be part of ChainComplexExtras
    "isIsomorphic",
--    "prependZeroMap",
--    "appendZeroMap",
--    "removeZeroTrailingTerms",
    "trivialHomologicalTruncation",
--    "isChainComplex",
--    "nonzeroMin",
--    "nonzeroMax",
--    "minimize",
--    "isMinimalChainComplex",
--    "resolutionOfChainComplex",
--    "chainComplexMap",
    "InitialDegree",
    "isQuism",
    "isAction"
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

-- Helper functions for findMatrixCorners
extendRows = (I, r) -> (S := ring I; transpose (transpose I | matrix map(S^(numColumns I), S^r, 0)))
extendCols = (I, c) -> (S := ring I; matrix map(S^(numRows I), S^c, 0) | I)
extendMatrix = (I, r, c) -> extendCols(extendRows(I, r), c)

shiftRight = m -> (S := ring m; m * extendMatrix(id_(S^(numColumns m - 1)), 1,1))
shiftLeft  = m -> (S := ring m; extendMatrix(id_(S^(numRows m - 1)), 1,1) * m)
shiftHoms  = m -> (S := ring m; m' := diff(S_0, m);  shiftLeft m' + shiftRight m')

taxiCabCover = mat -> (
    nat := shiftHoms mat;
    oat := nat;
    while nat != 0 do (nat = shiftHoms nat; oat = oat + nat);
    oat
    )

-- Only usable for product of two projective spaces
findMatrixCorners = m -> (
    corners := {};
    (rows, cols) := (new MutableList, new MutableList);
    for r to numrows m - 1 do (
	rows#r = null;
	for c to numcols m - 1 do (
	    if m_(r, c) != 0 then (
		if rows#r === null then rows#r = -1;
		if not cols#?c or cols#c === null then cols#c = infinity;
	    	rows#r = max(c + 1, rows#r);
	    	cols#c = min(r - 1, cols#c);
    	    	)));
    if cols#0 === null then cols#0 = -1;
    for r to numrows m - 2 do (
	if rows#(r+1) === null then rows#(r+1) = infinity;
    	if rows#r > rows#(r+1) then rows#(r+1) = rows#r;
    	for c from 1 to numcols m - 1 do (
	    if cols#c === null then cols#c = -1;
	    if cols#(c-1) > cols#c then cols#(c-1) = cols#c;
	    ));
    for r to numrows m - 2 do (
    	if rows#r < rows#(r+1) then (
	    for c from 1 to numcols m - 1 do (
	    	if cols#(c-1) < cols#c then (
		    if r === cols#c and rows#r === c then corners = append(corners, {r, c});
		    ))));
    corners
    )

-- borrowed from LinearTruncations:
multigradedPolynomialRing = n -> (
     x := local x;
     xx := flatten apply(#n, i -> apply(n_i+1, j -> x_(i,j)));
     degs := flatten apply(#n, i -> apply(n_i+1, k ->
	     apply(#n, j -> if i == j then 1 else 0)));
     ZZ/32003[xx, Degrees=>degs]
     )

-- Usable for products of any number of projective spaces
findHashTableCorners = ht -> (
    L := pairs ht;
    t := #L_0_0_0;
    P := multigradedPolynomialRing toList(t:0);
    low := apply(t, i -> min (L / (ell -> ell_0_0_i - 1)));
    gt := new MutableHashTable;
    apply(L, ell -> if ell_1 != 0 and ell_0_1 > 0 then (
	    gt#(ell_0_0) = true;
	    apply(t, j -> gt#(ell_0_0 + degree P_j) = true);
	    ));
    I := ideal apply(L, ell -> if not gt#?(ell_0_0) then product(t, j -> P_j^(ell_0_0_j - low_j)) else 0);
    apply(flatten entries mingens I, g -> (flatten exponents g) + low)
    )

findCorners = method()
findCorners Matrix := List => m -> findMatrixCorners taxiCabCover m
findCorners(Matrix, List, List) := List => (m, low, high) -> (
    findCorners m / (ell -> {low_0 + ell_1, high_1 - ell_0})
    )
findCorners HashTable := List => ht -> findHashTableCorners ht


multigradedRegularity = method()
multigradedRegularity Module := List => M -> (
    S := ring M;
    n := #(degrees S)_0;
    low := -toList(n:#(gens S) - n);
    high := toList(n:regularity M);
--    m := cohomologyMatrix(M, low, high);
--    findCorners(m, low, high)
    ht := cohomologyHashTable(M, low, high);
    findCorners ht
    )


coarseMultigradedRegularity = method(Options =>
               {Strategy =>"MinimalResolution"})

-*coarseMultigradedRegularity ChainComplex := o-> F -> (
    --we assume F starts in homol degree 0.
    el := length F;
    r := degreeLength ring F;
    D := apply((min F..max F), i-> degrees F_i);
    --replace with D = hashTable
    L := flatten apply(length D, i-> apply(D_i, s -> s-toList(r:i)));
    regs := apply(r, p-> max(apply(L, q-> q_p)));
    d := max(regularity F, sum regs);
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
3    F = allGradings(F',Tm, S));
    coarseMultigradedRegularity F
    )
*-
LL = method()
LL (ZZ,ZZ) := (d,t) -> (
    if t==1 then {{d}} else
    flatten apply(d+1, i->
	apply(LL(d-i,t-1),ell ->flatten prepend(i,ell)))
    )

LL (ZZ,List) := (d,n) -> (
    L1 := LL(d,#n);
    select(L1, ell -> all(#n, i-> ell_i<= (1+n_i)));
    )

coarseMultigradedRegularity ChainComplex := o-> F -> (
    --we assume F starts in homol degree 0.
    t := degreeLength ring F;
    range := toList(min F..max F-1);
    degsF := flatten apply(range,i -> degrees (F_i));
    --lowerbounds := flatten flatten apply(range, i->(
    --	  apply(degsF_i, d -> apply(LL(i,t), ell -> d-ell))
    --	  ));
    --only changes degsF if t=1
    apply(t, i-> max apply(degsF, ell->ell_i))
    )

coarseMultigradedRegularity Module := o-> M-> (
    t := degreeLength ring M;
    if o.Strategy == "MinimalResolution" then F := res M else
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
    coarseMultigradedRegularity(F, Strategy => o.Strategy)
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


tateData = method()
tateData Ring := (S) -> if not S.?TateData then
   error "expected ring created with 'productOfProjectiveSpaces'" else S.TateData

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
    C := tateResolution(M, low, high);
    cohomologyMatrix(C, low , high))


eulerPolynomialTable = method()
eulerPolynomialTable HashTable := H ->(
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
eulerPolynomialTable(Module, List, List) := (M,low,high) ->
    eulerPolynomialTable cohomologyHashTable(M,low,high)
eulerPolynomialTable(ChainComplex, List, List) := (T,low,high) ->
    eulerPolynomialTable cohomologyHashTable(T,low,high)

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
    C := tateResolution(M, low, high);
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


--------------------------
--  The corner Complex  --
--------------------------



cornerComplex=method()
cornerComplex(ChainComplex,List) := (C,c) ->(
       d:=c-toList(#c:1);
       cornerComplex1(C,d)
       )

cornerComplex1=method()
cornerComplex1(ChainComplex,List) := (C,c) -> (
    -- added this line to make the function  work for the zero complex
    if C==0 then return C;
    --
    t:= numFactors ring C; -- list from 0 to the number of factors -1.
--    if max C -min C < #t then error " need a complex of length at least t";
    C':= C[min C+1]; -- last term in C' is C'_(-1)
    Cge := firstQuadrantComplex1(C'[-#t+1],c);    
    Cle := lastQuadrantComplex1(C',c);
--    <<(betti Cge, betti Cle) <<endl;
    A:=0;B:=0;AB:=0;d:=0;
    Ccorner:= chainComplex apply(max C- min C - #t-1, e-> (
	   d:=e+#t; A=Cge.dd_(d);
	   B= Cle.dd_(d); AB = cornerMap(C',c,d);
--	   print((betti A,betti AB,betti B));
	   (A|AB)||(map(target B, source A,0)|B))
          );
    return Ccorner[-min C-1])

-*
cornerComplex(Module, List, List) := (M,low, high) ->(
    --high, low are lists of length = degreeLength ring M
    (S,E) := (tateData ring M)#Rings;
    regs := coarseMultigradedRegularity M; --regs
    hi := apply(#regs, i->max(regs_i, high_i+1)); --hi
    N := presentation truncate(hi, M)**S^{hi};-- betti N
    Q := symExt(N,E); --betti Q
    (res (coker Q,LengthLimit=>(sum hi-sum low)))**E^{hi}[sum hi]
    )
*-

tateResolution=method()
tateResolution(Module, List, List) := (M,low, high) ->(
    -- make the Tate resolution or rather a free subquotient complex of it
    -- which covers all contributions in sheaf cohomological range between high and low
    --high, low are lists of length = degreeLength ring M
    (S,E) := (tateData ring M)#Rings;
    regs := coarseMultigradedRegularity M; --regs
    hi := apply(#regs, i->max(regs_i, high_i+1)); --hi
    N := presentation truncate(hi, M)**S^{hi};-- betti N
    Q := symExt(N,E); --betti Q
        
    (res (coker Q,LengthLimit=>(sum hi-sum low)))**E^{hi}[sum hi-#regs+2]
    )

tateResolution(Matrix, List, List) := (A, low, high) -> (
    -- if we have a map A : M -> N between two modules, 
    -- then compute the induced map T(A) : T(M) -> T(N)
    M := source A;
    N := target A;
    (S,E) := (tateData ring M)#Rings;
    regM := coarseMultigradedRegularity M;
    regN := coarseMultigradedRegularity N;

    hi := apply (#high, i->max(regM_i, regN_i, high_i+1));
    linearPresentationM := presentation truncate(hi, M)**S^{hi};
    linearPresentationN := presentation truncate(hi, N)**S^{hi};
    truncatedMapMtoN := (truncate(hi, A)**S^{hi});

    QM := symExt(linearPresentationM,E);
    QN := symExt(linearPresentationN,E);
    CM := (res (coker QM,LengthLimit=>(sum hi-sum low)));
    CN := (res (coker QN,LengthLimit=>(sum hi-sum low)));
   
    extPos := extend(CN[1], CM[1], sub(matrix truncatedMapMtoN, E));
    extNeg := dual extend(dual CM[-1], dual CN[-1], transpose sub(matrix truncatedMapMtoN,E));
    -- shift does not change the sign of map of complexes
    extAll := map(CN, CM, i->(if i<1 then extNeg#(i-1) else extPos#(i-1)));

    extAll**E^{hi}[sum hi-#regM+2]
    )

///
---------------------------------
restart
debug needsPackage "TateOnProducts"
needsPackage "ChainComplexExtras"
kk=ZZ/101
n={3}
(S,E)=productOfProjectiveSpaces n
A=random(S^{1:-1, 2:1}, S^{3:-2})

M=source A
N=target A
TA = tateResolution(A, -n, n);
TM = tateResolution(M, -n, n);
betti TM
TN = tateResolution(N, -n, n);
betti TN

source TA == TM
target TA == TN
isChainComplexMap TA

isIsomorphic (truncate(regularity M, HH^0 (beilinson TM)), truncate(regularity M, M))
isIsomorphic (truncate(regularity N, HH^0 (beilinson TN)), truncate(regularity N, N))

------------------------------
restart
debug needsPackage "TateOnProducts"
needsPackage "ChainComplexExtras"
kk=ZZ/101
n={2,1}
(S,E)=productOfProjectiveSpaces n
A=map(S^1, S^{1:{-1,0}}, {{S_0}})

M=source A
N=target A
regM=coarseMultigradedRegularity M
regN=coarseMultigradedRegularity N
TA = tateResolution(A, -n, n);
TM = tateResolution(M, -n, n);
betti TM
TN = tateResolution(N, -n, n);
betti TN

source TA == TM
target TA == TN
isChainComplexMap TA

isIsomorphic (truncate(regM, HH^0 (beilinson TM)), truncate(regM, M))
isIsomorphic (truncate(regN, HH^0 (beilinson TN)), truncate(regN, N))    
------------------------
///


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
    goodColumns:=select(#degSource,k -> 
	               (#select(I,i-> degSource_k_i >= cc_i)==#I and 
		        #select(I',i->degSource_k_i < cc_i)==#I'));
    goodRows:=select(#degTarget,k -> 
	            (#select(J,i-> degTarget_k_i >= cc_i)==#J and 
		     #select(J',i->degTarget_k_i < cc_i)==#J'));
    return ((M^goodRows)_goodColumns))

quadrantMap1 = method()
quadrantMap1(Matrix,List) := (M,c) -> (
    if M == 0 then return M;
    --the case I = J = {}
    --quadrantMap1(M,c) is the submatrix of M of all rows and cols
    --with row and col degrees < c in the partial order.
    degSource:=degrees source M;
    degTarget:=degrees target M;
    t:= numFactors ring M; --the list {0,...,t-1}
--    I':=select(t,j-> not member(j,I)); I' = t
--    J':=select(t,j-> not member(j,J)); J' = t
    --sign change for c ??
--    cc:=c;
    goodColumns:=select(#degSource,k -> all(t,i->degSource_k_i < c_i));
    goodRows:=select(#degTarget,k -> all(t,i->degTarget_k_i < c_i));
    return ((M^goodRows)_goodColumns))

firstQuadrantComplex=method()
firstQuadrantComplex(ChainComplex,List) := (C,c) -> (
     -- c index of the lower corner of the first quadrant
     firstQuadrantComplex1(C,c-toList(#c:1)) )

firstQuadrantComplex1=method()
firstQuadrantComplex1(ChainComplex,List) := (C,c) -> (
    -- c index of upper corner of the complementary last quadrant
    -- added this line to make the function  work for the zero complex
    if C==0 then return C;
    --
    s:=min C;
    C':=C[s];
    -- I:= numFactors ring C;
    -- sign change for c (1x)
    --now replace each map in C' with the corresponding "quadrantMap"
--    Cge:=chainComplex apply(max C'-1,d -> quadrantMap(C'.dd_(d+1),-c,{},{}));
    Cge:=chainComplex apply(max C'-1,d -> quadrantMap1(C'.dd_(d+1),-c));
    return Cge[-s])
///
restart
loadPackage "TateOnProducts"
restart
uninstallPackage "TateOnProducts"
restart
installPackage "TateOnProducts"
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
viewHelp TateOnProducts
///


lastQuadrantComplex=method()
lastQuadrantComplex(ChainComplex,List) := (C,c) -> (
    -- c index of the lower corner of the complentary first quadrant
    lastQuadrantComplex1(C,c-toList(#c:1)))


lastQuadrantComplex1=method()
lastQuadrantComplex1(ChainComplex,List) := (C,c) -> (
     -- c index of the upper corner of the last quadrant
     -- added this line to make the function  work for the zero complex
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
    -- added this line to make the function  work for the zero complex
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
   --- more desirable as it uses map in the code.  However, something squirly
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


-*
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
*-
isExact=method()
isExact(ChainComplex):=(C) -> (
   if (all((min C,max C), i -> (prune HH_i(C) == 0))) then true else false
)

isQuism=method()
isQuism(ChainComplexMap):=(phi) -> (
   isExact(cone phi)
)


-*
restart
loadPackage "TateOnProducts"
S=ZZ/101[x_0..x_2]
m=random(S^{1,0},S^{0,-1})
C=chainComplex{m}
target minimize C
*-

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

-*
aboveWindow = method()
aboveWindow(List,List) := (D,n) -> #D == #select(#D, i-> D_i>n_i)

gensInWindow = method()
gensInWindow(Module) := M ->(
    rd = ringData ring M;
    #D == #select(#D, i->(0<=D_i and D_i<=n_i)))
*-
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
pushAboveWindow Module := Matrix => M -> (
    --takes a free E-module M, returns a map to M from a free module 
    --minimally outside the Beilinson window.
    if M == 0 then return M;
    E:= ring M;
    (t,v,n,varsLists,irrList) := ringData E;
    --list the degrees of the generators of M:
    degList := degrees source gens M;
--    if degList == {} then return M; -- this is the zero module
    directSum apply(degList, D->if inWindow(D,n)
    then powers(v-D,irrList)**E^{ -D} else id_(E^{ -D}))
    )

pushAboveWindow1 = method()
pushAboveWindow1 Module := Matrix => M -> (
    --takes a free E-module M, returns a map to M from a free module 
    --minimally outside the Beilinson window.
    if M == 0 then return M;
    E:= ring M;
    (t,v,n,varsLists,irrList) := ringData E;
    --list the degrees of the generators of M:
    degList := degrees source gens M;
--    if degList == {} then return M; -- this is the zero module
    directSum apply(degList, D->if inWindow(D,n)
    then powers(v-D,irrList)**E^{ -D} else id_(E^{ -D}))
    )

TEST ///
debug TateOnProducts
(S,E) = productOfProjectiveSpaces {1,2}
(t,v,n,varsLists,irrList) = ringData E
M = E^{{0,0},-{ -1,0},-{1,2},-{1,3}}
T = pushAboveWindow M
assert(target T == M)
assert(select(degrees source T, D ->inWindow(D,n)) == {})
///


pushAboveWindow Matrix := Matrix =>A ->(
    --returns a matrix with the same target but source minimally outside the Beilinson window.
    if A==0 then return A;
    mingens image (A*pushAboveWindow source A)
    )

-*
(S,E) = productOfProjectiveSpaces {1,2}
A=matrix{{E_0,E_2}}
pushAboveWindow A
*-

pushAboveWindow(Matrix,Matrix) := (A,B) ->(
    --A is already correct, B is a matrix of syzygies.
    --return the matrix of syzygies of A that are outside the Beilinson window
    --and don't repeat things in the image of B
    assert(A*B == 0);
    C := pushAboveWindow syz A;
    mingens image(C % image B)
    )

pushAboveWindow(Matrix,Matrix,Matrix) := (A,B,C) ->(
    --A,B,C form a complex. return a list of 3 matrices that form a complex,
    --A, B|pushAboveWindow(A,B),C' where C' is changed only by adding a block of zeros.
    B2 := pushAboveWindow(A,B);
    assert((B|B2)*(C||map(source B2, source C, 0))== 0);
    assert(A*(B|B2) == 0);
    (B|B2, C||map(source B2, source C, 0))

)

pushAboveWindow List := List => L ->(
    --L = List of matrices that make a complex
    --returns list of matrices that make a complex, where 
    --syzygies of each matrix that are both outside the window and not 
    --already in the complex have been added.
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

pushAboveWindow ChainComplex := ChainComplex => C -> (
    --makes the chain complex into a list of matrices, 
    --does pushAboveWindow to that, 
    --and makes it back into a chain complex.
    --That is:  Takes a chain complex and adds all the syzygies of maps in the complex that
    --are outside the Beilinson window.
    C':=appendZeroMap appendZeroMap prependZeroMap C;
    L := chainComplexData C';
    M := pushAboveWindow L_2;
    chainComplexFromData(min C', M)
    )

TEST ///
debug TateOnProducts
        n={1,1};
        (S,E) = productOfProjectiveSpaces n;
	T1 = (dual res trim (ideal vars E)^2)[1];
	a=-{2,2};
	T2=T1**E^{a}[sum a];
	W=beilinsonWindow T2
	T3 = pushAboveWindow W
    	assert(beilinsonWindow T3 == W)
///

tateExtension=method()
tateExtension(ChainComplex) := W -> (
    -- input W : a Beilinson representative of an object in D^b(PP)
    -- output :  an Tate extension in a bounded range
    -- compute the TateExtension in a sloppy way: the Beilinson window of the extension is only
    -- isomorphic, bat not equal W.
    (t,v,n,irrList,idealList) := ringData ring W;
    if not inWindow W then error "expect a complex with terms only in the Beilinson window";
    W1 := removeZeroTrailingTerms W;
    -- W1:= W;
    TW1:=pushAboveWindow W1;
    ma:= nonzeroMax TW1; 
    mi:=nonzeroMin TW1;
    --betti W1,betti TW1
--Bbounds given for the length of the resolution have to be discussed
--They should come out of the proof of the theorem !!
    TW1e := res(coker TW1.dd_(ma),LengthLimit=>(3*sum v))[-ma];
    --betti TW1e
    --changed a sign here
    --TW1c := cornerComplex(TW1e,2*v);
    TW1c := cornerComplex(TW1e,-2*v); -- replace with upper quad cplx
--cohomologyMatrix(TW1c, -4*v,4*v)
    --betti TWc
    TW2 := dual res(coker transpose TW1c.dd_(ma+sum v),
	LengthLimit =>(ma+3*sum v -mi))[-ma-sum v+1];
--cohomologyMatrix(TW2, -4*v,4*v)
    --betti TW2
    TW2
    )

-*
--this code is not used
continueComplex = method(Options => options res)
continueComplex ChainComplex := o->C ->(
    ma := nonzeroMax C;
    C' := res(image (C.dd_ma),LengthLimit => o.LengthLimit)[-ma];
    ma' := nonzeroMax C';
    D := new ChainComplex;
    D.ring = ring C;
    apply(toList(min C..ma'),i->(
	    D_i = if i<= ma then C_i else C'_i));
    apply(toList(1+min C..ma'),i->(
	    D.dd_i = if i<=ma then C.dd_i else C'.dd_i));
    D
    )

TEST ///
debug TateOnProducts
n={1,1};
(S,E) = productOfProjectiveSpaces n;
p = 2
C = res(coker vars E,LengthLimit =>p)[2]
C1 = continueComplex(C, LengthLimit =>2)
assert((C1.dd)^2 == 0)
///
*-
---------------------------------------
-- Construction of Beilinson functor --
---------------------------------------
beilinson = method(Options=>{BundleType=>PrunedQuotient}) -- other options: QuotientBundle, SubBundle, FreeBundle.
-- beilinson(free E-Module, BundleType=>PrunedQuotient) returns an S-module
-- beilinson(free E-Matrix, BundleType=>PrunedQuotient) returns a matrix over S
-- beilinson(free E-ChainComplex, BundleType=>PrunedQuotient) returns a chain complex over S.
-- These three functions implement the beilinson functor
--   from the category of graded free E-modules to the category of graded S-modules
--   (the actual beilinson functor is this followed by sheafification).
-- example uses:
-*
     (S,E) = productOfProjectiveSpaces {1,2}
     beilinson(E^{{-1,-1}})
     beilinson random(E^{{-1,0}}, E^{{-2,-1}})
*-

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
--sub(transpose diff(sortedB#(i-1), transpose sortedB#i), vars S) ** S^{(i-2)*d}
        )
    )

koszulmapShiftedByOne = (i,sortedB,S) -> (
    --cokernel(koszul(i+2,sortedB#1)) ** S^{i}
    if i == 0 then map(S^0, S^1, 0)
    else (
        d := first degrees source sortedB#1;
        (sub(diff(transpose sortedB#(i-1), sortedB#i), vars S)) ** S^{(i-1)*d}
--sub(transpose diff(sortedB#(i-1), transpose sortedB#i), vars S) ** S^{(i-1)*d}
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
	
	if opts.BundleType === MapsBetweenFreeBundles then (
	    if a === -1 then (
		    deg := for i from 0 to t-1 list if i === whichblock then -1 else 0;
		    map (S^0, S^{deg}, 0)
		    )	     	 
		else if a === (n#whichblock)+1 then map (S^1, S^0, 0)
	--if a === (n#whichblock)+1 then map (S^1, S^0, 0)
		else if a < -1 or a > (n#whichblock) + 1 then map (S^0, S^0, 0)
	--else if a < 0 or a > (n#whichblock) + 1 then map (S^0, S^0, 0)
		else (
		    koszulmapShiftedByOne (a+1, sortedB, S)
		    )
	    )
    else (
        if a < -1 or a > (n#whichblock) + 1 then S^0 -- need to fix when opts=SubBundle or FreeBundle
        else (
            if opts.BundleType === PrunedQuotient then (
                if a === 0 then S^1
                else if a === n#whichblock then (
                    deg = for i from 0 to t-1 list if i === whichblock then -1 else 0;
                    S^{deg}
                    )
		else if a === (n#whichblock)+1 then S^0
		else if a === (-1) then S^0
                else coker koszulmap(a+2, sortedB, S)
                )
            else if opts.BundleType === QuotientBundle then (
                if a === n#whichblock then (
                    deg = for i from 0 to t-1 list if i === whichblock then -1 else 0;
                    S^{deg}
                    )
		else if a === (n#whichblock)+1 then S^0
		else if a === (-1) then S^0
                else coker koszulmap(a+2, sortedB, S)
                )
	    else if opts.BundleType === DummyQuotientBundle then (
		-- contain a dummy presentation data, in particular, when a=-1.
                if a === n#whichblock then (
                    deg = for i from 0 to t-1 list if i === whichblock then -1 else 0;
                    S^{deg}
                    )
		else if a === (n#whichblock)+1 then S^0
                else coker koszulmap(a+2, sortedB, S)
                )
            else if opts.BundleType === SubBundle then (
--		if a === n#whichblock then (
--                    deg = for i from 0 to t-1 list if i === whichblock then -1 else 0;
--                    S^{deg}
--                    )
		if a === (n#whichblock)+1 then S^0
      		else if a === (-1) then S^0
		else ( 
                maptemp:=koszulmapShiftedByOne(a+1,sortedB, S);
--		print tally degrees target maptemp, print tally degrees S^(binomial((n#whichblock)+1, a))
--		assert (target maptemp == S^(binomial((n#whichblock) +1, a)));
		image koszulmapShiftedByOne(a+1, sortedB, S)
		)
		
                )
	    else if opts.BundleType === FreeBundle then (
		if a === (-1) then S^0
		else S^(binomial((n#whichblock) + 1, a))
		)
            else error "expected BundleType to be one of PrunedQuotient (default) or QuotientBundle or FreeBundle"
            -- note: once SubBundle is implemented, change this error message.
            )
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

    	if opts.BundleType === MapsBetweenFreeBundles then (
	    b := for i from 0 to t-1 list sum (0..i-1, j->a#j);
	    result := beilinsonBundle(a#0, 0, S, opts);
            for i from 1 to t-1 do result = result ** beilinsonBundle(a#i, i, S, opts) * (-1)^(b#i);
	    result
        )
    	else if opts.BundleType === SubBundle then (
	       productOfKoszulMaps := id_(S^1);
	       for i from 0 to t-1 do (
	       if beilinsonBundle(a#i,i,S,opts) == S^0 then return S^0
	       else (
		   b = sum (0..i-1, j->a#j);
		   productOfKoszulMaps = productOfKoszulMaps ** koszulmapShiftedByOne((a#i)+1,(sortedBases E)#i, S) * (-1)^b;
		   )
	   );
	   image productOfKoszulMaps
	)
    	else (
	result = beilinsonBundle(a#0, 0, S, opts);
        for i from 1 to t-1 do result = result ** beilinsonBundle(a#i, i, S, opts);
        result
	)
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
            else if opts.BundleType === QuotientBundle or opts.BundleType === DummyQuotientBundle then (
                mc = for i from 0 to #rowdeg-1 list
                    (sortedBases E)#i#(coldeg#i+1);
                mr = for i from 0 to #rowdeg-1 list (
                    -- The following is created in this funny way, because transpose of a matrix over
                    -- the exterior power introduces signs that we don't want here.
                    matrix transpose entries (sortedBases E)#i#(rowdeg#i+1)
                    );
                (tensor mc, tensor mr)
                )
	    else if opts.BundleType === FreeBundle then (
		mc = for i from 0 to #rowdeg-1 list
                    (sortedBases E)#i#(coldeg#i);
                mr = for i from 0 to #rowdeg-1 list (
                    -- The following is created in this funny way, because transpose of a matrix over
                    -- the exterior power introduces signs that we don't want here.
                    matrix transpose entries (sortedBases E)#i#(rowdeg#i)
                    );
                (tensor mc, tensor mr)
		)
            else if opts.BundleType === SubBundle then (
		-- CHECK later; maybe we do not need any information at degree a_i+1 
                mc = for i from 0 to #rowdeg-1 list
                    (sortedBases E)#i#(coldeg#i);
                mr = for i from 0 to #rowdeg-1 list (
                    -- The following is created in this funny way, because transpose of a matrix over
                    -- the exterior power introduces signs that we don't want here.
                    matrix transpose entries (sortedBases E)#i#(rowdeg#i)
                    );
                (tensor mc, tensor mr)
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
    ) else if opts.BundleType === QuotientBundle or opts.BundleType === DummyQuotientBundle then (
      product for i from 0 to #deg-1 list if deg#i == n#i then 1 else binomial(n#i + 1, deg#i+1)
    ) else if opts.BundleType === FreeBundle then (
      product for i from 0 to #deg-1 list binomial((n#i) + 1, deg#i)  
    ) else if opts.BundleType === SubBundle then (
      product for i from 0 to #deg-1 list binomial((n#i) + 1, deg#i)
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
    else if opts.BundleType === QuotientBundle or opts.BundleType === DummyQuotientBundle then (
        (mc, mr) = contractionData(rowdeg, coldeg, ring e, opts);
        if e == 0 then
            map(S^(numgensU(rowdeg,E,opts)), S^(numgensU(coldeg,E,opts)), 0)
        else
            substitute(contract(diff(e, mc), mr), S)
        )
    else if opts.BundleType === FreeBundle then (
       	(mc, mr) = contractionData(rowdeg, coldeg, ring e, opts);
        if e == 0 then
            map(S^(numgensU(rowdeg,E,opts)), S^(numgensU(coldeg,E,opts)), 0)
        else
	 --(
	      --print substitute(contract(diff(e, mc), mr), S);
--	   print endl;   
--	   print substitute(contract(mc,e*mr),S));
	  substitute(contract(diff(e, mc), mr), S) 
        )
    else if opts.BundleType === SubBundle then (
        -- implemented exactly same as Free bundle; CHECK later on extreme degrees
	(mc, mr) = contractionData(rowdeg, coldeg, ring e, opts);
        if e == 0 then
            map(S^(numgensU(rowdeg,E,opts)), S^(numgensU(coldeg,E,opts)), 0)
        else
            substitute(contract(diff(e, mc), mr), S)
        )
    else error "what?"
    )

inBeilinsonWindow = method(Options => options beilinson)
-- inBeilinsonWindow(deg, E)
--   returns a boolean value.
--   returns (beilinson(E^{-deg}) != 0),
--    i.e. whether the 'deg' values are 'in range'.
inBeilinsonWindow(List, Ring) := opts -> (deg, E) -> (
    (t,v,n,varsList,irrList) := ringData E;
    if opts.BundleType === PrunedQuotient or opts.BundleType === QuotientBundle then (
    for i from 0 to #deg-1 do (
        if deg#i < 0 or deg#i > n#i
        then return false;
        );
    true
    )
    else if opts.BundleType === FreeBundle then (
	for i from 0 to #deg-1 do (
            if deg#i < 0 or deg#i > (n#i)+1
            then return false;
        );
    	true
    )
    else if opts.BundleType === SubBundle  then (
	for i from 0 to #deg-1 do (
            if deg#i < 0 or deg#i > (n#i)
            then return false;
        );
    	true
    )    
    else if opts.BundleType === MapsBetweenFreeBundles then (
	for i from 0 to #deg-1 do (
	    if deg#i < (-1) or deg#i > (n#i)+1
	    then return false;
	    );
	true
	)
    else if opts.BundleType === DummyQuotientBundle then(
	for i from 0 to #deg-1 do (
        if deg#i < (-1) or deg#i > n#i
        then return false;
        );
    true
	)
)

beilinson Module := Module => opts -> F -> (
    -- F is a free E-module, and the result is the direct sum of
    --  the beilinson bundles of each rank 1 summand.
    if not isFreeModule F then error "expected a free module";
    if not member(opts#BundleType, set{PrunedQuotient, QuotientBundle, SubBundle, FreeBundle, MapsBetweenFreeBundles, DummyQuotientBundle})
    then error "expected BundleType to be one of PrunedQuotient, QuotientBundle, SubBundle, FreeBundle, MapsBetweenFreeBundles";
    E1 := ring F;
    tD := tateData E1;
    (S,E) := tD.Rings;
    degs := degrees F;
    pos := positions(degs, a -> inBeilinsonWindow(a,E,opts));
    if #pos == 0 then (
	if opts.BundleType === MapsBetweenFreeBundles then map(S^0,S^0,0)
	else S^0
    )
    else (
    directSum for a in pos list beilinsonBundle(degs#a,S,opts)
    )
)

beilinson Matrix := Matrix => opts -> o -> (
    if not member(opts#BundleType, set{PrunedQuotient, QuotientBundle, SubBundle, FreeBundle,MapsBetweenFreeBundles, DummyQuotientBundle})
    then error "expected BundleType to be one of PrunedQuotient, QuotientBundle, SubBundle, FreeBundle, MapsBetweenFreeBundles";
    E1 := ring o;
    tD := tateData E1;
    (S,E) := tD.Rings;
    coldegs := degrees source o;
    rowdegs := degrees target o;
    colpos := positions(coldegs, a -> inBeilinsonWindow(a,E,opts));
    rowpos := positions(rowdegs, a -> inBeilinsonWindow(a,E,opts));
    -- Only for the 'MapsBetweenFreeBundles' option
    if opts.BundleType === MapsBetweenFreeBundles then (
    	src := if #colpos == 0 then map(S^0,S^0,0) else directSum for a in colpos list beilinsonBundle(coldegs#a,S,opts);
	tar := if #rowpos == 0 then map(S^0,S^0,0) else directSum for a in rowpos list beilinsonBundle(rowdegs#a,S,opts);
	return (src, tar)
    );
    
    src = if #colpos == 0 then S^0 else directSum for a in colpos list beilinsonBundle(coldegs#a,S,opts);
    tar = if #rowpos == 0 then S^0 else directSum for a in rowpos list beilinsonBundle(rowdegs#a,S,opts);
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
    if opts.BundleType === SubBundle then(
	-- the method 'map' does not work in general; 
	-- the number of generators of 'tar', 'src' need not to fit with the size of 'mats'.
	-- 'inducedMap' method works in the case.
	-- however, seems to be it returns a matrix on their minimal presentations.
	-- to observe the original data 'mats', use the option 'FreeBundle'
	inducedMap(tar, src, mats) 
	)
    else(
    map(tar,src,mats) -- it only works when the size of matrix fits with the number of generators of src, tar
    )
)

beilinson ChainComplex := opts -> (BT) -> (
    -- BT should be a complex over E = exterior algebra
    data := chainComplexData BT;
    if opts.BundleType === MapsBetweenFreeBundles then (
	-- Do we also need a sign convention for them?
	tD := tateData ring BT;
    	(S,E) := tD.Rings;
	ones := toList((numgens degreesRing S):1);
	Csrc:=(beilinson(BT**E^{-ones}, BundleType=>FreeBundle))**S^{-ones};
	--Csrc:=quotientPresentationComplex(BT);
	Ctar:=beilinson(BT, BundleType=>FreeBundle);
	
	mapList:=data#2/(m->(beilinson(m,opts)));
	tempList:={};
	apply(mapList,i->(tempList=tempList|{i_1}));
	tempList=tempList|{(mapList_(#mapList-1))_0};
	
	f:= m->tempList_(m-data#0);
	
	map(Ctar,Csrc,f)
	-- Sometimes Ctar, Csrc may have different lengths (at boundary multidegrees).
	-- However, in any cases, we do not have to care since such maps are always 0;
	-- and the default map is 0 unless we assign.
	)
    else (
    removeZeroTrailingTerms chainComplexFromData{data#0, data#1,data#2/(m -> (beilinson(m, opts)))}
    )
    )

-- quotientPresentationComplex = method()
-- This method extracts the maps on twisted free modules of multidegree {-1,...,-1}.
-- The beilinson complex in 'QuotientBundle' option is a quotient of this free complex.
-- maybe it is not a natural answer; take 'contractionSequence' might be better.
-* quotientPresentationComplex ChainComplex := (BT) -> (
    BTQuot := beilinson (BT, BundleType=>QuotientBundle); -- already zero trailing terms are removed
    tmin:=(min BTQuot);
    tmax:=(max BTQuot);
    BTshifted:=BTQuot[tmin];
    C:=chainComplex(apply(1..tmax-tmin, i->map(target presentation target BTshifted.dd_(i), target presentation source BTshifted.dd_(i),BTshifted.dd_(i))));
    C[-tmin]
    )
*-

-*embeddingToFree ChainComplex := (BT) -> (
    -- return a list of embedding maps from a BT complex to a free complex
    data := chainComplexData BT;
    BTquotient := chainComplexFromData{data#0, data#1,data#2/(m -> (beilinson(m, BundleType=>QuotientBundle)))};
    BTfree := removeZeroTrailingTerms chainComplexFromData{data#0, data#1,data#2/(m -> (beilinson(m, BundleType=>FreeBundle)))};
    
    FreeComplexdata := chainComplexData BTfree;
    apply(toList(data#0, data#1), i-> 
    )
*-

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
-- end
restart
debug needsPackage "TateOnProducts"
n={1}
n = {2,1}
(S,E) = productOfProjectiveSpaces n

needsPackage "ChainComplexExtras"
C=chainComplex (matrix {{E_0}})
beilinson(C)
(beilinson(C, BundleType=>DummyQuotientBundle)).dd_1
(beilinson(C, BundleType=>FreeBundle)).dd_1
(beilinson(C, BundleType=>MapsBetweenFreeBundles))

  contractionFunctor(E^{{-1,-1}})
  contractionFunctor(E^{{1,1}})
  contractionFunctor(E^{{3,1}})
  m = map(E^{{0,-1}}, E^{{-2,-1}}, {{e_(0,1)*e_(0,2)}})
  contractionFunctor m

  T1 = (dual res trim (ideal vars E)^2 [1]);
  cohomologyMatrix(T1,-3*n,3*n)
  T2 = res(coker lowerCorner(T1, {2,2}), LengthLimit=>14)[4]
  cohomologyMatrix(T2,-3*n,3*n)
  betti contractionWindow (T2**E^{{0,0}})
  betti beilinsonWindow (T2**E^{{-1,-1}})
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



  
  tallyDegrees beilinsonWindow T2
  for i from min BT2 to max BT2 list i => numgens BT2_i
tallyDegrees  contractionFunctor (T2 ** E^{{-1,-1}})
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


  -- This function checks that beilinson is functorial, by creating random
  -- matrices in all possible degrees and checking functoriality on these.
  -- these two functions are not exported.
  testBeilinson = method(Options => options beilinson)
  testBeilinson List := opts -> n -> (
      (S,E) := productOfProjectiveSpaces n;
      zeros := toList(#n:0);
      degs := reverse toList(-n..zeros);
      m1 := random(E^degs, E^degs);
      m2 := random(E^degs, E^degs);
      shouldBeZero := beilinson(m1*m2,opts) - beilinson(m1,opts) * beilinson(m2,opts);
-*
      if shouldBeZero == 0 then return "OK";
      map(E^degs, E^degs, for tar in degs list for src in degs list (
          p1 := random(E^{tar}, E^degs);
          p2 := random(E^degs, E^{src});
          if beilinson(p1 * p2, opts) - beilinson(p1, opts) * beilinson(p2, opts) == 0
          then 0_E
          else 1_E
          ))
*-
      assert(shouldBeZero == 0)
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
      assert(# keys H == 0);
      -- if #keys H == 0 then null else H
      )

----------------------------------------
-- Examples in the Paper              --
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
	T=tateExtension aW_1;
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
	T=tateExtension W;
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

multMapE = method()
multMapE(Module,List,List):= (M,a',a) ->(
    --produces a map from
    --a sum of copies of S^a to a sum of copies of S^a'.
    --If the grading is changed to the "correct" one for E,
    --this code will need fixing!
    if sum a' - sum a != 1 then error"Sums must differ by 1";
    (S,E) := (tateData ring M)#Rings;
    pos := positions(gens S, e-> degree e == a'-a);
    ee := apply(gens S, e->map(M,M**S^{degree e},e));
    Ba := basis(a,M)**S^{a'-a};
    Ba' :=basis(a',M);
    map(E^{(numcols Ba'):a'}, E^{(numcols Ba):a},
	 sum(pos, p->E_p*sub(ee_p*Ba//Ba', E))
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

bgg = method(Options =>{LengthLimit => null})
bgg Module := o -> P -> (
    (S,E) := (tateData ring P)#Rings;
    D:= null; Ds:= null;
    freeModulesDegs:=null;
    tar:=null;sour:=null;
    utar:=0;usour:=null;
    a:=0;a':=0;u:=null;
    if ring P === E then(
    D = (degrees basis P)_1;
    Ds = sort apply(D, d->(sum d,d));
    minP := min(Ds/first);
    maxP := max(Ds/first);
    if o.LengthLimit != null then maxP=min(maxP,minP+1+o.LengthLimit);
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
    u = L->unique degrees L;
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
   return LP);
   if ring P === S then (
   if o.LengthLimit === null then LengLim := 1+numgens S else
                   LengLim = o.LengthLimit;
    M := P/((ideal vars S)^(LengLim+1));
    D = (degrees basis M)_1;
    Ds = sort apply(D, d->(sum d,d));
    minM := min(Ds/first);
    maxM := max(Ds/first);
--    (maxM - minM)
    freeModuleDegs = hashTable apply(toList(minM..maxM), i->
	    (-i=>select(Ds,d-> d_0 == i)/last)
	    );
    RM := new ChainComplex;
    RM.ring = E;
--define the modules as direct sums, with one degree per summand
    scan(toList(minM..maxM), i->
	RM#(-i) = directSum apply(unique freeModuleDegs#(-i), d ->
	    E^(select(freeModuleDegs#(-i), k-> d ==k))));
--define the maps
    u = L->unique degrees L;
    scan(toList(min RM+1..max RM), k->(
	    tar = RM_(k-1);
	    sour = RM_k;
	    utar = u tar;
	    usour = u sour;
	    RM.dd#k = sum(#utar, i->
		      sum(#usour, j->(
	              a' = -utar_i;
	              a = -usour_j;
                      map(tar,sour,
    		         tar_[i]*multMapE(M,a',a)*(sour^[j])
		         )
		     )))
               )
	);
   return RM))



///
restart
(RM.dd)^2
betti RM
apply(min RM..max RM-1, i-> HH_i RM)
HH_0 RM
loadPackage("TateOnProducts", Reload=>true)
(S,E) = productOfProjectiveSpaces{1,1}
P = module ideal vars S
LengLim = 5
#utar
#usour
i=0, j=0
ring tar_[i]
ring multMapE(M,a',a)
ring (sour^[j])
///

isDegreeZeroSurjection := method()
isDegreeZeroSurjection(Module,Module) := (A,B)->(
    --tests a random degree 0 map to see whether its a surjection
    H := Hom(A,B);
    B0 := basis(0,H); -- this seems to be total degree 0 in case of degreeLength>1
    f := homomorphism(B0*random(source B0, (ring B0)^1));
    coker f == 0)

isIsomorphic = method()
isIsomorphic(Module,Module) := (A,B) -> (
    --tests random degree 0 maps A->B, B->A and returns true
    --if both are surjective.
    if not(isHomogeneous A and isHomogeneous B) then 
	  error"not implemented for inhomogeneous modules";
    Ap := prune A;
    Bp := prune B;
    dA := set flatten degrees source gens Ap;
    dB := set flatten degrees source gens Bp;
    if dA =!= dB then false else    
    isDegreeZeroSurjection(Ap,Bp) and isDegreeZeroSurjection(Bp,Ap)
    )



---------------------------------------------------
-- Composed functions                            --
---------------------------------------------------

cornerComplex(Module,List,List,List) := (M,c,low,high) -> (
    -- form the Tate resolution T of M in the range high to low
    -- then make the corner complex of T at c
    T := tateResolution(M,low,high);
    T':= trivialHomologicalTruncation(T,-sum high, -sum low);
    cornerComplex(T',c)
    )

///
(S,E)=productOfProjectiveSpaces{2,1}
M=beilinson(E^{-{1,1}})
c={1,1}
low={-3,-3},high=-low
cohomologyMatrix(M,low,high)
C=cornerComplex(M,c,low,high)
cohomologyMatrix(C,2*low,2*high)
C.dd^2
betti C
P=(ker C.dd_(-1))**E^{c}
LP=bgg P
HH LP
///



projectionMapOnEs=method()
projectionMapOnEs(Module,List) := (M,I)->(
    S := ring M;
    t := #degree S_0;
    v := apply(unique degrees source vars S, d->
	#select(degrees source vars S,e->e==d));    
    if not all(I,i->0 <= i and i <= t-1) then error "expected a sublist of {0,..,t-1}";
--    J := select(toList(0..t-1),j-> not member(j,I));
    nI := apply(I,i-> v_i-1);
    kk := coefficientRing S;
    (SI,EI) := productOfProjectiveSpaces (nI,CoefficientField=>kk);
    a:= null;
    phi1:= matrix {flatten apply(t,i->
	if member(i,I) then ( 
	    l:=position(I,j->j==i);
	    a=sum(l,j->nI_j+1);
	    apply(v_i,k->EI_(a+k))
	    )
	else apply(v_i,k->0))};
    E := (tateData S)#Rings_1;
    map(EI,E,phi1)
    )


///
restart
loadPackage("TateOnProducts",Reload=>true)
(S,E)=productOfProjectiveSpaces{1,2,2}
M=S^{{-2,-2,-1}}
I={1}
projectionMapOnEs(M,I)

///    

directImageComplex=method(Options => options beilinson)
directImageComplex(Module,List) := opts -> (M,I) -> (
    -- Input: M module representing a sheaf sF on a product of t projective space
    --        I subset of {0,..,t-1}
    -- Output: the complex Rpi_* sF in D^b(PP^I)
    --        where pi: PP-> PP^I denotes the projection on the partial product
    --        X_{i in I} PP^{n_i}
    S := ring M;
    t := #degree S_0;
    J := select(toList(0..t-1),j-> not member(j,I));
    phi := projectionMapOnEs(M,I);
    E1 :=target phi;
    v := apply(unique degrees source vars S, d->
	#select(degrees source vars S,e->e==d));
    high:= v;low:=-high;
    T:=tateResolution(M,low,high);
    sT:=removeZeroTrailingTerms strand(T,toList(t:0),J);
    --print cohomologyMatrix(sT,low,high);
    sTW := removeZeroTrailingTerms beilinsonWindow sT;
    --print betti sTW;
    mi := min sTW; ma:=max sTW;
    W1 := new ChainComplex;
    W1.ring = E1;
    apply(toList(mi..ma),i-> W1_i = E1^(-apply(degrees sTW_i,d->d_I))); 
    apply(toList(mi+1..ma),i->W1.dd_i = map(W1_(i-1),W1_i,phi(sTW.dd_i)));
    beilinson (W1, opts)
    )


///  
restart
loadPackage("TateOnProducts",Reload=>true)
debug TateOnProducts
(S,E)=productOfProjectiveSpaces({1,2},CoefficientField=>kk)
M=(beilinson E^{{-1,-1}})**S^{{-1,-1}}
I={1}
RpiM=directImageComplex(M,I)
betti RpiM
prune HH^0 RpiM
prune HH^1 RpiM 
prune HH^2 RpiM
///


directImageComplex(Ideal,Module,Matrix) := opts-> (I,M,phi) -> (
    -- Input: I, the ideal of a projective scheme X in P^n,
    --        M, a module representing a coherent sheaf on X
    --        phi, a kxm matrix representing a rational map or morphism
    --             f: X -> P^m
    R := ring I;
    if I != saturate I then error "expected a saturated ideal I for Proj(R/I)";
    J := annihilator M;  
    if J+I != J then error "expected a module representing a sheaf on X=Proj(R/I)";
    if minors(2,phi)+I != I then error" expected phi to define a rational map on X=Proj(R/I)";
    m := rank source phi-1 ;
    n := numgens R-1  ;
    kk :=coefficientRing R;
    (S,E) := productOfProjectiveSpaces({n,m},CoefficientField=>kk);    
    p1 := map(S,R,(vars S)_{0..n});
    gI := gens I; 
    dgI := apply(degrees source gI,d->{-d_0,0});   
    I1 := ideal map(S^1,S^dgI,p1(gI));    
    gM := presentation M;    
    dtgM := apply(degrees target gM,d->{-d_0,0});   
    dsgM := apply(degrees source gM,d->{-d_0,0});   
    gM1 := map(S^dtgM,S^dsgM,p1(gM));    
    dtphi :=  apply(degrees target phi,d->{-d_0,0});   
    dsphi :=  apply(degrees source phi,d->{-d_0,0});    
    phi1 := map(S^dtphi,S^dsphi,p1(phi));    
    y := (vars S)_{n+1..n+m+1};    
    graph := trim(minors(2,y||phi1)+I1);
    Mgraph := coker(gens graph**id_(target gM1)|gM1);
    RphiM := directImageComplex(Mgraph,{1},opts);
    RphiM
    )

-*
-- uses product of projective spaces, so contains a choice of basis issue
-- so we will take an easier way

directImageAction=method()
directImageAction(Ideal, Module) :=  (I,M)->(
    -- Input : I, the ideal of a projective variety Y in P^n,
    --	       M, a module on Y
    -- Y does not intersect with the linear space (x_0=...=x_c=0).
    
    -- Output : a list of maps between chain complex RpM -> RpM (1) induced by the multiplication by x_0, ..., x_n
    -- Hope that they define right actions to give an O_Y-module structure.
        
    R := ring I;
    if I != saturate I then error "expected a saturated ideal I for Proj(R/I)";
    J := annihilator M;
    if J+I != J then error "expected a module representing a sheaf on Y=Proj(R/I)";

    n := numgens R-1; -- the dimension of the ambient space    
    d := dim I-1; -- the dimension of Y
    kk := coefficientRing R;
    (S,E) := productOfProjectiveSpaces({n,d}, CoefficientField=>kk); 
  
    -- Need to define Y -> P^d the linear projection, consist of copies of rational maps
    -- projCenter := random(R^{(codim I + 1):1}, R^1); -- pick a general linear subspace of dim = codim Y - 1
    -- assert (dim saturate (I+(ideal projCenter)) == -1); -- check so that Y and the proj. center do not intersect
    
    --time directImageComplex(I,M,transpose projCenter)

    -- or equivalently; use "NoetherNormalization" package;


--    time directImageComplex(I,M,((noetherNormalization I)_0).matrix)
        
    -- Both ideas are too stupid; mostly it takes too much time in practice.
    -- Moreover, it gets harder to observe an action;
    -- Take a linear projection by elimination so that a number of actions become diagonal.

    -- Linear projection induced by (x_0, ..., x_n) |-> (x_0, ..., x_d)
    projCenter := transpose matrix apply(dim I, i->{R_i});
    if dim saturate (I + ideal projCenter) != -1 then error "expected Y=Proj(R/I) should not intersect with the linear space x_0=...=x_(codim Y)=0";
    -- shuffling I by a linear automorphism, and take a projection seems to be much faster.

    -- computing the direct image complex onto P^d
    -- maybe we can make it simpler since there is a similar code in the method "directImageComplex" 
    p1 := map(S,R,(vars S)_{0..n});
    gI := gens I; 
    dgI := apply(degrees source gI,d->{-d_0,0});   
    I1 := ideal map(S^1,S^dgI,p1(gI));    
    gM := presentation M;    
    dtgM := apply(degrees target gM,d->{-d_0,0});   
    dsgM := apply(degrees source gM,d->{-d_0,0});   
    gM1 := map(S^dtgM,S^dsgM,p1(gM));    
    dtproj :=  apply(degrees target projCenter,d->{-d_0,0});   
    dsproj :=  apply(degrees source projCenter,d->{-d_0,0});    
    proj1 := map(S^dtproj,S^dsproj,p1(projCenter));    
    y := (vars S)_{n+1..n+d+1};    
    graph := trim(minors(2,y||proj1)+I1);
    Mgraph := coker(gens graph**id_(target gM1)|gM1);

    projOnEs:=projectionMapOnEs(Mgraph,{1});
    E1:=target projOnEs; 
        
    -- method to deal a sign issue
    signChange:= unsignedMap -> (
	 r := rank source unsignedMap;
	 signedMap := matrix apply(entries unsignedMap, ent->(
		 for j from 0 to r-1 list(
		     	 sign:=if ent_j==0 then 1 else (-1)^((degree ent_j)_0);
			 sign*ent_j
		     )
		 )
	 );
     signedMap);


    -- if we take a range from -{n+1,d+1} to {n+1,d+1}, then sometimes it makes an error
    -- take a bigger range to assure that both T(M) and T(M(1,0)) behaves nicely
    T:=tateResolution(Mgraph,-{n+2,d+2},{n+2,d+2});
    sT:=removeZeroTrailingTerms strand(T,toList(2:0),{0});
    mi := min sT; ma:=max sT;
    W := new ChainComplex;
    W.ring = E1;
    apply(toList(mi..ma),i-> W_i = E1^(-apply(degrees sT_i,d->d_{1})));
    apply(toList(mi+1..ma),i->W.dd_i = map(W_(i-1),W_i,signChange projOnEs sT.dd_i));

    -- original direct image complex, as a quotient of sums of S(-1)
    -- presented by 'DummyQuotient', which assigns U^(-1) as a quotient of \wedge^0 S(-1)
    RpM:=beilinson(W,BundleType=>DummyQuotientBundle);
    RpMfree:=beilinson(W,BundleType=>FreeBundle);
    RpMbetweenFree:=beilinson(W,BundleType=>MapsBetweenFreeBundles);

    -- reading off all the multiplication maps by coordinate functions on E-sides    
    -- collect the maps in T which are linear in 1st variables
    multMapOnEs:=i->(
    	srcDegs := positions(degrees T_i, j->member(j, degrees sT_i));
	tarDegs := positions(degrees T_(i-1), j->member(j+{1,-1}, degrees sT_(i-1)));
    	(T.dd_i)_srcDegs^tarDegs
    );

    -- compute the action on the projected S-side
    multMap:=(k,i)->(
	tar:= E1^(apply(degrees target multMapOnEs(i),degs->-degs_1));
	src:= E1^(apply(degrees source multMapOnEs(i),degs->-degs_1));
	beilinson(map(tar,src,projOnEs contract(E_k,multMapOnEs(i))),BundleType=>FreeBundle)
	); 
     
     T10 := (T**E^{{-1,0}})[-1]; -- shift and twist formula
     sT10 := removeZeroTrailingTerms strand(T10,toList(2:0),{0});
      
     mi = min sT10; ma=max sT10;
     W10 := new ChainComplex;
     W10.ring = E1;
     apply(toList(mi..ma),i-> W10_i = E1^(-apply(degrees sT10_i,d->d_{1})));
     apply(toList(mi+1..ma),i->W10.dd_i = map(W10_(i-1),W10_i,projOnEs(sT10.dd_i)));
     
     -- compute a quasi-isomorphism from the universal exact sequence
     (RpM10, B, B'', mapFromB, qIs) := inverseQIsoFromTate(W10);
          
     firstMap := t-> (
	functionDefiningFirstMap := i-> multMap(t,i);
	map(B,RpMfree, functionDefiningFirstMap)
	);
    
    retVal := for t from 0 to (# gens R - 1) list (
	actionList := i-> (
    	    mapFromB_i * (firstMap(t))_i * (map(RpMfree,RpM,RpMbetweenFree))_i
	);
    	   map(B''[1],RpM,actionList)
	);
    
    retVal
)
*-

inverseQIsoFromTate=method()
inverseQIsoFromTate(ChainComplex) :=  (W)->(
    -- Input: W, a Tate resolution on a single projective space
    -- W should be big enough so that the corresponding S-complex is exact
    
    -- Output: (B',B,B'',mapFromB,qIs)
    -- B', the beilinson complex U(W), implemented as 'DummyQuotientBundle' option
    -- B, the split exact free complex, together with a natural inclusion from B'
    -- B'', the cokernel

    -- Note that the connecting homomorphism induces a quasi-isomorphism B''->B'[1].
    -- Since B is split exact, we may compute its inverse explicitly.
    
    -- mapFromB, the natural map from a pseudoinverse of B to B''[-1]
    -- qIs, the inverse quasi-isomorphism B'->B''[-1], as a map of chain complexes

    -- maybe it is better to exclude exceptions    
    -- can we plug in the option on BundleType?
    
    B':= beilinson(W,BundleType=>DummyQuotientBundle);
    B := beilinson(W,BundleType=>FreeBundle); -- this must be exact
    assert(isExact B);
    -- embedding of B' into B
    phi := beilinson(W,BundleType=>MapsBetweenFreeBundles);
    
    -- quotient of B
    E := ring W;
    R := ring B';
    kk := coefficientRing R;
    B'' := beilinson (W**E^{1}, BundleType=>DummyQuotientBundle) ** R^{1};
--    assert(B'' == coker phi);
    psi := map(B'', B, id_B);
    
    -- computing pseudoinverse
    mapsB := for i from (min B)+1 to max B list sub(B.dd_i,kk); -- reduction to the vector space maps
    tempB := chainComplexFromData{min B, max B, mapsB};
    Binv:= (pseudoInverse tempB)**R; -- takes too much time in several cases
--    assert(arePseudoInverses(tempB, Binv));

--    splittingMapsFromB:= for i from min B to max B list psi_(i+1)*Binv.dd_(-i);
--    mapFromB:=map(B''[1],B,i->splittingMapsFromB_(i-min B));
    
    splittingMapsFromB':= for i from min B to max B list psi_(i+1)*Binv.dd_(-i)*phi_i;
    qIs:= map(B''[1],B',i->splittingMapsFromB'_(i-min B));
    
--    (B',B,B'',mapFromB,qIs)
    qIs
)


--
tateOfDirectImage=method()
tateOfDirectImage(Ideal, Module) := (I,M)->(
    -- Input: I, the ideal of a projective variety Y in P^n of dimension d
    -- 	      M, a finitely generated module on Y    
    -- Output: T, a part of Tate resolution of M on Y \subseteq P^n.
    --         I, the ideal of Y
    
    R := ring I;
    if I != saturate I then error "expected a saturated ideal I for Proj(R/I)";
    J := annihilator M;
    if J+I != J then error "expected a module corresponding to a sheaf on Y=Proj(R/I)";

    n := numgens R-1; -- the dimension of the ambient space    
    kk := coefficientRing R;
    -- provide TateData on R
    (S,E) := productOfProjectiveSpaces({n}, CoefficientField=>kk);
    mapRtoS := map(S,R,vars S);
    MS := mapRtoS**M;

    -- I am not sure at the moment that the following range is too wide (e.g. -d-1..d+1 makes sense?)
    T := tateResolution(MS, -{n+1},{n+1});
    
    (I,T)
)
tateOfDirectImage(Ideal,Module,Matrix) := (I,M,phi) -> (
    -- Input: I, the ideal of a projective scheme X in P^n,
    --        M, a module representing a coherent sheaf on X
    --        phi, a kxm matrix representing a rational map or morphism
    --             f: X -> P^m
    -- Output: T, a part of Tate resolution of the direct image of M on Y \subseteq P^m
    -- 	       IY, ideal of Y of the image of X under f.
    R := ring I;
    if I != saturate I then error "expected a saturated ideal I for Proj(R/I)";
    J := annihilator M;  
    if J+I != J then error "expected a module representing a sheaf on X=Proj(R/I)";
    if minors(2,phi)+I != I then error" expected phi to define a rational map on X=Proj(R/I)";
    m := rank source phi-1;
    n := numgens R-1;
    kk :=coefficientRing R;
    (S,E) := productOfProjectiveSpaces({n,m},CoefficientField=>kk);    
    p1 := map(S,R,(vars S)_{0..n});
    gI := gens I; 
    dgI := apply(degrees source gI,d->{-d_0,0});   
    I1 := ideal map(S^1,S^dgI,p1(gI));    
    gM := presentation M;    
    dtgM := apply(degrees target gM,d->{-d_0,0});   
    dsgM := apply(degrees source gM,d->{-d_0,0});   
    gM1 := map(S^dtgM,S^dsgM,p1(gM));    
    dtphi :=  apply(degrees target phi,d->{-d_0,0});   
    dsphi :=  apply(degrees source phi,d->{-d_0,0});    
    phi1 := map(S^dtphi,S^dsphi,p1(phi));    
    y := (vars S)_{n+1..n+m+1};    
    graph := trim(minors(2,y||phi1)+I1);
    Mgraph := coker(gens graph**id_(target gM1)|gM1);

    psi := projectionMapOnEs(Mgraph,{1});
    E1 :=target psi;
    S1 :=((tateData E1).Rings)#0;
    v := apply(unique degrees source vars S, d->
	#select(degrees source vars S,e->e==d));
    high:= v;low:=-high;
    T:=tateResolution(Mgraph,low,high);
    sT:=removeZeroTrailingTerms strand(T,toList(2:0),{0}); 
--    sTW := removeZeroTrailingTerms beilinsonWindow sT;
    --print betti sTW;
    mi := min sT; ma:=max sT;
    W1 := new ChainComplex;
    W1.ring = E1;
    apply(toList(mi..ma),i-> W1_i = E1^(-apply(degrees sT_i,d->d_1))); 
    apply(toList(mi+1..ma),i->W1.dd_i = map(W1_(i-1),W1_i,psi(sT.dd_i)));

    IY:=saturate intersect apply(rank target phi, i->ker map(R, S1, phi^{i}));
    
    (IY,W1)
)
///
restart
debug needsPackage "TateOnProducts"
kk=ZZ/101
R=kk[x_0..x_4]
m=matrix{{x_0,x_1,x_3},{x_1,x_2,x_4}}
J=minors(2,m)
dim J, degree J
s=2, d=-2
N=symmetricPower(s, coker m)**R^{d}
betti res N
ann N == J
phi=transpose m
RphiN=time directImageComplex(J,N,phi);
ARphiN=time actionOnDirectImage(J,N,phi);
keys ARphiN

S=kk[y_0..y_1];
pphi=map(R,S,phi^{0})
I=ker pphi
isAction(I, apply(dim S, i->prune HH^1 ARphiN#1#i))

R1=prune HH^1 source ARphiN#1#0
isIsomorphic(truncate(2,R1),truncate(2,dual dual R1))
dual dual R1
----------------------
s=3,d=1
N=symmetricPower(s, coker m)**R^{d}

RphiN=time directImageComplex(J,N,phi);

netList apply(toList(min RphiN.. max RphiN),i->
          {-i, saturate annihilator HH^(-i) RphiN,betti res HH^(-i) RphiN})

R0=prune HH^0 RphiN
dim R0, degree R0
betti (sR0Dual = syz transpose presentation R0)
saturate annihilator coker transpose sR0Dual
dual source sR0Dual 

ARphiN=time actionOnDirectImage(J,N,phi);
keys ARphiN

isAction(I, apply(dim S, i->prune HH^0 ARphiN#0#i))

R0= prune HH^0 source ARphiN#0#0
isIsomorphic(truncate(2,R0),truncate(2,dual dual R0))
dual dual R0
///

actionOnDirectImage=method()
actionOnDirectImage(Ideal,ChainComplex) := (I,T) -> (
    -- Input : T, the Tate resolution of a sheaf, or a complex on a projective variety Y in P^n of dim. d
    --         I, the ideal of Y (annihilating the sheaves appear in HH beilinson T)
    -- Output : retTable, a Hashtable whose keys are the cohomology indices i where R^i survives;
    -- 	       	    	  and the list of induced maps of the multiplication by coordinates y_0, ..., y_n
    --	      	      	  on each strand representing R^i on P^d (after taking a linear projection from Y to P^d).
    
    -- WANT : to check whether T is a part of a Tate resolution or not
    
    (S,E):=(tateData ring T).Rings;
    R:=ring I;
    n:=numgens S-1;
    kk:=coefficientRing S;
    d:=dim I-1;
    
    -- Noether normalization by a linear projection from some coordinates
    listOfProjIndices := rsort subsets(toList(0..n),dim I);

     projErrorCounter:=0; projIndex:=listOfProjIndices_0;
     apply(listOfProjIndices, i->(
	     monIdeal:=ideal flatten apply(i, j->{R_j});
	     if dim saturate (I + monIdeal) == -1 then (projIndex=i; projErrorCounter=1; break;)
	     )
	 );
     if projErrorCounter==0 then error "expected Y=Proj(R/I) should not intersect with some coordinate plane of codimension (dim Y + 1)";

     projIdeal := flatten apply(projIndex, j->{S_j});
     << "We take a finite projection from P^n to P^d as " << projIdeal;
     
     (S1,E1) := productOfProjectiveSpaces ({d}, CoefficientField=>kk);
     
     tempCounter:=0;
     tempList:={};
     for i from 0 to n do(
	 if member(i, projIndex) then (tempList=tempList|{E1_tempCounter}; tempCounter=tempCounter+1;) else tempList=tempList|{0};
	 );
     projOnE := map(E1, E, tempList); -- quotient map E -> E1 induced by a linear projection
     ---------
    
     -- construct a complex representing R(pi \circ f)
     mi := min T; ma:=max T;
     W := new ChainComplex;
     W.ring = E1;
     apply(toList(mi..ma),i-> W_i = E1^(-apply(degrees T_i,d->d_{0})));
     apply(toList(mi+1..ma),i->W.dd_i = map(W_(i-1),W_i,projOnE T.dd_i));

     BW := beilinson (W, BundleType=>DummyQuotientBundle);
     regW:=max apply(min HH BW..max HH BW, i->regularity HH_i BW);

     -- Not clear that T is long enough; strands of T should be linear and separated each other.
     -- Need to extract linear submatrices of T (w.r.t. its degree)
     r:=max(n+1, regW);
          
     Tlin := (dual res (coker transpose (T[ma]).dd_0, LengthLimit=>ma+r+1))[-ma];
     -- need : separate every strand of Tlin; by reading off the linear submatrices
     -- CAUTION : Tlin might be slightly different from T (choice of bases issue)

     cohIndex:=sort apply(unique flatten degrees source Tlin.dd_(-r), i->i+r); -- index set for cohomology groups R^i
     linSubmatrix:=i->submatrixByDegrees(Tlin.dd_(-r), -r-1+i,-r+i); -- linear submatrix corresponding to R^i

     actionOnEachStrand:=linSub->(
	sT := res (coker ((-1)^(r+1)*linSub), LengthLimit=>ma+r+1)[r+1];
	sT' := (sT**E^{-1})[-1];
		
	sW:= new ChainComplex;
	sW.ring = E1;
	apply(toList(min sT..max sT),i->sW_i=E1^(-apply(degrees sT_i,d->d_{0})));
	apply(toList(min sT+1..max sT),i->sW.dd_i=map(sW_(i-1),sW_i,projOnE sT.dd_i));
	
	sW' := new ChainComplex;
	sW'.ring = E1;
	apply(toList(min sT'..max sT'),i->sW'_i=E1^(-apply(degrees sT'_i,d->d_{0})));
	apply(toList(min sT'+1..max sT'),i->sW'.dd_i=map(sW'_(i-1),sW'_i,projOnE sT'.dd_i));
	
	multMapOnW := j->(
	    contrMap:=map((sT'[-r])_0, (sT[-r])_0, contract(E_j,sT.dd_(-r)));
	    shiftedMap := extend(sT'[-r],sT[-r], contrMap);
	    map(sW', sW, i-> projOnE shiftedMap_(i+r))	
	    );

	-- computing the induced map on Beilinson complexes     
	-- at the moment, it is implemented only for "DummyQuotientBundle" option,
	-- i.e., U^(-1) is not precisely zero but a quotient of S(-1)
	BsW := beilinson (sW, BundleType=>DummyQuotientBundle);
	BsW1 := BsW**(S1)^{1};
	BsW' := beilinson (sW', BundleType=>DummyQuotientBundle);

	-- computing the inverse quasi-isomorphism
	qIs:=inverseQIsoFromTate sW';
	--     assert(isQuism qIs);

	multMap := j->(
    	    firstMap := map(BsW', BsW, i->beilinson((multMapOnW(j))_i, BundleType=>DummyQuotientBundle));
	    map(BsW1,BsW,i->qIs_i*firstMap_i)
	);
	actionList := for j from 0 to n list multMap(j);
	
	actionList
	);
    
    retTable := hashTable apply(cohIndex, i->(i,actionOnEachStrand linSubmatrix i));

    retTable
    )

actionOnDirectImage(Ideal, Module) := (I,M)->(
    -- Input: I, the ideal of a projective variety Y in P^n of dimension d
    -- 	      M, a finitely generated module on Y
    -- Output : retTable, a Hashtable whose keys are the cohomology indices i where R^i survives;
    -- 	       	    	  and the list of induced maps of the multiplication by coordinates y_0, ..., y_n
    --	      	      	  on each strand representing R^i on P^d (after taking a linear projection from Y to P^d).

    actionOnDirectImage tateOfDirectImage(I,M)
    )
actionOnDirectImage(Ideal,Module,Matrix) := (J,N,phi) -> (
    -- Input: J, the ideal of a projective scheme X in P^n,
    --        N, a module representing a coherent sheaf on X
    --        phi, a kxm matrix representing a rational map or morphism
    --             f: X -> P^m
    -- Output : retTable, a Hashtable whose keys are the cohomology indices i where R^i survives;
    -- 	       	    	  and the list of induced maps of the multiplication by coordinates y_0, ..., y_n
    --	      	      	  on each strand representing R^i on P^d (after taking a linear projection from Y to P^d).

    actionOnDirectImage tateOfDirectImage(J,N,phi)
    )
-*
------------------------
actionOnDirectImage(Ideal, Module) := (I,M)->(
    -- Input: I, the ideal of a projective variety Y in P^n of dimension d
    -- 	      M, a finitely generated module on Y    
    -- Output: actionList, a list of (n+1) map of chain complexes C->C(1)
    --	      	      	   which correspond to the multiplication map by (n+1) coordinates.
    -- C is a Beilinson complex representing \pi_{*} M where \pi : P^n -> P^d is a finite linear projection.
    
    -- should put an exception : I, M should be defined over a single projective space
    R := ring I;
    if I != saturate I then error "expected a saturated ideal I for Proj(R/I)";
    J := annihilator M;
    if J+I != J then error "expected a module corresponding to a sheaf on Y=Proj(R/I)";

    n := numgens R-1; -- the dimension of the ambient space    
    d := dim I-1; -- the dimension of Y
    r := regularity M; -- the Castelnuovo-Mumford regularity of M
    kk := coefficientRing R;
    -- provide TateData on R
    (S,E) := productOfProjectiveSpaces({n}, CoefficientField=>kk);
    mapRtoS := map(S,R,vars S);
--    MS := coker (mapRtoS**(presentation M));
    MS := mapRtoS**M;
    
     --------------------------
     -- should be generalized; by taking a certain Noether normalization 
     listOfProjIndices := rsort subsets(toList(0..n),dim I);

     projErrorCounter:=0; projIndex:=listOfProjIndices_0;
     apply(listOfProjIndices, i->(
	     monIdeal:=ideal flatten apply(i, j->{R_j});
	     if dim saturate (I + monIdeal) == -1 then (projIndex=i; projErrorCounter=1; break;)
	     )
	 );
     if projErrorCounter==0 then error "expected Y=Proj(R/I) should not intersect with some coordinate plane of codimension (dim Y + 1)";

     projIdeal := flatten apply(projIndex, j->{R_j});
     << "We take a finite projection from P^n to P^d as " << projIdeal;
     
     (S1,E1) := productOfProjectiveSpaces ({d}, CoefficientField=>kk);
     
     tempCounter:=0;
     tempList:={};
     for i from 0 to n do(
	 if member(i, projIndex) then (tempList=tempList|{E1_tempCounter}; tempCounter=tempCounter+1;) else tempList=tempList|{0};
	 );
     projOnE := map(E1, E, tempList); -- quotient map E -> E1 induced by a linear projection

     --------------------------    
    C:=tateResolution(MS, -{n+1},{n+1});
    -- C becomes linear on (-r) and lower homological degrees
    
    C':=(C**E^{-1})[-1]; -- Tate for M(1) by the shift and twist formula
    
    -- map from C to C', induced by contracting i-th coordinate
    -- ``extend'' method only extends in further positive degrees
    -- we need to dualize to fill out the opposite ray  
     multMapOnT:=i->(
    	 contrMap:=map((C'[-r])_0, (C[-r])_0, contract(E_i,C.dd_(-r)));
	 posExtension := extend(C'[-r],C[-r], contrMap);
	 negExtension := dual extend(dual(C[-r]), dual(C'[-r]), transpose contrMap);
	 shiftedMap := map(C'[-r],C[-r], i->if i >= -r then posExtension_i else negExtension_i);
	 
	 shiftedMap[r]
	 );     

     -- construct a projected complex of E1-modules
     mi := min C; ma:=max C;
     W := new ChainComplex;
     W.ring = E1;
     apply(toList(mi..ma),i-> W_i = E1^(-apply(degrees C_i,d->d_{0})));
     apply(toList(mi+1..ma),i->W.dd_i = map(W_(i-1),W_i,projOnE C.dd_i));

     mi = min C'; ma=max C';
     W' := new ChainComplex;
     W'.ring = E1;
     apply(toList(mi..ma),i-> W'_i = E1^(-apply(degrees C'_i,d->d_{0})));
     apply(toList(mi+1..ma),i->W'.dd_i = map(W'_(i-1),W'_i,projOnE C'.dd_i));
     
     multMapOnW := i->(map(W', W, j->projOnE (multMapOnT(i))_j));

     -- computing the induced map on Beilinson complexes
     -- at the moment, it is implemented only for "DummyQuotientBundle" option,
     -- i.e., U^(-1) is not precisely zero but a quotient of S(-1)
     BW := beilinson (W, BundleType=>DummyQuotientBundle);
     BW' := beilinson (W', BundleType=>DummyQuotientBundle);
     firstMap := i->(map(BW', BW, j->beilinson((multMapOnW(i))_j, BundleType=>DummyQuotientBundle)));
    
     -- computing the inverse quasi-isomorphism
     pseudoInvData := inverseQIsoFromTate (W'); -- should return necessary data only
--     qIs := pseudoInvData#4;
     qIs:=pseudoInvData;
--     assert(isQuism qIs);
     
     multMap := i->(map(BW**(S1)^{1},BW,j-> qIs_j * (firstMap(i))_j));
     actionList := for i from 0 to n list multMap(i);
     
     actionList
    )
---------------------------------------
*-
--Test functions checking whether we found a right action or not
substituteMatrices=method()
substituteMatrices(RingElement, List) := (f, ListOfMatrices)->(
    R := ring f;
    T := ring ListOfMatrices_0;
    d := degree f;
    assert (isHomogeneous f);

    monomialData := coefficients f;
    monomialList := flatten entries monomialData#0;
    coefficientList := flatten entries monomialData#1;

    productOfMatrices := apply(monomialList, j->(
	    m:=# factor j;
	    monomialInducedMap:= k->(
		varIndex:= index ((factor j)#(m-1-k))#0;
		varPower:= ((factor j)#(m-1-k))#1;
		(matrix ListOfMatrices_varIndex)^(varPower)
		);
	    
	    temp:=matrix id_(source ListOfMatrices_0);
	    
	    for l from 0 to m-1 do(
		temp=temp*monomialInducedMap(l);
		);
	    
    	    map((source ListOfMatrices_0)**T^{d}, (source ListOfMatrices_0), temp)
	       )
	   );
       
       sum (for i from 0 to # monomialList-1 list (sub(coefficientList_i,T) * productOfMatrices_i))
)

isAction=method()
isAction(Ideal, List) := (I,ActionList)->(
    -- Input: I, an ideal of a projective variety in P^n
    -- 	      ActionList, a list of (n+1) square matrices whose entries are linear
   
    -- Assumed to be: ActionList should correspond to induced maps of multiplication maps
    --	      	      given by (n+1) coordinates of P^n.

    n:=# ActionList - 1;
    retVal:=true;
    if n<0 then error "List of matrices is empty";
    if # gens ring I != n+1 then error "The number of matrices does not coincide with the number of variables";

    R:= ring ActionList_0;
    t:= numgens source ActionList_0;
    -- exceptions
    for i from 0 to n do(
	    if numgens source ActionList_i != t then error "All the matrices must have the same size";
	    if numgens target ActionList_i != t then error "All the matrices must be square matrices";
	    if target ActionList_i != (source ActionList_i)**R^{1} or isHomogeneous ActionList_i != true then error "Every matrix must present a map M->M(1) for some graded module M";
	);
        
    testCommutative := apply(subsets(toList(0..n),2),i->(ActionList_(i#0)**R^{1})*ActionList_(i#1)==(ActionList_(i#1)**R^{1})*ActionList_(i#0));
    if (unique testCommutative)!={true} then (print "not commutative"; retVal=false;);

    listOfGenerators:=flatten entries gens I;
    if listOfGenerators!={} and (unique apply(listOfGenerators, i->substituteMatrices(i,ActionList)==0))!={true} then (print "Matrices do not give the right action on Y"; retVal=false;);
    
    retVal
    )

-*
linearizeMatrix=method()
linearizeMatrix(Matrix):= M->(
    -- Input : M, a matrix presenting a graded module homomorphism
    -- Output : linM, a matrix which is composed of linear entries of M only.
    if (isHomogeneous M===false) then error "the matrix should present a graded module homomorphism";
    
    R:=ring M;
    src:=source M;
    tar:=target M;
    t:=symbol t;
    Rt:=R[t];
    weightedMap:=map(Rt,R,t*sub(vars R, Rt));
    quotMap:=map(R,Rt,{0});
    
    map(target M, source M, quotMap diff(t,weightedMap(M)))
    )

isLinearizable=method()
isLinearizable(Matrix):=M->(
    -- Input : M, a matrix presenting a graded module homomorphism
    -- If the original matrix and its linearized matrix induces the same Hilbert series
    -- then this method returns true.
    
    R:=ring M;
    n:=(# gens R)-1;
    
    hilbertSeries (n,betti syz gens gb image M) === hilbertSeries (n,betti syz gens gb image linearizeMatrix M)
    )

*-
---------------------------------------

---------------------------------------
///
restart
debug needsPackage "TateOnProducts"
kk=ZZ/101
R=kk[x_0..x_2]
I=ideal(x_0*x_1-x_2^2)
M=(coker matrix{{x_0, x_2},{x_2,x_1}})**R^{0} -- Ulrich module
-- M=(R^1/I)**R^{0}
betti M

T=time tateOfDirectImage(I,M);
RM=time directImageComplex(I,M,matrix id_R);
time prune HH RM
degree prune HH^0 RM, dim prune HH^0 RM, ann prune HH^0 RM
AA=time actionOnDirectImage(I,M);
keys AA
isAction(I, apply(dim R,i->(AA#0#i)_0))
isAction(I, apply(dim R,i->(prune HH^0 AA#0#i)))

needsPackage "ChainComplexExtras"
tally apply(0..2, i->isHomogeneous (AA#0#i))
tally apply(0..2, i->isChainComplexMap (AA#0#i))
------------
restart
debug needsPackage "TateOnProducts"
kk=ZZ/101
R=kk[x_0..x_3]
m=matrix{{x_0,x_3,x_2},{x_3,x_2,x_1}}
I=minors(2,m)
-- M=symmetricPower(2,coker m) ** R^{1}
M=(R^1/I)**R^{2} -- O(2) of a twisted cubic
betti res M

RM=time directImageComplex(I,M,matrix id_R);
time prune HH RM
degree prune HH^0 RM, dim prune HH^0 RM, ann prune HH^0 RM

AA=time actionOnDirectImage(I,M);
needsPackage "ChainComplexExtras"
tally apply(dim R, i->isHomogeneous (AA#0#i))
tally apply(dim R, i->isChainComplexMap (AA#0#i))
isAction(I, apply(dim R, i-> (AA#0#i)_0))
isAction(I, apply(dim R, i->(prune HH AA#0#i)_0))

-------------------
restart
debug needsPackage "TateOnProducts"
kk=ZZ/101
R=kk[x_0..x_3]
m=matrix{{x_0,x_3,x_2},{x_3,x_2,x_1}}
I=minors(2,m)
M=directSum((R^1/I)**R^{1},(R^1/I)**R^{-3})
betti res M

time betti (RM=directImageComplex(I,M,matrix id_R)); -- 11.94 seconds
time prune HH RM
degree prune HH^0 RM, dim prune HH^0 RM, ann prune HH^0 RM

AA=time actionOnDirectImage(I,M); -- 3.44 seconds
keys AA
needsPackage "ChainComplexExtras"
tally apply(dim R, i->isHomogeneous (AA#0#i))
tally apply(dim R, i->isChainComplexMap (AA#0#i))
isAction(I, apply(dim R, i-> (AA#0#i)_0))
isAction(I, apply(dim R, i->(prune HH AA#0#i)_0))
---------------------
restart
debug needsPackage "TateOnProducts"
kk=ZZ/101;
n=7
R=kk[x_0..x_n];
m=matrix{{x_0..x_(n-1)},{x_1..x_n}}
I=minors(2,m);
dim I,degree I,genus I
M=(R^1/I);
phi=vars R

T=time tateOfDirectImage(I,M); -- 0.169 seconds
BT=time beilinson T#1; -- 2.559 seconds
betti BT -- too big!
betti prune HH BT
--BT=time directImageComplex(I,M,phi);
AA=time actionOnDirectImage(I,M); -- 1.377 seconds

keys AA
betti source AA#0#0

needsPackage "ChainComplexExtras"
tally apply(dim R, i->isHomogeneous (AA#0#i))
tally apply(dim R, i->isChainComplexMap (AA#0#i))
isAction(I, apply(dim R, i-> (AA#0#i)_0))
isAction(I, apply(dim R, i->(prune HH AA#0#i)_0))
----------
restart
debug needsPackage "TateOnProducts"
kk=ZZ/101
d=2
needsPackage "Resultants"
vd=veronese(2,d,kk)
R=target vd;
S=source vd;
f=R_0^3+R_1^3+R_2^3
I=ker map(R/ideal f, S, vd)

M=(ideal (R_0, R_1+R_2))/(ideal f)
--M=R^1/(ideal f)

RM=time directImageComplex(ideal f,M,matrix vd); -- 22.106 seconds
AA=time actionOnDirectImage(ideal f,M,matrix vd); -- 24.756 seconds

betti RM
time betti prune HH RM
keys AA
betti source AA#0#0

needsPackage "ChainComplexExtras"
tally apply(dim S, i->isHomogeneous (AA#0#i))
tally apply(dim S, i->isChainComplexMap (AA#0#i))
isAction(I, apply(dim S, i-> (AA#0#i)_0))
isAction(I, apply(dim S, i->(prune HH AA#0#i)_0))

------------
restart
debug needsPackage "TateOnProducts"
kk=ZZ/101
d=6
needsPackage "Resultants"
vd=veronese(1,d,kk)
R=target vd;
S=source vd;

M=R^{1:-1}
I=ann M -- zero ideal
J=ker vd

RM=time directImageComplex(I,M,matrix vd); -- 54.488 seconds
AA=time actionOnDirectImage(I,M,matrix vd); -- 44.913 seconds

betti RM
time betti prune HH RM
keys AA
betti source AA#0#0

needsPackage "ChainComplexExtras"
tally apply(dim S, i->isHomogeneous (AA#0#i))
tally apply(dim S, i->isChainComplexMap (AA#0#i))
isAction(J, apply(dim S, i-> (AA#0#i)_0))
isAction(J, apply(dim S, i->(prune HH AA#0#i)_0))
------------
restart
debug needsPackage "TateOnProducts"
kk=ZZ/101
R=kk[x_0..x_2]
I=ideal (x_2^3-x_2*x_0^2-x_0*x_1^2)
dim I,degree I,genus I
M=(R^1/I)**R^{2} 

RM=time directImageComplex(I,M,matrix id_R);
AA=time actionOnDirectImage(I,M,matrix id_R);
needsPackage "ChainComplexExtras"
tally apply(dim R, i->isHomogeneous (AA#0#i))
tally apply(dim R, i->isChainComplexMap (AA#0#i))
isAction(I, apply(dim R, i-> (AA#0#i)_0))
isAction(I, apply(dim R, i->(prune HH AA#0#i)_0))
---------------------
restart
debug needsPackage "TateOnProducts"
kk=ZZ/101    
R=kk[x_0..x_4]
m=matrix {{ x_0+x_4,x_1,x_3+2*x_2},{x_1-3*x_3,x_2+x_4,x_4}}
I=minors(2,m)
dim I, degree I, genera I
--M=symmetricPower(2,coker m) -- Ulrich
M=(R^1/I)**R^{1}
--M=(R^1/I)**R^{2} -- too high computational cost
betti res M

AA=time actionOnDirectImage(I,M);
keys AA
needsPackage "ChainComplexExtras"
tally apply(dim R, i->isHomogeneous (AA#0#i))
tally apply(dim R, i->isChainComplexMap (AA#0#i))
isAction(I, apply(dim R, i-> (AA#0#i)_0))
isAction(I, apply(dim R, i->(prune HH^0 AA#0#i)))

-------------
-- EXAMPLE : toward the direct image of complexes
-- Pushforward of L=O(P \times C+Q \times C+D) on C \times C via the second projection
restart
debug needsPackage "TateOnProducts"
--kk=ZZ/41
kk=ZZ/1009
--R=kk[x_0..x_2,y_0..y_2, Degrees=>{3:{1,0},3:{0,1}}]
(S,E)=productOfProjectiveSpaces({2,2},CoefficientField=>kk)

-- 192^4 = -1 in kk
sub(1^4 + 192^4 + 0^4, kk)
sub(1^4+3^4+0^4,kk)

fx=S_0^4+S_1^4+S_2^4
fy=S_3^4+S_4^4+S_5^4
ICC=ideal (fx,fy)

-- 2 points on C. Note that the line passing through P and Q is V(x_2).
P=matrix{{1,192,0}}
Q=matrix{{192,1,0}}

--P=matrix{{1,3,0}}
--Q=matrix{{3,1,0}}
varX=matrix{{S_0,S_1,S_2}}
varY=matrix{{S_3,S_4,S_5}}
PX=P||varX
QX=Q||varX

I1=ideal(fy, minors(2,PX)) -- ideal for P*C
I2=ideal(fy, minors(2,QX)) -- ideal for Q*C
I3=ideal(fx,fy, minors(2, varX||varY)) -- ideal for the diagonal D
I=intersect(I1,I2,I3) -- ideal for P*C+Q*C+D

SC=S/ICC;
Ldual=I/ICC;

betti (LCC=Hom(Ldual, SC^1))
-- Betti table does not change, M is already saturated
betti saturate (LCC, ideal (vars SC)_{0..2})
betti saturate (LCC, ideal (vars SC)_{3..5}) 

phi = map(SC,S,vars SC);
betti (L=prune pushForward(phi,LCC))

-- Take a truncation so that the resolution becomes linear
Ltr = (truncate ({2,2},L))**S^{{2,2}};
betti res Ltr

Q=symExt(presentation Ltr, E);
T=time (res (coker Q,LengthLimit=>12))**E^{{2,2}}[4];
cohomologyMatrix (T, -{5,5},{3,3})
sT=strand(T,{0,0},{0}); -- strand associated to Rpi_{*}L, where pi:C \times C \to C is the 2nd projection
cohomologyMatrix (sT, -{5,5},{3,3})
betti sT

sTFull=new ChainComplex;
sTFull.ring = ring sT
ma=7;
sTFull=(dual res (coker transpose (sT[ma]).dd_0, LengthLimit=>2*ma))[-ma];
betti sTFull -- a Tate resolution for Rpi_{*}L, consisted of two strands
-- note that the boundary maps of sTFull are not exactly same as the maps of sT (basis choice issue)

(S',E')=productOfProjectiveSpaces({2},CoefficientField=>kk);
projOnE=map(E', E, toList(3:0)|(gens E'));
mi=min sTFull; ma=max sTFull;
W=new ChainComplex;
W.ring = E';
apply(toList(mi..ma),i-> W_i = E'^(-apply(degrees sTFull_i,d->d_{1})));
apply(toList(mi+1..ma),i->W.dd_i = map(W_(i-1),W_i,projOnE sTFull.dd_i));


betti W
IC=ideal (S'_0^4+S'_1^4+S'_2^4)
AA=time actionOnDirectImage(W, IC);
keys AA

needsPackage "ChainComplexExtras"
tally apply(dim S', i->isHomogeneous (AA#0#i))
tally apply(dim S', i->isHomogeneous (AA#1#i))
tally apply(dim S', i->isChainComplexMap (AA#0#i))
tally apply(dim S', i->isChainComplexMap (AA#1#i))
isAction(IC, apply(dim S', i-> (AA#0#i)_0))
isAction(IC, apply(dim S', i-> (AA#1#i)_(-1)))
isAction(IC, apply(dim S', i->prune HH^0 AA#0#i))
isAction(IC, apply(dim S', i->prune HH^1 AA#1#i))

RpiL = prune HH beilinson W
R0piL = prune HH^0 beilinson W
R1piL = prune HH^1 beilinson W

degree R0piL
betti res R0piL
primaryDecomposition ann R0piL

degree R1piL
betti res R1piL
primaryDecomposition ann R1piL

R0=prune HH^0 source AA#0#0
degree R0, rank R0, ann R0
dual dual R0
isIsomorphic(truncate(3, R0), truncate(3,dual dual R0))
-- R0 is isomorphic to O+O(-1)+O(-2)+O(-3), which is the pushforward of O_C.

R1=prune HH^1 source AA#1#0
degree R1, rank R1, ann R1
-- R1 is a skyscrapper sheaf at the the images of P and Q (indeed, P and Q map to the same point of P^1)
--------------------------------


------------------------------------
-- Example : product of elliptic curves
restart
debug needsPackage "TateOnProducts"
kk=ZZ/101
--R=kk[x_0..x_2,y_0..y_2, Degrees=>{3:{1,0},3:{0,1}}]
(S,E)=productOfProjectiveSpaces({2,2},CoefficientField=>kk)

fx=S_0^3+S_1^3+S_2^3
fy=S_3^3+S_4^3+S_5^3
ICC=ideal (fx,fy)

-- a point on C
P=matrix{{1,-1,0}}

varX=matrix{{S_0,S_1,S_2}}
varY=matrix{{S_3,S_4,S_5}}
PX=P||varX

I1=ideal(fy, minors(2,PX)) -- ideal for P*C
I2=ideal(fx,fy, minors(2, varX||varY)) -- ideal for the diagonal D

SCC=S/ICC;
temp=Hom(I2/ICC, SCC^1)
betti temp
betti saturate (temp, ideal (vars SCC)_{0..2})
betti saturate (temp, ideal (vars SCC)_{3..5}) 

phi = map(SCC,S,vars SCC);
betti (prune pushForward(phi,temp))
betti (L=prune (I1*(prune pushForward(phi,temp)))) -- O(D-P*C)
--coarseMultigradedRegularity L
--betti (L=prune pushForward(phi,temp))

Ltr = (truncate ({2,2},L))**S^{{2,2}};
betti res Ltr

Q=symExt(presentation Ltr, E);
T=(res (coker Q,LengthLimit=>14))**E^{{2,2}}[4];
cohomologyMatrix (T, -{5,5},{5,5})
sT=strand(T,{0,0},{0}); -- strand associated to Rpi_{*}L, where pi:C \times C \to C is the 2nd projection
cohomologyMatrix (sT, -{5,5},{5,5})
betti sT

sTFull=new ChainComplex;
sTFull.ring = ring sT
ma=7;
sTFull=(dual res (coker transpose (sT[ma]).dd_0, LengthLimit=>2*ma))[-ma];
betti sTFull -- a Tate resolution for Rpi_{*}L, consisted of two strands
-- note that the boundary maps of sTFull are not exactly same as the maps of sT (basis choice issue)

(S',E')=productOfProjectiveSpaces({2},CoefficientField=>kk);
projOnE=map(E', E, toList(3:0)|(gens E'));
mi=min sTFull; ma=max sTFull;
W=new ChainComplex;
W.ring = E';
apply(toList(mi..ma),i-> W_i = E'^(-apply(degrees sTFull_i,d->d_{1})));
apply(toList(mi+1..ma),i->W.dd_i = map(W_(i-1),W_i,projOnE sTFull.dd_i));
betti W

IC=ideal(S'_0^3+S'_1^3+S'_2^3);
AA=actionOnDirectImage(W,IC);
keys AA -- Only R^1 survives

R1=prune HH^1 source AA#1#0
degree R1, rank R1, ann R1 -- skyscrapper sheaf at the image of P.
-------------------

///

--composedFunctions = method()
composedFunctions = () -> (
    print "
      n={1,1}, v=n+{1,1}
      high=3*n, low=-high
      (S,E)=productOfProjectiveSpaces n
     --Text
     -- We build the example from Section 4 of our paper
     -- which corresponds to a rank 3 vector bundle on P^1xP^1. 
     --Example
      P=(image transpose gens trim (ideal vars E)^2)**E^{n}
      betti P
      LP=bgg P 
      M = (HH^0 LP)**S^{-n}
      betti res M
      T = tateResolution(M,low,high) 
      cohomologyMatrix(T,low,high)
     --Text 
     -- T is the part of the Tate resolution, which is complete in the range low to high.
     -- (In a wider range some terms are missing or are incorrect)  
     --Example
      cohomologyMatrix(T,2*low,2*high)
     --Text 
     -- Alternatively we can recover M from its Beilinson monad derived 
     -- from T.
     --Example 
      B=beilinson T
      M'=prune HH^0 B
      prune HH^1 B
      isIsomorphic(M,M')
     --Text
     -- We study the corner complex of T at c={0,0} . 
     --Example
      C=cornerComplex(T,{0,0});
      betti C      
      cohomologyMatrix(C,low,high)
      betti C.dd_0
      P=ker C.dd_0**E^{v}
     --Text
     -- The tensor product with E^{v} is necessary because we work with E instead of omega_E.
     -- M can be recovered by applying the bgg functor to P. 
     --Example
      LP=bgg P;
      betti LP
      coLP=apply(toList(min LP..max LP),i->prune HH^(-i) LP);
      apply(coLP,h->dim h)
      M1=HH^0 LP
      betti M1,betti M
      isIsomorphic(M,M1)
     --Text
     -- It works also for different syzygy modules in the corner complex.
     -- It works for all P=ker C.dd_k in the range where C.dd_k is computed 
     -- completely. We check the case k=1 and k=-2.
     --Example
      k=1
      P=ker C.dd_(-k)**E^{v}; betti P
      LP=bgg P
      betti LP
      coLP=apply(toList(min LP..max LP),i->prune HH^(-i) LP);
      apply(coLP,h->dim h)
      M1=HH^(-k) LP
      betti M1, betti M
      isIsomorphic(M,M1)
     --Text
     -- Note that we have to take HH^{(-k)} == HH_k because of the homological position in which
     -- P sits.
     --Example
      k=-2
      P=ker C.dd_(-k)**E^{v}; betti P
      LP=bgg P;
      betti LP
      coLP=apply(toList(min LP..max LP),i->prune HH^(-i) LP);
      apply(coLP,h->dim h)
      M1=HH^(-k) LP
      betti M1,betti M
      isIsomorphic(M,M1)
     --Text
     -- Next we check the functor bgg on S-modules.
     --Example
      RM=bgg M
      cohomologyMatrix(RM,low,high)
      betti RM
      uQ=firstQuadrantComplex(T,{0,0});
      cohomologyMatrix(uQ,low,high)      
     --Text
     -- The additional entry h in the zero position of the cohomology matrix of uQ
     -- indicates that we can recover
     -- the original square of the maximal ideal of E from the differential of of the first quadrant complex uQ
     -- in this specific case.
     --Example
      uQ.dd_(-1)
     --Text
     -- Next we test reciprocity.
     --Example
      T1=tateResolution(M,low,3*high);
      c={2,2}
      CM=cornerComplex(T1,c);
      RMc=firstQuadrantComplex(T1,c);
      cohomologyMatrix(CM,low,3*high)     
      coRMc=apply(toList(-10..-4),i-> HH^(-i) RMc==0)
      P1=ker CM.dd_(-sum c)
      LP=bgg (P1**E^{-c+v}) 
      betti LP
      coLP=apply(toList(min LP..max LP),i->dim HH^(-i) LP)
     --Text
     -- Hence both Lp and RMc are azyclic.
     --Example
      Mc=prune truncate(c,M)**S^{c}
      betti (Mc'=HH^0 LP), betti Mc
      isIsomorphic(Mc',Mc)
     --Example
      c={3,1}
      cohomologyMatrix(T1,low,2*high)
      CM=cornerComplex(T1,c);
      cohomologyMatrix(CM,low,3*high)
      RMc=firstQuadrantComplex(T1,c);     
      coRMc=apply(toList(-9..-4),i-> HH^(-i) RMc==0)
      P1=ker CM.dd_(-sum c)
      LP=bgg (P1**E^{-c+v}) 
      betti LP
      coLP=apply(toList(min LP..max LP),i->dim HH^(-i) LP)
     
      Mc=prune truncate(c,M)**S^{c}
      betti (Mc'=HH^0 LP), betti Mc
      isIsomorphic(Mc',Mc)
     --Text
     -- Now we test tateExtension.
     --Example
      W=beilinsonWindow T
      T'=tateExtension W 
      comT'=cohomologyMatrix(T',low,high) 
      comT=cohomologyMatrix(T,low,high)
      assert(sub(comT',vars ring comT)==comT)
     --Text
     -- Finally we illustate how shifting the Beilinson window works.
     --Example
      cohomologyMatrix(T,low,high)
      cohomologyMatrix(beilinsonWindow T,low, high)
      B = beilinson T
      d={2,2}
      T1=T**E^{d}[sum d]
      cohomologyMatrix(beilinsonWindow T1,low,high)
      B1 =beilinson T1
      decompose annihilator HH^1 B1
      decompose annihilator HH^2 B1
      M1=HH^0 B1
      dim M1
      betti M1, betti M
      isIsomorphic(M1,M**S^{-d})
     --Text
     -- Another shift:
     --Example 
      d={-1,-2}
      T2=T**E^{d}[sum d]
      cohomologyMatrix(beilinsonWindow T2,low,high)
      cohomologyMatrix(T,low,high)
      B2 =beilinson T2
      HH^(-1) B2 == 0
      M2=HH^0 B2
      dim M2
      betti M2, betti M, betti truncate(-d,M)
      isIsomorphic(M2,truncate(-d,M)**S^{-d}")



///
restart
loadPackage("TateOnProducts",Reload=>true)
debug TateOnProducts
composedFunctions()
n={1,1}
high=3*n, low=-high
(S,E)=productOfProjectiveSpaces n
  -- the example 4.1 from the paper
S.?TateData
E.?TateData
T1=(dual res(coker gens trim (ideal vars E)^2,LengthLimit=>11))[1]

(ring T1).?TateData

cohomologyMatrix(T1,low,high)

c={4,4}
betti(uc= upperCorner(T1,c))
ring uc
T=res(coker uc,LengthLimit=>12)[sum c]
betti T
ring T
cohomologyMatrix(T,2*low,2*high)
B=beilinson T
M=prune HH^0 B
prune HH^1 B

comM=cohomologyMatrix(M,low,high)
comT=cohomologyMatrix(T,low,high)
assert(comM===sub(comT,vars ring comM))


C=cornerComplex(T,{0,0});
betti C
cohomologyMatrix(C,low,high)
P=ker C.dd_0
LP=bgg P;
betti LP

coLP=apply(toList(min LP..max LP),i->prune HH^(-i) LP)
apply(coLP,h->dim h)
M1=last coLP
betti M1,betti M
-- some how wrong twist
M1'=M1**S^{{-2,-2}}
betti M1',betti M
isIsomorphic(M,M1')


P=ker C.dd_2; betti P
LP=bgg P;
betti LP

coLP=apply(toList(min LP..max LP),i->prune HH^(-i) LP)
apply(coLP,h->dim h)
M1=last coLP
betti M1,betti M
M1'=M1**S^{{-2,-2}}
betti M1',betti M
isIsomorphic(M,M1')
-- we comclude: It works for various P=kerC.dd_p


RM=bgg M
betti T1
betti trivialHomologicalTruncation(T1,-7,-2)==betti trivialHomologicalTruncation(RM,-7,-2)
cohomologyMatrix(RM,low,high)
P=ker RM.dd_(-2)
betti RM
RM.dd_(-2)==0 -- Frank: I do not understand why the first differential is zero
isIsomorphic(image(RM.dd_(-3)),image(T1.dd_(-3)))


--Testing Reciprocity
c={2,2}
CM=cornerComplex(T1,c)
cohomologyMatrix(CM,2*low,2*high)
P1=ker CM.dd_(-sum c)
LP=removeZeroTrailingTerms bgg P1

coLP=apply(toList(min LP1..max LP),i->prune HH^(-i) LP)
apply(coLP,h->dim h)
Mc=prune truncate(c,M)
betti (Mc'=(first coLP)**S^{-{2,2}}), betti Mc
isIsomorphic(Mc',Mc)

--Testing Reciprocity
c={3,1}
CM=cornerComplex(T1,c)
cohomologyMatrix(CM,2*low,2*high)
P1=ker CM.dd_(-sum c)
LP=removeZeroTrailingTerms bgg P1

coLP=apply(toList(min LP..max LP),i->prune HH^(-i) LP)
apply(coLP,h->dim h)
Mc=prune truncate(c,M)
-- Frank: I do not understand why {-2,-2} is the right correction for the twist.
betti (Mc'=(first coLP)**S^{-{2,2}}), betti Mc
isIsomorphic(Mc',Mc)

--Testing Reciprocity
c={3,2}
CM=cornerComplex(T1,c)
cohomologyMatrix(CM,2*low,2*high)
P1=ker CM.dd_(-sum c)
LP=removeZeroTrailingTerms bgg P1

coLP=apply(toList(min LP..max LP),i->prune HH^(-i) LP)
apply(coLP,h->dim h)
Mc=prune truncate(c,M)
-- Frank: I do not understand why {-2,-2} is the right correction for the twist.
betti (Mc'=(first coLP)**S^{-{2,2}}), betti Mc
isIsomorphic(Mc',Mc)



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
  ". It allows computing the direct image complexes of a coherent sheaf along the projection onto a product
  of any of the factors.",
   PARA{},"The main differences from the paper are:",
   UL{  "the exterior algebra E is positively graded ",
        "we use E instead of omega_E ",
	" all complexes are chain complexes instead of cochain complexes"
	},

   PARA{},
    SUBSECTION "Beilinson monads",
    UL{
	TO beilinsonWindow,
	TO tateResolution,
	TO tateExtension,
	TO beilinson,
	TO bgg,
	TO directImageComplex,
	TO actionOnDirectImage,
	TO composedFunctions
        },
   SUBSECTION "Numerical Information",
   UL{
      TO cohomologyMatrix,
      TO eulerPolynomialTable,
      TO cohomologyHashTable,
      TO tallyDegrees
     },
   SUBSECTION "From graded modules to Tate resolutions",
   UL{   TO productOfProjectiveSpaces,
	 TO symExt,
	 TO lowerCorner,
	 TO upperCorner
      },
    SUBSECTION "Subcomplexes",
    UL{
       TO cornerComplex,
       TO regionComplex,
       TO strand,
       TO firstQuadrantComplex,
       TO lastQuadrantComplex
      },
    PARA{}, "Acknowledgement: The work of Yeongrak Kim and Frank-Olaf Schreyer was supported by Project I.6 
    of the SFB-TRR 195 ''Symbolic Tools in Mathematics and their Application'' of the German Research Foundation (DFG)."
   }
doc ///
   Key
    isIsomorphic
    (isIsomorphic,Module,Module)
   Headline
    probabilistic test for homogeneous isomorphism
   Usage
    v = isIsomorphic(A,B)
   Inputs
    A:Module
    B:Module
   Outputs
    v:Boolean
   Description
    Text
     First checks that the generator degrees are the same. Then
     computes a random degree 0 map A --> B and B --> A, 
     and returns true iff both are surjections.
    Example
     S = ZZ/11[a,b]
     M = coker random(S^{-2,0,1,2}, S^{3:-3})
     N = coker (random(cover M, cover M)*presentation M)
     tally apply(100, j->isIsomorphic(M,N))
   Caveat
    If the function returns true then the modules ARE isomorphic. But if it returns false
    they may be isomorphic anyway.
///


doc ///
   Key
    composedFunctions
   Headline
    composed functions
   Usage
    cmds=composedFunctions() 
   Outputs
    cmds: String
     the commands used in the testing/illustration of composed functions
   Description
     Text
      Prints the commands which illustrate / test various composition of functions.
     Example
      n={1,1}, v=n+{1,1}
      high=3*n, low=-high
      (S,E)=productOfProjectiveSpaces n
     Text
      We build the example from Section 4 of the paper
      @ HREF("https://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces") @
      which corresponds to a rank 3 vector bundle on P^1xP^1. 
     Example
      P=(image transpose gens trim (ideal vars E)^2)**E^{n}
      betti P
      LP=bgg P 
      M = (HH^0 LP)**S^{-n}
      betti res M
      T = tateResolution(M,low,high) 
      cohomologyMatrix(T,low,high)
     Text 
      T is the part of the Tate resolution, which is complete in the range low to high.
      (In a wider range some terms are missing or are incorrect)  
     Example
      cohomologyMatrix(T,2*low,2*high)
     Text 
      Alternatively we can recover M from its Beilinson monad derived 
      from T.
     Example 
      B=beilinson T
      M'=prune HH^0 B
      prune HH^1 B
      isIsomorphic(M,M')
     Text
      We study the corner complex of T at c=\{0,0\} . 
     Example
      C=cornerComplex(T,{0,0});
      betti C      
      cohomologyMatrix(C,low,high)
      betti C.dd_0
      P=ker C.dd_0**E^{v}
     Text
      The tensor product with E^{\{v\}} is necessary because we work with E instead of $\omega_E$.
      M can be recovered by applying the bgg functor to P. 
     Example
      LP=bgg P;
      betti LP
      coLP=apply(toList(min LP..max LP),i->prune HH^(-i) LP);
      apply(coLP,h->dim h)
      M1=HH^0 LP
      betti M1,betti M
      isIsomorphic(M,M1)
     Text
      It works also for different syzygy modules in the corner complex.
      It works for all P=ker C.dd_k in the range where C.dd_k is computed 
      completely. We check the case k=1 and k=-2.
     Example
      k=1
      P=ker C.dd_(-k)**E^{v}; betti P
      LP=bgg P
      betti LP
      coLP=apply(toList(min LP..max LP),i->prune HH^(-i) LP);
      apply(coLP,h->dim h)
      M1=HH^(-k) LP
      betti M1, betti M
      isIsomorphic(M,M1)
     Text
      Note that we have to take HH^{(-k)} == HH_k because of the homological position in which
      P sits.
     Example
      k=-2
      P=ker C.dd_(-k)**E^{v}; betti P
      LP=bgg P;
      betti LP
      coLP=apply(toList(min LP..max LP),i->prune HH^(-i) LP);
      apply(coLP,h->dim h)
      M1=HH^(-k) LP
      betti M1,betti M
      isIsomorphic(M,M1)
     Text
      Next we check the functor bgg on S-modules.
     Example
      RM=bgg M
      cohomologyMatrix(RM,low,high)
      betti RM
      uQ=firstQuadrantComplex(T,{0,0});
      cohomologyMatrix(uQ,low,high)      
     Text
      The additional entry h in the zero position of the cohomology matrix of uQ
      indicates that we can recover
      the original square of the maximal ideal of E from the differential of of the first quadrant complex uQ
      in this specific case.
     Example
      uQ.dd_(-1)
     Text
      Next we test reciprocity.
     Example
      T1=tateResolution(M,low,3*high);
      c={2,2}
      CM=cornerComplex(T1,c);
      RMc=firstQuadrantComplex(T1,c);
      cohomologyMatrix(CM,low,3*high)     
      coRMc=apply(toList(-10..-4),i-> HH^(-i) RMc==0)
      P1=ker CM.dd_(-sum c)
      LP=bgg (P1**E^{-c+v}) 
      betti LP
      coLP=apply(toList(min LP..max LP),i->dim HH^(-i) LP)
     Text
      Hence both Lp and RMc are azyclic.
     Example
      Mc=prune truncate(c,M)**S^{c}
      betti (Mc'=HH^0 LP), betti Mc
      isIsomorphic(Mc',Mc)
     Example
      c={3,1}
      cohomologyMatrix(T1,low,2*high)
      CM=cornerComplex(T1,c);
      cohomologyMatrix(CM,low,3*high)
      RMc=firstQuadrantComplex(T1,c);     
      coRMc=apply(toList(-9..-4),i-> HH^(-i) RMc==0)
      P1=ker CM.dd_(-sum c)
      LP=bgg (P1**E^{-c+v}) 
      betti LP
      coLP=apply(toList(min LP..max LP),i->dim HH^(-i) LP)
     
      Mc=prune truncate(c,M)**S^{c}
      betti (Mc'=HH^0 LP), betti Mc
      isIsomorphic(Mc',Mc)
     Text
      Now we test tateExtension.
     Example
      W=beilinsonWindow T
      T'=tateExtension W 
      comT'=cohomologyMatrix(T',low,high) 
      comT=cohomologyMatrix(T,low,high)
      assert(sub(comT',vars ring comT)==comT)
     Text
      Finally we illustrate how shifting the Beilinson window works.
     Example
      cohomologyMatrix(T,low,high)
      cohomologyMatrix(beilinsonWindow T,low, high)
      B = beilinson T
      d={2,2}
      T1=T**E^{d}[sum d]
      cohomologyMatrix(beilinsonWindow T1,low,high)
      B1 =beilinson T1
      decompose annihilator HH^1 B1
      decompose annihilator HH^2 B1
      M1=HH^0 B1
      dim M1
      betti M1, betti M
      isIsomorphic(M1,M**S^{-d})
     Text
      Another shift:
     Example 
      d={-1,-2}
      T2=T**E^{d}[sum d]
      cohomologyMatrix(beilinsonWindow T2,low,high)
      cohomologyMatrix(T,low,high)
      B2 =beilinson T2
      HH^(-1) B2 == 0
      M2=HH^0 B2
      dim M2
      betti M2, betti M, betti truncate(-d,M)
      isIsomorphic(M2,truncate(-d,M)**S^{-d})
   SeeAlso
    bgg
    upperCorner
    cornerComplex
    cohomologyMatrix
    beilinson
    tateExtension
    isIsomorphic
///


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
     Uses a free resolution and takes the maximum degree of the terms.
    Example
     (S,E) = productOfProjectiveSpaces{1,1,2}
     I = ideal(x_(0,0)^2,x_(1,0)^3,x_(2,0)^4)
     R = coarseMultigradedRegularity(S^1/I)
     N = truncate(R,S^1/I);
     betti res N
     netList toList tallyDegrees res N
    Text
     See the proof of Proposition 2.7 in
       @ HREF("https://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces")@.
   SeeAlso
    productOfProjectiveSpaces
    tallyDegrees
///

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
      E.TateData, so that S and E can subsequently find each other
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
///

doc ///
   Key
    InitialDegree
   Headline
    Option for chainComplexMap
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
    ContractionData
   Headline
    name of a cached datum
///


doc ///
   Key
    TateData
   Headline
    symbol used in beilinsonBundle
   SeeAlso
    beilinsonBundle
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
       cohomologyHashTable or eulerPolynomialTable instead.

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
  SeeAlso
   cohomologyHashTable
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
	eulerPolynomialTable H'
  Caveat
        In case of hypercohomology, we write k
	instead of h^{-1}, and use the cohomology ring
	ZZ[h,k].
  SeeAlso
        productOfProjectiveSpaces
     	cohomologyMatrix
        eulerPolynomialTable
	cornerComplex
///

doc ///
  Key
    eulerPolynomialTable
    (eulerPolynomialTable,Module,List,List)
    (eulerPolynomialTable,ChainComplex,List,List)
    (eulerPolynomialTable,HashTable)
  Headline
    cohomology groups of a sheaf on a product of projective spaces, or of (part) of a Tate resolution
  Usage

    H=eulerPolynomialTable(M,low,high)
    H=eulerPolynomialTable(T,low,high)
    H=eulerPolynomialTable H'
  Inputs
    M: Module
       graded module representing a sheaf on a product of projective spaces
    T: ChainComplex
       free complex over the exterior algebra
    H': HashTable
       output of cohomologyHashTable  low: List
    high: List
       two lists representing multi-degrees, the range for computation.
  Outputs
    H: HashTable
       values are hypercohomology polynomials
  Description
     Text
       If M is a multi-graded module representing a coherent sheaf F on $P^n := P^{n_0} x .. x P^{n_{t-1}}$,
       the script returns a hash table with entries
       a => sum_i h^i(F(a))*h^i \in ZZ[h,k],
       where k represents h^{-1},
       where a is a multi-index, low<=a<=high in the partial order
       (thus the value is 0 when i is not in the range 0..sum n.)
       In case T is a Tate resolution corresponding to an object F in D^b(P^n), then
       the values returned are the polyomials of the hypercohomology groups of twists of F, and
       the values can be nonzero in a wider range.

       In case the number of factors t is 2, the output of @ TO cohomologyMatrix @ is
       easier to parse. In general the script @ TO cohomologyHashTable @ gives the same
       information as this script, but in a less compact form.

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
	H = eulerPolynomialTable H'
	H = eulerPolynomialTable (M, low, high)
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
       C3=trivialHomologicalTruncation(C2,2,2)
///

-*
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
*-


--------------------------
-- subcomplexes         --
--------------------------

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
        T=tateExtension W;
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
  SeeAlso
    upperCorner
    lowerCorner
    beilinsonWindow
    tateExtension
    firstQuadrantComplex
    lastQuadrantComplex
    cohomologyMatrix
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
	corresponds to the Tate resolution $R{\pi_J}_*(F(c))$ where $J =\{0,\ldots,t-1\} - I$ is the complement and $\pi_J: \mathbb PP \to \prod_{j \in J} \mathbb P^{n_j}$
	denotes the projection.
     Example
        n={1,1};
        (S,E) = productOfProjectiveSpaces n;
	T1 = (dual res trim (ideal vars E)^2)[1];
	a=-{2,2};T2=T1**E^{a}[sum a];
	W=beilinsonWindow T2,cohomologyMatrix(W,-2*n,2*n)
        T=tateExtension W;
    	low = -{2,2};high = {2,2};
	cohomologyMatrix(T,low,high)
	sT1=strand(T,{-1,0},{1});
	cohomologyMatrix(sT1,low,high)
	sT2=strand(T,{-1,0},{0});
	cohomologyMatrix(sT2,low,high)
	sT3=strand(T,{-1,0},{0,1});
	cohomologyMatrix(sT3, low,high)
  SeeAlso
    upperCorner
    lowerCorner
    beilinsonWindow
    tateExtension
    firstQuadrantComplex
    lastQuadrantComplex
    cohomologyMatrix
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
  SeeAlso
    upperCorner
    lowerCorner
    lastQuadrantComplex
    cohomologyMatrix
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
  SeeAlso
    upperCorner
    lowerCorner
    firstQuadrantComplex
    cohomologyMatrix
///

doc ///
  Key
    tateResolution    
    (tateResolution,Module,List,List)
    (tateResolution,Matrix,List,List)
  Headline
    compute the Tate resolution 
  Usage
    T = tateResolution(M,low,high)
    phi = tateResolution(A,low,high)
  Inputs
    M: Module
       multi-graded module representing a sheaf F
    low:List
       a multidegree
    high:List
       a multidegree
    A: Matrix
    	a homomorphism of multi-graded modules from M to N
  Outputs
    T : ChainComplex
       a bounded free complex over the exterior algebra
    phi : ChainComplexMap
       an induced map from T(M) to T(N) over the exterior algebra
  Description
     Text
       The call

       tateResolution(M,low,high)

       forms a free subquotient complex the Tate resolution of the sheaf F represented by M 
       in a range that covers all generators corresponding to 
       cohomology groups of
       twists F(a) of F  in the range low <= a <= high,
       see        
       @  HREF("http://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces") @.
     Example
        (S,E) = productOfProjectiveSpaces{1,1}
	low = {-3,-3};high = {3,3};
	T=tateResolution( S^{{1,1}},low, high);
	cohomologyMatrix(T,low,high)
     Text
        The complex contains some trailing terms and superfluous terms in a wider range, which can be removed
	using trivial homological truncation.
     Example
	cohomologyMatrix(T,2*low,2*high)
	betti T
	T'=trivialHomologicalTruncation(T, -sum high,-sum low)
	betti T'
	cohomologyMatrix(T',2*low,2*high)
     Text
     	 The call
	 
	 tateResolution(A,low,high)
	 
	 where A is a matrix representing the multi-graded module homomorphism from M to N
	 computes the induced map between two free subquotients of Tate resolutions of M and N
	 in the given range.
     Example
	 (S,E)=productOfProjectiveSpaces {2,1}
	 low=-{2,1}; high={2,1};
	 A=map(S^1, S^{1:{-1,0}}, {{S_0}})
	 M=source A; N=target A;
	 TA = tateResolution(A, low, high);
	 TM = tateResolution(M, low, high);
	 TN = tateResolution(N, low, high);
	 (source TA == TM, target TA == TN)
  SeeAlso
    upperCorner
    lowerCorner
    trivialHomologicalTruncation
    cohomologyMatrix
///



doc ///
  Key
    cornerComplex
    (cornerComplex,ChainComplex,List)
    (cornerComplex,Module,List,List,List)
  Headline
    form the corner complex
  Usage
    C = cornerComplex(T,c)
    C = cornerComplex(M,c,low,high)
  Inputs
    T: ChainComplex
       a (part of a) Tate resolution on a product of t projective spaces
    c: List
       cohomological degree of upper corner of the  last quadrant complex which is part of the corner complex
    M: Module
       multi-graded module representing a sheaf F
    low:List
       a multidegree
    high:List
       a multidegree
  Outputs
    C : ChainComplex
       The corner complex
  Description
     Text       
       The call

       cornerComplex(T,c)

       forms the corner complex with corner c of a (part of a) Tate resolution T as defined in
       @  HREF("http://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces") @.
       The call

       cornerComplex(M,c,low,high)

       first computes the Tate resolution T of the sheaf F represented by M
       in the range covering low to high and then takes the corner complex of T.
     Example
        (S,E) = productOfProjectiveSpaces{1,1}
	low = {-4,-4};high = {3,2};
	T1= (dual res( trim (ideal vars E)^2,LengthLimit=>8))[1];
	T2=res(coker upperCorner(T1,{4,3}),LengthLimit=>13)[7];
     Text
        Finally, we can define T,
	the sufficient part of the Tate resolution:
     Example
        T=trivialHomologicalTruncation (T2,-5,6);
	cohomologyMatrix(T,low,high)
     Text
       In the following we will produce a corner complex cT with
       corner at $c =\{-2,-1\}.$ To do this we need a big enough part
       T of a Tate resolution so that all the strands around
       the corner are exact. This example corresponds to the
       Example of Section 4 of our paper referenced above. The Tate resolution
       in question is that corresponding to a rank 3 natural
       sheaf on P^1xP^1.
     Example
        c =  -{2,1};
	cT=cornerComplex(T,c);
	betti cT
	cohomologyMatrix(cT,low,high)
     Text
        The corner complex is built from a first quadrant
	complex fqT and a last quadrant complex lqT
	connected by the corner map between these complexes.
     Example
    	fqT=firstQuadrantComplex(T,c);
	lqT=lastQuadrantComplex(T,c);
	cohomologyMatrix(fqT,low,high)
	cohomologyMatrix(lqT,low,high)
     	betti fqT
        betti lqT
	betti cT
     Text
        Here the corner map is cT.dd_2
     Example
        betti (cT.dd_(-sum c-1))      
     Text
        In general the corner map is a chain complex map
	from lqT to fqT spread over several homological degrees.
-----------------
     Text
        Putting the corner in $c = \{-1,-1 \} $ we get a different
	picture:
     Example
        c = {-1,-1}
	cT=cornerComplex(T,c);
	betti cT
	cohomologyMatrix(cT,low,high)
     Text
        The corner complex is built from a first quadrant
	complex fqT and a last quadrant complex lqT
	connected by the corner map between these complexes.
     Example
    	fqT=firstQuadrantComplex(T,c);
	lqT=lastQuadrantComplex(T,c);
	cohomologyMatrix(fqT,low,high)
	cohomologyMatrix(lqT,low,high)
     	betti fqT
        betti lqT
	betti cT
     Text
        Here the corner map is cT.dd_1
     Example
        betti (cT.dd_1)
     Text
        In general the corner map is a chain complex map
	from lqT to fqT spread over several homological degrees.
	
	Next we give an example obtained from a module
     Example
       (S,E)=productOfProjectiveSpaces{2,1}
       M=beilinson(E^{-{1,1}})
       c={1,1}
       low={-3,-3},high={4,4}
       cohomologyMatrix(M,low,high)
       C=cornerComplex(M,c,low,high)
       cohomologyMatrix(C,low,high)
       cohomologyMatrix(C,2*low,2*high)
       betti C
       C.dd_(-sum c +1)  
  SeeAlso
    upperCorner
    lowerCorner
    firstQuadrantComplex
    lastQuadrantComplex
    cohomologyMatrix
    beilinson
///

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
        time T=tateExtension W;
        cohomologyMatrix(T,-{3,3},{3,3})
	W=beilinsonWindow T
	cohomologyMatrix(W,-{2,2},{2,2})
        a={2,-3}
        W2=beilinsonWindow (T**E^{a}[sum a])
        cohomologyMatrix(W2,-{2,2},{2,2})
        cohomologyMatrix(tateExtension W2,-{2,2},{2,2})
  SeeAlso
    beilinsonWindow
    cohomologyMatrix
///

doc ///
  Key
    tateExtension
    (tateExtension,ChainComplex)
  Headline
    extend the terms in the Beilinson window to a part of a corner complex of the corresponding Tate resolution
  Usage
    T=tateExtension W
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
       computes part of a suitable chosen corner complex of the Tate resolution T(F).

     Example
        n={1,1};
        (S,E) = productOfProjectiveSpaces n;
	T1 = (dual res trim (ideal vars E)^2)[1];
	a=-{2,2};
	T2=T1**E^{a}[sum a];
	W=beilinsonWindow T2
	cohomologyMatrix(W,-2*n,2*n)
        T=tateExtension W
	cohomologyMatrix(T,-3*n,4*n)
	cohomologyMatrix(beilinsonWindow T,-n,n)
	cohomologyMatrix(T,-5*n,4*n) -- the view including the corner
  Caveat
     Note that the Beilinson window of tateExtension of the beilinson window W is not equal but just
     isomorphic to the original W.

     The implicit bounds in the computation are only a guess and certainly not optimal. This should be improved.
  SeeAlso
     cohomologyMatrix
     beilinsonWindow
///
-*
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
       that computes part of a suitable chosen corner complex of the Tate resolution T(F).
     Example
        n={1,1};
        (S,E) = productOfProjectiveSpaces n;
	T1 = (dual res trim (ideal vars E)^2)[1];
    	isChainComplex T1
	a=-{2,2};
	T2=T1**E^{a}[sum a];
	W=beilinsonWindow T2
	cohomologyMatrix(W,-2*n,2*n)
        T=tateExtension W;
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
*-

doc ///
  Key
    beilinson
    (beilinson,Module)
    (beilinson,Matrix)
    (beilinson,ChainComplex)
    [beilinson,BundleType]
  Headline
    apply the beilinson functor
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
       the possible values are described in BundleType
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
       sheafifications are the corresponding sheaves. The corresponding graded S-module are chosen as quotients of
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
	betti T
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
    [beilinsonContraction,BundleType]
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
    Option in beilinson with values PrunedQuotient, QuotientBundle, DummyQuotientBundle, SubBundle, FreeBundle, or MapsBetweenFreeBundles
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
      The Beilinson bundlse U^a will be represented by subbundles.
  SeeAlso
    beilinson
    BundleType
    PrunedQuotient
    QuotientBundle
    DummyQuotientBundle
    FreeBundle
    MapsBetweenFreeBundles
///

doc ///
  Key
    PrunedQuotient
  Headline
    value for the option BundleType in beilinson
  Description
     Text
      The Beilinson bundles U^a will be represented by quotient bundles. On a single component P^n, 
      U^0 is represented as S^1 (S is the homogeneous coordinate ring for P^n).
  SeeAlso
    beilinson
    BundleType
    QuotientBundle
    DummyQuotientBundle
    SubBundle
    FreeBundle
    MapsBetweenFreeBundles
///

doc ///
   Key
    QuotientBundle
   Headline
    value for the option BundleType in beilinson
   Description
      Text
       The Beilinson bundles U^a will be represented by quotient bundles.
   SeeAlso
    beilinson
    BundleType
    PrunedQuotient
    DummyQuotientBundle
    SubBundle
    FreeBundle
    MapsBetweenFreeBundles
///

doc ///
   Key
    DummyQuotientBundle
   Headline
    value for the option BundleType in beilinson
   Description
      Text
       The Beilinson bundles U^a will be represented by quotient bundles. On a single component P^n,
       U^{-1} (indeed, it is the zero sheaf) is represented as the cokernel of the Koszul map \oplus^{n+1} S(-2)->S(-1). 
   SeeAlso
    beilinson
    BundleType
    PrunedQuotient
    QuotientBundle
    SubBundle
    FreeBundle
    MapsBetweenFreeBundles
///

doc ///
   Key
    FreeBundle
   Headline
    value for the option BundleType in beilinson
   Description
      Text
       This option computes the natural free bundles which contains the Beilinson bundles U^a as subbundles.
   SeeAlso
    beilinson
    BundleType
    PrunedQuotient
    QuotientBundle
    DummyQuotientBundle
    SubBundle
    MapsBetweenFreeBundles
///

doc ///
   Key
    MapsBetweenFreeBundles
   Headline
    value for the option BundleType in beilinson
   Description
      Text
       This option computes the maps from natural free bundles which generates the Beilinson bundles U^a 
       to natural free bundles which contains the Beilinson bundles U^a as subbundles. 
       In other words, this option provides a map between free modules \wedge^{a+1}W\otimes S(-1)\to \wedge^{a}W\otimes S which
       factors through U^a where W=S_1. In particular, it represents a natural embedding of U^a into the corresponding free bundle.  
   SeeAlso
    beilinson
    BundleType
    PrunedQuotient
    QuotientBundle
    DummyQuotientBundle
    SubBundle
    FreeBundle
///

---------------------------
-- composed functions
---------------------------

doc ///
  Key
    directImageComplex
    (directImageComplex,Module,List)
    (directImageComplex,Ideal,Module,Matrix)
  Headline
    compute the direct image complex 
  Usage
    RpiM = directImageComplex(M,I)
    RphiN = directImageComplex(J,N,phi)
  Inputs
    M: Module
       representing a sheaf F on a product of projective spaces
    I: List
      corresponding to the factors to which pi projects
    J: Ideal
      the saturated ideal of a projective scheme X in some P^n
    N: Module
      representing a sheaf on X
    phi: Matrix
      a kx(m+1) matrix of homogeneous polynomials on P^n
      which define a morphism or rational map phi:X -> P^m,
      i.e. the 2x2 minors of phi vanish on X.      
    BundleType => Symbol
       the possible values are described in BundleType
  Outputs
    RpiM: ChainComplex
       a chain complex of modules over a symmetric algebra
    RphiN: ChainComplex
       a chain complex of modules over the coordinate ring of P^m
  Description
     Text
       Let M represent a coherent sheaf F on a product P=P^{n_0}x..xP^{n_{t-1}} 
       of t projective space. 
       
       Let $pi: P -> P^I= X_{i \in I} P^{n_i}$ denote the projection onto some factors. 
       We compute a chain complex of S_I modules whose
       sheafication is $Rpi_* F$. 
       
       The algorithm is based on the properties of strands,
       and the beilinson functor on $P^I$, see       
       @ HREF("http://arxiv.org/abs/1411.5724","Tate Resolutions on Products of Projective Spaces") @.
       Note that the resulting complex is a chain complex instead of a cochain complex,
       so that for example HH^1 RpiM is the module representing $R^1 pi_* F$
       
             In the second version we start with a projective scheme X =Proj(R/J) defined by J in some 
       P^n= Proj R with R \cong K[x_0..x_n] a polynomial ring,
       an  R-module N of representing a sheaf on X, and a matrix phi of homogeneous
       forms who's rows define a morphism phi: X -> P^m. In particular
       the 2x2 minors of phi vanish on X, and phi defines a morphism if and only if
       the entries of phi have no common zero in X.
       The algorithm passes to the graph of phi in P^n x P^m, and calls the first version
       of this function.
       
       Here is an example of the first kind.
     Example
       t=2
       n={1,2}
       (S,E)=productOfProjectiveSpaces{1,2}
       M=(beilinson E^{{-1,-1}})**S^{{-2,-1}}
     Text
       We compute the direct image complex of M by projecting to 
       the second factor P^2.
     Example
       I={1}
       J=select({0,t-1},i-> not member(i,I))
       RpiM=directImageComplex(M,I)
       betti RpiM
       prune HH_0 RpiM
       prune HH^1 RpiM 
       prune HH^2 RpiM
       dim HH^2 RpiM
     Text
       HH_{-2} RpiM is artinian, hence its sheafication is zero.
       Thus the direct image complex in this case is concentrated in 
       the single sheaf
       $Rpi_* F = R^1pi_* F$
     Example
       cohomologyMatrix(M,-3*n,3*n)
       T=tateResolution(M,-2*n,2*n);
       cohomologyMatrix(strand(T,{0,0},J),-2*n,2*n)
     Text
       As an example of the second version, we consider the ruled cubic surface scroll
       X subset P^4 defined by the 2x2 minors of the matrix
       $$ m= matrix \{ \{x_0,x_1,x_3\},\{x_1,x_2,x_4\} \},$$
       and the morphism f: X -> P^1 onto the base.
       f is defined by ratio of the two rows of m, hence by the 3x2 matrix phi=m^t.
       
       As a module N we take a symmetric power of the cokernel m, twisted by R^{\{d\}}.
     Example
       kk=ZZ/101    
       R=kk[x_0..x_4]
       m=matrix {{ x_0,x_1,x_3},{x_1,x_2,x_4}}
       J=minors(2,m)
       dim J, degree J
       s=2,d=-2
       N=symmetricPower(s,coker m)**R^{d}; 
       betti res N
       annihilator N == J
       phi= transpose m
       RphiN = directImageComplex(J,N,phi)
       T= ring RphiN
       HH^1 RphiN
     Text
       Now a different symmetric power and a different twist.
     Example
       s=3,d=1
       N=symmetricPower(s,coker m)**R^{d};
       RphiN = directImageComplex(J,N,phi)
       T=ring RphiN
       netList apply(toList(min RphiN.. max RphiN),i-> 
	   {-i, saturate annihilator HH^(-i) RphiN,betti res HH^(-i) RphiN})
       R0=prune HH^0 RphiN
       dim R0, degree R0
       betti (sR0Dual = syz transpose presentation R0)
       saturate annihilator coker transpose sR0Dual
       dual source sR0Dual       
     Text
       We conclude that the sheaf represented by R0 is O(5)+O(4) on P^1, which is correct
       because N represents phi^*O(1) and phi_* O_X(H) = O(2)+O(1). 
  Caveat
     Note that the resulting complex is a chain complex instead of a cochain complex,
     so that for example HH^i RpiM = HH_{-i} RpiM.

  SeeAlso
     cohomologyMatrix
     tateResolution
     strand
     beilinson
///

doc ///
 Key
  actionOnDirectImage
  (actionOnDirectImage,Ideal,Module)
  (actionOnDirectImage,Ideal,Module,Matrix)
  (actionOnDirectImage,Ideal,ChainComplex)
 Headline
  recover the module structure via a Noether normalization
 Usage
  retTable = actionOnDirectImage(I,M)
  retTable = actionOnDirectImage(J,N,phi)
  retTable = actionOnDirectImage(I,T)
 Inputs
  I: Ideal
      the saturated ideal of a projective variety Y (of dimension r) in some P^m
  M: Module
      representing a sheaf G on Y
  J: Ideal
      the saturated ideal of a projective variety X in some P^n
  N: Module
      representing a sheaf F on X
  phi: Matrix
      a k\times (m+1) matrix of homogeneous polynomials on P^n which define a morphism or rational map 
      phi:X\to P^m, i.e., the 2\times 2 minors of phi vanish on X
  T: ChainComplex
      a (long enough) part of the Tate resolution of some complex of sheaves on Y
 Outputs
  retTable: HashTable
      whose keys are indices of cohomology groups R^i which survive,
      and the entry for each key is the list of (m+1) maps of chain complexes
 Description
  Text
      This method provides another representation of the direct image complex. 
      
      Let M represent a coherent sheaf G on Y, and let \pi:Y\to P^r be a Noether normalization of Y.
      Note that \pi is chosen among finite linear projections P^m\to P^r from certain coordinate planes.
      Each coordinate y_i of P^m gives a multiplication map G\to G(1), and its induced map
      \pi_{*}G\to (\pi_{*}G)(1). Note that these induced maps provide an O_Y-module structure 
      on \pi_{*}G, in other words, we may recover the O_Y-module F from \pi_{*}G and this action.
      
      If no map is specified, it computes the complex C on P^r and a list of induced maps 
      between chain complexes C\to C(1) on P^r associated to the multiplication by y_0,...,y_m,
      where C represents the Beilinson monad of \pi_{*}G (or R\pi_{*}U(T)).
      
      If a map is specified by a matrix phi, then it computes the complex C on P^r and a list of induced maps 
      between chain complexes C\to C(1) on P^r associated to the multiplication by y_0,...,y_m,
      where C represents the Beilinson monad of R(\pi \cdot phi)_{*}F.
      
      When n is quite big compared to r, it is not very efficient to deal with Beilinson bundles on P^n since they have 
      huge rank and presentation matrices. In particular, the method directImageComplex becomes slow down.
      
      The following is an example of direct images of the structure sheaf on a rational normal curve of degree $d$.
  Example
      kk=ZZ/101; d=4;
      needsPackage "Resultants";
      vd=veronese(1,d,kk);
      R=target vd; S=source vd;
      
      M=R^{1:-1}; I=ann M; J=ker vd;
      
      RM=directImageComplex(I,M,matrix vd);
      
      for i from min RM to max RM list (rank RM_i)
  Text
      RM looks complicated since it is consisted of universal bundles on $P^4$, which are of high rank.
  Example
      retTable=actionOnDirectImage(I,M,matrix vd);
      keys retTable
  Text
      We see that 0 is the only key, in other words, there is no other R^i vd_{*} except i=0.
      To see whether it gives an action on S/J, we can use the test function isAction.

      Note that list retTable#i is consisted of maps of chain complexes R^i(\pi \cdot phi)_{*}(y_j) : C\to C(1) where 
      C represents the direct image R^i(\pi \cdot phi)_{*}F. In general, it does not give a right action on C itself. 
      The induced maps on cohomology groups provide a right action.
  Example
      isAction(J, apply(dim S, i->prune HH^0 retTable#0#i))
  Text
      The following is a little more complicated example with nontrivial higher direct images. 
      Let X be the product of two quartic curves C, and f : X \to C be the second projection. 
      Let P, Q be two distinct points of C, and let L = O(P\times C + Q\times C + D) be a line bundle 
      on X where D is the diagonal. We want to compute the higher direct images R^i f_{*}L. 
      We choose C as the Fermat quartic, and choose P, Q as points on the intersection of C and the line V(x_2).
  Example
      kk=ZZ/1009;
      (S,E)=productOfProjectiveSpaces({2,2},CoefficientField=>kk);
      f1=S_0^4+S_1^4+S_2^4; f2=S_3^4+S_4^4+S_5^4;
      IX=ideal (f1,f2);
      -- 2 points on C. Note that the line passing through P and Q is V(x_2).
      P=matrix{{1,192,0}}; Q=matrix{{192,1,0}};
      varX=matrix{{S_0,S_1,S_2}}; varY=matrix{{S_3,S_4,S_5}};
      PX=P||varX; QX=Q||varX;
      
      I1=ideal(f2, minors(2,PX)); -- ideal for P*C
      I2=ideal(f2, minors(2,QX)); -- ideal for Q*C
      I3=ideal(f1,f2, minors(2, varX||varY)); -- ideal for the diagonal D
      I=intersect(I1,I2,I3); -- ideal for P*C+Q*C+D
      SX=S/IX; Ldual=I/IX;
      
      betti (LX=Hom(Ldual, SX^1))
  Text
      To compute its Tate resolution on the ambient space P^2 \times P^2, we first consider it as a sheaf on P^2 \times P^2,
      and then take a linear presentation matrix via a truncation.
  Example
      phi = map(SX,S,vars SX);
      betti (L=prune pushForward(phi,LX))
      Ltr = (truncate ({2,2},L))**S^{{2,2}};
      betti res Ltr
  Text
      We read off (a finite subquotient of) the Tate resolution of Rf_{*}L as follows.
  Example
      Q=symExt(presentation Ltr, E);
      T=(res (coker Q,LengthLimit=>12))**E^{{2,2}}[4];
      cohomologyMatrix (T, -{5,5},{3,3})
      sT=strand(T,{0,0},{0});

      sTFull=new ChainComplex;
      sTFull.ring = ring sT;
      ma=6;
      sTFull=(dual res (coker transpose (sT[ma]).dd_0, LengthLimit=>2*ma))[-ma];
      (S',E')=productOfProjectiveSpaces({2},CoefficientField=>kk);
      projOnE=map(E', E, toList(3:0)|(gens E'));
      mi=min sTFull; ma=max sTFull;
      W=new ChainComplex; W.ring = E';
      apply(toList(mi..ma),i-> W_i = E'^(-apply(degrees sTFull_i,d->d_{1})));
      apply(toList(mi+1..ma),i->W.dd_i = map(W_(i-1),W_i,projOnE sTFull.dd_i));
      betti W
  Text
      One can check that W has two strands (corresponding to R^0f_{*}L and R^1f_{*}L, respectively).
      By taking the Beilinson functor, one can check that R^0f_{*}L is the structure sheaf on C,
      and R^1f_{*}L is a torsion sheaf supported on two points lying on the intersection of C and
      the line V(x_2) other than P, Q.
  Example
    R0fL = prune HH^0 beilinson W
    R1fL = prune HH^1 beilinson W
    
    degree R0fL
    primaryDecomposition ann R0fL
    
    degree R1fL
    primaryDecomposition ann R1fL
  Text
      These module also can be seen as in the following way via a finite linear projection. 
      We take a further projection \pi:C\to  P^1, and check whether these modules induce an action
      on the direct image under \pi, in other words, provide {O_C}-module structures. As results,
      these actions make (the sheafification of) M0 and M1 into {O_C}-modules which are identical to 
      R^0f_{*}L and R^1f_{*}L. 
  Example
      J=ideal (S'_0^4+S'_1^4+S'_2^4);
      retTable=actionOnDirectImage(J,W);
      keys retTable
  Text
      We see that 0, 1 appear as keys, in other words, both R^0f_{*}L and R^1f_{*}L survives.
  Example
      prunedActionList = i->apply(dim S',j->prune HH^i retTable#i#j);
      apply(keys retTable, i->isAction(J,prunedActionList(i)))
	        
      M0=source (prunedActionList(0))_0
      (rank M0, degree M0, betti res M0)
      isIsomorphic(truncate(regularity M0, M0), truncate(regularity M0, dual dual M0))
      dual dual M0

      M1=source (prunedActionList(1))_0
      (rank M1, degree M1, betti res M1)
  Text
      Note that the sheafification of M0 (=R^0(\pi \cdot f)_{*}L) is a rank 4 vector bundle O \oplus O(-1) \oplus O(-2) \oplus O(-3) on P^1,
      and the sheafification of M1 (= R^1(\pi \cdot f)_{*} L) is a torsion sheaf on P^1 supported on the double point at [1:0].
      Together with the induced action on S', they have an O_C-module structure as desired.  
      
 Caveat
     Note that the resulting complex is a chain complex instead of a cochain complex,
     so that for example HH^i RpiM = HH_{-i} RpiM. Also note that this requires a pseudo-inverse computation
     of a split exact sequence, which might fail over finite fields (see SVDComplexes.m2 and its documentations).
 SeeAlso
      directImageComplex
///

doc ///
    Key
    	isAction
	(isAction,Ideal,List)
    Headline
    	test whether a list of square matrices induces an action
    Usage
    	isAction(I,actionList)
    Inputs
    	I: Ideal
	    the saturated ideal of a projective variety X in P^N
	actionList: List
	    the list \{X_0,...,X_N\} \, of maps M\to M(1) for some module M
    Outputs
    	v: Boolean
    Description
    	Text	    
	    Let S be the homogeneous coordinate ring of P^N, and x_0,...,x_N be the coordinates. 
	    Let \pi:X\to P^n be a Noether normalization. Note that giving a coherent sheaf F on X is 
	    equivalent to giving a sheaf G (=\pi_{*}F) on P^n together with multiplication maps 
	    X_i (=\pi_{*} (\cdot x_i)) : G\to G(1) such that X_i X_j = X_j X_i for every i, j, and 
	    f(X_0, ..., X_n)=0 for every f \in I. In other words, \{X_0,...,X_N\} \, gives an action which makes 
	    G into an O_X-module. 
	    
	    This method checks first that actionList is composed of commuting matrices, and then
	    checks whether f(X_0,...,X_n)=0 for each generator f of I.
	    
	    The following is an example when C is a conic, F=O_C, and \pi\, is a linear projection 
	    at the coordinate point [0:0:1]. In the case, the pushforward \pi_{*}F = O_{P^1} \oplus O_{P^1}(-1).
	Example
	    S=QQ[x_0..x_2]; R=QQ[y_0,y_1];
	    I=ideal(x_0*x_1-x_2^2);
	    M=R^{{1:0},{1:-1}};
	    X0=map(M**R^{1},M,{{y_0,0},{0,y_0}})
	    X1=map(M**R^{1},M,{{y_1,0},{0,y_1}})
	    X2=map(M**R^{1},M,{{0,y_0*y_1},{1,0}})
	    isAction(I,{X0,X1,X2})
	    
///

-*
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

*-

-*
     doc ///
        Key
	 resolutionOfChainComplex
	 (resolutionOfChainComplex, ChainComplex)
	 [resolutionOfChainComplex,LengthLimit]
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

*-

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

-*
document {
     Key => {chainComplexMap, (chainComplexMap,ChainComplex,ChainComplex,List),
     [chainComplexMap,InitialDegree]},
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
*-

-*
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
*-

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
///


doc ///
   Key
    bgg
    (bgg, Module)
    [bgg,LengthLimit]
   Headline
    make a linear free complex from a module over an exterior algebra or a symmetric algebra
   Usage
    LP = bgg P
    RM = bgg(M,LengthLimit=>4)
   Inputs
    P: Module
     module over an exterior algebra E
    M: Module
      module over an symmetric algebra S
   Outputs
    LP:ChainComplex
     over a symmetric algebra
    RM:ChainComplex
     over a exterior algebra
   Description
    Text
     If P is an E-module, then LP becomes a linear complex of free S-modules,
     where (S,E) is the Koszul pair corresponding to a
     product of projective spaces.
     Similarly, if M is an S-module, them RM becomes a linear free complex over
     the exterior algebra E of length bounded by the LengthLimit.

     The complex LP is that produced from P by the
     Bernstein-Gel'fand-Gel'fand functor called L in
     our paper
     @ HREF("http://arxiv.org/abs/","Tate Resolutions on Products of Projective Spaces") @.
     Similarly, the complex RM produced from M is a bounded piece of the infinite complex of
     the Bernstein-Gel'fand-Gel'fand
     functor called R in loc.cit. L and R form a pair of adjoint
     functors.
    Example
     (S,E) = productOfProjectiveSpaces{1,2}
     P = prune truncate({1,2},E^1)**E^{{1,2}};
     LP = bgg P
     netList apply(toList(min LP..max LP), i-> decompose ann HH_i LP)
     M = prune HH_0 LP
     betti res M
     high = {3,3}
     cohomologyMatrix(M, -high, high)
    Example
     M=module ideal vars S
     RM = bgg(M,LengthLimit=>3)
     betti RM
     tallyDegrees RM
   SeeAlso
    productOfProjectiveSpaces
    tallyDegrees
///


doc ///
   Key
    contractionData
    (contractionData, List, List, Ring)
    [contractionData,BundleType]
   Headline
    Compute the action of monomials in the exterior algebra on the Beilinson monad
   Usage
    contractionData(a, b, E)
   Inputs
    a:List
     row degrees
    b:List
     degrees
    E: Ring
     exterior algebra
   Outputs
    :List
     maps from U^a to U^b
   Caveat
    Mike will finish this some day
///

------------------------------------
-----TESTS-----
------------------------------------
TEST ///
S = ZZ/32003[a,b, Degrees =>{{1,0},{0,1}}]
M = S^{{1,0},{0,1}}
M' = S^{{0,1},{1,0}}
A = S^{{1,-1}}
B = S^1
assert(isIsomorphic(M,M') ===true)
assert(isIsomorphic(A,B) ===false)
///

TEST ///
(S,E) = productOfProjectiveSpaces{1,2}
P = prune truncate({1,2},E^1)
L = bgg P
assert( (betti L) === new BettiTally from {(-3,{-1,-2},-3) => 6, (-5,{-2,-3},-5) => 1, (-4,{-2,-2},-4)
      --------------------------------------------------------------------------------------------------------
      => 3, (-4,{-1,-3},-4) => 2} );
assert all(min L +1..max L, i-> L.dd_(i-1)*L.dd_i == 0)
--assert( (prune HH_(-3) L) === cokernel map((S)^{{0,1},{0,1},{0,1}},(S)^1,{{x_(1,0)}, {-x_(1,1)},
--       {-x_(1,2)}}) );
///

TEST ///
(S,E) = productOfProjectiveSpaces{1,1};
C = tateResolution (S^1,{0,0},{3,3});
assert (cohomRing = ZZ[h,k];
    (sub (cohomologyMatrix (C, {0,0},{3,3}), cohomRing) ===
	map(cohomRing^4,cohomRing^4,{{4, 8, 12, 16}, {3, 6, 9, 12}, {2, 4, 6, 8}, {1, 2, 3, 4}})
	))
///

TEST ///
(S,E) = productOfProjectiveSpaces{1,2}
M = S^{{-1,2}}
high = {3,3}
C = tateResolution(M,-high,high)
betti C
BW = beilinsonWindow C
B = beilinson C
netList toList tallyDegrees B
cohomologyMatrix(M, -high,high)
cohomologyMatrix(BW, -high,high)
netList apply(toList(min B..max B), i-> ann HH_(i) B)
M' = HH_0 B
assert(beilinsonWindow tateResolution(M',-high,high) == BW)
///

TEST ///
(S,E) = productOfProjectiveSpaces{1,2}
M = coker random(S^2, S^{2:{-1,-1}})
high = {3,3}
C = tateResolution(M,-high,high);
BW = beilinsonWindow C
betti BW
B = beilinson C
M' = HH_0 B
assert isIsomorphic(M',M)
--note: isomorphic, not equal!
BW' = beilinsonWindow tateResolution(M',-high,high)
assert( all(2, i->BW_i == BW'_i))
assert(isIsomorphic(coker BW.dd_1, coker BW'.dd_1))
///

TEST ///
(S,E) = productOfProjectiveSpaces{1,2}
M = coker random(S^2, S^{2:{-1,-1}})
high = {3,3}
C = tateResolution(M,-high,high);
B = beilinson C
M' = HH_0 B
assert isIsomorphic(M',M)
--note: isomorphic, not equal!
///

TEST ///
  -- XXX Mike working on this test
  -- of beilinson functor
-*
  restart
  needsPackage "TateOnProducts"
*-
  (S,E) = productOfProjectiveSpaces{1,2}
  assert(beilinson(E^1) == S^1)
  U1 = beilinson(E^{{-1,0}})
  V1 = beilinson(E^{{0,-1}})
  V2 = beilinson(E^{{0,-2}})
  assert(V2 == S^{{0,-1}})
  assert(beilinson(E^{{-1,-1}}) == U1 ** V1)
  assert(beilinson(E^{{-1,-2}}) == U1 ** V2)
  assert(beilinson(E^{{-2,0}}) == 0)

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

  debug TateOnProducts -- for inBeilinsonWindow
  degs = flatten for a from -3 to 3 list for b from -3 to 3 list {a,b}
  for d in degs do (
      assert(inBeilinsonWindow(d, E) or beilinson(E^{-d}) == 0)
      )
///

TEST ///
  -- test of beilinson
-*
  restart
  needsPackage "TateOnProducts"
*-
  (S,E) = productOfProjectiveSpaces{3,3}
  assert(beilinson(E^1) == S^1)
  U1 = beilinson(E^{{-1,0}})
  V1 = beilinson(E^{{0,-1}})
  V2 = beilinson(E^{{0,-2}})
  assert(  V2 == beilinsonBundle({0,2},S))
  assert(beilinson(E^{{-1,-1}}) == U1 ** V1)
  assert(beilinson(E^{{-1,-2}}) == U1 ** V2)
  assert(beilinson(E^{{-2,1}}) == 0)

  debug TateOnProducts -- for inBeilinsonWindow
  degs = flatten for a from -3 to 3 list for b from -3 to 3 list {a,b}
  for d in degs do (
      assert(inBeilinsonWindow(d, E) or beilinson(E^{-d}) == 0)
      )
///


TEST ///

  -- tests of beilinson functoriality
  --the commented tests worked but were slow (>.5sec) on June 7, 2018 in Leipzig.
-*
  restart
*-
  debug needsPackage "TateOnProducts"
elapsedTime  testBeilinson({1,2}, BundleType=>PrunedQuotient)
elapsedTime   testBeilinson({1}, BundleType=>PrunedQuotient)
elapsedTime   testBeilinson({4}, BundleType=>PrunedQuotient)
--elapsedTime   testBeilinson({1,1,1,1}, BundleType=>PrunedQuotient)
--elapsedTime   testBeilinson({1,1,2,1}, BundleType=>PrunedQuotient)
elapsedTime   testBeilinson({2,2}, BundleType=>PrunedQuotient)
--elapsedTime   testBeilinson({1,2,3}, BundleType=>PrunedQuotient)
--elapsedTime   testBeilinson({2,2,2}, BundleType=>PrunedQuotient)
--elapsedTime   testBeilinson({3,3}, BundleType=>PrunedQuotient)
--elapsedTime   testBeilinson({3,4}, BundleType=>PrunedQuotient)

elapsedTime   testBeilinson({1,2}, BundleType=>QuotientBundle)
elapsedTime   testBeilinson({1}, BundleType=>QuotientBundle)
elapsedTime   testBeilinson({4}, BundleType=>QuotientBundle)
--elapsedTime   testBeilinson({1,1,1,1}, BundleType=>QuotientBundle)
--elapsedTime   testBeilinson({1,1,2,1}, BundleType=>QuotientBundle)
elapsedTime   testBeilinson({2,2}, BundleType=>QuotientBundle)
--elapsedTime   testBeilinson({1,2,3}, BundleType=>QuotientBundle)
--elapsedTime   testBeilinson({2,2,2}, BundleType=>QuotientBundle)
--elapsedTime   testBeilinson({3,3}, BundleType=>QuotientBundle)
--elapsedTime   testBeilinson({3,4}, BundleType=>QuotientBundle)

elapsedTime   testBeilinson1({1,2}, BundleType=>PrunedQuotient)
elapsedTime   testBeilinson1({1}, BundleType=>PrunedQuotient)
elapsedTime   testBeilinson1({4}, BundleType=>PrunedQuotient)
--elapsedTime   testBeilinson1({1,1,1,1}, BundleType=>PrunedQuotient)
--elapsedTime   testBeilinson1({1,1,2,1}, BundleType=>PrunedQuotient)
elapsedTime   testBeilinson1({2,2}, BundleType=>PrunedQuotient)
--elapsedTime   testBeilinson1({1,2,3}, BundleType=>PrunedQuotient)
--elapsedTime   testBeilinson1({2,2,2}, BundleType=>PrunedQuotient)
--elapsedTime   testBeilinson1({3,3}, BundleType=>PrunedQuotient)
--elapsedTime   testBeilinson1({3,4}, BundleType=>PrunedQuotient)

elapsedTime   testBeilinson1({1,2}, BundleType=>QuotientBundle)
elapsedTime   testBeilinson1({1}, BundleType=>QuotientBundle)
elapsedTime   testBeilinson1({4}, BundleType=>QuotientBundle)
--elapsedTime   testBeilinson1({1,1,1,1}, BundleType=>QuotientBundle)
--elapsedTime   testBeilinson1({1,1,2,1}, BundleType=>QuotientBundle)
elapsedTime   testBeilinson1({2,2}, BundleType=>QuotientBundle)
--elapsedTime   testBeilinson1({1,2,3}, BundleType=>QuotientBundle)
--elapsedTime   testBeilinson1({2,2,2}, BundleType=>QuotientBundle)
--elapsedTime   testBeilinson1({3,3}, BundleType=>QuotientBundle)
--elapsedTime   testBeilinson1({3,4}, BundleType=>QuotientBundle)

elapsedTime   testBeilinson {1,1}
elapsedTime   testBeilinson {1,2}
elapsedTime   testBeilinson {2,1}
elapsedTime   testBeilinson {3,1}
elapsedTime   testBeilinson {2,2}
elapsedTime   testBeilinson {1,3}
elapsedTime   testBeilinson {4,1}
elapsedTime   testBeilinson {3,2}
elapsedTime   testBeilinson {2,3}
elapsedTime   testBeilinson {1,4}
elapsedTime   testBeilinson {1}
elapsedTime   testBeilinson {2}
elapsedTime   testBeilinson {3}
elapsedTime   testBeilinson {4}
elapsedTime   testBeilinson {5}
elapsedTime   testBeilinson {6}
--elapsedTime   testBeilinson {5,3}
--elapsedTime   testBeilinson {1,3,1,1}
--elapsedTime   testBeilinson {1,3,1,2}

elapsedTime   testBeilinson1 {1,1}
elapsedTime   testBeilinson1 {1,2}
elapsedTime   testBeilinson1 {2,1}
elapsedTime   testBeilinson1 {3,1}
--elapsedTime   testBeilinson1 {2,2}
--elapsedTime   testBeilinson1 {1,3}
--elapsedTime   testBeilinson1 {4,1}
--elapsedTime   testBeilinson1 {3,2}
--elapsedTime   testBeilinson1 {2,3}
--elapsedTime   testBeilinson1 {1,4}
elapsedTime   testBeilinson1 {1}
elapsedTime   testBeilinson1 {2}
elapsedTime   testBeilinson1 {3}
elapsedTime   testBeilinson1 {4}
--elapsedTime   testBeilinson1 {5}
--elapsedTime   testBeilinson1 {6}

elapsedTime   testBeilinson1 {1,1,1}
--elapsedTime   testBeilinson1 {1,1,2}
--elapsedTime   testBeilinson1 {1,2,1}
--elapsedTime   testBeilinson1 {2,1,1}
--elapsedTime   testBeilinson1 {3,1,1}

--elapsedTime   testBeilinson1 {1,2,2}
--elapsedTime   testBeilinson1 {2,1,2}
--elapsedTime   testBeilinson1 {2,2,1}

--elapsedTime   testBeilinson1 {1,1,1,1}
///


 ------------Tests that aren't necessarily tests yet:



TEST ///
-*
restart
needsPackage "TateOnProducts"

error"the test below works, but we don't understand the
correspondence of positions in the cohomology Matrix and the tally"
*-
  n={1,2}
  (S,E) = productOfProjectiveSpaces n
  m = matrix{{x_(0,0),x_(1,0)},
         {x_(0,1),0},
         {0,x_(1,1)},
         {0,x_(1,2)}}
  mE = symExt(m,E)
  betti(T = res coker mE)
  TD = tallyDegrees T;
  CD = cohomologyHashTable(T, -{2,2},{1,1});
  assert((TD_0)#{-1,0} == CD#{{1,0},-1})
///

----The next two tests were commented out, along with the "corner"
--scripts
-*
restart



*-
TEST /// 
--error"we don't know what this should be testing. Note that 'corner'
--no longer exists"

debug needsPackage "TateOnProducts"
n={1,2}
(S,E) = productOfProjectiveSpaces n
F=dual (res((ker transpose vars E)**E^{{ 2,3}},LengthLimit=>10))
cohomologyMatrix(F,-2*n,2*n)
tallyDegrees F

deg = {2,1}
m = upperCorner(F,deg)
betti m
tally degrees source m, tally degrees target m
Fm=(res(coker m,LengthLimit=>10))[sum deg]
betti Fm
betti F
cohomologyMatrix(Fm,deg-{5,5},deg+{1,1})
///

TEST ///
--error"we don't know what this should be testing. Note that 'corner'
--no longer exists"

debug needsPackage "TateOnProducts"
n={1,1}
(S,E) = productOfProjectiveSpaces n

time fB=dual res(coker random(E^7,E^{13:{ -1,0},11:{0,-1}}),LengthLimit=>10);
cohomologyMatrix(fB,-{1,1},{5,5})
deg={3,3}
m= upperCorner(fB,deg);
f= res( coker  m,LengthLimit=> 10)[6]
tallyDegrees f
cohomologyMatrix(f,-{3,3},{5,5})
C= cornerComplex(f,{1,1});
cohomologyMatrix(C,-{3,3},{5,5})

///

///
restart
loadPackage ("TateOnProducts", Reload =>true)
///
TEST ///
n={1,2}; (S,E) = productOfProjectiveSpaces n;
M = S^1;
low = {-3,-3};high = {3,3};
H = cohomologyHashTable(M, low,high);
pH = pairs eulerPolynomialTable (M, low, high);
pH' = pairs eulerPolynomialTable H;
CR = ring pH_0_1;
assert(pH == apply(pH', p -> (p_0,sub(p_1,CR))))
///



TEST ///
n={4}
(S,E) = productOfProjectiveSpaces n
C=res ideal vars E
C1=C**E^{{ +1}}[0]
W=beilinsonWindow C1
scan(min W+1 ..max W,k->assert(W.dd_k==C1.dd_k))
///

TEST ///
debug needsPackage "TateOnProducts"
S=ZZ[x,y]/ideal(x*y)
C=(chainComplex(matrix{{x}},matrix{{y^2}},matrix{{x^2}}))[3]
L=chainComplexData C
C'=chainComplexFromData L
assert(C'== C)
///


///
-- Frank: I removed this test many because I do not understand it
--restart
--loadPackage( "TateOnProducts", Reload=>true)
   n={2,1};
  (S,E) = productOfProjectiveSpaces n;
  T1 = (dual res trim (ideal vars E)^2 [1]);
--  cohomologyMatrix(T1,-3*n,3*n)
--  beilinson  beilinsonWindow T1
  --beilinson T1
--  beilinson(T1, BundleType=>QuotientBundle)
  T2 = res(coker lowerCorner(T1, {2,2}), LengthLimit=>10)[4];
--  cohomologyMatrix(T2,-3*n,3*n)
--  BW2 = beilinsonWindow T2
--  cohomologyMatrix(BW2, -5*n,5*n)
--  B2 = beilinson T2
  B2 = beilinson(T2, BundleType=>QuotientBundle);
  F2 = (prune HH B2)_0;
  
  -- now another shift
--  BW3 = beilinsonWindow ((T2 ** E^{{-2,-2}})[-4])
--  B3 = beilinson ((T2 ** E^{{-2,-2}})[-4])
  B3 = beilinson( ((T2 ** E^{{-2,-2}})[-4]), BundleType=>QuotientBundle);
--  B3.dd^2 == 0
  F3 = (prune HH B3)_0 ** S^{{-2,-2}};
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
 

--  degrees F2
--  degrees F3
--  h = homomorphism (Hom(F3,F2))_{0};

  tdeg = {3,3} -- for QuotientBundle
  F3a = prune truncate(tdeg,F3);
  F2a = prune truncate(tdeg,F2);
assert(numgens F3a == numgens F2a)
--  isIsomorphic(F3a, F2a) -- this is too slow!


  -- Now shift another time
debug TateOnProducts
  a = {3,3}
  T4 = ((T2 ** E^{a})[sum a])
  cohomologyMatrix(oo, -5*n,5*n)

  BW4 =  beilinsonWindow T4
  BW4.dd^2 == 0
  BW4 = removeZeroTrailingTerms beilinsonWindow T4
assert(  BW4.dd^2 == 0)

  B4 = (beilinson BW4) ** S^{a};
--  B4.dd^2 == 0
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
  BW5 =  beilinsonWindow T5
  betti BW5
  beilinson BW5
 
  for i from nonzeroMin B4 to nonzeroMax B4 do if i != 0 then 
           assert(saturate(ann HH_i(B4), irrelevant) == 1)

///


///
--This was a test, but very slow, and didn't test much
-- Keep this one?  It takes a bit of time...
  -- Take a sheaf on P^2 x P^3, e.g. the graph of a rational map

  restart
  needsPackage "TateOnProducts"

  n={2,3};
  (S,E) = productOfProjectiveSpaces n;

  m = random(S^1, S^{4:{-3,0}}) || matrix {{S_3, S_4, S_5, S_6}}
  m = random(S^1, S^{4:{-2,0}}) || matrix {{S_3, S_4, S_5, S_6}}  
  M = coker m;
  tdeg = {6,2}
  tM = truncate(tdeg, M);
  m1 = (presentation tM) ** S^{tdeg};
  betti m1
  corner1 = symExt(m1,E);
  T = ((res(coker corner1, LengthLimit => 4)) ** E^{tdeg})[sum tdeg]
  betti T
--  cohomologyMatrix(T, -5*n,5*n)
  T1 = T ** E^{{-3,0}}[-3];
  BW =  beilinsonWindow T1;
--  cohomologyMatrix(BW, -5*n, 5*n)
  assert(BW.dd^2 == 0)
  assert(isHomogeneous BW)
--  betti BW
  B = beilinson BW;
--  betti B
  assert(B.dd^2 == 0)

///


TEST ///
-- YYY
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
  U = for i from 0 to n#0 list beilinsonBundle({i,0},S);
  V = for i from 0 to n#1 list beilinsonBundle({0,i},S);
  for x in toList({0,0}..n) do (
      assert(beilinsonBundle(x,S) == U#(x#0) ** V#(x#1))
      )
///



------------------------------------

-- Example of beilinson
TEST ///
--restart
  -- XXX
--  needsPackage "TateOnProducts"
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
--restart
--  needsPackage "TateOnProducts"
  n = {2,1}
  (S,E) = productOfProjectiveSpaces n;
  assert(degrees beilinsonBundle({0,0},S) == {{0,0}})
  U1 = beilinsonBundle({1,0},S)
  U2 = beilinsonBundle({2,0},S)
  V1 = beilinsonBundle({0,1},S)
  assert(rank sheaf U1 == n#0) -- is this computation correct?

  assert(U1 ** V1 == beilinsonBundle({1,1},S))
  assert(U2 ** V1 == beilinsonBundle({2,1},S))
///

TEST ///

--restart
  debug needsPackage "TateOnProducts"
  n={2,1};
  (S,E) = productOfProjectiveSpaces n;
  assert(numgensU({0,0},E) == 1)
  assert(numgensU({0,1},E) == 1)
  assert(numgensU({1,0},E) == 3)
  assert(numgensU({1,1},E) == 3)
  assert(numgensU({2,0},E) == 1)
  assert(numgensU({2,1},E) == 1)
  e1 = map(E^{{0,0}}, E^{{-1,0}}, {{e_(0,1)}})
  e2 = map(E^{{-1,0}}, E^{{-2,0}}, {{e_(0,2)}})
  assert(beilinson e1 * beilinson e2 == beilinson(e1 * e2))
///

///
-- XXX how much of this to keep?
--this doesn't have any asserts
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

///

 ///
 --no asserts
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
  T = tateExtension W
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

 ///
 --no asserts
restart
  needsPackage "TateOnProducts"
  n={2,1};
 (S,E) = productOfProjectiveSpaces n;

  T1 = (dual res trim (ideal vars E)^2)[1];
  a=-{2,2};
  T2=T1**E^{a}[sum a];
  cohomologyMatrix(T2,-3*n,3*n)
  W=removeZeroTrailingTerms beilinsonWindow T2,cohomologyMatrix(W,-2*n,2*n)
  T = tateExtension W
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

restart
needsPackage "TateOnProducts"

-- experiment with the old dual: Question can the wrong dual produce a resolution with wrong betti numbers?

        kk=ZZ/101;n=4;
	E=kk[e_0..e_n,SkewCommutative =>true]
	m=map(E^{0,1},,matrix{{ e_0,e_1*e_2+e_3*e_4},{e_3*e_4-e_1*e_2,e_0*e_1*e_4}})
        isHomogeneous m
        dual m
    	fm=res coker m
	betti fm
	dualfm = dual fm
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
	betti fm
	dualfm = dual fm
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
time T=tateExtension(W)
betti (qT=firstQuadrantComplex(T,{0,0}))
cohomologyMatrix(qT,-n,2*n),cohomologyMatrix(T,-2*n,2*n)
-------------

-- viewHelp res seems that a some point either Dan or Mike thought about installing res(ChainComplex)
methods res
S=ZZ/101[x,y,z]/ideal(x*y)
M0=((S^1/ideal y)**S^{2}), M1=S^1, M2=S^{ -1}
C=chainComplex({map(M0,M1,matrix{{x^2}}),map(M1,M2,matrix{{y}})})
isHomogeneous C

--------------

restart
loadPackage("TateOnProducts",Reload=>true)
(S,E) = productOfProjectiveSpaces {1,2}
xx = apply(2, i->S_i)
yy = select(gens S, v -> degree v =={0,1})
ee = select(gens E, v -> degree v =={1,0})
ff = select(gens E, v -> degree v =={0,1})
up = random(E^{5:{1,0}}, E^3)
right = random(E^{3:{0,1}}, E^3)
tot = up||right
T1 = res(coker tot, LengthLimit => 12);
high = {6,6}
low = -high
cohomologyMatrix(T1,low,high)
T2=cornerComplex(T1, -{5,5})
--why the numbering of T2?
betti T2
phi = transpose T2.dd_9;
T = dual res(image phi, LengthLimit=>15)**E^{{3,4}}
high = high+{3,4}
low = low+{3,4}
cohomologyMatrix(T,low, high)
sT = strand(T, {4,4}, {0})
cohomologyMatrix(sT, low, high)
(S1,E1) = productOfProjectiveSpaces{2}
p = map(E1,E,matrix{{0,0}}|vars E1)
sT' = p sT
isHomogeneous sT'
betti sT'
tar = (sT'_(-15)); s = chainComplex for i from min sT'+1 to max sT'-1 list(
	phi = map(tar,,sT'.dd_(i+1));
	tar = source phi;
	phi);
betti s
betti(s[5]**E1^{{5}})
B = beilinsonWindow (s[6]**E1^{{6}})
betti B
ann (HH_(-1) beilinson B)
eulerPolynomialTable(B,{-5},{5})


-------------------------------------
(S,E) = productOfProjectiveSpaces{1,2}
M = S^1/random({3,1},S)
RM = bgg(M)
high = {3,3}
low = -high
cohomologyMatrix(RM,low,high)
cohomologyMatrix(M,low,high)
C = cornerComplex(M,low,high)
cohomologyMatrix(C,low,high)
BC = beilinson C
betti BC
tallyDegrees BC
M' = HH_0 BC
isIsomorphic(truncate({3,1},M), truncate({3,1},M'))
isIsomorphic(truncate({2,0},M), truncate({2,0},M'))
cohomologyMatrix(M,low,high)
cohomologyMatrix(RM,low,high)

(S,E) = productOfProjectiveSpaces{1,2}
M = S^1/random({1,3},S)
RM = bgg(M)
high = {3,3}
low = -high
cohomologyMatrix(RM,low,high)
cohomologyMatrix(M,low,high)
T = cornerComplex(M,low,high)
betti T
cohomologyMatrix(T,low,high)
BT = beilinson T
betti BT
tallyDegrees BT
M' = HH_0 BT
isIsomorphic(truncate({3,0},M), truncate({3,0},M'))
isIsomorphic(truncate({2,1},M), truncate({2,1},M'))

C = cornerComplex(T,{0,0})
cohomologyMatrix(C,low,high)
betti C
ann HH_(-8)(bgg ker C.dd_0)
betti(bgg image C.dd_0)
netList apply(values(HH (bgg image C.dd_0)), v->ann v)
apply(betti C.dd_0)

(values HH)
(apply(8, i->ann HH_(-i)(bgg image C.dd_0)))/codim
M' = HH_(-4)(bgg image C.dd_0);
isIsomorphic(truncate({2,2},M), truncate({2,2},M'))
cohomologyMatrix (M', low, high)
cohomologyMatrix (M, low, high)


cohomologyMatrix(M,low,high)
cohomologyMatrix(RM,low,high)


---------------------------------
restart
loadPackage "TateOnProducts"
--Hard examples for Mike
--I believe the time is all taken up with the resolution
        n={1,2}; 
        (S,E) = productOfProjectiveSpaces n;
	T1 = (dual res trim (ideal vars E))[1];
	a=-{2,2};
	T2=T1**E^{a}[sum a];
	W=beilinsonWindow T2
time    T=tateExtension W; -- 2 sec

        n={2,2};
        (S,E) = productOfProjectiveSpaces n;
	T1 = (dual res trim (ideal vars E))[1];
	a=-{2,2};
	T2=T1**E^{a}[sum a];
	W=beilinsonWindow T2
time    T=tateExtension W; -- 84 seconds

        n={1,1,1};
        (S,E) = productOfProjectiveSpaces n;
	T1 = (dual res trim (ideal vars E)^2)[1];
	a=-{2,2,3};
	T2=T1**E^{a}[sum a];
	W=beilinsonWindow T2
time    T=tateExtension W; -- still computing 10 minutes later...
