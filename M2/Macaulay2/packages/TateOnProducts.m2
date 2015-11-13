newPackage(
	"TateOnProducts",
    	Version => "0.2", 
    	Date => "September 14, 2014",
    	Authors => { {Name => "Daniel Erman", 
		  Email => "derman@math.wisc.edu", 
		  HomePage => "http://www.math.wisc.edu/~derman/"},
	         {Name => "David Eisenbud", 
		  Email => "de@msri.org", 
		  HomePage => "http://www.msri.org/~de/"},
	         {Name => "Frank-Olaf Schreyer", 
		  Email => "schreyer@math.uni-sb.de", 
		  HomePage => "http://www.math.uni-sb.de/ag/schreyer/"}
                   },
    	Headline => "Tate resolutions on products of projective spaces",
    	DebuggingMode => true
    	)

export {
    "boxDegrees",
    "setupRings",
    "symExt",
    "numFactors",
    "cohomologyTable",
    "tallyDegrees",
    "truncateInE",
    "lowerCorner",
    "upperCorner",
    "beilinsonWindow",
    "tateExtension",
    "sloppyTateExtension",
    "pushAboveWindow",
    "firstQuadrantComplex",
    "lastQuadrantComplex",
    "cornerComplex",
    "cornerComplex1",
    "regionComplex",
    "strand",
    "ringData",
    "cornerCohomologyTablesOfUa",
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
--needsPackage "ChainComplexExtras"
----------------------------------------------
-- from graded modules to Tate resolutions  --
----------------------------------------------
setupRings=method()
setupRings(Ring,List) := (kk,n) -> (
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
TEST ///
n={1,2}
kk=ZZ/101
(S,E)=setupRings(kk,n)
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
cohomologyTable( T, -{2,2},{6,6})
///


ringData = method()
ringData Ring := E ->(
differentDegrees := unique last degrees vars E;
varsLists := apply(differentDegrees, deg -> select (gens E, x-> degree x == deg));
t := #varsLists;
irrList := apply(varsLists, L -> ideal(L));
v := varsLists/(L->#L);
n := apply(v, i-> i-1);
(t,v,n,varsLists,irrList)
)
ringData Module := M -> ringData ring M

///
v = {2,3}
E = kk[e_0..e_1, f_0..f_2, Degrees => {v_0:{1,0},v_1:{0,1}}, SkewCommutative => true]
ringData E
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

    
TEST ///       
n={1,2}
(S,E)=setupRings(ZZ/101,n)
use S
vars S
m=map(S^4,S^{{ -1,0},{0,-1}}, transpose matrix{{S_0,S_1,0,0},{S_2,0,S_3,S_4}})
mE=symExt(m,E)

///

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

{*
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
TEST ///
n={1,2}
kk=ZZ/101
(S,E)=setupRings(kk,n)
F=dual (res((ker transpose vars E)**E^{{ 2,3}},LengthLimit=>10))
cohomologyTable(F,-2*n,2*n)
tallyDegrees F

deg={2,1} 
m=corner(F,deg);
tally degrees source m, tally degrees target m
Fm=(res(coker m,LengthLimit=>10))[sum deg]
betti Fm
betti F
cohomologyTable(Fm,deg-{5,5},deg+{1,1})
///

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
TEST///
n={1,1}
(S,E)=setupRings(ZZ/101,n)

time fB=dual res(coker random(E^7,E^{13:{ -1,0},11:{0,-1}}),LengthLimit=>10);	 	  
cohomologyTable(fB,-{1,1},{5,5})
deg={3,3}
m= corner(fB,deg);
f= res( ker  m,LengthLimit=> 4)[3]
tallyDegrees f
betti m, tally degrees target m, tally degrees source m
m1= corner(f,-1,{2,0});
betti m1, tally degrees target m1, tally degrees source m1
///
*}


---------------------------------------------------------
-- numerical information                               --
---------------------------------------------------------

cohomologyTable=method()


cohomologyTable(ChainComplex,List,List) := (F,da,db) -> (
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

TEST ///
n={1,2};kk=ZZ/101;
        (S,E)=setupRings(ZZ/101,n);
	a={1,1}; U=E^{ -a};
	W=(chainComplex {map(E^0,U,0),map(U,E^0,0)})[1]
	tallyDegrees W
	cohomologyTable(W,-{3,3},{3,3})
        time T=trivialHomologicalTruncation(sloppyTateExtension W,-9,6)
	cohomologyTable(T,-{3,3},{3,3})
	cohomologyTable(T,-{2,3},{3,3})	
///

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
 	    
TEST ///
n={1,2,2}
(S,E)=setupRings(ZZ/101, n);
boxDegrees E
///




beilinsonWindow=method()
beilinsonWindow(ChainComplex) := C-> (
     E:= ring C;
     length C;
--    C':=C[min C];
     T:=#unique degrees E;
     n:=apply(unique degrees E,d-> (#select( degrees  E, e-> e==d)-1));
     Ck:=0;sourceCK:=0; targetCK:=0;d:=0;
     W:=chainComplex apply(min C+1..max C,k-> (Ck=(-1)^(min C)*C.dd_k;
	  --source indices and target rows and columns in the Beilison window
	  sourceCK = select(rank source Ck,i-> (d=degree (source Ck)_i;#select(#d,i->d_i>=0 and d_i<=n_i)==#n));	
          targetCK =  select(rank target Ck,i-> (d=degree (target Ck)_i;#select(#d,i->d_i>=0 and d_i<=n_i)==#n));	
     	  (Ck^targetCK)_sourceCK));
     return W[-min C]) 

TEST ///
n={4}
(S,E)=setupRings(ZZ/101,n)
C=res ideal vars E
C1=C**E^{{ +1}}[0]
W=beilinsonWindow C1
apply(min W+1 ..max W,k->(W.dd_k==C1.dd_k,betti W.dd_k))
W
///

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



-- is still needed in one of the examples in TateExtension -- should go eventually
truncateInE=method()
truncateInE(List,Module):= (d,M) -> (
    base:=basis(M);
    degs:=degrees source base;
    m:=base_(select(#degs,k->#select(#d,i->degs_k_i >= d_i)==#d));
    image m 	
    )    



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
    
cornerComplex=method()
cornerComplex(ChainComplex,List) := (C,c) -> (d:=c-toList(#c:1);cornerComplex1(C,d))

TEST ///
c={1,2}
#c
toList(#c:1)
///

    
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

TEST ///
S=ZZ[x,y]/ideal(x*y)
C=(chainComplex(matrix{{x}},matrix{{y^2}},matrix{{x^2}}))[3]
isHomogeneous C
-- chainComplexData is no longer exported by ChainComplexExtras
-- L=chainComplexData C
-- C'=chainComplexFromData L
-- assert(C'== C)
///



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
(S,E)=setupRings(ZZ/101,{1,2})
pushAboveWindow(E^{{0,0},-{ -1,0},-{1,2},-{1,3}})
///


pushAboveWindow Matrix := A ->(
    if A==0 then return A;
    mingens image (A*pushAboveWindow source A)
    )

///
(S,E)=setupRings(ZZ/101,{1,2})
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

----------------------------------------
-- Examples in the Paper                                   --
----------------------------------------
cornerCohomologyTablesOfUa = method()
cornerCohomologyTablesOfUa(List) := n-> (
    if not #n ==2 then error "expect product with two factors only";
    (S,E):=setupRings(ZZ/101,n);a:=0;U:=0;W:=0;T:=0;cTa:=0;cTb:=0;cTb1:=0;
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
        (cohomologyTable(aT_2,-2*n,2*n),
        cohomologyTable(cTa,-2*n,2*n),
        cohomologyTable(aT_1,-2*n,2*n),
        --cohomologyTable(cTb,-2*n,2*n),
	cohomologyTable(cTb1,-2*n,2*n),
--	betti trivialHomologicalTruncation(aT_1,1-sum n,-1+ sum n),
	betti trivialHomologicalTruncation(cTa,1-sum n,-1+ sum n),
	betti trivialHomologicalTruncation(cTb1,1-sum n,-1+ sum n)
	)
    ))
)

cornerCohomologyTablesOfUa(List,List) :=(n,a)-> (
    if not #n ==2 then error "expect product with two factors only";
    if not (a_0 <=n_0 and 0 <= a_0 and a_1 <=n_1 and 0 <= a_1) then error "expected 0 <= a <=n"; 
    (S,E):=setupRings(ZZ/101,n);U:=0;W:=0;T:=0;cTa:=0;cTb:=0;cTb1:=0;
            U=E^{ -a};
            W=(chainComplex {map(E^0,U,0),map(U,E^0,0)})[1];
	T=sloppyTateExtension W;
	T=trivialHomologicalTruncation(T,-2*sum n,2*sum n);
	cTa=cornerComplex(T,-a);
	--cTb=dual cornerComplex(dual T,a-{1,1});
	cTb1=cornerComplex(T,{1,1})[-1];
        {cohomologyTable(T,-2*n,2*n),
        cohomologyTable(cTa,-2*n,2*n),
        cohomologyTable(W,-2*n,2*n),
        --cohomologyTable(cTb,-2*n,2*n),
	cohomologyTable(cTb1,-2*n,2*n),
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


--------------------------
-- Begin of the documentation
------------------------
beginDocumentation()

document { 
  Key => TateOnProducts,
  Headline => "Computation of parts of the Tate resolution on products",
  "This package contains implementations of the algorithm from our paper ",
   HREF("http://arxiv.org/abs/","Tate Resolutions on Products of Projective Spaces"),
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
   UL{   TO setupRings,
	 TO symExt,
	 TO lowerCorner,
	 TO upperCorner
      },
   SUBSECTION "Numerical Information",
   UL{ 
      TO cohomologyTable,
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
	TO pushAboveWindow
        },
    
    SUBSECTION "Examples from the papers",
    UL{ 
	TO cornerCohomologyTablesOfUa,
       },
    
    SUBSECTION "Missing pieces",
    UL{ "BGG functor R for complexes of S-modules",
	"projective resolutions of complexes",
	"Beilinson complex of sheaves/S-modules from a Beilinson window",
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

doc ///
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
        (S,E)=setupRings(ZZ/101,n)
	vars S, vars E
        m=map(S^4,S^{{ -1,0},{0,-1}}, transpose matrix{{S_0,S_1,0,0},{S_2,0,S_3,S_4}})
        mE=symExt(m,E)

///



{*
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
*}

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
        n={1,2}; kk=ZZ/101; (S,E)=setupRings(kk,n);
        F=dual res((ker transpose vars E)**E^{{ 2,3}},LengthLimit=>4)
	cohomologyTable(F,-{3,3},{4,4})
        betti F
	tallyDegrees F
        deg={2,1} 
        m=upperCorner(F,deg);
        tally degrees target m, tally degrees source m
        Fm=(res(coker m,LengthLimit=>4))[sum deg+1]
        betti Fm
        cohomologyTable(Fm,-{3,3},{4,4})
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
        n={1,2}; kk=ZZ/101; (S,E)=setupRings(kk,n);
        F=dual res((ker transpose vars E)**E^{{ 2,3}},LengthLimit=>4)
        betti F
	tallyDegrees F
        deg={2,1} 
        m=lowerCorner(F,deg);
        tally degrees target m, tally degrees source m
        Fm=(res(coker m,LengthLimit=>7))[sum deg]
        betti Fm
        cohomologyTable(Fm,-{3,3},{4,4})
///



----------------------------
-- numerical information  --
----------------------------
doc ///
  Key
    cohomologyTable
    (cohomologyTable,ChainComplex,List,List)
    --(cohomologyTable,ChainComplex,List,List,Ring)    
  Headline
    compute the the cohomology groups of a (part) of a Tate resolution or sheaf on products of projective spaces 
  Usage
    H=cohomologyTable(T,a,b)
  Inputs
    T: ChainComplex
       free complex over the exterior algebra 
    a: List
    b: List
       two lists a=(a_1,b_1), b=(b_1,b_2) representing bidegrees
  Outputs
    H: Matrix
       a (b1-a1)x(b2-a2) matrix of ring elements in $\mathbb Z[h,k]$
  Description
     Text
       Under the assumption that T is part of a Tate resolution of a sheaf F on a product of
       two projective space P^{n_1} x P^{n_2}, the function returns a matrix of cohomology polynomials 
       $$\sum_{i=0}^{|n|} \, dim H^i(\mathbb P^{n_1}\times \mathbb P^{n_2},\mathcal F(c_1,c_2)) * h^i \in \, \mathbb Z[h,k]$$
       for every c=(c_1,c_2) with $a_1 \le c_1 \le b_1$ and $a_2 \le c_2 \le b_2$.
       In case T corresponds to an object in the derived category D^b(P^{n_1}x P^{n_2}), then
       hypercohomology polynomials are returned, with the convention that k stands for k=h^{ -1}.
       
       If T is not a large enough part of the Tate resolution, such as W below, 
       then the function collects only
       the contribution of T to the cohomology table of the Tate resolution, according to the formula in
       Corollary 0.2 of @ HREF("http://arxiv.org/abs/","Tate Resolutions on Products of Projective Spaces") @.
        
       The polynomial for
       (b_1,b_2) sits in the north-east corner, the one corresponding to (a_1,a_2) in the south-west
       corner.       
     Example
        n={1,2};kk=ZZ/101;
        (S,E)=setupRings(ZZ/101,n);
	a={1,1}; U=E^{ -a};
	W=(chainComplex {map(E^0,U,0),map(U,E^0,0)})[1]
	cohomologyTable(W,-{3,3},{3,3})
        time T=sloppyTateExtension W
	cohomologyTable(T,-{3,3},{3,3})	
	cohomologyTable(T,-{3,4},{3,3})
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
       Torm the region complex T_c(I,J,K) as defined in section 3 of 
       @  HREF("http://arxiv.org/abs/","Tate Resolutions on Products of Projective Spaces") @. 
     Example
        (S,E)=setupRings(ZZ/101,{1,2})
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
       @  HREF("http://arxiv.org/abs/","Tate Resolutions on Products of Projective Spaces") @. 
     Example
        (S,E)=setupRings(ZZ/101,{1,2})
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
        @ HREF("http://arxiv.org/abs/","Tate Resolutions on Products of Projective Spaces") @
	section 3. Note that different from the paper I,J,K are sublists of 0...t-1 and not subsets of 1..t. 
	In the examples below, only rT2 and rT3 are proper region complexes. 
     Example
        n={1,1};(S,E)=setupRings(ZZ/101,n);
	T1 = (dual res trim (ideal vars E)^2)[1];
	a=-{2,2};T2=T1**E^{a}[sum a];
	W=beilinsonWindow T2,cohomologyTable(W,-2*n,2*n)
        T=sloppyTateExtension W;
	cohomologyTable(T,-{3,3},{3,3})
	c={1,0}
	rT0=regionComplex(T,c,({},{0,1},{})); --a single position
	cohomologyTable(rT0,-{3,3},{3,3})
    	rT1=regionComplex(T,c,({0},{1},{})); --a horizontal half line
	cohomologyTable(rT1,-{3,3},{3,3})
       	rT2=regionComplex(T,c,({},{0},{})); -- a vertical line
    	cohomologyTable(rT2,-{3,3},{3,3})
	rT3=regionComplex(T,c,({},{},{1})); -- a upper half plane
    	cohomologyTable(rT3,-{3,3},{3,3})
	rT4=regionComplex(T,c,({0},{},{1})); --a north east quadrant
    	cohomologyTable(rT4,-{3,3},{3,3})
	rT5=regionComplex(T,c,({1},{},{0})); --a south west quadrant
    	cohomologyTable(rT5,-{3,3},{3,3})
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
        HREF("http://arxiv.org/abs/","Tate Resolutions on Products of Projective Spaces") @
	Theorem 0.4. If T is (part of) the Tate resolution of a sheaf $F$, then the I-strand of $T$ through $c$
	correponds to the Tate resolution $R{\pi_J}_*(F(c))$ where $J =\{0,\ldots,t-1\} - I$ is the complement and $\pi_J: \mathbb PP \to \prod_{j \in J} \mathbb P^{n_j}$ 
	denotes the projection. 
     Example
        n={1,1};(S,E)=setupRings(ZZ/101,n);
	T1 = (dual res trim (ideal vars E)^2)[1];
	a=-{2,2};T2=T1**E^{a}[sum a];
	W=beilinsonWindow T2,cohomologyTable(W,-2*n,2*n)
        T=sloppyTateExtension W;
	cohomologyTable(T,-{3,3},{3,3})
	sT1=strand(T,-{1,1},{1});
	cohomologyTable(sT1,-{3,3},{3,3})
	sT2=strand(T,{1,1},{0});
	cohomologyTable(sT2,-{3,3},{3,3})
	sT3=removeZeroTrailingTerms strand(T,{1,-1},{0,1})
	cohomologyTable(sT3,-{3,3},{3,3})
	
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
       @  HREF("http://arxiv.org/abs/","Tate Resolutions on Products of Projective Spaces") @. 
     Example
        (S,E)=setupRings(ZZ/101,{1,1});T1= (dual res( trim (ideal vars E)^2,LengthLimit=>8))[1];
        T=trivialHomologicalTruncation(T2=res(coker upperCorner(T1,{4,3}),LengthLimit=>13)[7],-5,6);
    	betti T
	cohomologyTable(T,-{4,4},{3,2})
    	fqT=firstQuadrantComplex(T,-{2,1});
    	betti fqT	
	cohomologyTable(fqT,-{4,4},{3,2})
	cohomologyTable(fqT,-{2,1},-{1,0})
	lqT=lastQuadrantComplex(T,-{2,1});
    	betti lqT	
	cohomologyTable(lqT,-{4,4},{3,2})
	cohomologyTable(lqT,-{3,2},-{2,1})
	cT=cornerComplex(T,-{2,1});
	betti cT	
	cohomologyTable(cT,-{4,4},{3,2})
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
       @  HREF("http://arxiv.org/abs/","Tate Resolutions on Products of Projective Spaces") @. 
     Example
        (S,E)=setupRings(ZZ/101,{1,1});T1= (dual res( trim (ideal vars E)^2,LengthLimit=>8))[1];
        T=trivialHomologicalTruncation(T2=res(coker upperCorner(T1,{4,3}),LengthLimit=>13)[7],-5,6);
    	betti T
	cohomologyTable(T,-{4,4},{3,2})
    	fqT=firstQuadrantComplex(T,-{2,1});
    	betti fqT	
	cohomologyTable(fqT,-{4,4},{3,2})
	cohomologyTable(fqT,-{2,1},-{1,0})
	lqT=lastQuadrantComplex(T,-{2,1});
    	betti lqT	
	cohomologyTable(lqT,-{4,4},{3,2})
	cohomologyTable(lqT,-{3,2},-{2,1})
	cT=cornerComplex(T,-{2,1});
	betti cT	
	cohomologyTable(cT,-{4,4},{3,2})	
///

doc ///
  Key
    cornerComplex
    (cornerComplex,ChainComplex,List)
  Headline
    form the corner complex
  Usage
    cornerComplex(T,c)
  Inputs
    T: ChainComplex
       a (part of a) Tate resolution on a product of t projective spaces
    c: List
       cohomological degree of upper corner of the  last quadrant complex which is part of the corner complex   
  Outputs
     : ChainComplex
  Description
     Text
       Form the corner complex with corner c of a (part of a) Tate resolution T as defined in
       @  HREF("http://arxiv.org/abs/","Tate Resolutions on Products of Projective Spaces") @. 
     Example
        (S,E)=setupRings(ZZ/101,{1,1});T1= (dual res( trim (ideal vars E)^2,LengthLimit=>8))[1];
        T=trivialHomologicalTruncation(T2=res(coker upperCorner(T1,{4,3}),LengthLimit=>13)[7],-5,6);
    	betti T
	cohomologyTable(T,-{4,4},{3,2})
    	fqT=firstQuadrantComplex(T,-{2,1});
    	betti fqT	
	cohomologyTable(fqT,-{4,4},{3,2})
	cohomologyTable(fqT,-{2,1},-{1,0})
	lqT=lastQuadrantComplex(T,-{2,1});
    	betti lqT	
	cohomologyTable(lqT,-{4,4},{3,2})
	cohomologyTable(lqT,-{3,2},-{2,1})
	cT=cornerComplex(T,-{2,1});
	betti cT	
	cohomologyTable(cT,-{4,4},{3,2})
///
{*     Example
        (S,E)=setupRings(ZZ/101,{1,2});
	T1= (dual res( trim (ideal vars E)^2,LengthLimit=>4))
	isChainComplex T1
	tallyDegrees T1
	cohomologyTable(T1,-{10,10},{10,10})
	cohomologyTable(T1[-3],-{10,10},{10,10})
	T2=T1++T1**E^{{1,1}}[2]
    	T3=firstQuadrantComplex(T2,-{1,1})
	d= nonzeroMax T3;
	T4=res(coker T3.dd_d,LengthLimit=>13)[-d];
	T=removeZeroTrailingTerms lastQuadrantComplex(T4,-{3,3})
	betti T
        cT=cornerComplex(T,-{1,1})
	betti cT
	cohomologyTable(cT,-{4,4},{3,3})
	cohomologyTable(T,-{4,4},{3,3})	
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
       @  HREF("http://arxiv.org/abs/","Tate Resolutions on Products of Projective Spaces") @
       contributed to the Beilinson complex U(T) of T, i.e. W is the smallest free subquotient complex of T
       such that U(W) = U(T) 
     Example
        n={1,1};(S,E)=setupRings(ZZ/101,n);
        W=(chainComplex {map(E^0,E^1,0),map(E^1,E^0,0)})[1]
        time T=sloppyTateExtension W;
        cohomologyTable(T,-{3,3},{3,3})
	W=beilinsonWindow T
	cohomologyTable(W,-{2,2},{2,2})
        a={2,-3}
        W2=removeZeroTrailingTerms beilinsonWindow (T**E^{a}[sum a])
        cohomologyTable(W2,-{2,2},{2,2})
        cohomologyTable(sloppyTateExtension W2,-{2,2},{2,2})
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
       @ HREF("http://arxiv.org/abs/","Tate Resolutions on Products of Projective Spaces") @
       computes part of a suitable choosen corner complex of the Tate resolution T(F). 
       
       The phrase sloppy refers to the fact that the Beilinson window of T is not equal, 
       but only isomorphic to W. Moreover the bounds in the computation are only a guess and certainly not optimal.
     Example
        n={1,1};(S,E)=setupRings(ZZ/101,n);
	T1 = (dual res trim (ideal vars E)^2)[1];
    	isChainComplex T1
	a=-{2,2};
	T2=T1**E^{a}[sum a];
	W=beilinsonWindow T2
	cohomologyTable(W,-2*n,2*n)
        T=sloppyTateExtension W
	cohomologyTable(T,-3*n,4*n)
	cohomologyTable(beilinsonWindow T,-n,n)
	cohomologyTable(T,-5*n,4*n) -- the view including the corner
///

doc ///
  Key
    pushAboveWindow
    (pushAboveWindow,ChainComplex)
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
       @ HREF("http://arxiv.org/abs/","Tate Resolutions on Products of Projective Spaces") @
       that computes part of a suitable choosen corner complex of the Tate resolution T(F).        
     Example
        n={1,1};(S,E)=setupRings(ZZ/101,n);
	T1 = (dual res trim (ideal vars E)^2)[1];
    	isChainComplex T1
	a=-{2,2};
	T2=T1**E^{a}[sum a];
	W=beilinsonWindow T2
	cohomologyTable(W,-2*n,2*n)
        T=sloppyTateExtension W;
	cohomologyTable(T,-5*n,4*n) -- a view with the corner
	puT=trivialHomologicalTruncation(pushAboveWindow W,-1, 6)
	cohomologyTable(puT,-3*n,{1,1})
	betti W
	qT=trivialHomologicalTruncation(lastQuadrantComplex(T,{0,0}),-1,6)
	cohomologyTable(qT,-3*n,{1,1})
	betti puT
	betti qT
    	betti T	
	puT.dd_3_{0}
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
       @ HREF("http://arxiv.org/abs/","Tate Resolutions on Products of Projective Spaces") @.
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
     Usage => "isChainComplexMap(phi)",
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
(S,E)=setupRings(ZZ/101,n) 
a={0,1}
Ua=E^{ -a}
W=chainComplex(map(E^0,Ua,0),map(Ua,E^0,0))[1] 
time T=sloppyTateExtension(W) 
betti (qT=firstQuadrantComplex(T,{0,0}))
cohomologyTable(qT,-n,2*n),cohomologyTable(T,-2*n,2*n)
-------------

-- viewHelp res seems that a some point either Dan or Mike thought about installing res(ChainComplex)
methods res
S=ZZ/101[x,y,z]/ideal(x*y)
M0=((S^1/ideal y)**S^{2}), M1=S^1, M2=S^{ -1}
C=chainComplex({map(M0,M1,matrix{{x^2}}),map(M1,M2,matrix{{y}})})
isChainComplex C
isHomogeneous C

	
	

