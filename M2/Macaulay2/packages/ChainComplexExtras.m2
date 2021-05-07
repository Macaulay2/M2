newPackage(
     "ChainComplexExtras",
     Version => "1.1",
     Date => "Jan 11, 2016",
     Authors => {
	  {Name => "David Eisenbud", Email => "de@msri.org", HomePage => "http://www.msri.org/~de"},
	  {Name => "Frank Moore", Email => "fmoore@math.unl.edu", HomePage => "http://www.math.unl.edu/~s-wmoore3"},
	  {Name => "Frank-Olaf Schreyer", Email => "schreyer@math.uni-sb.de", HomePage => "http://www.math.uni-sb.de/ag/schreyer/"},
	  {Name => "Gregory G. Smith", Email => "ggsmith@mast.queensu.ca", HomePage => "http://www.mast.queensu.ca/~ggsmith"},
	  {Name => "Lily Silverstein", Email => "lsilverstein@cpp.edu", HomePage => "https://www.cpp.edu/faculty/lsilverstein/"}
	  },
     Headline => "some additional ChainComplex Functions",
     Keywords => {"Homological Algebra"},
     DebuggingMode =>false
     )

export "isExact"
export "isChainComplex"
export "isChainComplexMap"
export "isQuasiIsomorphism"
--export "isQuism"
export "koszulComplex"
export "taylor"
export "taylorResolution"
export "chainComplexMap"
export "InitialDegree"
export "minimize"
export "isMinimalChainComplex"
export "extendFromMiddle"
export "resolutionOfChainComplex"
export "cartanEilenbergResolution"
--the following are simple manipulations used in TateResolution.m2
export "prependZeroMap" -- prepend a zero map to chain complex
export "appendZeroMap" -- append a zero map to chain complex
export "removeZeroTrailingTerms" -- remove trailing zero terms of a chain complex
export "trivialHomologicalTruncation" -- return the trivial truncation of a chain complex
export "nonzeroMin" -- computes the homological position of the first non-zero module in a ChainComplex
export "nonzeroMax" -- computes the homological position of the last non-zero module in a ChainComplex
--for debugging purposes -- comment out when done:
--export "chainComplexData"
--export "chainComplexFromData"
export "scarfComplex"

substitute(ChainComplex,Ring):=(C,newRing)->(
   --- this function is just a version of substitute for chain complexes
   chainComplex(apply((min C + 1..max C), i -> substitute(C.dd_i, newRing)))
)

-*
chainComplexData = C->(
    minC := min C;
    maxC := max C;
    C':=C[minC];
    --note that this changes the signs of the differentials by (-1)^minC
    {minC, maxC, apply(toList(1..maxC-minC), i-> (C').dd_i)}
)
chainComplexFromData = method()
chainComplexFromData List := L ->(
    --format of L is desired min, desired max, list of 
    --shifted maps
    C := chainComplex L_2;
    assert( min C == 0);
    --this is indeed the inverse of chainComplexData, but the maps may have different signs than those in the list.
    C[-L_0])


--the functionality of this form is subsumed by that of the form without the ZZ option!
chainComplexFromData(ZZ, List) := (minC,L) ->(
    --minC will become the min of the output complex
    C := chainComplex L;
    assert( min C ==0);
    C[-minC])
*-
-*
--the following versions from before 2018 do not take into account the case where
--C has only one module and no maps.

chainComplexData = C->(
    minC := min C;
    maxC := max C;
    C':=C[minC];
    {minC, maxC, apply(toList(1..maxC-minC), i-> (-1)^minC*(C').dd_i)}
)

chainComplexFromData = method()
chainComplexFromData List := L ->(
    --format of L is desired min, desired max, list of 
    --shifted maps
    C := chainComplex apply(L_2, d->(-1)^(L_0)*d);
    assert( min C == 0);
    C[-L_0])
*-

--(change made 180114 -- DE:) 
--these versions deal with the case where C has 1 module and no maps, min C === max C.
chainComplexData  = method()
chainComplexData ChainComplex := C->(
    minC := min C;
    maxC := max C;
    C':=C[minC];
    cont := if minC === maxC then {C'_0} else 
         apply(toList(1..maxC-minC), i-> (-1)^minC*(C').dd_i);
    {minC, maxC, cont}
)

chainComplexFromData = method()
chainComplexFromData List := L ->(
    --format of L is desired min, desired max, and, if min C != max C, then list of 
    --shifted maps, otherwise the unique module.
    if L_0===L_1 then C:= L_2_0[0] else
    C = chainComplex apply(L_2, d->(-1)^(L_0)*d);
    assert( min C == 0);
    C[-L_0]
    )

--the functionality of this form is subsumed by that of the form without the ZZ option!
chainComplexFromData(ZZ, List) := (minC,L) ->(
    --here L is a list of maps. Note that this form 
    --minC will become the min of the output complex
    C := chainComplex apply(L, d->(-1)^minC*d);
    assert( min C ==0);
    C[-minC])


///
S=ZZ[x,y]/ideal(x*y)
C=(chainComplex(matrix{{x}},matrix{{y^2}},matrix{{x^2}}))[3]
isHomogeneous C
L=chainComplexData C
C'=chainComplexFromData L
assert(C'== C)
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
	
    
-*
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
    
removeZeroTrailingTerms = method()
removeZeroTrailingTerms(ChainComplex) := W -> (
    E := ring W;
    mi := nonzeroMin W;
    ma := nonzeroMax W;
    W' := W[mi];
    if mi==ma then (return (chainComplex({map(E^0,W'_0,0),map(W'_0,E^0,0)}))[-mi+1]) else
    (chainComplex apply(toList(1..ma-mi),i->W'.dd_i))[-mi]
    )
*-



prependZeroMap= method()
prependZeroMap ChainComplex := C->(
    L := chainComplexData(C);
    if L_0 == L_1 then 
      return (chainComplexFromData {L_0,L_1,{chainComplex map((ring C)^0, C_(L_0),0)}})[1];
    minC := L_0;
    newd := map((ring C)^0, target L_2_0, 0);
    (chainComplexFromData(minC-1,prepend(newd,L_2)))
    )
appendZeroMap = method()
appendZeroMap ChainComplex := C->(
    L := chainComplexData(C);
    if L_0 == L_1 then 
      return chainComplexFromData {L_0,L_1,{chainComplex map(C_(L_0),(ring C)^0, 0)}};
    minC := L_0;
    newd := map(source last L_2,(ring C)^0, 0);
    chainComplexFromData (minC,append(L_2,newd))
    )    
-*    
appendZeroMap= method()
appendZeroMap ChainComplex := C->(
    L := chainComplexData(C);
    minC := L_0;
    newd := map(source last L_2,(ring C)^0, 0);
    chainComplexFromData(minC,append(L_2,newd))
    )    
*-  
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
restart
loadPackage("ChainComplexExtras", Reload =>true)
R=ZZ[tt]
C=chainComplex {matrix{{R_0}}}
C1=appendZeroMap prependZeroMap C
removeZeroTrailingTerms C1
///

Hom(ChainComplex,ChainComplex) := (F,G)->(
   outputCx := new ChainComplex;
   outputCx.ring = ring F;
   topDegree := max G - min F;
   botDegree := min G - max F;
   index1 := topDegree;
   while (index1 > botDegree) do
   {
      sourceList := toList (min F .. max G - index1);
      targetList := toList (min F .. max G - (index1 - 1));
      myFn := i -> (fold((a,b) -> (a || b),
		         apply(targetList,
			       j -> (if (j == i) then 
			               Hom(F_i,G.dd_(i+index1))
				     else if (j == i+1) then
				       (-1)^index1*Hom(F.dd_j,G_(i+index1))
				     else map(Hom(F_j,G_(j+index1-1)),Hom(F_i,G_(i+index1)),0)
				     )
				)
			   )
		     );
      diffl := fold((a,b) -> (a | b), apply(sourceList, myFn));
      outputCx#index1 = source diffl;
      outputCx#(index1 - 1) = target diffl;
      outputCx.dd#index1 = diffl;
      index1 = index1 - 1;
   };
   outputCx.max = topDegree;
   outputCx.min = botDegree;
   outputCx.length = topDegree - botDegree;
   outputCx
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

isExact=method(Options => {LengthLimit => infinity})
isExact(ChainComplex):= o -> (C) -> (
   if all((min C,min(max C,o.LengthLimit)), i -> (prune HH_i(C) == 0)) then true else false
)

isChainComplex=method()
isChainComplex(ChainComplex):=(inputComplex)->(
   if (inputComplex.dd^2 == 0) then true else false
)

-*
isChainComplexMap=method()
isChainComplexMap(ChainComplexMap):=(inputMap)->(
   isChainComplex(cone inputMap)
)
*-
isChainComplexMap=method()
isChainComplexMap(ChainComplexMap):=(inputMap)->(
   degs := sort select(keys inputMap, k->class inputMap#k === Matrix);
   if degs == {} then return true;
   A := trivialHomologicalTruncation(source inputMap, min degs, max degs);
   B := trivialHomologicalTruncation(target inputMap, min degs, max degs);
   restrictedMap := chainComplexMap(B,A,apply(degs, i-> inputMap_i), InitialDegree => min degs);
   isChainComplex(cone restrictedMap))
///
restart
loadPackage("ChainComplexExtras", Reload =>true)
S = ZZ/101[t]
C = chainComplex map (S^1,S^1,t)
D = chainComplex{map(S^1/t, S^1,1), map(S^1,S^1,-t)}[1]
phi = chainComplexMap(D,C,apply({0,1},i->id_(C_i)))
isChainComplexMap phi

--A test from BGG
restart
  needsPackage "BGG"
  needsPackage "ChainComplexExtras"  
  kk = ZZ/101
  A = kk[a]
  S = A[x,y]
  M1 = S^{{-1,0},{0,0}}
  C1 = directImageComplex(id_M1)
  assert(C1_0 == 1)
  assert isChainComplexMap C1

  M2 = S^{{-1,0}}
  C2 = directImageComplex(id_M2)
  assert isChainComplexMap C2
  assert(C2_0 == 0)

  M3 = S^{{-2,0}}
  C3 = directImageComplex(id_M3)
  assert isChainComplexMap C3
  assert(C3_0 == 0)

///

isQuasiIsomorphism=method(Options => {LengthLimit => infinity})
isQuasiIsomorphism(ChainComplexMap):= o -> (phi)-> (
   isExact(cone phi, LengthLimit => o.LengthLimit)
)

--isQuism = isQuasiIsomorphism

ChainComplexMap | ChainComplexMap := (f,g) -> (
   --- this function overloads the | operator for ChainComplexMap
   retVal := 0;
   matrList := {};
   if (target f == target g) then
      retVal = map(target f, source f ++ source g, (i -> (f_i | g_i)))
   else
   {
      retVal = Nothing;
      error "Targets of ChainComplexMaps must be the same.";
   };
   retVal
)

ChainComplexMap || ChainComplexMap := (f,g) -> (
   --- this function overloads the | operator for ChainComplexMap
   retVal := 0;
   matrList := {};
   if (source f == source g) then
      retVal = map(target f ++ target g, source f, (i -> (f_i || g_i)))
   else
   {  
      retVal = Nothing;
      error "Source of ChainComplexMaps must be the same.";
   };
   retVal
)

koszulComplex=method(
    Options => {LengthLimit => 0}
)

koszulComplex(Ideal):= o -> (I)->(
    --- this function just returns the Koszul complex
    --- where I represents the first differential.
    if not instance(o.LengthLimit, ZZ)
    then error "The optional LengthLimit must be an integer.";
    lengthLimit := 0;
    if (o.LengthLimit == 0) then
       lengthLimit = numgens I
    else
       lengthLimit = o.LengthLimit;
    chainComplex(apply(toList (1 .. lengthLimit), i -> koszul(i, gens I)))
)

myLcm = method()
myLcm(List):=(ringList)->(
   --- just a short method computing the lcm of the list of elements
   myList := apply(ringList, i -> ideal(i));
   (intersect myList)_0
)

taylor = method()
taylor(ZZ,MonomialIdeal):= (n,I)->(
   --- create the nth differential in Taylor's resolution
   retVal := Nothing;
   if (n == 1) then 
      retVal = gens I
   else
   {
      idealList := flatten entries gens I;
      R := ring I;
      sourceSubsets := subsets(toList (0..(numgens I - 1)),n);
      targetSubsets := subsets(toList (0..(numgens I - 1)),n-1);
      sourceList := apply(sourceSubsets, i -> myLcm(idealList_i));
      targetList := apply(targetSubsets, i -> myLcm(idealList_i));
      getCoeff := (i,j) -> if (isSubset(targetSubsets_i,sourceSubsets_j)) then
                             (-1)^(position(sourceSubsets_j, k -> k == (toList(set sourceSubsets_j - set targetSubsets_i))_0))
			   else 0_R;
      myFn := (i,j) -> (tempElt := sourceList_j / targetList_i;
	                if (liftable(tempElt,R)) then getCoeff(i,j)*lift(tempElt,R) else 0_R);
      retVal = map(R^(-apply(targetList, i -> degree i)), R^(-apply(sourceList, i -> degree i)), myFn);
   };
   retVal
)

taylorResolution=method(
    Options => {LengthLimit => 0}
)
taylorResolution(MonomialIdeal):= o -> (I)->(
    --- this function just returns the Koszul complex
    --- where I represents the first differential.
    if not instance(o.LengthLimit, ZZ)
    then error "The optional LengthLimit must be an integer.";
    lengthLimit := 0;
    if (o.LengthLimit == 0) then
       lengthLimit = numgens I
    else
       lengthLimit = o.LengthLimit;
    chainComplex(apply((1..lengthLimit), i -> taylor(i,I)))
)

syzMap = method()
syzMap(Matrix) := (F) -> (
   -- given any R-homomorphism F : M --> N,
   -- compute a matrix P --> M, where P is free
   -- and the image is ker(F).
   z := modulo(matrix F, presentation target F);
   map(source F, source z, z)
)


///
symbol tt
R=ZZ[tt]
C=chainComplex {matrix{{R_0}}}
C1=appendZeroMap prependZeroMap C
nonzeroMax C1,max C1
nonzeroMin C1, min C1
///

isMinimalChainComplex = C -> (
    S := ring C;
    red := map(S,S,toList(numgens S:0_S));
    T :=true;
    scan(toList(1+min C..max C),
	i-> if 0 != red(C.dd_i) then T = false);
    T
    )


-- local functions for finding the extremal homological degrees of the
-- nonzero modules in a graded module
nonzeroMin = method()
nonzeroMin ChainComplex := (cacheValue symbol nonzeroMin)(C -> (
   complete C;
   min for i from min C to max C list if C_i == 0 then continue else i))

nonzeroMax = method()
nonzeroMax ChainComplex := (cacheValue symbol nonzeroMax)(C -> (
   complete C;
   max for i from min C to max C list if C_i == 0 then continue else i))

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
--      if not isQuasiIsomorphism m then 
--           error"didn't produce a quasi-isomorphic complex";
--    E' := pmC[-min E];
--    E'.cache.pruningMap = m[-min E];
--    E'
--    )

///
S = ZZ/32003[a,b]
red = map(S,S,toList(numgens S:0_S))
C = koszul gens (ideal vars S)^2
G = S^{0,-1,-2,-3,-4,-5,-6}
D = apply(length C+1, i-> C_i++G++G)
zG = map(G,G,0)
difs0 = apply(length C, 
    i-> 
    map(D_i, D_(i+1), matrix{
	    {C.dd_(i+1),map(C_i,G,0), map(C_i,G,0)},
	    {map(G,C_(i+1),0),zG,zG},
	    {map(G,C_(i+1),0),id_G,zG}}
	)
);
len = #difs0
Q = apply(len, i-> random(target difs0_i, target difs0_i))|
       {random(source difs0_(len-1), source difs0_(len-1))};
difs1 = apply(len, i-> Q_i*difs0_i*Q_(i+1)^(-1));
E = chainComplex difs1
assert(isMinimalChainComplex E == false)
m = minimize (E[1]);
assert (isQuasiIsomorphism m)
assert (E[1] == source m)
E' = target m
assert (isChainComplex E'==true)
assert(isMinimalChainComplex E' == true)
///    	    


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

     

cartanEilenbergResolution = method(Options=>{LengthLimit => infinity})
cartanEilenbergResolution ChainComplex := o-> C -> (
   --- C is a ChainComplex
   --- returns a free resolution map to C from the cartanEilenbergResolution of C; that is, 
   -- a surjective quasi-isomorphism from a free complex computed by the method of Cartan-Eilenberg.
   --- NOTICE: When using this function, the source complex, as well as the
   ---         map, will always be correct.  However, if you try and take
   ---         the mapping cone of the map and the target complex has some
   ---         zero differentials, the mapping cone complex may not be exact!
   ---         This is a bug in M2, as of 0.9.20, on 10/30/2006.
   mapList := {};
   difflList := {};
   R := ring C;
   lengthLimit := max C + numgens R;
   if (o.LengthLimit != infinity) then lengthLimit = min C + o.LengthLimit;
         
   prevf := syzMap(C.dd_(min C));
   mapList = append(mapList, prevf);
   prevf''' := prevf;
   prevd := map(R^0, source prevf,0);
   index1 := min C;
   while (index1 <= lengthLimit) do
   {
      --- f''' is the part of the previous f map we need for this step
      --- it is a cover of the kernel of the diffl C_i ---> C_(i-1)
      (newd,newf,newf''') := nextReslnStep(prevd,prevf,C.dd_(index1+1),prevf''');
      mapList = append(mapList,newf);
      difflList = append(difflList,newd);
      prevd = newd;
      prevf = newf;
      prevf''' = newf''';
      if (newd == 0) then index1 = lengthLimit;
      index1 = index1 + 1;
   };
   --P := chainComplex(difflList, min C);
   --- this line is here instead of the above one because I cannot get M2 to override
   --- chainComplex(List,ZZ)
   P := chainComplex(difflList)[-min C];
   P#dd = (-1)^(min C)*P.dd;
--   P.cache.comparisonMap = chainComplexMap(C,P,mapList);
--   P
   chainComplexMap(C,P,mapList)
)

nextReslnStep=method()
nextReslnStep(Matrix,Matrix,Matrix,Matrix) := (prevPDiffl,prevQuism,CDiffl,PDifflCover) -> (
   -- prevPDiffl : P_i --> P_(i-1)
   -- prevQuism : P_i --> C_i
   -- CDiffl == C.dd_(i+1)
   -- PDifflCover = part of prevDQuism, P_i''' --> C_i, cover of kernel of C_i --> C_(i-1)
   -- Returns a triple of maps (d,f,f''')
   -- where
   -- d : P_(i+1) --> P_i
   -- f : P_(i+1) --> C_(i+1)
   -- f''' : P_(i+1)''' ---> C_(i+1)
   -- VARIABLE CONVENTION:
   --              '    <---> fixing prevQuism and prevPDiffl
   --              ''   <---> surjectivity of the quism
   --              '''  <---> fixing CDiffl
   d' := syzMap ( prevQuism || prevPDiffl);
   f' := map(source CDiffl, source d', 0);
   g := map(target CDiffl, source matrix CDiffl, matrix CDiffl);
   f'' := g // CDiffl;
   d'' := g // PDifflCover;
   --- change the target of d'' to be the source of f'''
   F := (source prevQuism)_{0..(numgens source prevQuism-numgens source PDifflCover-1)};
   inc1 := map(source F, source PDifflCover, 0);
   inc2 := id_(source PDifflCover);
   inc := inc1 || inc2;
   d'' = inc * d'';
   f''' := syzMap CDiffl;
   d''' := map(source prevPDiffl, source f''', 0);
   (d' | d'' | d''', f' | f'' | f''', f''')
)



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

scarfComplex = method()
scarfComplex(MonomialIdeal) := (I) -> (
    R := ring I;
    n := numgens R;
    gensList := flatten entries gens I;
    if #gensList == 0 then return chainComplex gens I;
    allIndices := for i from 1 to min(n+1, #gensList) list subsets(0..(#gensList-1), i);
    allLcms := apply(allIndices, v -> apply(v, w -> myLcm(gensList_w)));
    uniqueLcms := keys select(tally flatten allLcms, v -> v == 1);
    targetSubsets := allIndices_0;
    targetList := apply(targetSubsets, v -> myLcm(gensList_v));
    S := {gens I};
    hdeg := 1;
    while hdeg < min(n, #gensList) do(
	hdeg = hdeg + 1;
	sourceSubsets := select(allIndices_(hdeg-1), v -> member(myLcm(gensList_v), uniqueLcms));
	sourceList := apply(sourceSubsets, v -> myLcm(gensList_v));
	getCoeff := (i,j) -> (
	    if (isSubset(targetSubsets_i,sourceSubsets_j)) then(
		(-1)^(position(sourceSubsets_j, k -> k == (toList(set sourceSubsets_j - set targetSubsets_i))_0))
		)
	    else 0_R
	    );
      	myFn := (i,j) -> (
	    tempElt := sourceList_j / targetList_i;
	    if (liftable(tempElt,R)) then getCoeff(i,j)*lift(tempElt,R) else 0_R
	    );
      	S = append(S, map(R^(-apply(targetList, i -> degree i)), R^(-apply(sourceList, i -> degree i)), myFn));
	targetSubsets = sourceSubsets;
	targetList = sourceList;
	if #targetList == 0 then break;
	);
    chainComplex S
    )

///
restart
uninstallPackage "ChainComplexExtras"
installPackage "ChainComplexExtras"
check "ChainComplexExtras"

kk= ZZ/101
S = kk[a,b,c,d]
F1 = koszul matrix"a,b,c"
F2 = (res module ideal"a,b,c")
F1_1
F2_0
f = map(F1_1, F2_0,id_(F2_0))
cone extendFromMiddle(F1,F2,f, 1)
///

resolution ChainComplex := o -> C -> resolutionOfChainComplex C

beginDocumentation()

document {
     Key => ChainComplexExtras,
     Headline => "More ChainComplex Functionality."
     }

document {
     Key => (substitute,ChainComplex,Ring),
     Headline => "Change the ring over which the ChainComplex is defined.",
     Usage => "substitute(chCx, S)",
     Inputs => {
	  "chCx" => {},
	  "S" => {},
     },
     Outputs => {
	  {"The ChainComplex chCx over the ring R"}
     },
     EXAMPLE {
	     "R = ZZ/101[a,b,c]",
	     "S = R/ideal{a^2,b^2,c^2}",
	     "kRes = res coker vars R",
	     "kResS = substitute(kRes, S)",
	     }
     }

document {
     Key => (Hom,ChainComplex,ChainComplex),
     Headline => "Create the homomorphism complex of a pair of chain complexes.",
     Usage => "Hom(F,F)",
     Inputs => {
	  "F" => {},
	  "G" => {},
     },
     Outputs => {
	  {"Hom(F,G)"}
     },
     EXAMPLE {
	     "R = ZZ/101[a,b,c]",
	     "kRes = res coker vars R",
	     "Hom(kRes,kRes)",
	     }
     }

document {
     Key => {isExact, (isExact,ChainComplex)},
     Headline => "Test to see if the ChainComplex is exact.",
     Usage => "isExact(F)",
     Inputs => {
	  "F" => {},
     },
     Outputs => {
	  {"Boolean"}
     },
     EXAMPLE {
	     "R = ZZ/101[a,b,c]",
	     "k = coker vars R",
	     "kRes = res k",
	     "isExact kRes",
	     "trivialCx = chainComplex matrix {{1_R}}",
	     "isExact trivialCx",
	     }
     }

document {
     Key => (resolution,ChainComplex),
     Headline => "Resolves a ChainComplex.",
     Usage => "resolution C",
     Inputs => {
	  "C" => {},
     },
     Outputs => {
	  {"ChainComplex"}
     },
     "Returns a surjective ChainComplexMap that is a quasi-isomorphism
     from a (generally non-minimal) ChainComplex of free modules. 
     resolution C is the same as resolutionOfChainComplex C.
     The quasi-isomorphism is computed
     by the method of iterated mapping cones. (For the computation of the 
     Cartan-Eilenberg resolution, which is usually slower and results in
     a larger complex, use cartanEilenbergResolution C",
     EXAMPLE {
	        "R = ZZ/32003[a..d]",
		"I = monomialCurveIdeal(R,{1,2,3})",
		"C = koszulComplex(ideal vars R) ** (R^1/I);",
		"m = res C;",
		"isQuasiIsomorphism m",
		"betti source m",
		"C == target m"
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

document {
     Key => {isChainComplexMap, (isChainComplexMap,ChainComplexMap)},
     Headline => "Test to see if the ChainComplexMap commutes with the differentials.",
     Usage => "isChainComplexMap(phi)",
     Inputs => {
	  "phi" => {},
     },
     Outputs => {
	  {"Boolean"}
     },
     EXAMPLE {
	     "S = ZZ/101[a,b,c]",
	     "kRes = res coker vars S",
	     "multBya = extend(kRes,kRes,matrix{{a}})",
	     "isChainComplexMap(multBya)",
	     "",
	     "T = chainComplex(map(S^1,S^1,a))",
	     "T' = chainComplex{map(S^1/(ideal a),S^1, 1), map(S^1,S^1, -a)}[1]",
	     "phi = chainComplexMap(T',T,apply(toList(min T..max T), i->id_(T_i)))",
	     "isChainComplexMap phi"
	     },
     PARA{},
     "Caveat: The script uses trivialHomologicalTruncation to truncates the source and target of the map phi
     to include only the indices for which phi has matrices. This eliminates the problem of complexes of
     different lengths seen in an earlier version, which would have made the last line in the example return false."
     }

document {
     Key => InitialDegree,
     Headline => "Used to specify an initial degree for chainComplexMap.",
     TT "InitialDegree", " -- an optional argument for chainComplexMap",
     "used to specify the starting degree of the map.",
     PARA{},
     "This symbol is provided by the package ", TO ChainComplexExtras, "."
     }
document {
     Key => [chainComplexMap,InitialDegree],
     Headline => "Specify initial degree.",
     Usage => "chainComplexMap(...,InitialDegree=>n)",
     Inputs => {
        "n" => ZZ => "The initial degree of the ChainComplexMap."
       },
     Consequences => {
        {"The ChainComplexMap starts in degree n instead of the start of source."}
       }
     }

document {
     Key => {isQuasiIsomorphism, (isQuasiIsomorphism,ChainComplexMap)},
     Headline => "Test to see if the ChainComplexMap is a quasi-isomorphism.",
     Usage => "isQuasiIsomorphism(phi)",
     Inputs => {
	  "phi" => {},
     },
     Outputs => {
	  {"Boolean"}
     },
     "A quasi-isomorphism is a chain map that is an isomorphism in homology. ",
     "Mapping cones currently do not work properly for complexes concentrated ",
     "in one degree, so isQuasiIsomorphism could return bad information in that case.",
     EXAMPLE {
	     "R = ZZ/101[a,b,c]",
	     "kRes = res coker vars R",
	     "multBya = extend(kRes,kRes,matrix{{a}})",
	     "isQuasiIsomorphism(multBya)",
	     "F = extend(kRes,kRes,matrix{{1_R}})",
	     "isQuasiIsomorphism(F)",
	     }
     }

-*
document {
     Key => {isQuism, (isQuism,ChainComplexMap)},
     Headline => "Test to see if the ChainComplexMap is a quasi-isomorphism.",
     Usage => "isQuism(phi)",
     Inputs => {
	  "phi" => {},
     },
     Outputs => {
	  {"Boolean"}
     },
     "This is a synonym for isQuasiIsomorphism. 
     A quasi-isomorphism is a chain map that is an isomorphism in homology. ",
     "Mapping cones currently do not work properly for complexes concentrated ",
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
*-
doc ///
   Key
    [isQuasiIsomorphism,LengthLimit]
   Headline
    Option to check quasi-isomorphism only up to a certain point
   Usage
    t = isQuasiIsomorphism(F, LengthLimit => n)
   Inputs
    F:ChainComplexMap
    n:ZZ
   Outputs
    t:Boolean
   Description
    Text
     Useful, for example, when checking whether a map is a resolution of a complex
     in cases where the actual resolution is infinite
    Example
     kk= ZZ/101
     S = kk[a,b,c]
     R = S/ideal(a^3)
     M = R^1/ideal(a)
     C = chainComplex{map(M,R^0,0)}
     m=cartanEilenbergResolution (C, LengthLimit => 10)
     isQuasiIsomorphism(m, LengthLimit=> 10)
     isQuasiIsomorphism(m, LengthLimit => 12)
   SeeAlso
    isExact
///
doc ///
   Key
    [isExact,LengthLimit]
   Headline
    Option to check exactness only up to a particular homological degree
   Usage
    t = isExact(F, LengthLimit => n)
   Inputs
    F:ChainComplex
    n:ZZ
   Outputs
    t:Boolean
   Description
    Text
    Example
     kk= ZZ/101
     S = kk[a,b,c]
     R = S/ideal(a^3)
     M = R^1/ideal(a)
     C = chainComplex{map(M,R^0,0)}
     n =resolutionOfChainComplex (C, LengthLimit => 10)
     isExact(cone n, LengthLimit=> 10)
     isExact(cone n, LengthLimit=> 12)     
   SeeAlso
    isQuasiIsomorphism
///

document {
     Key => {koszulComplex, (koszulComplex,Ideal)},
     Headline => "Gives the Koszul complex on the generators of I.",
     Usage => "koszulComplex(I)",
     Inputs => {
	  "I" => {},
     },
     Outputs => {
	  {"ChainComplex"}
     },
     EXAMPLE {
	     "R = ZZ/101[a,b,c]",
	     "K = koszulComplex(ideal vars R)",
	     }
     }

document {
     Key => {taylor, (taylor,ZZ,MonomialIdeal)},
     Headline => "Gives the nth differential in the Taylor resolution of a monomial ideal I.",
     Usage => "T = taylor(n,I)",
     Inputs => {
	  "n" => {},
	  "I" => {},
     },
     Outputs => {
	  {"T, the nth differential in the Taylor Resolution of I."}
     },
     EXAMPLE {
	     "R = ZZ/101[a,b]",
	     "I = monomialIdeal (ideal vars R)^3",
	     "T2 = taylor(2,I)",
    	     "T3 = taylor(3,I)",
	     }
     }

document {
     Key => {taylorResolution, (taylorResolution,MonomialIdeal)},
     Headline => "Gives the Taylor resolution of a monomial ideal I.",
     Usage => "taylorResolution(I)",
     Inputs => {
	  "I" => {},
     },
     Outputs => {
	  {"ChainComplex"}
     },
     EXAMPLE {
	     "R = ZZ/101[a,b]",
	     "I = monomialIdeal (ideal vars R)^3",
	     "T = taylorResolution(I)",
	     "T.dd",
	     }
     }


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
	  time n = cartanEilenbergResolution C;
	  betti source m
	  betti source n
	  betti target minimize source n
	 Text
	  The resolution of a free complex is of course the same complex. resolutionOfChainComplex 
	  returns this minimal object directly, but cartanEilenbergResolution does not:
	 Example
	  C=koszul (gens (ideal vars S)^2)
	  betti source resolutionOfChainComplex C
	  betti source cartanEilenbergResolution C
        SeeAlso
	 minimize
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
     
     To illustrate we first make a nonminimal complex by adding 
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
     isQuasiIsomorphism m
     E[1] == source m
     E' = target m
     isChainComplex E'
     isMinimalChainComplex E'
///

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
    extendFromMiddle
    (extendFromMiddle, ChainComplex, ChainComplex, Matrix, ZZ)
   Headline
    extends a map between ChainComplexes
   Usage
    m = extendFromMiddle(F1,F2,f,i)
   Inputs
    F1:ChainComplex
    F2:ChainComplex
    f:Matrix
     homomorphism from F2_0 to F1_i
    i:ZZ
   Outputs
    m:ChainComplexMap
   Description
    Text
     If f is a map to F1_i from F2_0, the script computes a ChainComplexMap to F1 from F2e,
     where F2e is a chain complex obtained from F2 by prepending zeros.
   Caveat
     the process of making a new ChainComplex seems to destroy
     the direct sum information in the source and target modules.
   SeeAlso
///

doc ///
   Key
    cartanEilenbergResolution
    (cartanEilenbergResolution, ChainComplex)
   Headline
    Computes free resolution of a ChainComplex
   Usage
    m = cartanEilenbergResolution C
   Inputs
    C:ChainComplex
   Outputs
    m:ChainComplexMap
     surjective quasi-isomorphism from a free ChainComplex
   Description
    Text
     Uses a different algorithm than resolutionOfChainComplex, often slower and less nearly minimal, to compute
     a free resolution of a chain complex. See resolutionOfChainComplex for an example.
   SeeAlso
    resolutionOfChainComplex
///

doc ///
   Key
    [cartanEilenbergResolution, LengthLimit]
   Headline
    How many steps to compute
   Usage
    m = cartanEilenbergResolution(C,LengthLimit => n)
   Inputs
    C:ChainComplex
    n:ZZ
     non-negative integer or infinity
   Outputs
    m:ChainComplexMap
   Description
    Text
     Computes LengthLimit steps beyond the length of C
///

doc ///
   Key
    [resolutionOfChainComplex, LengthLimit]
   Headline
    How many steps to compute
   Usage
    m = resolutionOfChainComplex(C,LengthLimit => n)
   Inputs
    C:ChainComplex
    n:ZZ
     non-negative integer or infinity
   Outputs
    m:ChainComplexMap
   Description
    Text
     Computes LengthLimit steps beyond the length of C
///
doc ///
   Key
    [taylorResolution, LengthLimit]
   Headline
    How many steps to compute
   Usage
    m = taylorResolution(C,LengthLimit => n)
   Inputs
    C:ChainComplex
    n:ZZ
     non-negative integer or infinity
   Outputs
    m:ChainComplex
   Description
    Text
     Computes LengthLimit steps
///
doc ///
   Key
    [koszulComplex, LengthLimit]
   Headline
    How many steps to compute
   Usage
    m = koszulComplex(C,LengthLimit => n)
   Inputs
    C:ChainComplex
    n:ZZ
     non-negative integer or infinity
   Outputs
    m:ChainComplex
   Description
    Text
     Computes LengthLimit steps
///

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
    tests whether the differentials compose to zero 
  Usage
    isChainComplex C
  Inputs
    C: ChainComplex
  Outputs
     : Boolean
  Description
     Text
       tests that the differentials compose to zero.
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


-*
doc ///
   Key
    chainComplexData
   Headline
    Extract data from a chain complex
   Usage
    L = chainComplexData C
   Inputs
    C:ChainComplex
   Outputs
    L:List
   Description
    Text
     Output is a list with elements min C, max C and a sublist of the differentials of C     
    Example
     S=ZZ[x,y]/ideal(x*y)
     C=(chainComplex(matrix{{x}},matrix{{y^2}},matrix{{x^2}}))[3]
     chainComplexData C
   SeeAlso
     chainComplexFromData
///
doc ///
   Key
    chainComplexFromData
    (chainComplexFromData, List)
    (chainComplexFromData, ZZ, List)
   Headline
    constructs a ChainComplex from a list of data, with optional shift
   Usage
    C = chainComplexFromData(m, L)
   Inputs
    m:ZZ
    L:List
   Outputs
    C:ChainComplex
   Description
    Text
     Without the optional argument m this is the inverse of chainComplexData; L should 
     be a list in the form {ZZ, ZZ, List}, where the first element represents the
     desired min, and the last element the list of differentials.
     If m is present, then the form is a simple list of differentials, and m
     becomes the minimal degree of C.
    Example
     S=ZZ[x,y]/ideal(x*y)
     C=(chainComplex(matrix{{x}},matrix{{y^2}},matrix{{x^2}}))[3]
     L = chainComplexData C
     C == chainComplexFromData L
     C == chainComplexFromData(-3,L_2)
   SeeAlso
    chainComplexData
///
*-

doc ///
   Key
    scarfComplex
    (scarfComplex, MonomialIdeal)
   Headline
    constructs the algebraic Scarf complex of a monomial ideal
   Usage
    C = scarfComplex I
   Inputs
    I:MonomialIdeal
   Outputs
    C:ChainComplex
   Description
    Text
     The algebraic Scarf complex of a monomial ideal is the sub-chain complex of the 
     @ TO taylorResolution@ supported on subsets of generators with unique LCMs.
    Example
     R = QQ[a,b,c,d,e];
     I = monomialIdeal(b^4*c^3, a*b^3*c*d^2*e, a*b^2*c^2*d*e^2, a^2*d^3*e^5, b*c^2*d^5*e^4);
     s = scarfComplex I
     s.dd
    Text
     The Scarf complex of I is always a subcomplex of the minimal free resolution of I, 
     computed in M2 with the command {\tt res I}. In this first example the Scarf complex
     is strictly smaller.
    Example
     (betti s, betti res I)
    Text
     In some cases, such as when I is a generic monomial ideal, the Scarf complex of I     
     is a minimal free resolution of I. In this case {\tt scarfComplex I} and {\tt res I} will be
     isomorphic but not necessarily equal.
    Example 
     I = monomialIdeal(a^2*b^11*c^7*d*e, a^5*b^10*c^2*d^3*e^2, a^6*b^8*c^11*d^2*e^3, a^3*b^5*c^3*d^5*e^4, a^8*b^2*c*d^4*e^7);
     isExact(prependZeroMap scarfComplex I)
     isMinimalChainComplex scarfComplex I
     betti scarfComplex I == betti res I  
     scarfComplex I == res I
    Text 
     See @TO "chain complexes"@ for an overview of chain complexes in Macaulay2.
   SeeAlso
    (resolution, Ideal)
    taylorResolution
///

TEST///-- Tests for scarfComplex

-- For each one we:
-- Check that the ranks are correct 
-- Check that the maps are defined right so we actually have a chain complex
-- Check that the twists are correct in each hom degree, by checking that the betti keys are a subset of the MFR's betti keys
R = QQ[a,b,c,d,e];
I = monomialIdeal(a^2*b^11*c^7,a^5*b^10*c^2*d^2*e,a^5*b^8*c^4*d*e^2,a^2*b^5*c^4*d^5*e^4,a^8*b^2*d^3*e^7);
SC1 = scarfComplex(I);
assert(apply(1 + length SC1, i -> rank SC1_i) == {1, 5, 8, 3})
assert(isChainComplex SC1)
assert(isSubset(keys betti SC1, keys betti res I))

I2 = monomialIdeal apply( {{6,2,6,6,0}, {2,4,6,8,0}, {2,13,1,3,1}, {3,4,2,10,1}, {3,11,2,1,3}, {1,3,6,5,5}, {9,3,1,0,7}}, i -> R_i);
SC2 = scarfComplex(I2);
assert(apply(1 + length SC2, i -> rank SC2_i) == {1, 7, 17, 15, 4})
assert(isChainComplex SC2)
assert(isSubset(keys betti SC2, keys betti res I2))

I3 = monomialIdeal apply( {{4,14,2,0,0}, {3,14,0,2,1}, {0,12,0,4,4}, {3,4,7,0,6}, {2,7,1,4,6}, {0,0,4,9,7}}, i -> R_i);
SC3 = scarfComplex(I3);
assert(apply(1 + length SC3, i -> rank SC3_i) == {1, 6, 9, 3})
assert(isChainComplex SC3)
assert(isSubset(keys betti SC3, keys betti res I3))
///

TEST///
-- The next ideal is strongly generic so the Scarf resolution needs to be a minimal free resolution.
R = QQ[a,b,c,d,e];
I = monomialIdeal(a^2*b^11*c^7*d*e, a^5*b^10*c^2*d^3*e^2, a^6*b^8*c^11*d^2*e^3, a^3*b^5*c^3*d^5*e^4, a^8*b^2*c*d^4*e^7);
SC = scarfComplex(I);
assert(isExact prependZeroMap SC)
///

TEST/// --using a multigraded ring
R = QQ[a,b,c,d,e, Degrees => entries id_(ZZ^5)];
I = monomialIdeal(a^2*b^11*c^7,a^5*b^10*c^2*d^2*e,a^5*b^8*c^4*d*e^2,a^2*b^5*c^4*d^5*e^4,a^8*b^2*d^3*e^7);
SC1 = scarfComplex(I);
assert(apply(1 + length SC1, i -> rank SC1_i) == {1, 5, 8, 3})
assert(isChainComplex SC1)
assert(isSubset(keys betti SC1, keys betti res I))

I2 = monomialIdeal apply( {{6,2,6,6,0}, {2,4,6,8,0}, {2,13,1,3,1}, {3,4,2,10,1}, {3,11,2,1,3}, {1,3,6,5,5}, {9,3,1,0,7}}, i -> R_i);
SC2 = scarfComplex(I2);
assert(apply(1 + length SC2, i -> rank SC2_i) == {1, 7, 17, 15, 4})
assert(isChainComplex SC2)
assert(isSubset(keys betti SC2, keys betti res I2))

I3 = monomialIdeal apply( {{4,14,2,0,0}, {3,14,0,2,1}, {0,12,0,4,4}, {3,4,7,0,6}, {2,7,1,4,6}, {0,0,4,9,7}}, i -> R_i);
SC3 = scarfComplex(I3);
assert(apply(1 + length SC3, i -> rank SC3_i) == {1, 6, 9, 3})
assert(isChainComplex SC3)
assert(isSubset(keys betti SC3, keys betti res I3))

I = monomialIdeal(a^2*b^11*c^7*d*e, a^5*b^10*c^2*d^3*e^2, a^6*b^8*c^11*d^2*e^3, a^3*b^5*c^3*d^5*e^4, a^8*b^2*c*d^4*e^7);
mSC = scarfComplex(I);
assert(betti mSC == betti res I)
assert(isExact prependZeroMap mSC)
///

TEST///-- Make sure scarfComplex handles zero ideal the same way res does
R = QQ[x,y,z];
K = monomialIdeal (0_R);
assert (scarfComplex K == res K) 
///

TEST///-- Make sure scarfComplex handles improper ideal the same way res does
R = QQ[x,y,z,w];
L = monomialIdeal (1_R);
assert (scarfComplex L == res L)
///


TEST///
C = QQ^10[-1]
C' = appendZeroMap C
C'' = prependZeroMap C'
assert(C''_0 == 0 and C''_1 == QQ^10 and C''_2 == 0)
///

TEST///
kk= ZZ/101
S = kk[a,b,c]

R = S/ideal(a^3)
M = R^1/ideal(a)
C = chainComplex{map(M,R^0,0)}
source (m=cartanEilenbergResolution (C, LengthLimit => 10))
source (n =resolutionOfChainComplex (C, LengthLimit => 10))
assert (isQuasiIsomorphism(m, LengthLimit=> 10))
assert(not isQuasiIsomorphism(m, LengthLimit => 12))
assert(isQuasiIsomorphism(n, LengthLimit=> 10))
m = resolutionOfChainComplex (C[3])
assert(target m == C[3])

use S
M0  = coker matrix"a4, b4,ab"
M1 = S^{ -1}**M0; M2 = S^{ -1}**M1
phi1 = map(M0,M1,matrix"a"); phi2 = map(M1,M2,matrix"b")
C = chainComplex{phi1,phi2}
C = koszul gens (ideal vars S)^2

time m = resolutionOfChainComplex C;
time n = cartanEilenbergResolution C;
assert(C == source m)
assert (C == target n)
assert (isQuasiIsomorphism n)
///

-*
TEST ///
S=ZZ[x,y]/ideal(x*y)
C=(chainComplex(matrix{{x}},matrix{{y^2}},matrix{{x^2}}))[3]
isHomogeneous C
L=chainComplexData C
C'=chainComplexFromData L
assert(C'== C)
///
*-

TEST///
S = ZZ/32003[a,b]
red = map(S,S,toList(numgens S:0_S))
C = koszul gens (ideal vars S)^2
G = S^{0,-1,-2,-3,-4,-5,-6}
D = apply(length C+1, i-> C_i++G++G)
zG = map(G,G,0)
difs0 = apply(length C, 
    i-> 
    map(D_i, D_(i+1), matrix{
	    {C.dd_(i+1),map(C_i,G,0), map(C_i,G,0)},
	    {map(G,C_(i+1),0),zG,zG},
	    {map(G,C_(i+1),0),id_G,zG}}
	)
);
len = #difs0
Q = apply(len, i-> random(target difs0_i, target difs0_i))|
       {random(source difs0_(len-1), source difs0_(len-1))};
difs1 = apply(len, i-> Q_i*difs0_i*Q_(i+1)^(-1));
E = chainComplex difs1
assert(isMinimalChainComplex E == false)
m = minimize (E[1]);
assert (isQuasiIsomorphism m)
assert (E[1] == source m)
E' = target m
assert (isChainComplex E'==true)
assert(isMinimalChainComplex E' == true)
///    	    

TEST///
kk= ZZ/101
S = kk[a,b,c,d]

R = S/ideal(a^3)
M = R^1/ideal(a)
C = chainComplex{map(M,R^0,0)}
m=cartanEilenbergResolution (C, LengthLimit => 10)
n =resolutionOfChainComplex (C, LengthLimit => 10)
assert (isQuasiIsomorphism(m, LengthLimit=> 10))
assert(not isQuasiIsomorphism(m, LengthLimit => 12))
assert(isQuasiIsomorphism(n, LengthLimit=> 10))
m = resolutionOfChainComplex (C[3])
assert(target m == C[3])
assert(isChainComplexMap m)

use S
M0  = coker matrix"a4, b4,ab"
M1 = S^{ -1}**M0; M2 = S^{ -1}**M1
phi1 = map(M0,M1,matrix"a"); phi2 = map(M1,M2,matrix"b")

C = koszul gens (ideal vars S)^2
m = resolutionOfChainComplex C;
n = cartanEilenbergResolution C;
assert(C == source m)
assert (C == target n)
assert (isQuasiIsomorphism n)
assert (isExact cone n)
C = chainComplex{phi1,phi2}
m = resolutionOfChainComplex C;
n = cartanEilenbergResolution C;
assert(C == target m)
assert (C == target n)

assert (isQuasiIsomorphism n)
assert (isQuasiIsomorphism m)
///

TEST///
S = ZZ/101[a,b]
R=S/ideal"a3,a2+b2"
F = res coker vars R
assert(isExact F == false)
assert (isExact chainComplex{F.dd_3, b*syz F.dd_3} == false)
assert(isChainComplex substitute(F,S) == false)
///

TEST///
S = ZZ/101[a,b,c]
i = monomialIdeal"ab4,a2b3,abc2"
j = ideal"ab4,a2b3,abc2"
T = taylorResolution i
Tr = res i
Tr.dd_2 
T' = chainComplex({map(S^1/j,S^1,1)}|apply(3, i->(-1)*T.dd_(i+1)))[1]
assert(isExact T')
assert(
    taylor(2,i)==map(T_1,T_2,
	matrix {{-b, -c^2, 0}, {a, 0, -c^2}, {0, a*b^2, b^3}})
	    )
assert ((taylorResolution i).dd_2 == taylor(2,i))
assert(koszulComplex i == koszul gens i)
assert(true ==
    isChainComplexMap chainComplexMap(
	T,T,apply(toList(min T..max T), i->id_(T_i))))
assert(T == trivialHomologicalTruncation (T,0,3))
assert (T != trivialHomologicalTruncation (T,1,4))

assert (T == trivialHomologicalTruncation (T', 0, 3))

T'' = prependZeroMap (T[-1])
assert (1 ==nonzeroMin T'')

--NOTE: the following should return "true" but instead creates
--an error
--Actually if a "chainComplexMap is one for which the cone is a complex, this is NOT an error.
--phi = chainComplexMap(T',T, apply(toList(min T..max T), i->id_(T_i)))
--phi1 = chainComplexMap(T'[1],T[1], apply(toList(min (T[1])..max (T[1])), i->id_((T[1])_i)))
--(cone phi).dd

phi = chainComplexMap(T', prependZeroMap T, apply(toList(min T..max T), i->id_(T_i)), InitialDegree => 0)
assert(isChainComplexMap phi)

S = ZZ/101[t]
T = chainComplex(map(S^1,S^1,t))
T' = chainComplex{map(S^1/(ideal t),S^1, 1), map(S^1,S^1, -t)}[1]
phi = chainComplexMap(T',T,apply(toList(min T..max T), i->id_(T_i)))
assert(isChainComplexMap phi == true)
///

TEST///
       E=ZZ/101[e_0,e_1,SkewCommutative=>true];F=res ideal vars E;
       C=dual res (coker transpose F.dd_3,LengthLimit=>8)[-3]
       C1=trivialHomologicalTruncation(C,-2,2)
       C2=trivialHomologicalTruncation(C1,-3,3)
assert(nonzeroMin C2 == -2)
assert(nonzeroMax C2 == 2)
assert(max C2 == 4)
       C3=removeZeroTrailingTerms C2
assert(max C3 == 2)
assert(max appendZeroMap C3 == 3)
///

end--

restart
uninstallPackage "ChainComplexExtras"
installPackage "ChainComplexExtras"
check "ChainComplexExtras"
viewHelp ChainComplexExtras




