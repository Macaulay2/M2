-- -*- coding: utf-8 -*-
needsPackage "Polyhedra"
newPackage(
     "NormalToricVarieties",
     AuxiliaryFiles => true,
     Version => "0.76",
     Date => "4 December 2009",
     Authors => {{
	       Name => "Gregory G. Smith", 
	       Email => "ggsmith@mast.queensu.ca", 
	       HomePage => "http://www.mast.queensu.ca/~ggsmith"}},
     Headline => "normal toric varieties",
     DebuggingMode => true
     )

export { 
     NormalToricVariety, 
     normalToricVariety, 
     projectiveSpace, 
     hirzebruchSurface, 
     weightedProjectiveSpace, 
     kleinschmidt,
     anticanonicalDivisor,
     classGroup,
     isAmple,
     isCartier,
     isQQCartier,
     isDegenerate,
     isFano,
     isProjective,
     isSimplicial,
     latticeIndex,
     makeSimplicial,
     makeSmooth,
     resolveSingularities,
     weilDivisors, 
     cartierDivisors, 
     cartierToPicard,
     cartierToWeil, 
     picardGroup,
     picardToClass,
     nef,
     smoothFanoToricVariety,
     WeilToClass,
     weilToClass,
     insertCone
     }
--------------------------------------------------------------------------------
needsPackage "FourierMotzkin"
needsPackage "Polyhedra"

--------------------------------------------------------------------------------
-- CODE
--------------------------------------------------------------------------------
NormalToricVariety = new Type of Variety
NormalToricVariety.synonym = "normal toric variety"
NormalToricVariety.GlobalAssignHook = globalAssignFunction
NormalToricVariety.GlobalReleaseHook = globalReleaseFunction

expression NormalToricVariety := X -> new FunctionApplication from { 
     normalToricVariety, Adjacent{rays X, Adjacent{",",max X}}}

-- The methods 'rays' is defined in 'Polyhedra'
-- rays = method(TypicalValue => List)
rays NormalToricVariety := List => X -> X.rays
max  NormalToricVariety := List => X -> X.max

normalToricVariety = method(
     TypicalValue => NormalToricVariety, 
     Options => {
	  CoefficientRing => QQ,
	  Variable => symbol x,	  
	  WeilToClass => null})

normalToricVariety (List, List) := opts -> (V,F) -> (
     X := new NormalToricVariety from {
	  symbol rays => V,
	  symbol max => F,
	  symbol cache => new CacheTable};
     if opts.WeilToClass =!= null then X.cache.weilToClass = opts.WeilToClass;
     X.cache.CoefficientRing = opts.CoefficientRing;
     X.cache.Variable = opts.Variable;
     return X)


projectiveSpace = method()
projectiveSpace ZZ := NormalToricVariety => d -> (
     if d < 1 then (
	  error "-- expected a positive integer");
     
     V := {toList(d:-1)} | entries id_(ZZ^d);
     F := subsets(d+1,d);
     X := normalToricVariety(V,F);
     return X)


hirzebruchSurface = method()
hirzebruchSurface ZZ := NormalToricVariety => a -> (
     V := {{1,0},{0,1},{-1,a},{0,-1}};
     F := {{0,1},{1,2},{2,3},{0,3}};
     W := matrix{{1,-a,1,0},{0,1,0,1}};
     X := normalToricVariety(V,F, WeilToClass => W);
     return X)


weightedProjectiveSpace = method()
weightedProjectiveSpace List := NormalToricVariety => q -> (
     if not all(q, i -> i > 0) then (
	  error "-- expected positive integers");
     d := #q-1;
     if not all(subsets(q,d), s -> gcd s === 1) then (
	  error ("-- " | toString d | "-elements have a common factor"));

     V := entries kernelLLL matrix {q};
     F := subsets(d+1,d);
     
     X := normalToricVariety(V,F);
     return X)


NormalToricVariety ** NormalToricVariety := NormalToricVariety => (X,Y) -> (
     V1 := transpose matrix rays X;
     V2 := transpose matrix rays Y;
     V := entries transpose (V1 ++ V2);
     
     F1 := max X;
     F2 := max Y;
     n := #rays X;
     F2 = apply(F2, s -> apply(s, i -> i+n));
     F := flatten table(F1,F2, (s,t) -> s|t);
     
     XY := normalToricVariety(V,F);
     return XY)


kleinschmidt = method()
kleinschmidt (ZZ,List) := NormalToricVariety => (d,a) -> (
     if d < 0 then (
	  error "-- expected a nonnegative integer");
     r := #a;
     s := d-r+1;
     e := entries id_(ZZ^d);
     if r >= d then (
	  error "-- list is too long"); 
     
     V := apply(r, i -> e#i) | {sum(r, i -> -e#i)};
     V = V | apply(s-1, j -> e#(r+j));
     V = V | {sum(r, i -> a#i*e#i)- sum(s-1, j -> e#(r+j))};
     
     L := toList(0..r+s);
     F := flatten table(toList(0..r),toList(r+1..r+s), 
	  (i,j) -> select(L, k -> i =!= k and j =!= k));
     
     X := normalToricVariety(V,F);
     return X)


-- This local function creates a HashTable with the defining data for the low
-- dimensional smooth Fano toric varieties.
file := currentFileDirectory | "NormalToricVarieties/smoothFanoToricVarieties.txt"
getFano := memoize(
     () -> (
	  if notify then stderr << "--loading file " << file << endl;
	  hashTable apply( lines get file,
	       x -> (
	       	    x = value x;
	       	    ((x#0,x#1),drop(x,2))))))

smoothFanoToricVariety = method()
smoothFanoToricVariety (ZZ,ZZ) := NormalToricVariety => (d,i) -> (
     if d < 1 or i < 0 then (
	  error "-- expected positive dimension or nonnegative index")
     else if d === 1 and i > 0 then (
	  error "-- there is only one smooth Fano toric curve")
     else if d === 2 and i > 4 then (
	  error "-- there are only five smooth Fano toric surfaces")
     else if d === 3 and i > 17 then (
	  error "-- there are only 18 smooth Fano toric 3-folds")
     else if d === 4 and i > 123 then (
	  error "-- there are only 124 smooth Fano toric 4-folds")
     else if d > 4 then (
	  error "-- database doesn't include varieties with dimension > 4")
     else if i === 0 then return projectiveSpace d
     else (
	  s := (getFano())#(d,i);
	  X := normalToricVariety(s#0,s#1, WeilToClass => transpose matrix s#2);
	  return X))


dim NormalToricVariety := ZZ => (cacheValue symbol dim)(X -> #(rays X)#0)
--??
dim (List,NormalToricVariety):= ZZ => (sigma,X) -> (
	if (not X.cache.?cones) then X.cache.cones = new MutableHashTable;
	if (not X.cache.cones#?sigma) then (
		X.cache.cones#sigma = if #sigma>1 then (
			N := (smithNormalForm matrix (rays X)_sigma)_0;
			(rank N,  product toList apply(rank N, i-> N_(i,i)))
			) else (1,1));
	X.cache.cones#sigma#0)


--??
latticeIndex = method()
latticeIndex (List,NormalToricVariety):= (sigma,X) ->(
	if (not X.cache.?cones) then X.cache.cones = new MutableHashTable;
	if (not X.cache.cones#?sigma) then ( 
		X.cache.cones#sigma = if #sigma>1 then (
			N := (smithNormalForm matrix (rays X)_sigma)_0;
			(rank N,  product toList apply(rank N, i-> N_(i,i)))
			) else (1,1));
	X.cache.cones#sigma#1)


isWellDefined NormalToricVariety := Boolean => X -> (
     V := rays X;
     F := max X;
     m := #F;
     flag := true;
     dualCones := new MutableHashTable;
     -- check whether every ray appears in some maximal cone
     if set toList(0..#V-1) =!= set flatten F then (
	  if debugLevel > 0 then (
	       << "-- some ray does not appear in maximal cone" << endl);
	  flag = false);
     
     -- loop over all maximal cones
     if flag === true then for i to m-1 do (
	  f := F#i;
	  -- check whether the rays in the cone have the same length
	  try C := transpose matrix apply(f, i -> V#i) else (
	       if debugLevel > 0 then (
		    << "-- not all rays have the same length" << endl);
	       flag = false; 
	       break);
	  -- check whether the rays are lists of integers
	  if ZZ =!= ring C then (
	       if debugLevel > 0 then (
		    << "-- not all rays are lists of integers" << endl);
	       flag = false; 
	       break);
	  H := fourierMotzkin C;
     	  dualCones#i = H#0 | H#1 | -H#1;
	  (C',L) := fourierMotzkin H;
	  -- check whether the maximal cone is strongly convex
	  if L != 0 then (
	       if debugLevel > 0 then (
		    << "-- not all maximal cones are strongly convex" << endl);
	       flag = false; 
	       break);
	  -- check whether the rays are the primitive generators of the cone
	  if set entries transpose C' =!= set entries transpose C then (
	       if debugLevel > 0 then (
		    << "-- the rays are not the primitive generators" << endl);
	       flag = false; 
	       break));
     
     -- check whether the intersection of each pair of maximal cones is a cone
     if flag === true then for i to m-2 do (
	  for j from i+1 to m-1 do (
	       C := set apply(toList(set(F#i)*set(F#j)), k -> V#k);	       
     	       (C',L) := fourierMotzkin (dualCones#i | dualCones#j);
	       if C =!= set entries transpose C' then (
		    if debugLevel > 0 then (
			 << "-- intersection of cones is not a cone" << endl);
		    flag = false; 
		    break)));

     return flag)


isAmple = method()
isAmple (List,NormalToricVariety) := Boolean => (D,X) -> (
     if not X.cache.?isAmple then X.cache.isAmple = new MutableHashTable;
     if not X.cache.isAmple#?D then (
	  X.cache.isAmple#D = isCartier(D,X) and isComplete X and (
	       complementSelect := (L,ind) -> L_(sort toList(set(0..#L-1) - set ind));
	       M := - X.cache.isQQCartier#D;
	       all(#(max X), i -> (
		    	 C := (max X)#i;
		    	 m := M#i;
		    	 R := complementSelect(rays X,C);
		    	 a := - complementSelect(D,C);
		    	 all(#R, j -> ((matrix{R#j} * m) - a#j)_(0,0) > 0)))));
     X.cache.isAmple#D)


isVeryAmple (List,NormalToricVariety) := Boolean => (D,X) -> (
     if not X.cache.?isVeryAmple then X.cache.isVeryAmple = new MutableHashTable;
     if not X.cache.isVeryAmple#?D then X.cache.isVeryAmple#D = isAmple(D,X) and isVeryAmple polytope(D,X);
     X.cache.isVeryAmple#D)


isCartier = method()
isCartier (List,NormalToricVariety) := Boolean => (D,X) -> (
     if not X.cache.?isCartier then X.cache.isCartier = new MutableHashTable;
     if not X.cache.isCartier#?D then X.cache.isCartier#D = isQQCartier(D,X) and all(X.cache.isQQCartier#D, m -> liftable(m,ZZ));
     X.cache.isCartier#D)

isQQCartier = method()
isQQCartier (List,NormalToricVariety) := Boolean => (D,X) -> (
     if not X.cache.?isQQCartier then X.cache.isQQCartier = new MutableHashTable;
     if not X.cache.isQQCartier#?D then (
	  systemSolver := (R,F) -> (
     	       (R1,Lmatrix,Rmatrix) := smithNormalForm lift(R,ZZ);
     	       F1 := entries(Lmatrix * F);
     	       Rmatrix * (matrix apply(numRows R1, i -> F1#i / R1_(i,i)) || map(QQ^(numColumns R1 - numRows R1),QQ^(#(F1#0)),0)));
     	  X.cache.isQQCartier#D = for C in max X list (
	       U := matrix((rays X)_C);
	       a := transpose matrix {D_C};
	       n := numColumns U;
	       print(U,a);
	       m := systemSolver(if numRows U > n then (U^{0..n-1},a^{0..n-1}) else (U,a));--^{0..n-1},a^{0..n-1});
	       if U*m-a != 0 then break{} else m));
     X.cache.isQQCartier#D != {})


isDegenerate = method()
isDegenerate NormalToricVariety := Boolean => (cacheValue symbol isDegenerate)(
     X -> kernel matrix rays X != 0)


isFano = method()
isFano NormalToricVariety := Boolean => (cacheValue symbol isFano)(
     X -> (
	  D := anticanonicalDivisor X;
	  isCartier(D,X) and isAmple(D,X)))     


   
isSimplicial = method()
isSimplicial NormalToricVariety := Boolean => (cacheValue symbol isSimplicial)(
     X -> (
     	  V := transpose matrix rays X;
     	  return all(max X, s -> #s == rank V_s)))

     
isSmooth NormalToricVariety := Boolean => (cacheValue symbol isSmooth)(
     X -> (
     	  V := transpose matrix rays X;
     	  b := all(max X, s -> #s == rank V_s and 1 == minors(#s,V_s));
	  if b == true then X.cache.simplicial = true;
	  return b))


anticanonicalDivisor = method()
anticanonicalDivisor NormalToricVariety := List => X -> toList(#(rays X):1)



classGroup = method()
classGroup NormalToricVariety := Module => (cacheValue symbol classGroup)(
     X -> (
	  rawC := cokernel matrix rays X;
	  C := prune rawC;
	  
	  -- We also compute the map to the group of Weil divisors
	  W := weilDivisors X;
	  local A;
	  if X.cache.?weilToClass then A = matrix X.cache.weilToClass
	  else A = map(C, W, matrix (C.cache.pruningMap)^-1);
	  X.cache.weilToClass = map(C,W,A);
	  
     	  return C))


weilToClass = method()
weilToClass NormalToricVariety := Matrix => X -> (
     if not X.cache.?classGroup then classGroup X;
     return X.cache.weilToClass)


weilDivisors = method()
weilDivisors NormalToricVariety := Module => (cacheValue symbol weilDivisors)(
     X -> ZZ^(#rays X))


cartierDivisors = method()
cartierDivisors NormalToricVariety := Module => (cacheValue symbol CDiv)(
     X -> (
	  local CDiv;
	  if isSmooth X then (
	       CDiv = weilDivisors X;
	       X.cache.cartierToWeil = id_CDiv;
	       return CDiv)
	  else (
	       V := transpose matrix rays X;
	       F := max X;
	       d := dim X;
	       n := #rays X;
	       
	       H1 := new HashTable from (
		    apply(F, s -> {s, coker (fourierMotzkin V_s)#1}));
	       H2 := new HashTable from (
		    flatten apply(toList(0..#F-1), 
     	       	    	 i -> apply(toList(i+1..#F-1), 
			      j -> (
	       		      	   s := select(F#i, k -> member(k,F#j));
	       		      	   if #s > 0 then (
					M := coker (fourierMotzkin V_s)#1;
					{(F#i,F#j), M})))));
	       K := keys H1;
	       P1 := directSum apply(K, k -> k => H1#k);
	       local D;
	       if #keys H2 == 0 then D = ker map(ZZ^0,P1,0) 
	       else (
     		    P2 := directSum apply(keys H2, k -> k => H2#k);
     		    M := transpose matrix table(K, keys H2, 
			 (j,k) -> if j == k#0 then 1 
     	  		 else if j == k#1 then -1 else 0);
     		    D = kernel map(P2,P1,M ** id_(ZZ^d)));
	       CDiv = prune D;
	       
	       L := apply(n, i -> position(K, s -> member(i,s)));
	       inc := matrix table(n,keys H1, 
		    (i,s) -> if s == K#(L#i) then 1 else 0);
	       
	       -- We also compute the map to the group of Weil divisors
	       local iota;
	       iota = inc^{0} ** transpose V_{0};
	       scan(#L - 1, i -> iota = iota || inc^{i+1} ** transpose V_{i+1});
	       iota = map(weilDivisors X, D, iota * gens D);
	       eta := CDiv.cache.pruningMap;
	       X.cache.cartierToWeil = map(weilDivisors X, CDiv, iota * eta);
	       
	       return CDiv)))


cartierToWeil = method()
cartierToWeil NormalToricVariety := Matrix => X -> (
     if not X.cache.?cartierToWeil then cartierDivisors X;
     return X.cache.cartierToWeil)


picardGroup = method()
picardGroup NormalToricVariety := Module => (cacheValue symbol picardGroup)(
     X -> (
	  local C;
	  if isSmooth X then (
	       C = classGroup X;
	       X.cache.picardToClass = id_C;
	       X.cache.cartierToPicard = weilToClass X;
	       return C)
	  else (
               V := rays X;
               d := dim X;
               phi := map(weilDivisors X, ZZ^d, matrix V);
               psi := cartierToWeil X;
	       rawP := subquotient(psi,phi);
	       P := prune rawP;
	       iota := P.cache.pruningMap;
	       X.cache.cartierToPicard = map(P,cartierDivisors X, iota^-1);
	       C = cokernel matrix rays X;
	       theta := inducedMap(C,rawP);
	       eta := map(classGroup X, C, matrix weilToClass X);
               X.cache.picardToClass = eta * theta * iota;
	       return P)))


picardToClass = method()
picardToClass NormalToricVariety := Matrix => X -> (
     if not X.cache.?picardToClass then picardGroup X;
     return X.cache.picardToClass)


cartierToPicard = method()
cartierToPicard NormalToricVariety := Matrix => X -> (
     if not X.cache.?cartierToPicard then picardGroup X;
     return X.cache.cartierToPicard)
     

nef = method()
nef NormalToricVariety := List => X -> (
     if not isSimplicial X then (
	  error "-- not yet implemented for non-simplicial toric varieties");
     B := ideal X;
     A := transpose matrix degrees ring X;
     outer := 0 * A_{0};
     scan(B_*, m -> outer = outer | (fourierMotzkin A_(apply(support(m), z -> index z)))#0 );
     entries transpose ((fourierMotzkin outer)#0)^{0..(#rays X - dim X-1)})



ring NormalToricVariety := PolynomialRing => (cacheValue symbol ring)(
     X -> (
	  if isDegenerate X then (
	       error "-- not yet implemented for degenerate varieties");
	  if not isFreeModule classGroup X then (
	       error "-- gradings by torsion groups not yet implemented");

	  K := X.cache.CoefficientRing;	  
	  x := X.cache.Variable;	  
	  n := #rays X;
	  deg := entries transpose matrix weilToClass X;
	  return K(monoid[x_0..x_(n-1), Degrees => deg])))


ideal NormalToricVariety := Ideal => (cacheValue symbol ideal)(
     X -> (
	  S := ring X;
	  n := numgens S;
	  return ideal apply(max X, L -> product(n, 
		    i -> if member(i,L) then 1_S else S_i))))

monomialIdeal NormalToricVariety := MonomialIdeal => X -> monomialIdeal ideal X



sheaf (NormalToricVariety,Module) := CoherentSheaf => (X,M) -> (
     if ring M =!= ring X then (
	  error "-- expected module and variety to have the same ring");
     if not isHomogeneous M then (
	  error "-- expected a homogeneous module");
     
     new CoherentSheaf from {
	  symbol module => M,
	  symbol variety => X})


sheaf (NormalToricVariety,Ring) := SheafOfRings => (X,R) -> (
     if ring X =!= R then (
	  error "-- expected the ring of the variety");
     if not X.cache.?structureSheaf then (
     	  X.cache.structureSheaf = new SheafOfRings from { 
	       symbol variety => X, 
	       symbol ring    => R });
     X.cache.structureSheaf)

sheaf NormalToricVariety := X -> sheaf_X ring X

installMethod(symbol _, OO, NormalToricVariety, (OO,X) -> sheaf(X, ring X))

CoherentSheaf Sequence := CoherentSheaf => (F,a) -> sheaf(variety F, 
     F.module ** (ring F)^{toList(a)})

SheafOfRings Sequence := CoherentSheaf => (O,a) -> O^1 a

super   CoherentSheaf := CoherentSheaf => F -> sheaf(variety F, super   module F)
ambient CoherentSheaf := CoherentSheaf => F -> sheaf(variety F, ambient module F)
cover   CoherentSheaf := CoherentSheaf => F -> sheaf(variety F, cover   module F)


-- This local function creates a HashTable describing the cohomology of all 
-- twists of the structure sheaf.  For more information, see Propositon~3.2 
-- in Maclagan-Smith "Multigraded regularity"
setupHHOO = X -> (
     X.cache.emsBound = new MutableHashTable;

     -- create a fine graded version of the total coordinate ring
     S := ring X;
     n := numgens S;
     fineDeg := entries id_(ZZ^n);
     h := toList(n:1);
     R := QQ(monoid [gens S, Degrees => fineDeg, Heft => h]);
     RfromS := map(R, S, gens R);
     B := RfromS ideal X;
     
     -- use simplicial cohomology find the support sets 
     quasiCech := Hom( res(R^1/B), R^1);
     supSets := delete({},subsets(toList(0..n-1)));
     d := dim X;
     sigma := new MutableHashTable;
     sigma#0 = {{}};
     for i from 1 to d do (
	  sHH := prune HH^(i+1)(quasiCech);
	  sigma#i = select(supSets, s -> (
		    m := product(s, j -> R_j);
		    basis(-degree m, sHH) != 0)));
     
     -- create rings
     degS := degrees S; 
     X.cache.rawHHOO = new HashTable from (
	  apply(d+1, i -> {i, apply(sigma#i, s -> (
		    v := - degree product(n, 
			 j -> if member(j,s) then S_j else 1_S);
		    degT := apply(n, 
			 j -> if member(j,s) then -degS#j else degS#j);
		    T := (ZZ/2)(monoid [gens S, Degrees => degT]);
		    {v,T,s}))})))


-- This local function defines the Frobenius power of an ideal
Ideal ^ Array := (I,p) -> ideal apply(I_*, i -> i^(p#0))


-- This local function creates a HastTable which stores the data for determining
-- the appropriate Frobenius power needed to compute the cohomology of a general
-- coherent sheaf; see Proposition 4.1 in Eisenbud-Mustata-Stillman.
emsbound = (i,X,deg) -> (
     if not X.cache.emsBound#?{i,deg} then (
	  if i < 0 or i > dim X then X.cache.emsBound#{i,deg} = 1
	  else X.cache.emsBound#{i,deg} = max ( {1} | apply(X.cache.rawHHOO#i, 
	       t -> #t#2 + max apply(first entries basis(deg-t#0,t#1),
		    m -> max (first exponents m)_(t#2)))));
     X.cache.emsBound#{i,deg})

cohomology (ZZ,NormalToricVariety,CoherentSheaf) := Module => opts -> (i,X,F) -> (
     if ring F =!= ring X then (
	  error "-- expected a coherent sheaf on the toric variety");
     S := ring X;
     kk := coefficientRing S;
     if not isField kk then (
	  error "-- expected a toric variety over a field");
     if i < 0 or i > dim X then kk^0
     else (
     	  if not X.cache.?rawHHOO then setupHHOO X;
     	  M := module F;
     	  if isFreeModule M then kk^(
	       sum apply(degrees M, deg -> sum apply(X.cache.rawHHOO#i,
		    	 t -> rank source basis(-deg-t#0,t#1))))
     	  else (
	       B := ideal X;
	       C := res M;
	       deg := toList(degreeLength S : 0);
	       bettiNum := flatten apply(1+length C, 
	  	    j -> apply(unique degrees C_j, alpha -> {j,alpha}));
	       b1 := max apply(bettiNum, 
		    beta -> emsbound(i+beta#0-1,X,deg-beta#1));
     	       b2 := max apply(bettiNum, 
			 beta -> emsbound(i+beta#0,X,deg-beta#1));	       
	       b := max(b1,b2);
     	       if i > 0 then kk^(rank source basis(deg, Ext^(i+1)(S^1/B^[b],M)))
	       else (
     	  	    h1 := rank source basis(deg, Ext^(i+1)(S^1/B^[b],M));
     	  	    h0 := rank source basis(deg, Ext^i(S^1/B^[b1],M));
     	  	    kk^(rank source basis(deg,M) + h1 - h0)))))

cohomology (ZZ,NormalToricVariety,SheafOfRings) := Module => opts -> (
     (i,X,O) -> HH^i(X,O^1))


--------------------------------------------------------------------------------
-- code that interfaces with the Polyhedra package

normalToricVariety Fan := opts -> F -> (
     R := rays F;
     Fs := apply(maxCones F, C -> (Cr:=rays C; Cr = set apply(numColumns Cr, i -> Cr_{i}); positions(R,r -> Cr#?r)));
     X := new NormalToricVariety from {
	  symbol rays => apply(R, r -> flatten entries r),
	  symbol max => Fs,
	  symbol cache => new CacheTable};
     X.cache.isFan = true;
     X.cache.Fan = F;
     X)

normalToricVariety Polyhedron := P -> normalToricVariety normalFan P


isComplete NormalToricVariety := X -> (
     if not X.cache.?Fan or not X.cache.?isComplete then X.cache.isComplete = isComplete fan X;
     X.cache.isComplete)

isPure NormalToricVariety := X -> (
     if not X.cache.?Fan or not X.cache.?isPure then X.cache.isPure = isPure fan X;
     X.cache.isPure)

isProjective = method(TypicalValue => Boolean)
isProjective NormalToricVariety := X -> (
     if not X.cache.?isFan then isFan X;
     if not X.cache.isFan then error("The data does not define a Fan");
     isPolytopal X.cache.Fan)




fan NormalToricVariety := X -> (
     if not X.cache.?Fan then (
	  R := promote(matrix transpose rays X,QQ);
	  L := sort apply(max X, C -> posHull R_C);
	  X.cache.Fan = new Fan from {
	       "generatingCones" => set L,
	       "ambient dimension" => numRows R,
	       "top dimension of the cones" => dim L#0,
	       "number of generating cones" => #L,
	       "rays" => set apply(rays X, r -> promote(matrix transpose {r},QQ)),
	       "number of rays" => numColumns R,
	       "isPure" => dim L#0 == dim last L,
	       symbol cache => new CacheTable});
     X.cache.Fan)


halfspaces (List,NormalToricVariety) := Matrix => (sigma,X) -> (
     if not X.cache.?halfspaces then X.cache.halfspaces = new MutableHashTable;
     if not X.cache.halfspaces#?sigma then X.cache.halfspaces#sigma = fourierMotzkin matrix transpose ((rays X)_sigma);
     -(X.cache.halfspaces#sigma#0))


hyperplanes (List,NormalToricVariety) := Matrix => (sigma,X) -> (
     if not X.cache.?halfspaces then X.cache.halfspaces = new MutableHashTable;
     if not X.cache.halfspaces#?sigma then X.cache.halfspaces#sigma = fourierMotzkin matrix transpose ((rays X)_sigma);
     X.cache.halfspaces#sigma#1)


makeSimplicial = method(Options => {Strategy => "addNoRays"})
makeSimplicial NormalToricVariety := NormalToricVariety => options -> X ->(
     R := rays X;
     Rm := matrix rays X;
     local Xsimp;
     Cones := partition(l -> dim(l,X) == #l,max X);
     simpCones := if Cones#?true then Cones#true else {};
     Cones = if Cones#?false then Cones#false else {};
     if options.Strategy == "centre" then (
	  while Cones != {} do (
	       C := Cones#0;
	       HS := halfspaces(C,X);
	       FL := reverse faceLatticeSimple(dim(C,X),transpose Rm^C,HS);
	       r := {};
	       for i from 1 to #FL-1 do (
	       	    nonsimpFace := select(1,FL#i, e -> #(toList e#0) != i+2);
	       	    if nonsimpFace != {} then (r = makePrimitive flatten entries sum toList nonsimpFace#0#0; break));
	       Y := normalToricVariety(R,Cones);
	       Y.cache.halfspaces = new MutableHashTable from for k in max Y list if X.cache.halfspaces#?k then k => X.cache.halfspaces#k else continue;
	       Y.cache.cones = new MutableHashTable from for k in max Y list if X.cache.cones#?k then k => X.cache.cones#k else continue;X.cache.cones;
	       X = stellarSubdivision(Y,r);
	       R = rays X;
	       Rm = matrix R;
	       Cones = partition(l -> dim(l,X) == #l, max X);
	       if Cones#?true then simpCones = simpCones | Cones#true;
	       if Cones#?false then Cones = Cones#false else Cones = {});
	  Xsimp = normalToricVariety(R,simpCones))
     else if options.Strategy == "addNoRays" then (
	  usedrays := {};
	  while Cones != {} do (
	       Ctally := tally select(flatten Cones, e -> not member(e,usedrays));
	       mCtally := max values Ctally;
	       r1 := (select(keys Ctally, k -> Ctally#k == mCtally))#0;
	       usedrays = usedrays | {r1};
	       r1 = (rays X)#r1;
	       X = stellarSubdivision(X,r1);
	       Cones = select(max X, l -> dim(l,X) =!= #l));
	  Xsimp = X);
     if X.cache.?halfspaces then Xsimp.cache.halfspaces = new MutableHashTable from for k in max Xsimp list if X.cache.halfspaces#?k then k => X.cache.halfspaces#k else continue;
     Xsimp.cache.cones = new MutableHashTable from for k in max Xsimp list if X.cache.cones#?k then k => X.cache.cones#k else continue;
     Xsimp)



faceLatticeSimple = (d,R,HS) -> (
     R = apply(numColumns R, i -> R_{i});
     HS = apply(numColumns HS, i -> transpose HS_{i});
     L := {(set R,set {},set HS)};
     {L}|apply(d-2, i -> (
	       Lnew := flatten apply(L, l -> (
			 l = (toList l#0,toList l#1,toList l#2);
			 lnew := apply(#(l#2), j -> (
				   hs := (l#2)#j;
				   newrs := select(l#0, r -> hs * r == 0);
				   newHS := select(drop(l#2,{j,j}), k -> any(newrs, r -> k * r == 0));
				   (set newrs,set(l#1|{hs}),set newHS)));
			 lnew = select(lnew, e -> all(lnew, f -> not isSubset(e#0,f#0) or e === f))));
	       L = unique Lnew)))


polytope NormalToricVariety := Polyhedron => X -> polytope fan X

polytope (List,NormalToricVariety) := Polyhedron => (D,X) -> (
     U := -matrix rays X;
     a := transpose matrix {D};
     intersection(U,a))


stellarSubdivision (NormalToricVariety,List) := (X,r) -> (
     replacement := {};
     rm := matrix transpose {r};
     (n,newRays) := if member(r,rays X) then (position(rays X, e -> e == r),rays X) else (#(rays X),rays X | {r});
     R := matrix rays X;
     newMax := flatten apply(max X, C -> (
	       M := promote(R^C,QQ);
	       if replacement == {} then (		    
		    if dim(C,X) == #C then (
			 -- Simplicial Cone
			 if numColumns M == numRows M then (
			      -- full dimensional Cone
			      M = inverse M;
			      v := flatten entries (transpose M * rm);
			      if all(v, i -> i >= 0) then (
			      	   replacement = positions(v, i -> i > 0);
			      	   replacement = apply(replacement, i -> C#i);
			      	   C = toList (set C - set replacement) | {n};
			      	   apply(#replacement, i -> sort(C|drop(replacement,{i,i}))))
			      else {C})
			 else (
			      -- not full dimensional cone
			      K := mingens ker M;
			      M = (inverse(M|| transpose K))_{0..numRows M -1};
			      vnf := flatten entries (transpose M * rm); print (M,K,vnf);
			      if all(vnf, i -> i >= 0) and transpose K * rm == 0 then (
			      	   replacement = positions(vnf, i -> i > 0);
			      	   replacement = apply(replacement, i -> C#i);
			      	   C = toList (set C - set replacement) | {n};
			      	   apply(#replacement, i -> sort(C|drop(replacement,{i,i}))))
			      else {C}))
		    else (
			 -- Non-simplicial Cone
			 HS := transpose halfspaces(C,X);
			 w := flatten entries (HS * rm);
			 if all(w, i -> i >= 0) and (dim(C,X) == numColumns HS or (transpose hyperplanes(C,X)) * rm == 0) then (
			      replacement = positions(w, i -> i == 0);
			      correctFaces := submatrix'(HS,replacement,);
			      HS = HS^replacement;
			      replacement = select(C, i -> HS * (transpose R^{i}) == 0);
			      apply(numRows correctFaces, i -> select(C, j -> (correctFaces^{i}) * (transpose R^{j}) == 0) | {n}))
			 else {C}))
	       else (
		    if isSubset(set replacement,set C) then (
			 if dim(C,X) == #C then (
			      -- Simplicial Cone
			      C = toList (set C - set replacement) | {n};
			      apply(#replacement, i -> sort(C|drop(replacement,{i,i}))))
			 else (
			      HS1 := transpose halfspaces(C,X);
			      for i from 0 to numRows HS1 -1 list (
				   if HS1^{i} * (transpose R^replacement) == 0 then continue else select(C, j -> HS1^{i} * (transpose R^{j}) == 0) | {n})))
		    else {C})));
     if replacement == {} then newRays = rays X;
     Y := new NormalToricVariety from {
	  symbol rays => newRays,
	  symbol max => newMax,
	  symbol cache => new CacheTable};
     if X.cache.?halfspaces then Y.cache.halfspaces = new MutableHashTable from for k in max Y list if X.cache.halfspaces#?k then k => X.cache.halfspaces#k else continue;
     Y.cache.cones = new MutableHashTable from for k in max Y list if X.cache.cones#?k then k => X.cache.cones#k else continue;
     Y)

makePrimitive = method()
makePrimitive List := w -> (g := gcd w; apply(w, e -> e//g))

insertCone = method()
insertCone (NormalToricVariety,List) := (X,C) -> (
     R := matrix rays X;
     Cr := transpose matrix C;
     C1 := {};
     maxX := max X;
     for i from 0 to #maxX - 1 do (
	  bigC := maxX#i;
	  M := promote(R^bigC,QQ);
	  v := if dim(bigC,X) == #bigC then (transpose inverse M) * Cr else (transpose halfspaces(bigC,X)) * Cr;
	  if all(flatten entries v, e -> e >= 0) then (
	       C1 = bigC;
	       maxX = drop(maxX,{i,i});
	       break)
	  else if any(numColumns v, i -> all(flatten entries v_{i}, e -> e > 0)) then break);
     if C1 == {} then error("The additional cone is not contained in any cone of the variety");
     oldMaxX := {};
     cones2handle := {(C1,C)} | for bigC in maxX list (
	  M := if dim(bigC,X) == #bigC then inverse promote(R^bigC,QQ) else halfspaces(bigC,X);
	  pos := select(C, r -> all(flatten entries(matrix{r}*M), e -> e >= 0));
	  if pos != {} then (bigC,pos) else (
	       oldMaxX = oldMaxX | {bigC};
	       continue));
     raysX := rays X | select(C, r -> not member(r,rays X));
     totalNewCones := flatten apply(cones2handle, p -> (
	       C1 = p#0;
	       C = p#1;
	       Cr = transpose matrix C;
     	       C1r := (rays X)_C1;
     	       CM := matrix transpose C;
     	       HS := if rank CM == numRows CM then (-(fourierMotzkin CM)#0,0) else (CM = posHull CM; (transpose halfspaces CM,transpose hyperplanes CM));
     	       newCones := apply(numColumns HS#0, i -> (
	       		 v := (HS#0)_{i};
	       		 select(C1r, r -> (matrix{r} * v)_(0,0) < 0) | select(C, r -> (matrix{r} *v)_(0,0) == 0)));
     	       if HS#1 != 0 then (
	  	    newCones = newCones | apply(numColumns HS#1, i -> (
		    	      v := (HS#1)_{i};
		    	      select(C1r, r -> (matrix{r} * v)_(0,0) < 0) | select(C, r -> (matrix{r} *v)_(0,0) == 0)));
	  	    newCones = newCones | apply(numColumns HS#1, i -> (
		    	      v := -(HS#1)_{i};
		    	      select(C1r, r -> (matrix{r} * v)_(0,0) < 0) | select(C, r -> (matrix{r} *v)_(0,0) == 0))));
     	       HS = halfspaces(C1,X);
     	       newCones = newCones | for i from 0 to numColumns HS - 1 list (
	  	    v := HS_{i};
	  	    facetrays := select(C1r, r -> (matrix{r} * v)_(0,0) == 0);
	  	    if any(newCones, nC -> isSubset(set facetrays, set nC)) then continue;
	  	    pos := transpose Cr * v;
	  	    posmin := min flatten entries pos;
	  	    if posmin == 0 then continue;
	  	    pos = positions(flatten entries pos, e -> e == posmin);
	  	    facetrays | C_pos);
     	       newCones = unique(newCones | {C});
     	       for i from 0 to #newCones - 1 list (
	  	    nC := newCones#i;
	  	    if any(drop(newCones,{i,i}), e -> isSubset(nC,e)) then continue;
	  	    nC)));	       
     maxX = oldMaxX | apply(totalNewCones, nC -> apply(nC, r -> position(raysX, e -> e == r)));
     Y := new NormalToricVariety from {
	  symbol rays => raysX,
	  symbol max => maxX,
	  symbol cache => new CacheTable};
     if X.cache.?halfspaces then Y.cache.halfspaces = new MutableHashTable from for k in max Y list if X.cache.halfspaces#?k then k => X.cache.halfspaces#k else continue;
     if X.cache.?cones then Y.cache.cones = new MutableHashTable from for k in max Y list if X.cache.cones#?k then k => X.cache.cones#k else continue;
     Y)



----------------------------------------------------------------
-- The following developed by Christine Berkesch, Diane Maclagan, Alexandra Seceleanu
---------------------------------------------------------------
--Internal procedures (no documentation)
----------------------------------------------------------------
--Procedure to return the largest prime dividing a number 
maxPrime=n->(
     f:=factor(n);
     return(max apply(#f,i->((f#i)#0)));
);

--Procedure to return the smallest prime dividing a number 
minPrime=n->(
     f:=factor(n);
     return(min apply(#f,i->((f#i)#0)));
);

--Procedure to find the mod p representative of a vector v with
--all entries between 0 (inclusive) and p (exclusive)
makePos=(v,pp)->(
	local i,j;
	i=0;
	while i<#v do (
		while v_i < 0 do (
			v=v+pp*(apply(#v,j->(if j==i then 1 else 0)));
		);
		while v_i > pp-1 do (
			v=v-pp*(apply(#v,j->(if j==i then 1 else 0)));
		);
		i=i+1;
	);
	v
)

-- Procedure to compute the maximal lattice index for the facets of X.
-- Note: (X is NOT required to be pure or simplicial)
maxIndex = method()
maxIndex (NormalToricVariety):= X ->(
	max apply(max X,sigma-> latticeIndex(sigma,X))
)

-------------------------------------------------------------------------
--This is currently an internal procedure
-- Procedure to find a new ray (as a List) at which to subdivide.

findNewRay = method(Options => {Strategy => "max"} ) 
findNewRay (NormalToricVariety,ZZ) := options ->(X,maxInd) ->(
	sigma := (select(1,max X, C -> latticeIndex(C,X) == maxInd))#0;
	--Now find the vector in this sigma to add
	p := 0;
	if options.Strategy == "min" then p = minPrime(floor maxInd) else if options.Strategy == "max" then p = maxPrime(floor maxInd); 
	R := ZZ/p;
	Ap := substitute(transpose matrix (rays X)_sigma, R);
	K := transpose gens ker Ap;
	k := flatten entries substitute(matrix({(entries(K))_0}),ZZ);
	k = matrix {makePos(k,p)};
	newA := flatten entries (k*matrix (rays X)_sigma);
	makePrimitive newA --output new vector in list form
)


----------------------------------------------------------------
--Internal procedures for resolution of singularities; 
-- resolve a simplicial normal toric variety

makeSmooth = method(Options => {Strategy=>"max"})
makeSmooth NormalToricVariety := NormalToricVariety => options -> X ->(
	if dim X ==1 then return normalToricVariety(apply(rays X, r-> makePrimitive r),max X);
	Xsimp := X; 
	maxInd := maxIndex Xsimp;
	--Now the main part of the procedure
	count := 0;
	while(maxInd > 1) do (
		count = count +1;	     
		newA = findNewRay(Xsimp,maxInd,Strategy=>options.Strategy);
		print concatenate{"blowup #",toString count," at ",toString newA," with lattice index ",toString maxInd};
		Xsimp = stellarSubdivision( Xsimp, newA);
		--Now update maxInd
		maxInd = maxIndex Xsimp;
	);
	Xsimp
)

resolveSingularities = method(Options=>{Strategy=>"max"}) 
resolveSingularities NormalToricVariety := NormalToricVariety => options -> X ->(
	Xsimp:=X;
	if isSimplicial X then Xsimp = X else Xsimp = makeSimplicial X;
	if isSmooth Xsimp then Xsimp else makeSmooth (Xsimp, Strategy => options.Strategy)
)
--------------------------------------------------------------------------------
-- THINGS TO IMPLEMENT?
--   cotangentBundle -> See Package ToricVectorBundles by Birkner,Ilten,Petersen
--   homology,NormalToricVariety
--   blow-ups
--   operational Chow rings
--   vector bundles? -> See Package ToricVectorBundles by Birkner,Ilten,Petersen
--   faces
--   linear series
--   isSemiprojective
--

--------------------------------------------------------------------------------
-- DOCUMENTATION
--------------------------------------------------------------------------------
beginDocumentation()
    
document { 
     Key => NormalToricVarieties,
     Headline => "normal toric varieties",
     "A toric variety is an integral scheme such that an algebraic
     torus forms a Zariski open subscheme and the natural action this
     torus on itself extends to an action on the entire scheme.
     Normal toric varieties correspond to combinatorial objects,
     namely strongly convex rational polyhedral fans.  This makes the
     theory of normal toric varieties very explicit and computable.",
     PARA{},     
     "This ", EM "Macaulay 2", " package is designed to manipulate
     normal toric varieties and related geometric objects.  An
     introduction to the theory of normal toric varieties can be found
     in the following textbooks:",     
     UL { 
	  {"David A. Cox, John B. Little, Hal Schenck, ", EM "Toric
	  varieties", ", preprint available at ", 
	  HREF("http://www.cs.amherst.edu/~dac/toric.html", 
	       TT "www.cs.amherst.edu/~dac/toric.html")},	       
	  {"GÃ¼nter Ewald, ", EM "Combinatorial convexity and algebraic
           geometry", ", Graduate Texts in Mathematics 168. 
	   Springer-Verlag, New York, 1996. ISBN: 0-387-94755-8" },
	  {"William Fulton, ", EM "Introduction to toric varieties",
	   ", Annals of Mathematics Studies 131, Princeton University 
	   Press, Princeton, NJ, 1993. ISBN: 0-691-00049-2" }, 
	  {"Tadao Oda, ", EM "Convex bodies and algebraic geometry, an
	   introduction to the theory of toric varieties", ",
	   Ergebnisse der Mathematik und ihrer Grenzgebiete (3) 15,
	   Springer-Verlag, Berlin, 1988. ISBN: 3-540-17600-4" },
	   },
     SUBSECTION "Contributors",
     "The following people have generously contributed code or worked
     on our code.",     
     UL {
	  {HREF("http://www.math.purdue.edu/~cberkesc/","Christine Berkesch")},
	  {HREF("http://page.mi.fu-berlin.de/rbirkner/indexen.htm",
		    "Rene Birkner")},
     	  {HREF("http://www.warwick.ac.uk/staff/D.Maclagan/","Diane Maclagan")},
	  {HREF("http://www.math.uiuc.edu/~asecele2/","Alexandra Seceleanu")},
	  },
     Subnodes => {
	  TO NormalToricVariety,
	  TO "Basic properties and invariants",
	  TO "Working with Cartier and Weil divisors",
	  TO "Cox ring and coherent sheaves",
	  TO "Resolution of singularities",
	  }
     }  


document { 
     Key => NormalToricVariety,
     Headline => "the class of all normal toric varieties",  
     "A normal toric variety corresponds to a strongly convex rational
     polyhedral fan in affine space.  ", 
     "In this package, the fan associated to a normal ", TEX ///$d$///,
     "-dimensional toric variety lies in the rational vector space ",
     TEX ///$\QQ^d$///, " with underlying lattice ", 
     TEX ///$N = \ZZ^d$///, ".  The fan is encoded by the minimal
     nonzero lattice points on its rays and the set of rays defining
     the maximal cones (meaning cones that are not proper subsets of
     another cone in the fan).",
     Subnodes => {
	  TO (rays,NormalToricVariety),
	  TO (max,NormalToricVariety),
	  TO (expression,NormalToricVariety),
	  TO normalToricVariety,	  
	  }
     }  


document { 
     Key => {(rays, NormalToricVariety)},
     Headline => "the rays of the fan",
     Usage => "rays X",
     Inputs => {"X" => NormalToricVariety},
     Outputs => {List => " of lists of integers; each entry
          corresponds to a minimal nonzero lattice point on the ray in
          the fan" },
     "A normal toric variety corresponds to a strongly convex rational
     polyhedral fan in affine space.  ", 	  	  
     "In this package, the fan associated to a normal ", 
     TEX ///$d$///, "-dimensional toric variety lies in the rational
     vector space ", TEX ///$\QQ^d$///, " with underlying
     lattice ", TEX ///$N = {\ZZ}^d$///, ".  As a result, each
     ray in the fan is determined by the minimal nonzero lattice point
     it contains.  Each such lattice point is given as a ",
     TO2(List,"list"), " of ", TEX ///$d$///, " ", TO2(ZZ,"integers"),
     ".",
     PARA{},
     "There is a bijection between the rays and torus-invariant Weil
     divisor on the toric variety.",
     PARA{},
     "The examples show the rays for the projective plane, projective
     ", TEX ///$3$///, "-space, a Hirzebruch surface, and a weighted
     projective space.",
     EXAMPLE lines ///
	  PP2 = projectiveSpace 2;
	  rays PP2
	  dim PP2
	  ///,
     EXAMPLE lines ///
	  PP3 = projectiveSpace 3;
	  rays PP3
	  dim PP3
	  ///,
     EXAMPLE lines ///
	  FF7 = hirzebruchSurface 7;
	  rays FF7
	  dim FF7
	  ///,
     EXAMPLE lines ///
	  X = weightedProjectiveSpace {1,2,3};
	  rays X
	  dim X
	  ///,     	  
     "When ", TT "X", " is nondegerenate, the number of rays equals
     the number of variables in the total coordinate ring.",
     EXAMPLE lines ///
	  #rays X == numgens ring X
          ///,
     "An ordered list of the minimal nonzero lattice points on the
     rays in the fan is part of the defining data of a toric variety.",
     SeeAlso => {
	  normalToricVariety, 
	  (max, NormalToricVariety),
	  (ring, NormalToricVariety)
	  } 
     }


document { 
     Key => {(max, NormalToricVariety)},
     Headline => "the maximal cones in the fan",
     Usage => "max X",
     Inputs => {"X" => NormalToricVariety},
     Outputs => {List => " of lists of nonnegative integers; each
	  entry indexes the rays which generate a maximal cone in the
	  fan"},
     "A normal toric variety corresponds to a strongly convex rational
     polyhedral fan in affine space.  ", 	  	  
     "In this package, the fan associated to a normal ", TEX ///$d$///,
     "-dimensional toric variety lies in the rational vector space ",
     TEX ///$\QQ^d$///, " with underlying lattice ", 
     TEX ///$N = \ZZ^d$///, ".  The fan is encoded by the minimal
     nonzero lattice points on its rays and the set of rays defining
     the maximal cones (meaning cones that are not proper subsets of
     another cone in the fan).  ",
     "The rays are ordered and indexed by nonnegative integers: ",
     TEX ///$0,\dots, n$///, ".  Using this indexing, a maximal cone
     in the fan corresponds to a sublist of ", 
     TEX ///$\{0,\dots,n\}$///, "; the entries index the
     rays that generate the cone.",
     PARA{},     
     "The examples show the maximal cones for the projective plane,
     projective 3-space, a Hirzebruch surface, and a weighted
     projective space.",
     EXAMPLE lines ///
	  PP2 = projectiveSpace 2;
	  #rays PP2
	  max PP2
	  ///,
     EXAMPLE lines ///
	  PP3 = projectiveSpace 3;
	  #rays PP3
	  max PP3
	  ///,
     EXAMPLE lines ///
	  FF7 = hirzebruchSurface 7;
	  #rays FF7
	  max FF7
	  ///,
     EXAMPLE lines ///
	  X = weightedProjectiveSpace {1,2,3};
	  #rays X
	  max X
	  ///,   
     "A list corresponding to the maximal cones in the fan is part of the 
     defining data of a toric variety.",
     SeeAlso => {
	  normalToricVariety, 
	  (rays, NormalToricVariety)
	  }
     }     


document { 
     Key => {(expression, NormalToricVariety)},
     Headline => "expression used to format for printing",
     Usage => "expression X",
     Inputs => {"X" => NormalToricVariety },
     Outputs => {Expression => {" used to format ", TT "X", 
	  " for printing"}},
     "This function is the primary function called upon by ", TO(symbol <<),
     " to format for printing.  It displays the minimal nonzero lattice 
     points on each ray and the subsets of rays which determine the maximal
     cones in the fan.",
     EXAMPLE lines ///
	  projectiveSpace 3
     	  rays projectiveSpace 3
	  max projectiveSpace 3
          ///,	
     EXAMPLE lines ///
	  hirzebruchSurface 7
	  rays hirzebruchSurface 7
	  max hirzebruchSurface 7
          ///,		    
     "After assignment to a global variable, ", EM "Macaulay 2", "
     knows the toric variety's name, and this name is used when
     printing.",
     EXAMPLE lines ///
	  PP2 = projectiveSpace 3
	  FF7 = hirzebruchSurface 7
	  ///,
     SeeAlso => {
	  normalToricVariety,
	  (rays,NormalToricVariety),
	  (max,NormalToricVariety)
	  }
     }   


document { 
     Key => {normalToricVariety, 
	  (normalToricVariety,List,List), 
	  [normalToricVariety,CoefficientRing],
	  [normalToricVariety,Variable],
	  [normalToricVariety,WeilToClass]	  
	  },
     Headline => "create a normal toric variety",
     Usage => "normalToricVariety(Rho,Sigma)",
     Inputs => {
	  "Rho" => List => "of lists of integers; each entry is the
	  minimal nonzero lattice point on a ray in the fan",
	  "Sigma" => List => "of lists of nonnegative integers; each
	  entry indexes the rays defining a maximal cone in the fan",
	  CoefficientRing => Ring => { "the coefficient ring of the
	  total coordinate ring"},
	  Variable => Symbol => {"the base symbol for the indexed
	  variables in the total coordinate ring"},
	  WeilToClass => Matrix => {"allows one to specify the map
	  from the group of torus-invariant Weil divisors to the class
	  group"},
          },
     Outputs => {NormalToricVariety => "the normal toric variety
	  determined by the fan" },
     "A normal toric variety corresponds to a strongly convex rational
     polyhedral fan in affine space.  ", 	  
     "In this package, the fan associated to a normal ", TEX ///$d$///,
     "-dimensional toric variety lies in the rational vector space ",
     TEX ///$\QQ^d$///, " with underlying lattice ", 
     TEX ///$N = \ZZ^d$///, ".  The fan is encoded by the minimal
     nonzero lattice points on its rays and the set of rays defining
     the maximal cones (meaning cones that are not proper subsets of
     another cone in the fan).  More precisely, ", TT "Rho", " lists
     the minimal nonzero lattice points on each ray
     (a.k.a. one-dimensional cone) in the fan.  Each lattice point is
     a ", TO2(List,"list"), " of ", TO2(ZZ,"integers"), ".  The rays
     are ordered and indexed by nonnegative integers: ",     
     TEX ///$0,\dots, n$///, ".  Using this indexing, a maximal cone
     in the fan corresponds to a sublist of ", 
     TEX ///$\{0,\dots,n\}$///, ".  All maximal cones are listed in ", 
     TT "Sigma", ".",
     PARA{},
     "The first example is projective ", TEX ///$2$///, "-space blown up
     at two points",
     EXAMPLE lines ///
	  Rho = {{1,0},{0,1},{-1,1},{-1,0},{0,-1}}
          Sigma = {{0,1},{1,2},{2,3},{3,4},{0,4}}
	  X = normalToricVariety(Rho,Sigma)
	  rays X
	  max X
	  dim X
	  ///,	 
     "The second example illustrates the data defining projective ",
     TEX ///$4$///, "-space.",     
     EXAMPLE lines ///	  
	  PP4 = projectiveSpace 4;
	  rays PP4
	  max PP4
	  dim PP4
	  ring PP4
	  PP4' = normalToricVariety(rays PP4, max PP4, CoefficientRing => ZZ)
	  ring PP4'
          ///,   
     "The optional argument ", TO WeilToClass, " allows one to specify
     the map from the group of torus-invariant Weil divisors to the
     class group.  In particular, this allows the user to choose her
     favourite basis for the class group.  This map also determines
     the grading on the total coordinate ring of the toric variety.",
     PARA{}, 
     "For example, we can choose the opposite generator for the class
     group of projective space as follows.",
     EXAMPLE lines ///
     	  PP2 = projectiveSpace 2;
	  A = weilToClass PP2
	  source A == weilDivisors PP2
	  target A == classGroup PP2
	  degrees ring PP2
	  deg = matrix {toList(3:-1)}
     	  X = normalToricVariety(rays PP2, max PP2, WeilToClass => deg);
	  A' = weilToClass X
	  source A' == weilDivisors X
	  target A' == classGroup X	  
	  degrees ring X
     	  (matrix A')*(matrix rays X)
	  ///,
     "The integer matrix ", TT "A", " should span the kernel of the
     matrix whose columns are the minimal nonzero lattice points on
     the rays of the fan.",
     PARA{},	  
     "We can also choose a basis for the class group of a
     blow-up of the projective plane such that the nef cone is the
     positive quadrant.",     
     EXAMPLE lines ///
     	  Rho = {{1,0},{0,1},{-1,1},{-1,0},{0,-1}};
	  Sigma = {{0,1},{1,2},{2,3},{3,4},{0,4}};
          Y = normalToricVariety(Rho,Sigma);
     	  weilToClass Y
	  nef Y
	  deg = matrix{{1,-1,1,0,0},{0,1,-1,1,0},{0,0,1,-1,1}}
          Y' = normalToricVariety(rays Y, max Y, WeilToClass => deg);	  
     	  weilToClass Y'
	  nef Y'
     	  ///,	  	  
     Caveat => {"This method assumes that the lists ", TT "Rho", " and
     	  ", TT "Sigma", " correctly encode a strongly convex rational
     	  polyhedral fan.  One can verify this by using ", 
	  TO (isWellDefined,NormalToricVariety), "."},
     Subnodes => {
	  TO projectiveSpace,
	  TO weightedProjectiveSpace,
	  TO hirzebruchSurface,
	  TO (symbol **, NormalToricVariety, NormalToricVariety),
	  TO smoothFanoToricVariety,
	  TO kleinschmidt,	  
	  TO WeilToClass
	  },	  
     SeeAlso => {
	  (rays, NormalToricVariety), 
	  (max,NormalToricVariety)
	  }
     }	


document { 
     Key => {WeilToClass},
     Headline => "name for an optional argument",
     "A symbol used as the name of an optional argument",
     SeeAlso => {
     	  normalToricVariety,
	  weilToClass,
	  (ring,NormalToricVariety)
	  }
     }     


document { 
     Key => {projectiveSpace, 
	  (projectiveSpace,ZZ)},
     Headline => "projective space",
     Usage => "projectiveSpace d",
     Inputs => {
	  "d" => "a positive integer",
	  },
     Outputs => {NormalToricVariety => {"projective ", TT "d", "-space"}},
     "Projective ", TEX ///$d$///, "-space is a smooth complete normal toric
     variety.  The rays are generated by the standard basis ", 
     TEX ///$e_1,\dots,e_d$///, " of ", TEX ///$\ZZ^d$///, " together with ",
     TEX ///$-e_1-\dots-e_d$///, ".  The maximal cones in the fan
     correspond to the ", TEX ///$d$///, "-element subsets of ", 
     TEX ///$\{0,...,d\}$///, ".",     
     PARA{},
     "The examples illustrate the projective line and projective ",
     TEX ///$3$///, "-space.",
     EXAMPLE lines ///
	  PP1 = projectiveSpace 1;
	  rays PP1
	  max PP1
	  dim PP1
	  ring PP1
	  ideal PP1
	  ///,
     EXAMPLE lines ///
	  PP3 = projectiveSpace 3;
	  rays PP3
	  max PP3
	  dim PP3
	  ring PP3
	  ideal PP3
	  ///,	  
     SeeAlso => {
	  normalToricVariety, 
	  weightedProjectiveSpace,
	  (ring,NormalToricVariety), 
	  (ideal,NormalToricVariety)
	  }
     }     


document { 
     Key => {hirzebruchSurface, 
	  (hirzebruchSurface,ZZ)},
     Headline => "Hirzebruch surface",
     Usage => "hirzebruchSurface a",
     Inputs => {"a" => ZZ},
     Outputs => {NormalToricVariety => "a Hirzebruch surface"},    
     "The ", TEX ///$a^{th}$///, " Hirzebruch surface is a
     complete normal toric variety.  It can be defined as the ",
     TEX ///$\PP^1$///, "-bundle over ", TEX ///$X = \PP^1$///,
     " associated to the sheaf ", 
     TEX ///${\mathcal O}_X(0) \oplus  {\mathcal O}_X(a)$///,
     ".  It is also the quotient of affine ", TEX ///$4$///, "-space by
     a rank two torus.",
     EXAMPLE lines ///
	  FF3 = hirzebruchSurface 3;
	  rays FF3
	  max FF3
	  dim FF3
	  ring FF3
	  degrees ring FF3
	  ideal FF3
          ///,
     "When ", TEX ///a = 0///, ", we obtain ", 
     TEX ///$\PP^1 \times \PP^1$///,
     ".",
     EXAMPLE lines ///
	  FF0 = hirzebruchSurface 0;
	  rays FF0
	  max FF0
	  dim FF0
	  ring FF0
	  degrees ring FF0
	  I = ideal FF0
	  decompose I
          ///,
     "The map from the torus-invariant Weil divisors to the class
     group is chosen so that the positive orthant corresponds to 
     the cone of nef line bundles.",
     EXAMPLE lines ///
     	  FF2 = hirzebruchSurface 2;
	  nef FF2     
     	  ///,     
     SeeAlso => {
	  normalToricVariety, 
	  (ring,NormalToricVariety), 
	  kleinschmidt,
	  nef
	  }
     }     


document { 
     Key => {weightedProjectiveSpace, 
	  (weightedProjectiveSpace,List)},
     Headline => "weighted projective space",
     Usage => "weightedProjectiveSpace q",
     Inputs => {
	  "q" => {" a ", TO2(List,"list"), " of relatively prime positive 
	       integers"}
	  },
     Outputs => {NormalToricVariety => "a weighted projective space"},
     "The weighted projective space associated to a list ", 
     TEX ///$\{q_0,\dots, q_d \}$///, ", where no ", TEX ///$d$///,
     "-element subset of ", TEX ///$q_0,\dots, q_d$///, " has a
     nontrivial common factor, is a normal toric variety built from a
     fan in ", TEX ///$N = \ZZ^{d+1}/\ZZ(q_0,\dots,q_d)$///, 
      ".  The rays are generated by the images of the standard basis
     for ", TEX ///$\ZZ^{d+1}$///, " and the maximal cones in the fan
     correspond to the ", TEX ///$d$///, "-element subsets of ", 
     TEX ///$\{0,...,d\}$///, ".",
     PARA{},
     "The first examples illustrate the defining data for three different
     weighted projective spaces.",     
     EXAMPLE lines ///
	  PP4 = weightedProjectiveSpace {1,1,1,1};
	  rays PP4
	  max PP4
	  dim PP4
	  X = weightedProjectiveSpace {1,2,3};
	  rays X
	  max X
	  dim X
	  Y = weightedProjectiveSpace {1,2,2,3,4};
	  rays Y
	  max Y
	  dim Y
          ///,
     "The grading of the total coordinate ring for weighted projective
     space is determined by the weights.  In particular, the class 
     group is ", TEX ///$\ZZ$///, ".",
     EXAMPLE lines ///
	  classGroup PP4
	  degrees ring PP4
	  classGroup X
	  degrees ring X
	  classGroup Y
	  degrees ring Y
          ///,
     "A weighted projective space is always simplicial but is typically 
     not smooth",
     EXAMPLE lines ///
     	  isSimplicial PP4
	  isSmooth PP4
	  isSimplicial X
	  isSmooth X
	  isSimplicial Y
	  isSmooth Y
	  ///,
     SeeAlso => {
	  projectiveSpace, 
	  (ring,NormalToricVariety), 
	  classGroup,
	  isSimplicial, 
	  isSmooth
	  } 
     }     


document { 
     Key => {(symbol **,NormalToricVariety,NormalToricVariety)},
     Headline => "the cartesian product",
     Usage => "X ** Y",
     Inputs => {"X", "Y" => NormalToricVariety },
     Outputs => {{"the product of ", TT "X", " and ", TT "Y"}},     
     "The cartesian product of two varieties ", TEX ///$X$///, " and
     ", TEX ///$Y$///, ", both defined the same ground field ", 
     TEX ///$k$///, ", is the fiber product ", 
     TEX ///$X \times_k Y$///, ".  For normal toric varieties, the fan
     of the product is given by the cartesian product of each pair of
     cones in the fans of the factors.",
     EXAMPLE lines ///
	  PP2 = projectiveSpace 2;
	  FF2 = hirzebruchSurface 2;
	  X = FF2 ** PP2;
	  #rays X == #rays FF2 + #rays PP2
     	  transpose matrix rays X
     	  transpose matrix rays FF2 ++ transpose matrix rays PP2
	  primaryDecomposition ideal X
	  flatten (primaryDecomposition \ {ideal FF2,ideal PP2})
          ///,
     SeeAlso => {normalToricVariety}
     }  


document { 
     Key => {kleinschmidt, 
	  (kleinschmidt,ZZ,List)},
     Headline => "smooth toric varieties with Picard rank two",
     Usage => "kleinschmidt(d,a)",
     Inputs => {
	  "d" => ZZ => " dimension of toric variety",
	  "a" => {" an increasing list of at most ", TT "d-1",
	       "nonnegative integers"},
	  },
     Outputs => {NormalToricVariety => "a smooth toric variety with
     	  Picard rank two"},
     "Peter Kleinschmidt constructs (up to isomorphism) all smooth
     normal toric varieties with dimension ", TEX ///$d$///, " and ",
     TEX ///$d+2$///, " rays; see P. Kleinschmidt, A classification of
     toric varieties with few generators, ", EM "Aequationes
     Mathematicae ", STRONG "35", " (1998) 254-266.",
     PARA{},
     "When ", TEX ///$d=2$///, ", we obtain a variety isomorphic to a
     Hirzebruch surface.",
     EXAMPLE lines ///
     	  X = kleinschmidt(2,{3});
	  rays X
	  max X
	  FF3 = hirzebruchSurface 3;
	  rays FF3
	  max FF3
          ///,
     "The normal toric variety associated to the pair ",
     TEX ///$(d,A)$///, " is Fano if and only if ", 
     TEX ///\sum_{i=0}^{r-1} a_i < d-r+1///, ".",
     EXAMPLE lines ///
     	  X1 = kleinschmidt(3,{0,1});	  
	  transpose matrix rays X1
	  Y1 = smoothFanoToricVariety(3,3);
	  transpose matrix rays Y1
     	  X2 = kleinschmidt(4,{0,0});	  
	  transpose matrix rays X2
	  Y2 = smoothFanoToricVariety(4,9);
	  transpose matrix rays Y2
	  P22 = projectiveSpace(2) ** projectiveSpace(2);
	  transpose matrix rays P22
          ///,
     SeeAlso => {
	  normalToricVariety, 
     	  hirzebruchSurface
	  }
     }    


document { 
     Key => {smoothFanoToricVariety, 
	  (smoothFanoToricVariety,ZZ,ZZ)},
     Headline => "database of smooth Fano toric varieties",
     Usage => "smoothFanoToricVariety(d,i)",
     Inputs => {
	  "d" => ZZ => " dimension of toric variety",
	  "i" => ZZ => " index of toric variety in database",
	  },
     Outputs => {NormalToricVariety => " a smooth Fano toric variety"},
     "This function accesses a database of all smooth Fano toric
     varieties of dimension at most 4.  The enumeration of the toric
     varieties follows ",  
     HREF("http://www.mathematik.uni-tuebingen.de/~batyrev/batyrev.html.en", 
	  "Victor V. Batyrev's"), 
     " classification; see ", 
     HREF("http://arxiv.org/abs/math/9801107", TT "arXiv:math/9801107v2"), 
     " and ",
     HREF("http://arxiv.org/abs/math/9911022", TT "arXiv:math/9011022"),   
     ".  There is a unique smooth Fano toric curve, five smooth Fano
     toric surfaces, eighteen smooth Fano toric threefolds, and ", 
     TEX ///$124$///, " smooth Fano toric fourfolds.",
     PARA{},
     "For all ", TEX ///$d$///, ", ", TT "smoothFanoToricVariety(d,0)", "
     yields projective ", TEX ///$d$///, "-space.",
     EXAMPLE lines ///
     	  PP1 = smoothFanoToricVariety(1,0);
     	  rays PP1	  
     	  max PP1
     	  PP4 = smoothFanoToricVariety(4,0);
     	  rays PP4
     	  max PP4
	  ///,
     "The following example was missing from Batyrev's table.",
     EXAMPLE lines ///
	  W = smoothFanoToricVariety(4,123);
	  rays W
	  max W
	  ///,
     SeeAlso => {normalToricVariety}
     }     


document { 
     Key => "Basic properties and invariants",
     "Once a ", TO2(NormalToricVariety, "normal toric variety"), " has
     been defined, one can compute some basic invariants or test for
     some elementary properties.",
     Subnodes => {
	  TO (dim,NormalToricVariety),
	  TO latticeIndex,
     	  TO isDegenerate,
     	  TO isProjective,
	  TO isSimplicial,
	  TO (isSmooth,NormalToricVariety),
	  TO (isWellDefined,NormalToricVariety)
	  }
     }  


document { 
     Key => {(dim, NormalToricVariety)},
     Headline => "the dimension of a normal toric variety",
     Usage => "dim X",
     Inputs => {"X" => NormalToricVariety },
     Outputs => {ZZ => "the dimension of the normal toric variety" },
     "The dimension of a normal toric variety equals the dimension of
     its dense algebraic torus.  In this package, the fan associated
     to a normal ", TEX ///$d$///, "-dimensional toric variety lies in
     the rational vector space ", TEX ///$\QQ^d$///, " with underlying
     lattice ", TEX ///$N = \ZZ^d$///, ".  Hence, the dimension equals
     the number of entries in a minimal nonzero lattice point on a
     ray.",
     PARA{},   
     "The following examples illustrate normal toric varieties of
     various dimensions.",
     EXAMPLE lines ///
     	  dim projectiveSpace 1
	  dim projectiveSpace 5
	  dim hirzebruchSurface 7
	  dim weightedProjectiveSpace {1,2,2,3,4}
	  W = normalToricVariety({{4,-1,0},{0,1,0}},{{0,1}})
	  dim W
	  isDegenerate W
          ///,
     Subnodes => {TO (dim,List,NormalToricVariety)},
     SeeAlso => {normalToricVariety, (rays, NormalToricVariety)}
     }  


doc ///
  Key
	(dim,List,NormalToricVariety)
  Headline
	the dimension of a cone in the fan of a normal toric variety
  Usage
	d = dim(sigma,X)
  Inputs
	sigma:List
	  of integers indexing a subset of (rays X)
	X:NormalToricVariety
  Outputs
	d:ZZ
	  The dimension of the cone in the fan of {\tt X} defined by the rays indexed by {\tt sigma}.
  Description
   Text
   Text
   Example
   Text
   Example
  Caveat 
	It is not checked that {\tt sigma} actually defines a face of the fan of X.
  SeeAlso
///

--doc ///
--  Key
--	latticeIndex
--	(latticeIndex,List,NormalToricVariety)
--  Headline
--	the index in the ambient lattice of a lattice generated by primitive vectors in the fan of a normal toric variety
--  Usage
--	m = latticeIndex(sigma, X)
--  Inputs
--	sigma:List
--	  of integers indexing a subset of (rays X)
--	X:NormalToricVariety
--  Outputs
--	m:ZZ
--	  The index in the ambient lattice of the lattice generated by the rays indexed by {\tt sigma}.
--  Description
--   Text
--   Text
--   Example
--   Text
--   Example
--  Caveat
--	It is not checked that {\tt sigma} actually defines a face of the fan of X.
--  SeeAlso
--///


document { 
     Key => {isDegenerate, 
	  (isDegenerate,NormalToricVariety)},
     Headline => "whether a toric variety is degenerate",
     Usage => "isDegenerate X",
     Inputs => {"X" => NormalToricVariety},
     Outputs => {Boolean => {TO2(true,"true"), " if the fan of ", 
	       TT "X", " is contained in a proper subspace of its ambient
	       space and ", TO2(false, "false"), " otherwise" }},
     "A ", TEX ///$d$///, "-dimensional normal toric variety is
     degenerate if its rays do not span ", TEX ///$\QQ^d$///, ".  For
     example, projective spaces and Hirzebruch surfaces are not
     degenerate.",
     EXAMPLE lines ///
	  isDegenerate projectiveSpace 3
	  isDegenerate hirzebruchSurface 7
	  ///,
     "Although one typically works with non-degenerate toric varieties,
     not all normal toric varieties are non-degenerate.",
     EXAMPLE lines ///
	  U = normalToricVariety({{4,-1,0},{0,1,0}},{{0,1}});
	  isDegenerate U
	  ///,
     SeeAlso => {(rays,NormalToricVariety), 
	  (ring, NormalToricVariety)}
     }     


document { 
     Key => {isProjective, (isProjective,NormalToricVariety)},
     Headline => "whether a toric variety is projective",
     Usage => "isProjective X",
     Inputs => {"X" => NormalToricVariety},
     Outputs => {{TO2(true,"true"), " if ", TT "X", " is a projective
     	       variety" }},
     "Insert details.",     
     SeeAlso => {
	  (max, NormalToricVariety)
	  }
     }     


document { 
     Key => {isSimplicial, 
	  (isSimplicial,NormalToricVariety)},
     Headline => "whether a toric variety is simplicial",
     Usage => "isSimplicial X",
     Inputs => {"X" => NormalToricVariety},
     Outputs => {Boolean => {TO2(true,"true"), " if the minimal
	       nonzero lattice points on the rays in each maximal cone
	       in the fan of ", TT "X", " form part of a ", 
	       TO QQ,"-basis and ", TO2(false, "false"), " otherwise" }},
     "A normal toric variety is simplical if every cone in its fan is
     simplicial and a cone is simplicial if its minimal generators are
     linearly independent over ", TEX ///$\QQ$///, ".  In fact, the
     following conditions on a normal toric variety ", TEX ///$X$///,
     " are equivalent:",
     UL{
	  {TEX ///$X$///, " is simplicial;"},
	  {"every Weil divisor on ", TEX ///$X$///, " has a positive
	  integer multiple that is Cartier;"},
	  {TEX ///$X$///, " is ", TEX ///$\QQ$///, "-Cartier;"},
	  {"the Picard group of ", TEX ///$X$///, " has finite index
	  in the class group of ", TEX ///$X$///, ";"},
	  {TEX ///$X$///, " has only finite quotient singularities."},
	  },
     "Projective spaces, weighted projective spaces and Hirzebruch 
     surfaces are simplicial.",
     EXAMPLE lines ///
	  isSimplicial projectiveSpace 4
	  isSimplicial weightedProjectiveSpace {1,2,3}
	  isSimplicial hirzebruchSurface 7
	  ///,
     "However, not all normal toric varieties are simplicial.",
     EXAMPLE lines ///
	  U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
	  isSimplicial U
	  isSmooth U
	  ///,
     EXAMPLE lines ///
	  C = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	  isSimplicial C
	  ///,
     SeeAlso => {
	  (rays,NormalToricVariety), 
	  (max, NormalToricVariety), 
	  isSmooth}
     }     


document { 
     Key => {(isSmooth,NormalToricVariety)},
     Headline => "whether a toric variety is smooth",
     Usage => "isSmooth X",
     Inputs => {"X" => NormalToricVariety},
     Outputs => {Boolean => {TO2(true,"true"), " if the minimal
	       nonzero lattice points on the rays in each maximal cone
	       in the fan of ", TT "X", " form part of a ", TO ZZ,
	       "-basis and ", TO2(false, "false"), " otherwise"}},
     "A normal toric variety is smooth if every cone in its fan is
     smooth and a cone is smooth if its minimal generators are
     linearly independent over ", TEX ///$\ZZ$///, ".  In fact, the
     following conditions on a normal toric variety ", TEX ///$X$///,
     " are equivalent:",
     UL{
	  {TEX ///$X$///, " is smooth;"},
	  {"every Weil divisor on ", TEX ///$X$///, " is Cartier;"},
	  {"the Picard group of ", TEX ///$X$///, " equals
	  the class group of ", TEX ///$X$///, ";"},
	  {TEX ///$X$///, " has no singularities."},
	  },
     "Projective spaces and Hirzebruch surfaces are smooth.",
     EXAMPLE lines ///
	  isSmooth projectiveSpace 4
	  isSmooth hirzebruchSurface 7
	  ///,
     "However, not all normal toric varieties are smooth.",  
     EXAMPLE lines ///
     	  isSmooth weightedProjectiveSpace {1,2,3}
     	  ///,  
     EXAMPLE lines ///
	  U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
	  isSimplicial U
	  isSmooth U
	  ///,
     EXAMPLE lines ///
	  U' = normalToricVariety({{4,-1},{0,1}},{{0},{1}});
	  isSmooth U'
	  ///,
     SeeAlso => {(rays,NormalToricVariety), 
	  (max, NormalToricVariety), 
	  isSimplicial}
     }     


document { 
     Key => {(isWellDefined,NormalToricVariety)},
     Headline => "whether a toric variety is well-defined",
     Usage => "isWellDefined X",
     Inputs => {"X" => NormalToricVariety},
     Outputs => {{TO2(true,"true"), " if the lists of rays and maximal
	       cones associated to ", TT "X", " determine a strongly
	       convex rational polyhedral fan" }},
     "A pair of lists ", TT "(Rho,Sigma)", " correspond to a
     well-defined normal toric variety if the following conditions
     hold:",
     UL {
	  {"the union of the elements of ", TT "Sigma", " equals the
	  set of indices of elements of ", TT "Rho"},
	  {"all elements of ", TT "Rho", " have the same length"},
	  {"all elements of ", TT "Rho", " are lists of integers"},
	  {"the rays indexed by an element of ", TT "Sigma", "
	  generate a strongly convex cone"},
	  {"the rays indexed by an element of ", TT "Sigma", " are the
	  unique minimal lattice points for the cone they generate"},
	  {"the intersection of the cones associated to two elements
	  of ", TT "Sigma", " is a face of each cone."}
	  },
     PARA{},
     "The first examples illustrate that small projective spaces
     are well-defined.",
     EXAMPLE lines ///
     	  for d from 1 to 6 list isWellDefined projectiveSpace d
          ///,     	  
     "The second examples show that a randomly selected Kleinschmidt
     toric variety and a weighted projective space are also
     well-defined.",     
     EXAMPLE lines ///
          setRandomSeed(currentTime());
	  a = sort apply(3, i -> random(7))
	  isWellDefined kleinschmidt(4,a)
	  ///,
     EXAMPLE {
          "q = apply(5, j -> random(1,9));",
	  "while not all(subsets(q,#q-1), s -> gcd s === 1) do (
	       q = apply(5, j -> random(1,9)));",
	  "q",
	  "isWellDefined weightedProjectiveSpace q"
	  },
     "The next seven examples illustrate various ways that two lists
     can fail to define a normal toric variety.  By making the current
     debugging level greater than one, one gets some addition information 
     about the nature of the failure.",
     EXAMPLE lines ///
     	  Sigma = max projectiveSpace 2;
          X1 = normalToricVariety({{-1,-1},{1,0},{0,1},{-1,0}},Sigma);
          isWellDefined X1
     	  debugLevel = 1;
          isWellDefined X1	  	  
	  ///,
     EXAMPLE lines ///	  
	  X2 = normalToricVariety({{-1,-1},{1,0},{0,1,1}},Sigma);
	  isWellDefined X2
	  ///,
     EXAMPLE lines ///	  
	  X3 = normalToricVariety({{-1,-1/1},{1,0},{0,1}},Sigma);
	  isWellDefined X3
	  ///,
     EXAMPLE lines ///	  
	  X4 = normalToricVariety({{1,0},{0,1},{-1,0}},{{0,1,2}});
	  isWellDefined X4
	  ///,
     EXAMPLE lines ///	  
	  X5 = normalToricVariety({{1,0},{0,1},{1,1}},{{0,1,2}});
	  isWellDefined X5
	  ///,
     EXAMPLE lines ///	  
	  X6 = normalToricVariety({{1,0,0},{0,1,0},{0,0,2}},{{0,1,2}});
	  isWellDefined X6
	  ///,
     EXAMPLE lines ///	  
	  X7 = normalToricVariety({{1,0},{0,1},{1,1}},{{0,1},{1,2}});
	  isWellDefined X7
	  ///,
     SeeAlso => {
	  (NormalToricVariety), 
	  (rays, NormalToricVariety),
	  (max, NormalToricVariety)
	  }
     }   
  

document { 
     Key => "Working with Cartier and Weil divisors",
     Subnodes => {
	  TO weilDivisors,
	  TO classGroup,
	  TO cartierDivisors,
	  TO picardGroup,
	  TO nef,  
	  }
     }  

document { 
     Key => {weilDivisors, 
	  (weilDivisors, NormalToricVariety)},
     Headline => "the group of torus-invariant Weil divisors",
     Usage => "weilDivisors X",
     Inputs => {"X" => NormalToricVariety },
     Outputs => {Module => "a finitely generated free abelian group" },
     "The group of torus-invariant Weil divisors on a normal toric variety ",
     TEX ///$X$///, " is the free abelian group generated by the prime 
     torus-invariant divisors.  Since the rays in the fan of ", TEX ///$X$///, 
     " are indexed by ", TEX ///0,\dots, n///, ", the group of Weil divisors is
     canonically isomorphic to ", TEX ///$\ZZ^{n+1}$///, ".",
     PARA {},
     "The examples illustrate various possible Weil groups.",
     EXAMPLE lines ///
     	  PP2 = projectiveSpace 2;
	  #rays PP2
	  weilDivisors PP2
     	  ///,
     EXAMPLE lines ///
     	  FF7 = hirzebruchSurface 7;
	  #rays FF7
	  weilDivisors FF7
	  ///,
     EXAMPLE lines ///
	  U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
	  #rays U
	  weilDivisors U
	  ///,
     Subnodes => {TO weilToClass},
     SeeAlso => {classGroup, 
	  cartierDivisors, 
	  cartierToWeil}
     }   

document { 
     Key => {weilToClass, 
	  (weilToClass, NormalToricVariety)},
     Headline => "map from Weil divisors to the class group",
     Usage => "weilDivisors X",
     Inputs => {"X" => NormalToricVariety },
     Outputs => {Matrix => "defining the surjection from the
     	  torus-invariant Weil divisors to the class group" },
     "For a normal toric variety, the class group has a presentation
     defined by the map from the group of torus-characters to group of
     torus-invariant Weil divisors induced by minimal nonzero lattice
     points on the rays of X.  Hence, there is a surjective map from
     the group of torus-invariant Weil divisors to the class group.
     This function returns a matrix representing this map.  Since the
     ordering on the rays of the toric variety determines a basis for
     the group of torus-invariant Weil divisors, this matrix is
     determined by a choice of basis for the class group.",
     PARA{},
     "The examples illustrate various possible Weil groups.",
     EXAMPLE lines ///
     	  PP2 = projectiveSpace 2;
     	  A = weilToClass PP2
	  source A == weilDivisors PP2
	  target A == classGroup PP2
     	  ///,
     EXAMPLE lines ///
     	  X = weightedProjectiveSpace {1,2,2,3,4};
     	  weilToClass X
     	  ///,	  
     EXAMPLE lines ///
     	  FF7 = hirzebruchSurface 7;
	  A' = weilToClass FF7
     	  (source A', target A') == (weilDivisors FF7, classGroup FF7)
	  ///,
     EXAMPLE lines ///
	  U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
	  weilToClass U
	  weilDivisors U
	  classGroup U
	  ///,
     "This matrix also induces the grading on the total coordinate
     ring of toric variety.",
     EXAMPLE lines ///
     	  degrees ring PP2
	  degrees ring X
	  degrees ring FF7
     	  ///,
     "The optional argument ", TO WeilToClass, " for the constructor ",
     TO normalToricVariety, " allows one to specify a basis of the class
     group.",	       
     SeeAlso => {weilDivisors,
	  cartierDivisors, 
	  normalToricVariety,
	  (ring,NormalToricVariety)}

     }   


document { 
     Key => {classGroup, 
	  (classGroup, NormalToricVariety)},
     Headline => "the class group",
     Usage => "classGroup X",
     Inputs => {
	  "X" => NormalToricVariety,
	  "A" => Matrix => {" which defines the map from the torus-invariant 
	  Weil divisors to the class group; this input is optional"}},
     Outputs => {Module => "a finitely generated abelian group" },
     "The class group of a variety is the group of Weil divisors divided by 
     the subgroup of principal divisors.  For a toric variety ", TEX ///$X$///, 
     ", the class group has a presentation defined by the map from the group 
     of torus-characters to group of torus-invariant Weil divisors induced 
     by minimal nonzero lattice points on the rays of ", TEX ///$X$///, ".",
     PARA {},
     "The following examples illustrate various possible class groups.",
     EXAMPLE lines ///
	  classGroup projectiveSpace 2
	  classGroup hirzebruchSurface 7
	  AA3 = normalToricVariety({{1,0,0},{0,1,0},{0,0,1}},{{0,1,2}});
     	  classGroup AA3
	  X = normalToricVariety({{4,-1},{0,1}},{{0,1}});
	  classGroup X
	  C = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	  classGroup C
          ///,
     PARA {},	  
     "The total coordinate ring of a toric variety is graded by its
     class group.",
     SeeAlso => {(rays,NormalToricVariety), 
	  weilDivisors,
	  (ring, NormalToricVariety), 
	  picardToClass,
	  weilToClass}
     }   

document { 
     Key => {cartierDivisors, 
	  (cartierDivisors, NormalToricVariety)},
     Headline => "the group of torus-invariant Cartier divisors",
     Usage => "cartierDivisors X",
     Inputs => {"X" => NormalToricVariety },
     Outputs => {Module => "a finitely generated abelian group" },
     "The group of torus-invariant Cartier divisors on ", 
     
     TEX ///$X$///, " is the subgroup of all locally principal
     torus-invarient Weil divisors.  On a normal toric variety, the
     group of torus-invariant Cartier divisors can be computed as an
     inverse limit.  More precisely, if ", TEX ///$M$///, " denotes
     the lattice of characters on ", TEX ///$X$///, " and the maximal
     cones in the fan of ", TEX ///$X$///, " are ", 
     TEX ///$s_0,\dots,s_{r-1}$///, " then we have ",
     TEX ///$CDiv(X) = ker( \oplus_{i} M/M(s_i{}) \to{} \oplus_{i<j} M/M(s_i \cap s_j{})$///, 
     ".",
     PARA{},     
     "When ", TEX ///$X$///, " is smooth, every Weil divisor is Cartier.",
     EXAMPLE lines ///
	  PP2 = projectiveSpace 2;
	  Div = weilDivisors PP2
	  Div == cartierDivisors PP2
	  id_Div == cartierToWeil PP2
	  isSmooth PP2
	  ///,
     EXAMPLE lines ///
	  FF1 = hirzebruchSurface 1;
	  cartierDivisors FF1
	  isIsomorphism cartierToWeil FF1
	  isSmooth FF1
          ///,
     "When ", TEX ///$X$///, " is simplicial, every Weil divisor is ",
     TEX ///$\QQ$///, "-Cartier --- every Weil divisor has a positive
     integer multiple that is Cartier.",
     EXAMPLE lines ///
	  U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
	  cartierDivisors U
	  weilDivisors U
	  cartierToWeil U
	  prune cokernel cartierToWeil U
	  isSimplicial U
	  ///,
     EXAMPLE lines ///
	  U' = normalToricVariety({{4,-1},{0,1}},{{0},{1}});
	  cartierDivisors U'
	  weilDivisors U'
	  cartierToWeil U'
	  isSmooth U'
          ///,     
     "In general, the Cartier divisors are only a subgroup of the Weil 
     divisors.",
     EXAMPLE lines ///
	  C = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	  cartierDivisors C
	  weilDivisors C
	  prune coker cartierToWeil C
	  isSimplicial C
	  ///,
     EXAMPLE lines ///	  
	  X = normalToricVariety({{ -1,-1,-1},{1,-1,-1},{ -1,1,-1},{1,1,-1},{ -1,-1,1},{1,-1,1},{ -1,1,1},{1,1,1}},{{0,2,4,6},{0,1,4,5},{0,1,2,3},{1,3,5,7},{2,3,6,7},{4,5,6,7}});
	  weilDivisors X
	  cartierDivisors X
	  prune cokernel cartierToWeil X
	  isSimplicial X
	  ///,
     Subnodes => {TO cartierToWeil,
	  TO cartierToPicard},
     SeeAlso => {weilDivisors, 
	  cartierToWeil, 
	  picardGroup}
     }   

document { 
     Key => {cartierToWeil, 
	  (cartierToWeil, NormalToricVariety)},
     Headline => "map from Cartier divisors to Weil divisors",
     Usage => "cartierToWeil X",
     Inputs => {"X" => NormalToricVariety },
     Outputs => {Matrix => " the inclusion map from the group of 
	  torus-invariant Cartier divisors to the group of torus-invariant
	  Weil divisors" },
     "The group of torus-invariant Cartier divisors is the
     subgroup of all locally principal torus-invariant Weil divisors.",
     PARA{},	  
     "On a smooth normal toric variety, every Weil divisor is Cartier.",
     EXAMPLE lines ///
	  PP2 = projectiveSpace 2;
	  cartierDivisors PP2
	  cartierToWeil PP2
	  isSmooth PP2
	  ///,
     EXAMPLE lines ///
	  FF1 = hirzebruchSurface 1;
	  cartierDivisors FF1
	  cartierToWeil FF1
	  isSmooth FF1
          ///,
     "On a simplicial normal toric variety, every Weil divisor is ", 
     TEX ///$\QQ$///, "-Cartier --- every Weil divisor has a positive 
     integer multiple that is Cartier.",
     EXAMPLE lines ///
	  U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
	  cartierDivisors U
	  weilDivisors U
	  cartierToWeil U
	  prune cokernel cartierToWeil U
	  isSimplicial U
	  ///,
     EXAMPLE lines ///
	  U' = normalToricVariety({{4,-1},{0,1}},{{0},{1}});
	  cartierDivisors U'
	  weilDivisors U'
	  cartierToWeil U'
	  isSmooth U'
          ///,     
     "In general, the Cartier divisors are only a subgroup of the Weil 
     divisors.",
     EXAMPLE lines ///
	  C = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	  cartierDivisors C
	  weilDivisors C
	  cartierToWeil C
	  prune coker cartierToWeil C
	  isSimplicial C
	  ///,
     EXAMPLE lines ///
	  X = normalToricVariety({{ -1,-1,-1},{1,-1,-1},{ -1,1,-1},{1,1,-1},{ -1,-1,1},{1,-1,1},{ -1,1,1},{1,1,1}},{{0,2,4,6},{0,1,4,5},{0,1,2,3},{1,3,5,7},{2,3,6,7},{4,5,6,7}});
	  weilDivisors X
	  cartierDivisors X
	  cartierToWeil X
	  prune cokernel cartierToWeil X
	  isSimplicial X
	  ///,
     SeeAlso => {weilDivisors, 
	  cartierDivisors}
     }   

document { 
     Key => {cartierToPicard, 
	  (cartierToPicard, NormalToricVariety)},
     Headline => "map from Cartier divisors to the Picard group",
     Usage => "cartierToPicard X",
     Inputs => {"X" => NormalToricVariety },
     Outputs => {Matrix => " the surjective map from the group of 
	  torus-invariant Cartier divisors to the Picard group" },
     "The Picard group of a variety is the group of Cartier divisors
     divided by the subgroup of principal divisors.  For a normal
     toric variety , the Picard group has a presentation defined by
     the map from the group of torus-characters to the group of
     torus-invariant Cartier divisors.  Hence, there is a surjective
     map from the group of torus-invariant Cartier divisors to the
     Picard group.  This function returns a matrix representing this
     map.",     	  
     PARA{},	  
     "On a smooth normal toric variety, the map from the Cartier
     divisors to the Picard group is the same as the map from the Weil
     divisors to the class group.",
     EXAMPLE lines ///
	  PP2 = projectiveSpace 2;
	  eta = cartierToPicard PP2
     	  eta == weilToClass PP2
	  ///,
     EXAMPLE lines ///
	  FF1 = hirzebruchSurface 1;
	  xi = cartierToPicard FF1
     	  xi == weilToClass FF1
          ///,
     "In general, there is a commutative diagram relating the map from
     the Cartier divisors to the Picard group and the map from the
     Weil divisors to the class group.",
     EXAMPLE lines ///
	  C = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	  cartierToPicard C
	  picardGroup C	  
	  weilToClass C
	  cartierToWeil C
	  picardToClass C
	  weilToClass C * cartierToWeil C == picardToClass C * cartierToPicard C
	  ///,
     EXAMPLE lines ///
	  X = normalToricVariety({{ -1,-1,-1},{1,-1,-1},{-1,1,-1},{1,1,-1},{-1,-1,1},{1,-1,1},{ -1,1,1},{1,1,1}},{{0,2,4,6},{0,1,4,5},{0,1,2,3},{1,3,5,7},{2,3,6,7},{4,5,6,7}});
	  cartierToPicard X
	  picardGroup X
	  weilToClass X
	  cartierToWeil X
	  picardToClass X
	  weilToClass X * cartierToWeil X == picardToClass X * cartierToPicard X
	  ///,
     SeeAlso => {cartierDivisors,
	  picardGroup,
	  weilToClass}
     }   

document { 
     Key => {picardGroup, 
	  (picardGroup,NormalToricVariety)},
     Headline => "the Picard group",
     Usage => "picardGroup X",
     Inputs => {"X" => NormalToricVariety},
     Outputs => {Module => " a finitely generated abelian group"},
     "The Picard group of a variety is the group of Cartier divisors
     divided by the subgroup of principal divisors.  For a normal toric
     variety ", TEX ///$X$///, ", the Picard group has a presentation
     defined by the map from the group of torus-characters to the
     group of torus-invariant Cartier divisors.",
     PARA {},
     "When ", TEX ///$X$///, " is smooth, the Picard group is
     isomorphic to the class group.",
     EXAMPLE lines ///
	  PP3 = projectiveSpace 3;
	  picardGroup PP3
	  classGroup PP3
	  ///,
     EXAMPLE lines ///
	  FF7 = hirzebruchSurface 7;
	  picardGroup FF7 == classGroup FF7
	  ///,
     "For an affine toric variety, the Picard group is trivial.",
     EXAMPLE lines ///
	  U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
	  picardGroup U
	  classGroup U
	  ///,
     EXAMPLE lines ///
	  U' = normalToricVariety({{4,-1},{0,1}},{{0},{1}});
	  picardGroup U'
	  classGroup U'
	  ///,
     "If the fan of ", TEX ///$X$///, " contains a cone of dimension
     ", TEX ///$dim(X)$///, " then the Picard group is free.",
     EXAMPLE lines ///
	  C = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	  picardGroup C
	  classGroup C
	  ///,
     EXAMPLE lines ///
	  X = normalToricVariety({{ -1,-1,-1},{1,-1,-1},{ -1,1,-1},{1,1,-1},{ -1,-1,1},{1,-1,1},{ -1,1,1},{1,1,1}},{{0,2,4,6},{0,1,4,5},{0,1,2,3},{1,3,5,7},{2,3,6,7},{4,5,6,7}});
	  picardGroup X
	  classGroup X
	  ///,
     Subnodes => {TO picardToClass},
     SeeAlso => {classGroup, 
	  cartierDivisors, 
	  weilDivisors},
     }     

document { 
     Key => {picardToClass, 
	  (picardToClass,NormalToricVariety)},
     Headline => "map from Picard group to class group",
     Usage => "picardToClass X",
     Inputs => {"X" => NormalToricVariety},
     Outputs => {Matrix => " the inclusion map from the Picard group to the
	   class group"},
     "The Picard group of a normal toric variety is a subgroup of the
     class group.",
     PARA{},
     "On a smooth normal toric variety, the Picard group is isomorphic
     to the class group, so the inclusion map is the identity.",
     EXAMPLE lines ///
	  PP3 = projectiveSpace 3;
	  picardGroup PP3
	  classGroup PP3
	  picardToClass PP3
	  ///,
     EXAMPLE lines ///
	  FF7 = hirzebruchSurface 7;
	  picardGroup FF7 == classGroup FF7
	  picardToClass FF7
	  ///,
     "For weighted projective space, the inclusion corresponds to ", 
     TEX ///$l \ZZ$///, " in ", TEX ///$\ZZ$///, ", where ", 
     TEX ///$l = lcm(q_0,\dots, q_d {})$///, ".",
     EXAMPLE lines ///
	  X = weightedProjectiveSpace {1,2,3};
	  picardGroup X
	  classGroup X
	  picardToClass X
	  ///,
     EXAMPLE lines ///
	  Y = weightedProjectiveSpace {1,2,2,3,4};
	  picardGroup Y
	  classGroup Y
	  picardToClass Y
	  ///,
     "The following examples illustrate some other possibilities.",
     EXAMPLE lines ///
	  C = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	  picardGroup C
	  classGroup C
	  picardToClass C
	  ///,
     EXAMPLE lines ///
	  X = normalToricVariety({{ -1,-1,-1},{1,-1,-1},{ -1,1,-1},{1,1,-1},{ -1,-1,1},{1,-1,1},{ -1,1,1},{1,1,1}},{{0,2,4,6},{0,1,4,5},{0,1,2,3},{1,3,5,7},{2,3,6,7},{4,5,6,7}});
	  picardGroup X
	  classGroup X
	  picardToClass X
	  prune cokernel picardToClass X
	  ///,
     SeeAlso => {
	  picardGroup, 
	  classGroup}
     }     

document { 
     Key => {nef, (nef,NormalToricVariety)},
     Headline => "generators for the Nef cone",
     Usage => "L = nef X",
     Inputs => {"X" => NormalToricVariety},     
     Outputs => {"L" => List => {" generators for the Nef cone"}},
     "The positive hull (a.k.a. convex cone) of the entries in ", 
     TT "L", ", each of which is a list of integers, equals the
     cone of numerically effective divisors up to linear equivalence
     in the class group.",
     PARA{},
     "For projective space, the Nef cone corresponds to the
     nonnegative integers.",
     EXAMPLE lines ///
	  PP3 = projectiveSpace 3;
	  nef PP3
          ///,
     "For a Hirzebruch surface, the grading of the total coordinate
     ring is chosen so that the positive quadrant corresponds to the
     nef line bundles.",
     EXAMPLE lines ///
	  FF3 = hirzebruchSurface 3;
	  nef FF3
          ///,
     "As projective 2-space blown up at three points demonstrates, the
     Nef cone is not always an orthant.",
     EXAMPLE lines ///
	  B = normalToricVariety({{1,0},{0,1},{-1,1},{-1,0},{0,-1},{1,-1}},{{0,1},{1,2},{2,3},{3,4},{4,5},{0,5}});
	  nef B
          ///,               
     "If interior of the Nef cone is empty then the toric variety is
     not projective.  In the following example, the Nef cone lies a
     two dimensional subspace of the Picard group, so this complete toric
     variety is not projective.",     
     EXAMPLE lines ///
	  Rho = {{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{-1,0,-1},{-1,-1,0},{-1,-1,-1}};
	  Sigma = {{0,1,2},{0,1,3},{1,2,4},{0,2,5},{1,3,4},{0,3,5},{2,4,5},{3,4,6},{3,5,6},{4,5,6}};
	  X = normalToricVariety(Rho,Sigma);
	  nef X
	  ///,     
     SeeAlso => {classGroup}
     }     

document { 
     Key => "Cox ring and coherent sheaves",
     Subnodes => {
	  TO (ring,NormalToricVariety),
	  TO (ideal,NormalToricVariety),
	  TO (sheaf,NormalToricVariety,Ring),	  	  
	  TO (sheaf,NormalToricVariety,Module),	  
	  TO (cohomology,ZZ,NormalToricVariety,CoherentSheaf)
	  }
     }  

document { 
     Key => {(ring, NormalToricVariety)},
     Headline => "the total coordinate ring (a.k.a. Cox ring)",
     Usage => "ring X",
     Inputs => {
	  "X" => NormalToricVariety,
	  },
     Outputs => {PolynomialRing => "the total coordinate ring"},
     "The total coordinate ring (a.k.a. the Cox ring) of a normal
     toric variety is a polynomial ring in which the variables
     correspond to the rays in the fan.  It is graded by the ",
     TO2(classGroup,"class group"), ".",
     PARA{},
     "The total coordinate ring for projective space is the standard graded 
     polynomial ring.",
     EXAMPLE lines ///
	  PP3 = projectiveSpace 3;
	  S = ring PP3;
	  isPolynomialRing S
	  gens S
	  degrees S
	  numgens S == #rays PP3
	  coefficientRing S
          ///,     
     "For a product of projective spaces, the total coordinate ring has a 
     bigrading.",
     EXAMPLE lines ///
	  X = projectiveSpace(2) ** projectiveSpace(3);
	  gens ring X
	  degrees ring X
	  ///,
     "A Hirzebruch surface also has a ", TT "ZZ", SUP TT "2", "-grading.",
     EXAMPLE lines ///
	  FF3 = hirzebruchSurface 3;
     	  gens ring FF3
	  degrees ring FF3
          ///,
     Caveat => "The total coordinate ring is not (yet?) implemented when
     the toric variety is degenerate or the class group has torsion.",  
     SeeAlso => {rays, classGroup, (ideal, NormalToricVariety), 
	  (sheaf,NormalToricVariety,Module)}
     }   

document { 
     Key => {(ideal, NormalToricVariety), (monomialIdeal, NormalToricVariety)},
     Headline => "the irrelevant ideal",
     Usage => "ideal X",
     Inputs => {"X" => NormalToricVariety},
     Outputs => {Ideal => {"in the total coordinate ring of
     	       ", TT "X", "."}},	       	       
     "The irrelevant ideal is a reduced monomial ideal in the total
     coordinate ring which encodes the combinatorics of the fan.  For
     each maximal cone in the fan, it has a minimal generator, the
     product of the variables not indexed by elements of the list
     corresponding to the maximal cone.",
     PARA{},
     "For projective space, the irrelevant ideal is generated by the
     variables.",
     EXAMPLE lines ///
	  PP4 = projectiveSpace 4;
	  I = ideal PP4
	  isMonomialIdeal I
          ///,
     "For an affine toric variety, the irrelevant ideal is the unit ideal.",
     EXAMPLE lines ///
	  C = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	  ideal C
	  ///,	  
     "The irrelevant ideal for a product of toric varieties is
     intersection of the irrelevant ideal of the factors.",
     EXAMPLE lines ///
	  X = projectiveSpace(3) ** projectiveSpace(4);
	  S = ring X;
	  I = ideal X
	  primaryDecomposition I
	  dual monomialIdeal I
          ///,
     "For a complete simplicial toric variety, the irrelevant ideal is
     the Alexander dual of the Stanley-Reisner ideal of the fan.",
     EXAMPLE lines ///
	  B = normalToricVariety({{1,0},{0,1},{ -1,1},{ -1,0},{0,-1}},{{0,1},{1,2},{2,3},{3,4},{0,4}});
	  max B
	  dual monomialIdeal B
	  ///,
     "Since the irrelevent ideal is a monomial ideal, the command ",
     TT "monomialIdeal", " also produces the irrelevant ideal.",
     EXAMPLE lines ///
	  code(monomialIdeal, NormalToricVariety)
	  ///,
     SeeAlso => {(max,NormalToricVariety), (ring,NormalToricVariety)}
     }     

document { 
     Key => {(sheaf,NormalToricVariety,Module)},
     Headline => "make a coherent sheaf",
     Usage => "sheaf(X,M)",
     Inputs => {
	  "X" => NormalToricVariety,
	  "M" => {"a graded ", TO2(Module,"module"), " over the total 
	       coordinate ring of ", TT "X"}
	       },
     Outputs => {CoherentSheaf => {"the coherent sheaf on ", TT "X", 
	       " corresponding to ", TT "M"}},
     "Let ", TT "I", " denote the irrelevent ideal of ", TT "X", ".  The 
     category of coherent ", TT "OO", TT SUB "X", "-modules is equivalent 
     to the category of graded modules over the total coordinate ring of ", 
     TT "X", " divided by the full subcategory of ", TT "I", "-torsion
     modules.  In particular, each finitely generated module over the total 
     coordinate ring of ", TT "X", " corresponds to coherent sheaf on ", 
     TT "X", " and every coherent sheaf arises in this manner.",
     PARA {},
     "Free modules correspond to reflexive sheaves.",
     EXAMPLE lines ///
	  PP3 = projectiveSpace 3;
	  sheaf(PP3, (ring PP3)^{{1},{2},{3}})
          ///,
     EXAMPLE lines ///
	  FF7 = hirzebruchSurface 7;
     	  sheaf(FF7, (ring FF7)^{{1,0},{0,1}})
	  ///,
     SeeAlso => {(ring,NormalToricVariety),(ideal,NormalToricVariety),
	  (sheaf,NormalToricVariety)}
     }     

document { 
     Key => {(sheaf,NormalToricVariety,Ring), (symbol _,OO,NormalToricVariety),
	  (sheaf,NormalToricVariety)},
     Headline => "make a coherent sheaf of rings",
     Usage => "sheaf(X,S)",
     Inputs => {
	  "X" => NormalToricVariety,
	  "S" => {"the total coordinate ring of ", TT "X"}
	       },
     Outputs => {SheafOfRings => {"the structure sheaf on ", TT "X"}},
     "Let ", TT "I", " denote the irrelevent ideal of ", TT "X", ".  The 
     category of coherent ", TT "OO", TT SUB "X", "-modules is equivalent 
     to the category of graded modules over the total coordinate ring of ", 
     TT "X", " divided by the full subcategory of ", TT "I", "-torsion
     modules.  In particular, the total coordinate ring corresponds to 
     the structure sheaf.",
     EXAMPLE lines ///
	  PP3 = projectiveSpace 3;
	  F = sheaf(PP3, ring PP3)
	  G = sheaf PP3
	  F === G
	  H = OO_PP3
	  F === H
          ///,
     EXAMPLE lines ///
	  FF7 = hirzebruchSurface 7;
	  sheaf FF7
	  ///,
     SeeAlso => {(ring,NormalToricVariety),(sheaf,NormalToricVariety,Module)}
     }    

document { 
     Key => {(cohomology,ZZ,NormalToricVariety,CoherentSheaf),
	  (cohomology,ZZ,NormalToricVariety,SheafOfRings)},
     Headline => "cohomology of a coherent sheaf",
     Usage => "HH^i(X,F)",
     Inputs => {"i" => ZZ,
	  "X" => NormalToricVariety,
	  "F" => CoherentSheaf => {" on ", TT "X"}
	  },
     Outputs => {{"the ", TT "i", "-th cohomology group of ", TT "F"}},
     "The cohomology functor ", TT "HH", TT SUP "i", TT "(X,-)", " from the
     category of sheaves of abelian groups to the category of abelian 
     groups is the right derived functor of the global sections functor.",
     PARA {},
     "As a simple example, we compute the dimensions of the cohomology 
     groups for some line bundles on the projective plane.",
     EXAMPLE {
	  "PP2 = projectiveSpace 2;",
	  "HH^0(PP2,OO_PP2(1))",
	  "apply(10, i -> HH^2(PP2,OO_PP2(-i)))",
	  ///loadPackage "BoijSoederberg";///,
	  ///loadPackage "BGG";///,
	  "cohomologyTable (CoherentSheaf, NormalToricVariety, ZZ,ZZ) := CohomologyTally => (F,X,lo,hi) -> (
     	       new CohomologyTally from select(flatten apply(1+dim X, 
	       		 j -> apply(toList(lo-j..hi), 
		    	      i -> {(j,i),rank HH^j(X,F(i))})), p -> p#1 != 0));",	  
          "cohomologyTable(OO_PP2^1,PP2,-10,10)",
     	  },
     "Compare this table with the first example in ", TO "BGG::cohomologyTable", 
     ".",
     PARA{},
     "For a second example, we compute the dimensions of the cohomology
     groups for some line bundles on a Hirzebruch surface",
     EXAMPLE {
	  "cohomologyTable (ZZ, CoherentSheaf, List, List) := (k,F,lo,hi) -> (
     	       new CohomologyTally from select(flatten apply(toList(lo#0..hi#0),
	       		 j -> apply(toList(lo#1..hi#1), 
		    	      i -> {(j,i-j), rank HH^k(variety F, F(i,j))})), 
	  	    p -> p#1 != 0));",
	  "FF2 = hirzebruchSurface 2;",
	  "cohomologyTable(0,OO_FF2^1,{ -7,-7},{7,7})",
	  "cohomologyTable(1,OO_FF2^1,{ -7,-7},{7,7})",
	  "cohomologyTable(2,OO_FF2^1,{ -7,-7},{7,7})",
      	  },
     PARA{},     
     "When ", TT "F", " is free, the algorithm based on Diane
     Maclagan, Gregory G. Smith, ",
     HREF("http://arxiv.org/abs/math.AC/0305214", "Multigraded
     Castelnuovo-Mumford regularity"), ", ", EM "J. Reine
     Angew. Math. ", BOLD "571", " (2004), 179-212.  The general case
     uses the methods described in David Eisenbud, Mircea MustaÅ£Ç,
     Mike Stillman, ", HREF("http://arxiv.org/abs/math.AG/0001159",
     "Cohomology on toric varieties and local cohomology with monomial
     supports"), ", ", EM "J. Symbolic Comput. ", BOLD "29", " (2000),
     583-600.",     
     SeeAlso => {(sheaf,NormalToricVariety,Module),
	  (sheaf,NormalToricVariety,Ring)}
     }     

document { 
     Key => "Resolution of singularities",
     Subnodes => {
	  TO (makeSimplicial,NormalToricVariety),
	  }
     }  

document { 
     Key => {makeSimplicial, (makeSimplicial,NormalToricVariety)},
     Headline => "???",
     Usage => "makeSimplical X",
     Inputs => {"X" => NormalToricVariety},
     Outputs => {NormalToricVariety},
     SeeAlso => {
	  NormalToricVariety, 
	  }
     }
     
doc ///
  Key
	makeSmooth
	(makeSmooth, NormalToricVariety)
  Headline
	resolve a simplicial normal toric variety
  Usage
	Y = makeSmooth X
  Inputs
	X:NormalToricVariety
  Outputs
	Y:NormalToricVariety
  Description
    Text
	Resolves the singularities of a simplicial {\tt X}, 
	yielding a smooth {\tt Y}.
    Text
	Given a simplicial @TO NormalToricVariety@ {\tt X}, 
	makeSmooth {\tt X} is obtained by successive 
	@TO stellarSubdivision@s of the fan of X until each 
	cone in the fan is contained in some basis for the 
	ambient lattice. 
    Text
	As a simple example, consider the affine (simplicial) 
	@TO NormalToricVariety@ determined by the 
	two-dimensional cone in \RR^2 with rays (4,-1) and (0,1) and 
	another with rays (3,-2) and (0,1). 
    Example
	U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
	V = makeSmooth U;
	expression V
	isSmooth V
    Example
	X = normalToricVariety({{3,-2},{0,1}},{{0,1}});
	Y = makeSmooth X;
	expression Y
    Text
	We now consider a small weightedProjectiveSpace. 
    Example
	X = weightedProjectiveSpace {1,2,3};
	Y = makeSmooth X;
	expression Y
	isSmooth Y
    Text
	If {\tt X} is already smooth, makeSmooth X will return {\tt X}.
    Example
	X = projectiveSpace 5;
	Y = makeSmooth X;
	X === Y
    Text
	Here is a three-dimensional affine non-simplicial 
	normalToricVariety {\tt X}. To make it smooth, we 
	first apply @TO makeSimplicial@ to {\tt X}. 
	These two methods combine to give the command 
	@TO resolveSingularities@.
    Example
	X = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	Y = makeSimplicial X;
	Z = makeSmooth Y;
	expression Z
	isSmooth Z
	U = resolveSingularities X;
	U === Z	
    Text
	Here is the normalToricVariety whose fan is determined 
	by the cube centered at the origin in three dimensions 
	with vertices whose entries are +1 and -1.	
    Example
	X = normalToricVariety({{-1,-1,-1},{1,-1,-1},{-1,1,-1},{1,1,-1},{-1,-1,1},{1,-1,1},{-1,1,1},{1,1,1}},{{0,2,4,6},{0,1,4,5},{0,1,2,3},{1,3,5,7},{2,3,6,7},{4,5,6,7}});
	Y = makeSimplicial X;
	Z = makeSmooth Y;
	expression Z
	isSmooth Z
    Text
	The input variety need not be pure-dimensional.
    Example
	X = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,1},{1,0,-1}},{{0,1,2,3},{0,4}});
	Y = makeSmooth X;
	Z = makeSimplicial Y;
	expression Z
    Text
	There is a Strategy option. The Strategy arguments currentl available are "min" and "max" which correspond to choosing 
	a ray generator corresponding to an element of minimal respectively maximal prime degree for subdividing. The following 
	is a comparison of the two strategies. Note that in this example both strategies use 10 blowups, but the lattice indices 
	are different.
    Example
	X = weightedProjectiveSpace ({1,9,10});
	Y1 = makeSmooth (X, Strategy => "max")
	Y2 = makeSmooth (X, Strategy => "min")
	Y1 === Y2
    Text	
	In this example the "max" strategy uses only 17 blowups while the "min" strategy uses 28 blowups.
    Example
	X = weightedProjectiveSpace {1,2,3,5,17};
	Y1 = makeSmooth (X, Strategy => "max")
	Y2 = makeSmooth (X, Strategy => "min")
	Y1 === Y2
    Text
      This command even works for 1-dimensional toric varieties.    
    Example
	X = normalToricVariety({{2}},{{0}}); 
 	Y = makeSmooth X;
	isSmooth Y 
	X === Y 
  Caveat
	It is assumed that {\tt X} is simplicial.
  SeeAlso
	 (isSmooth,NormalToricVariety)
	 makeSmooth
	 makeSimplicial
	 resolveSingularities
	 stellarSubdivision
///


doc ///
  Key
	resolveSingularities
	(resolveSingularities, NormalToricVariety)
  Headline
	resolve the singularities of a normal toric variety
  Usage
	Y = resolveSingularities X
  Inputs
	X:NormalToricVariety
  Outputs
	Y:NormalToricVariety
	  a resolution of {\tt X}
  Description
    Text
	This function returns the resolution of X obtained 
	by applying @TO makeSimplicial@ and then @TO makeSmooth@. 
    Text
	A simplicial @TO NormalToricVariety@ 
	{\tt X} is smooth exactly when 
	its fan is composed of simplicial cones whose generators 
	are subsets of bases of the ambient lattice.
    Text
	As a simple example, consider the affine (simplicial) 
	@TO NormalToricVariety@ {\tt X} determined by the cone 
	with rays (4,-1) and (0,1) in \ZZ^2. 
    Example
	U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
	V = resolveSingularities U;
	expression V
	isSmooth V
    Text
	The input variety need not be full-dimensional.
    Example
	U' = normalToricVariety({{4,-1},{0,1}},{{0},{1}});
	V' = resolveSingularities U';
	expression V'
	isSmooth V'
	U' === V' 
--  Example
--	--Here we see that quotient singularities are not resolved.
--	U' = normalToricVariety({{8,-2},{0,1}},{{0},{1}});
--	V' = resolveSingularities U';
--	expression V'
--	isSmooth V' --false
--	U' === V' --true
    Text 
	Showing again that the input need not be 
	full-dimensional, this example would also 
	be a good one on which to try the Strategy 
	(min,max) option.
    Example
	U' = normalToricVariety({{-2,3,0},{1,0,0},{0,-1,0},{0,0,1}},{{0,1},{0,2},{1,2},{2,3}});
	V' = resolveSingularities U';
	expression V'
	isSmooth V'
	U' === V' 
    Text
	Now consider a small weightedProjectiveSpace. 
    Example
	X = weightedProjectiveSpace {1,2,3};
	Y = resolveSingularities X;
	expression Y
    Text
	If {\tt X} is already smooth, resolveSingularities X will return {\tt X}.
    Example
	X = projectiveSpace 5;
	Y = resolveSingularities X;
	X === Y
    Text
	Here is a three-dimensional affine non-simplicial 
	normalToricVariety {\tt X}. 
    Example
	X = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	Y = resolveSingularities X;
	expression Y
	isSmooth Y
    Text
	Here is the normalToricVariety whose fan is determined 
	by the cube centered at the origin in three dimensions 
	with vertices whose entries are +1 and -1.	
    Example
	X = normalToricVariety({{-1,-1,-1},{1,-1,-1},{-1,1,-1},{1,1,-1},{-1,-1,1},{1,-1,1},{-1,1,1},{1,1,1}},{{0,2,4,6},{0,1,4,5},{0,1,2,3},{1,3,5,7},{2,3,6,7},{4,5,6,7}});
	Y = resolveSingularities X;
	expression Y
    Text
	The input variety need not be pure-dimensional.
    Example
	X = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,1},{1,0,-1}},{{0,1,2,3},{0,4}});  	
	Y = resolveSingularities X;
	expression Y
    Text
	There is a Strategy option. The Strategy arguments currently available are "min" and "max" which correspond to choosing 
	a ray generator corresponding to an element of minimal respectively maximal prime degree for subdividing. The following 
	is a comparison of the two strategies. Note that in this example both strategies use 10 blowups, but the lattice indices 
	are different.
    Example
	X = weightedProjectiveSpace ({1,9,10});
	Y1 = resolveSingularities (X, Strategy => "max")
	Y2 = resolveSingularities (X, Strategy => "min")
	Y1 === Y2
    Text	
	In this example the "max" strategy uses only 17 blowups while the "min" strategy uses 28 blowups.
    Example
	X = weightedProjectiveSpace {1,2,3,5,17};
	Y1 = resolveSingularities (X, Strategy => "max")
	Y2 = resolveSingularities (X, Strategy => "min")
	Y1 === Y2
  SeeAlso
	makeSimplicial
	makeSmooth
	stellarSubdivision
///


doc ///
  Key
	latticeIndex
	(latticeIndex,List,NormalToricVariety)
  Headline
	the index in the ambient lattice of a lattice generated by primitive vectors in the fan of a normal toric variety
  Usage
	m = latticeIndex(sigma, X)
  Inputs
	X:NormalToricVariety
	sigma:List
	  of integers indexing a subset of (rays X)
  Outputs
	m:ZZ
	  The index in the ambient lattice of the lattice generated by the rays indexed by {\tt sigma}.
  Description
    Text
	Given a cone sigma in a @TO normalToricVariety@, 
	the lattice (or group) generated by the integer vectors 
	in sigma need not coincide with the collection of 
	integer vectors found in its linear span. This method
	computes the number of (translated) copies it takes to 
	cover this a priori larger collection of integral points. 
    Text
	A cone corresponds to a nonsingular variety when 
	it is simplicial and has latticeIndex equal to 1.
    Text
	We provide an example of an affine singular 
	@TO normalToricVariety@, as well as a complete one. 
    Example
	U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
	sigma = (max U)#0;
	latticeIndex(sigma,U)
	tau = {0};
	latticeIndex(tau,U)
    Text
	Here we see a smooth divisor within a singular variety. 
    Example
	X = weightedProjectiveSpace {1,2,3,5,17};
	sigma = (max X)#0;
	latticeIndex(sigma,X)
	tau = {0,1,2};
	latticeIndex(tau,X)	
  Caveat
	It is not checked that {\tt sigma} actually defines a face of the fan of X.
///
-------------------------------------------------------------------------------- 
-- TEST
--------------------------------------------------------------------------------

TEST ///
PP4 = projectiveSpace 4;
assert(rays PP4 == {{-1,-1,-1,-1},{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}})
assert(max PP4 == {{0,1,2,3},{0,1,2,4},{0,1,3,4},{0,2,3,4},{1,2,3,4}})
assert(dim PP4 == 4)
assert(isSimplicial PP4 == true)
assert(isSmooth PP4 == true)
assert(isDegenerate PP4 == false)
assert(cartierDivisors PP4 == ZZ^5)
assert(weilDivisors PP4 == ZZ^5)
assert(cartierToWeil PP4 == id_(ZZ^5))
assert(classGroup PP4 == ZZ^1)
assert(degrees ring PP4 == {{1},{1},{1},{1},{1}})
assert(picardGroup PP4 == ZZ^1)
assert(picardToClass PP4 == id_(ZZ^1))
assert(ideal PP4 == ideal gens ring PP4)
assert(nef PP4 == {{1}})
assert(apply(6, i -> rank HH^0(PP4,OO_PP4(i))) == apply(6, i -> binomial(4+i,i)))
F = sheaf_PP4 truncate(2, (ring PP4)^1);
assert(apply(6, i -> rank HH^0(PP4,F(i))) == apply(6, i -> binomial(4+i,i)))
///

TEST ///
FF2 = hirzebruchSurface 2;
assert(rays FF2 == {{1,0},{0,1},{-1,2},{0,-1}})
assert(max FF2 == {{0,1},{1,2},{2,3},{0,3}})	  
assert(dim FF2 == 2)	  
assert(degrees ring FF2 == {{1,0},{-2,1},{1,0},{0,1}})
assert(isSimplicial FF2 == true)	  
assert(isSmooth FF2 == true)
assert(isDegenerate FF2 == false)
assert(cartierDivisors FF2 == ZZ^4)
assert(weilDivisors FF2 == ZZ^4)
assert(cartierToWeil FF2 == id_(ZZ^4))
assert(classGroup FF2 == ZZ^2)
assert(picardGroup FF2 == ZZ^2)
assert(picardToClass FF2 == id_(ZZ^2))
assert(ideal FF2 == intersect(ideal (gens ring FF2)_{0,2}, ideal (gens ring FF2)_{1,3}))
assert(nef FF2 == {{1,0},{0,1}})
assert(apply(6, i -> rank HH^0(FF2,OO_FF2(0,i))) == apply(6, i -> rank source basis({0,i},ring FF2)))	  
///

TEST ///
X = weightedProjectiveSpace {1,2,3};
assert(rays X == {{-2,-3},{1,0},{0,1}})
assert(max X == {{0,1},{0,2},{1,2}})
assert(dim X == 2)	  
assert(degrees ring X == {{1},{2},{3}})
assert(isSimplicial X == true)	  
assert(isSmooth X == false)
assert(isDegenerate X == false)
assert(cartierDivisors X == ZZ^3)
assert(weilDivisors X == ZZ^3)
assert(cartierToWeil X != id_(ZZ^4))
assert(classGroup X == ZZ^1)
assert(picardGroup X == ZZ^1)
assert(abs first first entries picardToClass X == lcm {1,2,3})
assert(ideal X == ideal gens ring X)
assert(nef X == {{1}})
assert(apply(6, i -> rank HH^0(X,OO_X(i))) == apply(6, i -> rank source basis({i},ring X)))     	  
///

TEST ///
U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
assert(rays U == {{4,-1},{0,1}})
assert(max U == {{0,1}})
assert(dim U == 2)	  
assert(isSimplicial U == true)	  
assert(isSmooth U == false)
assert(isDegenerate U == false)
assert(cartierDivisors U == ZZ^2)
assert(weilDivisors U == ZZ^2)
assert(cartierToWeil U != id_(ZZ^2))
assert(classGroup U == cokernel matrix{{4}})
assert(picardGroup U == 0)
assert(picardToClass U == 0)
///

TEST ///
U' = normalToricVariety({{4,-1},{0,1}},{{0},{1}});
assert(rays U' == {{4,-1},{0,1}})
assert(max U' == {{0},{1}})
assert(dim U' == 2)	  
assert(isSimplicial U' == true)	  
assert(isSmooth U' == true)
assert(isDegenerate U' == false)
assert(cartierDivisors U' == ZZ^2)
assert(weilDivisors U' == ZZ^2)
assert(cartierToWeil U' == id_(ZZ^2))
assert(classGroup U' == cokernel matrix {{4}})
assert(picardGroup U' == cokernel matrix {{4}})
assert(matrix picardToClass U' == id_(ZZ^1))
///

TEST ///
Q = normalToricVariety({{-1,-1,-1},{1,-1,-1},{-1,1,-1},{1,1,-1},{-1,-1,1},{1,-1,1},{-1,1,1},{1,1,1}},{{0,2,4,6},{0,1,4,5},{0,1,2,3},{1,3,5,7},{2,3,6,7},{4,5,6,7}});
assert(rays Q == {{-1,-1,-1},{1,-1,-1},{-1,1,-1},{1,1,-1},{-1,-1,1},{1,-1,1},{-1,1,1},{1,1,1}})
assert(max Q == {{0,2,4,6},{0,1,4,5},{0,1,2,3},{1,3,5,7},{2,3,6,7},{4,5,6,7}})
assert(dim Q == 3)	  
assert(isSimplicial Q == false)	  
assert(isSmooth Q == false)
assert(isDegenerate Q == false)
assert(cartierDivisors Q == ZZ^4)
assert(weilDivisors Q == ZZ^8)
assert(classGroup Q == cokernel transpose matrix{{2,0,0,0,0,0,0},{0,2,0,0,0,0,0}})
assert(picardGroup Q == ZZ^1)
///

TEST ///
PP4 = projectiveSpace 4;
assert(makeSmooth PP4 === PP4)
assert(resolveSingularities PP4 === PP4)
assert(apply(max PP4,i-> latticeIndex(i,PP4)) == apply(max PP4,i-> 1))
///

TEST ///
PP1 = projectiveSpace 1;
assert(makeSmooth PP1 === PP1)
assert(resolveSingularities PP1 === PP1)
assert(apply(max PP1,i-> latticeIndex(i,PP1)) == apply(max PP0,i-> 1))
///

TEST ///
FF2 = hirzebruchSurface 2;
assert(makeSmooth FF2 === FF2)
assert(resolveSingularities FF2 === FF2)
assert(apply(max FF2,i-> latticeIndex(i,FF2)) == apply(max FF2,i-> 1))
///

TEST ///
X = weightedProjectiveSpace {1,2,3};
--assert(makeSmooth X === normalToricVariety({{-2,3},{1,0},{0,1},{-1,2},{0,-1},{-1,1}},{{3,4},{1,4},{0,3},{2,5},{0,5},{1,2}}))
--assert(resolveSingularities X === normalToricVariety({{-2,3},{1,0},{0,1},{-1,2},{0,-1},{-1,1}},{{3,4},{1,4},{0,3},{2,5},{0,5},{1,2}}))
assert(apply(max X,i-> latticeIndex(i,X)) == {3,2,1})
///

TEST ///
U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
assert(rays U == {{4,-1},{0,1}})
assert(max U == {{0,1}})
--assert(makeSmooth U === normalToricVariety({{4,-1},{0,1},{1,0}},{{1,2},{0,2}}))
--assert(resolveSingularities U === normalToricVariety({{4,-1},{0,1},{1,0}},{{1,2},{0,2}}))
assert(apply(max U,i-> latticeIndex(i,U)) == {4})
///

TEST ///
U' = normalToricVariety({{4,-1},{0,1}},{{0},{1}});
assert(rays U' == {{4,-1},{0,1}})
assert(max U' == {{0},{1}})
assert(makeSmooth U' === U')
assert(resolveSingularities U' === U')
assert(apply(max U',i-> latticeIndex(i,U')) == apply(max U',i-> 1))
///

end     

--------------------------------------------------------------------------------
-- SCRATCH SPACE
--------------------------------------------------------------------------------
uninstallPackage "NormalToricVarieties"
restart
installPackage "NormalToricVarieties"
check NormalToricVarieties





