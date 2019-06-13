-- -*- coding: utf-8 -*-
------------------------------------------------------------------------------
-- Copyright 2009--2019 Gregory G. Smith
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
-- more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program.  If not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------------------------
newPackage(
  "NormalToricVarieties",
  AuxiliaryFiles => true,
  Version => "1.8",
  Date => "22 January 2019",
  Authors => {{
      Name => "Gregory G. Smith", 
      Email => "ggsmith@mast.queensu.ca", 
      HomePage => "http://www.mast.queensu.ca/~ggsmith"}},
  Headline => "a package for working with normal toric varieties",
  PackageExports => {"Polyhedra", "Schubert2"},
  PackageImports => {"FourierMotzkin","Normaliz","LLLBases"},
  DebuggingMode => false
  )

export { 
  "WeilToClass",       
   -- types
  "NormalToricVariety",
  "ToricDivisor",    
  --functions / methods
  "affineSpace",
  "toricBlowup",
  "cartierDivisorGroup", 
  "classGroup",
  "fromCDivToPic",  
  "fromCDivToWDiv",
  "fromPicToCl",     
  "fromWDivToCl",
  "hirzebruchSurface", 
  "isAmple",    
  "isCartier",  
  "isDegenerate",
  "isEffective",    
  "isFano", 
  "isNef",  
  "isProjective", 
  "isQQCartier", 
  "kleinschmidt",
  "makeSimplicial",
  "makeSmooth",
  "nefGenerators",  
  "normalToricVariety",  
  "orbits",   
  "picardGroup",    
  "toricProjectiveSpace",
  "smallAmpleToricDivisor",  
  "smoothFanoToricVariety",
  "toricDivisor",  
  "weightedProjectiveSpace",  
  "weilDivisorGroup"
  }

protect emsBound
protect rawHHOO

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- CODE
------------------------------------------------------------------------------
------------------------------------------------------------------------------
KK := QQ  -- global base ring
debug Core --- kludge to access "hasAttribute" and getAttribute

------------------------------------------------------------------------------
-- Constructing normal toric varieties
------------------------------------------------------------------------------
NormalToricVariety = new Type of Variety
NormalToricVariety.synonym = "normal toric variety"
NormalToricVariety.GlobalAssignHook = globalAssignFunction
NormalToricVariety.GlobalReleaseHook = globalReleaseFunction
expression NormalToricVariety := X -> if hasAttribute (X, ReverseDictionary) 
    then expression getAttribute (X, ReverseDictionary) else 
    new FunctionApplication from {normalToricVariety, (rays X, max X)}

normalToricVariety = method (TypicalValue => NormalToricVariety, 
    Options => {
    	CoefficientRing   => KK,
    	MinimalGenerators => false,
    	Variable          => getSymbol "x",	  
    	WeilToClass       => null
	}
    )
normalToricVariety (List, List) := opts -> (rayList, coneList) -> (
    coneList' := sort apply(coneList, sigma -> sort sigma);
    X := new NormalToricVariety from {
    	symbol rays  => rayList,
    	symbol max   => coneList',
    	symbol cache => new CacheTable
	};
    if opts.WeilToClass =!= null then X.cache.fromWDivToCl = opts.WeilToClass;
    X.cache.CoefficientRing = opts.CoefficientRing;
    X.cache.Variable = opts.Variable;
    X
    )
normalToricVariety Matrix := opts -> vertices -> (
    if ring vertices =!= ZZ then error "--expected an integer matrix";
    lifting := matrix {toList (numColumns vertices : 1_ZZ)} || vertices;
    H := fourierMotzkin lifting;
    if opts.MinimalGenerators == true then lifting = (fourierMotzkin H)#0;
    rayList := entries transpose ( -submatrix(H#0,{1..numRows vertices},));
    M := (transpose H#0) * lifting;
    coneList := apply(numColumns M, j -> select(toList(0..numRows M - 1),  
      	    i -> M_(i,j) == 0));
    normalToricVariety (rayList, coneList,  
    	WeilToClass     => opts.WeilToClass,
    	CoefficientRing => opts.CoefficientRing,
    	Variable        => opts.Variable
	)
    )

isWellDefined NormalToricVariety := Boolean => X -> (
    -- CHECK DATA STRUCTURE
    -- check keys
    K := keys X;
    expectedKeys := set {symbol rays, symbol max, symbol cache};
    if set K =!= expectedKeys then (
	if debugLevel > 0 then (
	    added := toList (K - expectedKeys);
	    missing := toList (expectedKeys - K);
	    if #added > 0 then 
	    << "-- unexpected key(s): " << toString added << endl;
	    if #missing > 0 then 
	    << "-- missing keys(s): " << toString missing << endl
	    );	 
    	return false
    	);
    -- check types
    if not instance (X.rays, List) then (
	if debugLevel > 0 then 
	    << "-- expected `rays' to be a list" << endl;
	return false
	);
    if not all (X.rays, r -> instance (r, List)) then (
	if debugLevel > 0 then 
	    << "-- expected `rays' to be a list of lists" << endl;
	return false
	);
    if not all (X.rays, r -> all (r, i -> instance(i, ZZ))) then (
	if debugLevel > 0 then 
  	    << "-- expected `rays' to be a list of lists of integers" << endl;
	return false
	); 
    rayList := rays X;
    d := # (rayList#0);
    if not all (X.rays, r -> #r === d) then (
	if debugLevel > 0 then 
	    << "-- expected `rays' to be a list of equal length lists" << endl;
	return false
	); 
    if not instance (X.max, List) then (
	if debugLevel > 0 then 
	    << "-- expected `max' to be a list" << endl;
	return false
	);
    if not all (X.max, sigma -> instance (sigma, List)) then (
	if debugLevel > 0 then 
	    << "-- expected `max' to be a list of lists" << endl;
	return false
	); 
    if not all (X.max, sigma -> all (sigma, i -> instance (i, ZZ))) then (
	if debugLevel > 0 then 
	    << "-- expected `max' to be a list of lists of integers" << endl;
	return false
	);            
    n := # rayList; 
    if not all (X.max, sigma -> all (sigma, i -> 0 <= i and i < n)) then (
	if debugLevel > 0 then 
	    << "-- expected `max' to correspond to subsets of rays" << endl;
	return false
	);        
    if not instance (X.cache, CacheTable) then (
    	if debugLevel > 0 then 
	    << "-- expected `X.cache' to be a CacheTable" << endl;
    	return false
	);    
    -- CHECK MATHEMATICAL STRUCTURE
    coneList := max X;
    -- check whether every ray appears in some maximal cone
    if set toList (0..n-1) =!= set flatten coneList then (
    	if debugLevel > 0 then 
      	    << "-- some ray does not appear in maximal cone" << endl;
    	return false
	);
    -- check whether the cones are maximal
    if coneList =!= unique coneList or 
    any (coneList, sigma -> any (coneList, 
	    tau -> sigma != tau and all(tau, i -> member(i, sigma))
	    )
	) 
    then (
    	if debugLevel > 0 then 
	    << "-- some cone is not maximal" << endl;
    	return false
	);    
    dualCones := new MutableHashTable;
    m := # max X;
    -- loop over all maximal cones
    for i to m-1 do (
	C := transpose matrix apply (coneList#i, j -> rayList#j);
    	H := fourierMotzkin C;
    	dualCones#i = H#0 | H#1 | -H#1;
    	(C',L) := fourierMotzkin H;
    	-- check whether the maximal cone is strongly convex
    	if L != 0 then (
      	    if debugLevel > 0 then (
		<< "-- not all maximal cones are strongly convex" << endl
		);
      	    return false
	    ); 
    	-- check whether the rays are the primitive generators of the cone
    	if set entries transpose C' =!= set entries transpose C then (
      	    if debugLevel > 0 then (
		<< "-- the rays are not the primitive generators" << endl
		);
      	    return false
	    )
	);
    -- check whether the intersection of each pair of maximal cones is a cone
    for i to m-2 do (
    	for j from i+1 to m-1 do (
      	    C := set apply (
		toList (set(coneList#i)*set(coneList#j)), k -> rayList#k
		);	       
      	    (C',L) := fourierMotzkin (dualCones#i | dualCones#j);
      	    if C =!= set entries transpose C' then (
		if debugLevel > 0 then (
	  	    << "-- intersection of cones is not a cone" << endl
		    );
		return false
		)
	    )
	);
    true);

affineSpace = method (
    Options => {
    	CoefficientRing => KK,
    	Variable        => getSymbol "x"
	}
    )
affineSpace ZZ := NormalToricVariety => opts -> d -> (
    if d < 1 then error "-- expected a positive integer";
    normalToricVariety (entries id_(ZZ^d), {toList(0..d-1)}, 
    	CoefficientRing => opts.CoefficientRing, 
	Variable        => opts.Variable
	)
    );

toricProjectiveSpace = method (
    Options => {
    	CoefficientRing => KK,
    	Variable        => getSymbol "x"
	}
    )
toricProjectiveSpace ZZ := NormalToricVariety => opts -> d -> (
    if d < 1 then error "-- expected a positive integer";
    rayList := {toList(d:-1)} | entries id_(ZZ^d);
    coneList := subsets (d+1,d);
    normalToricVariety (rayList, coneList,
    	CoefficientRing => opts.CoefficientRing, 
	Variable        => opts.Variable
	)
    );

weightedProjectiveSpace = method (
    Options => {
    	CoefficientRing => KK,
    	Variable        => getSymbol "x"
	}
    )
weightedProjectiveSpace List := NormalToricVariety => opts -> q -> (
    if #q < 2 then error "-- expected a list with at least two elements";
    if not all (q, i -> i > 0) then error "-- expected positive integers";
    d := #q-1;
    if not all (subsets (q,d), s -> gcd s === 1) then (
    	error ("--  the " | toString d | "-elements have a common factor")
	);
    rayList := entries kernelLLL matrix {q};
    coneList := subsets (d+1,d);
    normalToricVariety (rayList, coneList,
    	CoefficientRing => opts.CoefficientRing, 
	Variable        => opts.Variable
	)
    );

hirzebruchSurface = method (
    Options => {
    	CoefficientRing => KK,
    	Variable        => getSymbol "x"
	}
    )
hirzebruchSurface ZZ := NormalToricVariety => opts -> a -> (
    rayList := {{1,0},{0,1},{ -1,a},{0,-1}};
    coneList := {{0,1},{1,2},{2,3},{0,3}};
    W := matrix {{1,-a,1,0},{0,1,0,1}};
    normalToricVariety (rayList, coneList, 
    	CoefficientRing => opts.CoefficientRing, 
    	Variable        => opts.Variable,
    	WeilToClass     => W
	)
    );

NormalToricVariety ** NormalToricVariety := NormalToricVariety => (X,Y) -> (
    rayList1 := transpose matrix rays X;
    rayList2 := transpose matrix rays Y;
    rayList := entries transpose (rayList1 ++ rayList2);
    coneList1 := max X;
    coneList2 := max Y;
    n := #rays X;
    coneList2 = apply (coneList2, sigma -> apply (sigma, i -> i+n));
    coneList := flatten table (coneList1, coneList2, 
	(sigma, tau) -> sigma | tau
	);
    W1 := fromWDivToCl X;
    W2 := fromWDivToCl Y;
    normalToricVariety (rayList, coneList, 
    	CoefficientRing => coefficientRing ring X,
    	WeilToClass     => W1 ++ W2
	)
    );

NormalToricVariety ^** ZZ := NormalToricVariety => (X,n) -> (
    if n <= 0 then error "-- expected a positive integer";
    if n == 1 then return X
    else return X ** (X ^** (n-1))
    );

kleinschmidt = method (
    Options => {
    	CoefficientRing => KK,
    	Variable        => getSymbol "x"
	}
    )
kleinschmidt (ZZ, List) := NormalToricVariety => opts -> (d,a) -> (
    if d < 0 then error "-- expected a nonnegative integer";
    r := #a;
    s := d-r+1;
    e := entries id_(ZZ^d);
    if r >= d then error "-- list is too long"; 
    rayList := {sum (r, i -> -e#i)} | apply(r, i -> e#i);
    rayList = rayList | apply (s-1, j -> e#(r+j));
    rayList = rayList | {sum (r, i -> a#i*e#i) - sum (s-1, j -> e#(r+j))};
    L := toList (0..r+s);
    coneList := flatten table (toList (0..r), toList (r+1..r+s), 
    	(i,j) -> select(L, k -> i =!= k and j =!= k));
    deg := {{0,1}} | apply (r, i -> { -a#i,1}) | apply (s, j -> {1,0});
    normalToricVariety (rayList, coneList, 
    	CoefficientRing => opts.CoefficientRing, 
    	Variable        => opts.Variable,
    	WeilToClass     => transpose matrix deg
	)
    );

-- THIS FUNCTION IS NOT EXPORTED. By reading an auxiliary file, this function
-- creates a HashTable with the defining data for the low dimensional smooth
-- Fano toric varieties.
smoothFanoToricVarietiesFile := currentFileDirectory | "NormalToricVarieties/smoothFanoToricVarieties.txt"
smoothFanoToricVarietiesFile5 := currentFileDirectory | "NormalToricVarieties/smoothFanoToricVarieties5.txt"
smoothFanoToricVarietiesFile6 := currentFileDirectory | "NormalToricVarieties/smoothFanoToricVarieties6.txt"
getFano := memoize(d -> (
    	local fanoFile;
    	if d < 5 then fanoFile = smoothFanoToricVarietiesFile
    	else if d === 5 then fanoFile = smoothFanoToricVarietiesFile5
    	else if d === 6 then fanoFile = smoothFanoToricVarietiesFile6;
    	if notify then stderr << "--loading file " << fanoFile << endl;
    	hashTable apply( lines get fanoFile, x -> (
		x = value x;
		((x#0,x#1), drop (x,2)) 
		)
    	    )
	)
    );

smoothFanoToricVariety = method (
    Options => {
    	CoefficientRing => KK,
    	Variable        => getSymbol "x"
	}
    )
smoothFanoToricVariety (ZZ,ZZ) := NormalToricVariety => opts -> (d, i) -> (
    if d < 1 or i < 0 then 
    	error "-- expected positive dimension or nonnegative index";
    if d === 1 and i > 0 then 
    	error "-- there is only one smooth Fano toric curve";
    if d === 2 and i > 4 then 
    	error "-- there are only five smooth Fano toric surfaces";
    if d === 3 and i > 17 then 
    	error "-- there are only 18 smooth Fano toric 3-folds";
    if d === 4 and i > 123 then 
    	error "-- there are only 124 smooth Fano toric 4-folds";
    if d === 5 and i > 865 then 
    	error "-- there are only 866 smooth Fano toric 5-folds";
    if d === 6 and i > 7621 then 
    	error "-- there are only 7622 smooth Fano toric 6-folds";
    if d > 6 then 
    	error "-- database doesn't include varieties with dimension > 6";
    if i === 0 then return toricProjectiveSpace d;
    if d < 5 then (
    	s := (getFano (d))#(d,i);
    	return normalToricVariety (s#0, s#1, 
	    CoefficientRing => opts.CoefficientRing, 
	    Variable        => opts.Variable,
	    WeilToClass     => transpose matrix s#2
	    )
	);
    s = (getFano (d))#(d,i);
    normalToricVariety (s#0, s#1,
	CoefficientRing => opts.CoefficientRing, 
	Variable        => opts.Variable
	)
    );

-- this function interfaces with the Polyhedra package
normalToricVariety Fan := opts -> F -> (
    normalToricVariety (entries transpose rays F, maxCones F,
    	CoefficientRing => opts.CoefficientRing,
	Variable        => opts.Variable,	
    	WeilToClass     => opts.WeilToClass 
	)
    );
------------------------------------------------------------------------------
-- this function interfaces with the Polyhedra package
normalToricVariety Polyhedron := opts -> (cacheValue symbol variety) (P -> (
	Q := P;
	if not isFullDimensional Q then (
	    d := dim Q;
	    C := cone Q;
	    -- restrict to the linear subspace spanned by the polytope
	    H := transpose facets C % transpose hyperplanes C;
	    Q = polyhedronFromHData (transpose H^{1..d}, transpose H^{0});
	    );
	normalToricVariety(normalFan Q,
	    CoefficientRing => opts.CoefficientRing,
	    Variable        => opts.Variable,       
	    WeilToClass     => opts.WeilToClass
	    ) 
	)
    );

------------------------------------------------------------------------------
-- Basic properties and invariants
------------------------------------------------------------------------------
-- The method 'rays' is defined in 'Polyhedra'
rays NormalToricVariety := List => X -> X.rays
max  NormalToricVariety := List => X -> X.max
dim NormalToricVariety := ZZ => (cacheValue symbol dim) (X -> #(rays X)#0)

isDegenerate = method ()
isDegenerate NormalToricVariety := Boolean => (
    cacheValue symbol isDegenerate) (
    X -> kernel matrix rays X != 0
    )

isSimplicial NormalToricVariety := Boolean => (
    cacheValue symbol isSimplicial) (
    X -> (
    	rayGenMatrix := transpose matrix rays X;
    	all (max X, sigma -> #sigma == rank rayGenMatrix_sigma) 
	)
    );

isSmooth NormalToricVariety := Boolean => (
    cacheValue symbol isSmooth) (X -> (
    	rayGenMatrix := transpose matrix rays X;
    	b := all(max X, sigma -> 
	    #sigma === rank rayGenMatrix_sigma and 
	    1 == minors(#sigma, rayGenMatrix_sigma) 
	    );
	if b === true then X.cache.isSimplicial = true;
	b 
	)
    );

isComplete NormalToricVariety := Boolean => (
    cacheValue symbol isComplete) (
    X -> (
    	-- there is only one complete normal toric variety of dimension one
    	if dim X === 1 then return (set rays X === set {{ -1},{1}});
    	if orbits (X, 1) == {} then return false;	
    	-- check to see that every torus-invariant curve is projective
    	for C in orbits (X, 1) do (
      	    m := 0;
      	    for sigma in max X when m < 2 do 
		if all (C, i -> member (i,sigma)) then m = m+1;
      	    if m < 2 then (
		if debugLevel > 0 then 
		    << "the curve " << toString C << " is not projective" << endl;
		return false
		)
	    );
    	true 
	)
    )

isProjective = method ()
isProjective NormalToricVariety := Boolean => (
    cacheValue symbol isProjective) (
    X -> (
    	if not isComplete X then return false;
    	-- projectivity is checked using Gale duality; see Theorem~V.4.8 in
    	-- Ewald's "Combinatorial convexity and algebraic geometry"
    	clX := classGroup X;
	if not isFreeModule clX then (
	    smithMatrix := presentation clX;
	    torsionlessCoord := select (rank target smithMatrix,
		i -> smithMatrix^{i} == 0
		)
	    )
	else torsionlessCoord = toList (0.. rank clX - 1);
	galeDualMatrix := matrix (fromWDivToCl X)^torsionlessCoord;
	outerNormals := matrix {
	    for sigma in max X list (
		sigma' := select(# rays X, i -> not member (i, sigma));
		dualCone := (fourierMotzkin galeDualMatrix_sigma');
		dualCone#0 | dualCone#1 | - dualCone#1
		)
	    };
	coneGens := fourierMotzkin outerNormals;
	coneGens = (coneGens#0 | coneGens#1);
	if coneGens == 0 then return false;
	0 == (fromPicToCl X)^torsionlessCoord % coneGens 
	)
    );

-- this function interfaces with the Polyhedra package
fan NormalToricVariety := Fan => X -> (
    rayMatrix := promote (matrix transpose rays X, QQ); 
    fan(rayMatrix, max X) 
    );

-- This method is not exported
facesOfCone = method ()
-- Given a matrix 'R' whose columns are the rays of a strongly convex cone and
-- a list 's' whose entries label the rays, the method makes a HashTable whose
-- keys label the faces and values give the codimension.
facesOfCone (Matrix, List) := HashTable => (R, s) -> (
    H := fourierMotzkin R;
    H = H#0 | H#1; 
    incidenceMatrix := (transpose H) * R;
    h := numColumns H;  
    hyperplaneTable := new MutableHashTable from apply(h, i -> {{i}, 
	    select(s, j -> incidenceMatrix_(i, position(s, l -> l === j)) === 0
	    	)
	    }
    	);
    faceTable := new MutableHashTable from 
        apply (values hyperplaneTable, f -> {f,1});
    faceTable#s = 0;
    d := rank R;
    Q := apply(h, i -> {i});
    while Q =!= {} do (
    	q := first Q;
    	Q = drop (Q,1);
    	for i from 0 to h-1 do if not member (i,q) then (
      	    t := select (hyperplaneTable#q, j -> member (j,hyperplaneTable#{i}));
      	    k := sort (q | {i});
      	    if t =!= {} and not hyperplaneTable#?k and not faceTable#?t then (
		hyperplaneTable#k = t;
		faceTable#t = d - rank R_(positions (s, i -> member (i,t)));
		Q = Q | {k}
		)
	    )
	);
    d = numRows R - d;
    new HashTable from apply(keys faceTable, f -> {f,d+faceTable#f}));
-- Given a list 'L' whose entries label rays in a simplicial cone and an
-- integer 'i' which is the codimension of the cone, this method makes a
-- HashTable whose keys label the faces and values give the codimension, In
-- the simplicial case, we don't actually need the rays of the cone.
facesOfCone (List,ZZ) := (L,i) -> new HashTable from 
    apply (drop (subsets (L), 1), s -> {s,#L-#s+i});

orbits = method ()   
orbits NormalToricVariety := HashTable => (
    cacheValue symbol orbits) (
    X -> (
    	hTable := new HashTable;
    	raysMatrix := transpose matrix rays X; 
    	d := dim X;
    	if isSimplicial X and not isDegenerate X then (
      	    for s in max X do (
		hTable = merge(hTable, facesOfCone (s, d - rank raysMatrix_s), 
		    (p,q) -> p
		    )
		)
	    )
    	else for s in max X do (
	    hTable = merge(hTable, facesOfCone (raysMatrix_s,s), (p,q) -> p));
    	O := new MutableHashTable from apply (d+1, i -> {i,{}});
    	for k in keys hTable do O#(hTable#k) = O#(hTable#k) | {k};
    	new HashTable from apply (keys O, k -> {k, sort O#k}) | {{d,{{}}}} 
	)
    );
orbits (NormalToricVariety, ZZ) := List => (X,i) -> (
    if i < 0 or i > dim X then 
    	error "-- expected a nonnegative integer that is at most the dimension";
    O := orbits X;
    O#i
    )

------------------------------------------------------------------------------
-- divisor, class, and Picard groups
classGroup = method ()
classGroup NormalToricVariety := Module => (
    cacheValue symbol classGroup) (
    X -> (
    	rawClX := cokernel matrix rays X;
    	clX := prune rawClX;
    	-- we also compute the map to the group of Weil divisors
    	wDiv := weilDivisorGroup X;
    	if X.cache.?fromWDivToCl then A := matrix X.cache.fromWDivToCl
    	else A =  matrix (clX.cache.pruningMap)^(-1);
    	X.cache.fromWDivToCl = map(clX, wDiv, A);	  
    	clX 
	)
    );

fromWDivToCl = method ()
fromWDivToCl NormalToricVariety := Matrix => X -> (
    if not X.cache.?classGroup then classGroup X;
    X.cache.fromWDivToCl
    )

weilDivisorGroup = method ()
weilDivisorGroup NormalToricVariety := Module => (
    cacheValue symbol weilDivisorGroup) (
    X -> ZZ^(# rays X)
    )

cartierDivisorGroup = method ();
cartierDivisorGroup NormalToricVariety := Module => (
    cacheValue symbol cartierDivisorGroup) (
    X -> (    
    	if isSmooth X then (
	    cDiv := weilDivisorGroup X;
	    X.cache.fromCDivToWDiv = id_cDiv;
	    return cDiv);
    	raysMat := transpose matrix rays X;
    	charLat := target raysMat;
    	perpTab := new MutableHashTable;
    	-- Proposition 4.2.9 in Cox-Little-Schenck describes the group of Cartier
    	-- divisors as the kernel of the following map
	coneList := max X;
	pairwiseIntersections := select (orbits (X,1), tau -> (
		m := 0;
		for sigma in max X when m < 2 do 
	    	    if all (tau, i -> member(i, sigma)) then m = m+1;
		m === 2
		)
	    );	
	if pairwiseIntersections === {} then (
	    rawCDiv := directSum for sigma in coneList list (
		if not perpTab#?sigma then 
	    	    perpTab#sigma = ker transpose raysMat_sigma;
		charLat / perpTab#sigma
		)
	    )
	else (
    	    gluingMap := matrix for tau in pairwiseIntersections list (
    	    	seen := false;
    	    	if not perpTab#?tau then 
	    	    perpTab#tau = ker transpose raysMat_tau;
    	    	for sigma in coneList list (
	    	    if not perpTab#?sigma then 
		    	perpTab#sigma = ker transpose raysMat_sigma;
	    	    if isSubset(tau, sigma) then (
	    	    	coeff := if seen then -1_ZZ else (
		    	    seen = true;
		    	    1_ZZ
			    );
	    	    	coeff * map(charLat / perpTab#tau, charLat / perpTab#sigma, 1)
			)
	    	    else map(charLat / perpTab#tau, charLat / perpTab#sigma, 0)
		    )
		);
    	    rawCDiv = image LLL gens ker gluingMap); 
    	cDiv = prune rawCDiv;
    	-- we also compute the map to the group of Weil divisors
    	n := # rays X;
	-- inclusionMap * matrix apply(# max X, i -> {id_(charLat)}) == matrix rays X
    	inclusionMap := matrix for i from 0 to n-1 list (
    	    seen := false;    	    
    	    for sigma in coneList list (
	    	if member (i,sigma) then (
	    	    coeff := if seen then 0 else (
		    	seen = true;
		    	1
			);
	    	    coeff * map (ZZ^1, charLat / perpTab#sigma, transpose raysMat_{i})
		    )
	    	else map (ZZ^1, charLat / perpTab#sigma, 0)
		)
	    );
    	X.cache.fromCDivToWDiv = map (weilDivisorGroup X, cDiv,
	    (matrix inclusionMap) * (matrix gens rawCDiv) * (matrix cDiv.cache.pruningMap) 
	    );
    	cDiv ));

fromCDivToWDiv = method ()
fromCDivToWDiv NormalToricVariety := Matrix => X -> (
    if not X.cache.?fromCDivToWDiv then cartierDivisorGroup X;
    X.cache.fromCDivToWDiv 
    );

picardGroup = method ()
picardGroup NormalToricVariety := Module => (
    cacheValue symbol picardGroup) (
    X -> (
    	if isSmooth X then (
      	    clX := classGroup X;
      	    X.cache.fromPicToCl = id_clX;
      	    X.cache.fromCDivToPic = fromWDivToCl X;
      	    return clX 
	    );
	charLat := ZZ^(dim X);
      	rayMatrix := map (weilDivisorGroup X, charLat, matrix rays X);
      	rawPicGroup := subquotient (fromCDivToWDiv X, rayMatrix);
      	picGroup := prune rawPicGroup;
      	fromPicToCDiv := picGroup.cache.pruningMap;
      	X.cache.fromCDivToPic = map (picGroup, cartierDivisorGroup X, 
	    fromPicToCDiv^(-1));
      	X.cache.fromPicToCl = map (classGroup X, picGroup, 
	    (fromWDivToCl X) * (fromCDivToWDiv X) * matrix fromPicToCDiv);
      	picGroup 
	)
    );

fromPicToCl = method ()
fromPicToCl NormalToricVariety := Matrix => X -> (
    if not X.cache.?fromPicToCl then picardGroup X;
    X.cache.fromPicToCl 
    );

fromCDivToPic = method ()
fromCDivToPic NormalToricVariety := Matrix => X -> (
    if not X.cache.?fromCDivToPic then picardGroup X;
    X.cache.fromCDivToPic 
    );
     
nefGenerators = method ()
nefGenerators NormalToricVariety := Matrix => X -> (
    if isDegenerate X then 
	error "-- not implemented for degenerate varieties";
    clX := classGroup X;
    if clX == 0 then return matrix {{}};
    if not isFreeModule clX then (
	smithMatrix := presentation clX;
	torsionlessCoord := select (rank target smithMatrix, 
	    i -> smithMatrix^{i} == 0
	    )
	)
    else torsionlessCoord = toList (0.. rank clX - 1);
    galeDualMatrix := matrix (fromWDivToCl X)^torsionlessCoord;
    innerNormals := matrix {for sigma in max X list (
	    sigma' := select(# rays X, i -> not member(i, sigma));
	    dualCone := fourierMotzkin galeDualMatrix_sigma';
	    dualCone#0 | dualCone#1 | -dualCone#1 
	    )
	};
    coneGens := fourierMotzkin innerNormals;
    coneGens = coneGens#0 | coneGens#1 | - coneGens#1;
    if not isFreeModule clX then (
    	rowCounter := 0;
	coneGens = matrix for i to rank target smithMatrix - 1 list (
    	    if member(i, torsionlessCoord) then (
		rowCounter = rowCounter+1;
		{coneGens^{rowCounter-1}}
		)
    	    else {0*coneGens^{0}}
	    )
	);
    fromPic := matrix fromPicToCl X;
    indexOfPic := abs lcm (minors( rank source fromPic, fromPic^torsionlessCoord))_*;
    (indexOfPic * coneGens) // fromPic 
    );


------------------------------------------------------------------------------
-- toric divisors
------------------------------------------------------------------------------
ToricDivisor = new Type of HashTable
ToricDivisor.synonym = "toric divisor"

expression ToricDivisor := Expression => D -> (
   X := variety D;
    divisorSymbol := if hasAttribute(X,ReverseDictionary) then 
    	expression toString getAttribute(X,ReverseDictionary) 
	else expression "D";
    S := support D;
    if S === {} then return expression 0;
    Sum apply(S, j -> (
	    coeff := expression abs(D#j);
	    if D#j === -1 then Minus Subscript{divisorSymbol, j}
	    else if D#j < 0 then Minus {coeff * Subscript{divisorSymbol, j}}
	    else if D#j === 1 then Subscript{divisorSymbol, j}
	    else coeff * Subscript{divisorSymbol, j} 
	    )
	)
    );  

net ToricDivisor := D -> net expression D
ToricDivisor#{Standard,AfterPrint} = 
ToricDivisor#{Standard,AfterNoPrint} = D -> (
    << endl;				  -- double space
    << concatenate(interpreterDepth:"o") << lineNumber; 
    << " : ToricDivisor on " << variety D << endl
    );


normalToricVariety ToricDivisor := NormalToricVariety => opts -> D -> D.variety
variety ToricDivisor := NormalToricVariety => D -> normalToricVariety D
entries ToricDivisor := List => D -> apply (# rays variety D, i -> D#i)
vector ToricDivisor := Vector => D -> vector entries D
support ToricDivisor := List => D -> (
    n := # rays variety D;
    select ( toList (0..n-1), i -> D#i =!= 0)
    );
degree ToricDivisor := D -> entries ( (fromWDivToCl variety D) * (vector D));

monomials ToricDivisor := List => opts -> D -> (
    X := variety D;    
    if not isProjective X then 
	error "--expected the underlying toric variety to be projective";
    P := polytope D;
    S := ring X;
    degs := matrix rays X;
    coeff := matrix vector D;
    points := if isEmpty P then {} else latticePoints P;
    sort for v in points list (
	e := flatten entries (degs * v + coeff);
	S_e
	)
    );

toricDivisor = method (
    TypicalValue => ToricDivisor,
    Options => {
    	CoefficientRing => KK,
    	Variable        => getSymbol "x",
    	WeilToClass     => null
	}
    )
toricDivisor (List, NormalToricVariety) := ToricDivisor => opts -> 
(coefficientList, X) -> (
    n := #coefficientList;
    if n =!= # rays X then 
    	error "-- number of elements in the list is not equal to the number rays";
  new ToricDivisor from apply (#coefficientList, i -> i => coefficientList#i) | {
      symbol variety => X, 
      symbol cache   => new CacheTable
      }
  )

toricDivisor NormalToricVariety := ToricDivisor => opts -> X -> 
    sum(# rays X, i -> -X_i)

-- this function interfaces with the Polyhedra package
toricDivisor Polyhedron := ToricDivisor => opts -> P -> (
    if not isLatticePolytope P then error "expected a lattice polytope";
    verticesOfP := sub (vertices P, ZZ);
    X := normalToricVariety (verticesOfP, 
	CoefficientRing => opts.CoefficientRing,
    	Variable        => opts.Variable,
    	WeilToClass     => opts.WeilToClass
	);
    rayListMatrix := matrix rays X;
    coefficientList := apply (entries (- rayListMatrix * verticesOfP), 
	r -> max r);
    sum(# rays X, i -> coefficientList#i * X_i)
    );

-- THIS FUNCTION IS NOT EXPORTED. By reading an auxiliary file, this function
-- creates a HashTable with the defining data for the small smooth projective
-- toric varieties.
smallSmoothFile := currentFileDirectory | "NormalToricVarieties/smallSmoothProjectiveToricVarieties.txt"
getSmallSmooth := hashTable apply (lines get smallSmoothFile, x -> (
	x = value x;
	((x#0,x#1), drop (x,2)) 
	)
    );

smallAmpleToricDivisor = method (
    Options => {
    	CoefficientRing => KK,
    	Variable        => getSymbol "x",
	WeilToClass     => null
	}
    )

smallAmpleToricDivisor (ZZ, ZZ) := ToricDivisor => opts -> (d, i) -> (
    if i < 0 then error "-- expected nonnegative index";
    if d < 1 then error "-- expected a positive dimension";
    if d > 3 then error "-- expected the dimension to be at most 3";
    if d === 2 and i > 40 then 
	error "-- there are currently only 41 small smooth toric surfaces";
    if d === 3 and i > 102 then 
	error "-- there are currently only 103 small smooth toric 3-folds";
    s := getSmallSmooth#(d,i);
    X := normalToricVariety (s#0, s#1,
	CoefficientRing => opts.CoefficientRing, 
	Variable        => opts.Variable,
	WeilToClass     => opts.WeilToClass
	);
    toricDivisor (s#2, X) 
    );

	
isWellDefined ToricDivisor := Boolean => D -> (
    if not instance(D.variety, NormalToricVariety) then (
	if debugLevel > 0 then 
	    << "expected a divisor over a normal toric variety" << endl;
	return false
	);    
    n := # rays variety D;
    -- CHECK DATA STRUCTURE
    -- check keys
    K := keys D;
    expectedKeys := set (toList (0..n-1) | {symbol variety, symbol cache});
    if set K =!= expectedKeys then(
	if debugLevel > 0 then (
	    added := toList(K - expectedKeys);
      	    missing := toList(expectedKeys - K);
      	    if #added > 0 then 
		<< "-- unexpected key(s): " << toString added << endl;
      	    if #missing > 0 then 
		<< "-- missing key(s): " << toString missing << endl;
	    );
    	return false
	);
    -- check types
    for i from 0 to n-1 do (
	if not instance(D#i, ZZ) then (
	    if debugLevel > 0 then 
		<< "expected " << i << "-th coefficient to be an integer" << endl;
	    return false
	    )
	);
    if not instance(D.cache, CacheTable) then (
	if debugLevel > 0 then 
	    << "expected a cache to be a CacheTable" << endl;
	return false
	);	
    true 
    );

NormalToricVariety _ ZZ := ToricDivisor => (X, i) -> (
    n := # rays X;
    if i < 0 or i > n-1 then 
    	error "-- expect the integer to index a ray of normal toric variety";
    coefficientList := apply(n, j -> if j === i then 1_ZZ else 0_ZZ);
    toricDivisor(coefficientList, X) 
    );

ToricDivisor == ToricDivisor := Boolean => (D, E) -> 
    variety D === variety E and entries D === entries E;

ToricDivisor + ToricDivisor := ToricDivisor => (D,E) -> (
    X := variety D;
    if X =!= variety E then error "-- expected divisors on the same variety";
    toricDivisor (apply (# rays X, i -> D#i+E#i), X) 
    );
ToricDivisor - ToricDivisor := ToricDivisor => (D,E) -> (
    X := variety D;
    if X =!= variety E then error "-- expected divisors on the same variety";
    toricDivisor (apply (# rays X, i -> D#i-E#i), X) 
    );
ZZ * ToricDivisor := ToricDivisor => (n,D) -> (
    X := variety D;
    toricDivisor (apply (# rays X, i -> n*D#i), X) 
    );
- ToricDivisor := ToricDivisor => D -> (-1)*D

installMethod(symbol SPACE, OO, ToricDivisor, (OO, D) -> (
  	X := variety D;
  	a := toSequence entries (fromWDivToCl X * vector D);
  	OO_X a
	)
    );

isEffective = method ()
isEffective ToricDivisor := Boolean => D -> all (entries D, i -> i >= 0)

isCartier = method ()
isCartier ToricDivisor := Boolean => D -> 
    matrix vector D % fromCDivToWDiv variety D == 0

isQQCartier = method ()
isQQCartier ToricDivisor := Boolean => D -> (
    X := variety D;
    rayMatrix := (matrix rays X) ** QQ;
    a := (matrix vector D) ** QQ;
    coneList := max X;
    m := apply (coneList, s -> a^s // rayMatrix^s);
    all (apply (# coneList, 
	    i -> a^(coneList#i) - rayMatrix^(coneList#i)*m#i), j -> j == 0)
    );


-- THIS METHOD IS NOT EXPORTED.  Given a toric divisor which is assumed to be
-- Cartier, this method returns the characters on each maximal cone which
-- determine the Cartier divisor.
cartierCoefficients = method ()
cartierCoefficients ToricDivisor := List => D -> (
    X := variety D;
    rayMatrix := matrix rays X;
    coeffs := transpose (matrix {entries D});
    apply (max X, sigma -> coeffs^sigma // rayMatrix^sigma)
    );

isNef = method ()
isNef ToricDivisor := Boolean => D -> (
    X := variety D;
    if not isComplete X or not isQQCartier D then return false;
    -- the unique complete toric variety of dimension one is the projective
    -- line which we treat as a simply special case 	 
    if dim X === 1 then return sum entries D >= 0;
    -- a torus-invariant divisor is nef if and only if the intersection with
    -- every torus-invariant curve is nonnegative
    m := cartierCoefficients D;
    coneList := max X;
    rayMatrix := matrix rays X;
    all (orbits(X,1), 
	tau -> (
	    (p,q) := toSequence select (#coneList, 
		i -> all (tau, j -> member(j,coneList#i)));
	    k := position (coneList#q, i -> not member (i,tau));
	    v := promote (rayMatrix^{coneList#q#k} * (m#q-m#p), QQ);
	    N := prune coker transpose (rayMatrix^tau ** QQ);
	    u := transpose matrix (N.cache.pruningMap)^(-1);
	    w := promote (rayMatrix^{coneList#q#k}, QQ) * u;
	    if w_(0,0) < 0 then w = (-1)* w;
	    (v // w)_(0,0) >= 0 
	    )
	)
    );

isAmple = method ()
isAmple ToricDivisor := Boolean => D -> (
    X := variety D;
    if not isComplete X or not isCartier D then return false;
     -- the unique complete toric variety of dimension one is the projective
     -- line which we treat as a simply special case
     if dim X === 1 then return sum entries vector D > 0;
     -- the "toric Kleiman criterion" states that a torus-invariant divisor is
     -- ample if and only if the interesection with every torus-invariant
     -- curve is positive
     m := cartierCoefficients D;
     coneList := max X;
     rayMatrix := matrix rays X;
     all (orbits (X,1), 
	 c -> (
	     (p,q) := toSequence select (#coneList, 
		 i -> all (c, j -> member(j, coneList#i)));
	     k := position (coneList#q, i -> not member (i,c));
	     v := promote (rayMatrix^{coneList#q#k} * (m#q-m#p), QQ);
	     N := prune coker transpose (rayMatrix^c ** QQ);
	     u := transpose matrix (N.cache.pruningMap)^(-1);
	     w := promote (rayMatrix^{coneList#q#k},QQ) * u;
	     if w_(0,0) < 0 then w = (-1)* w;
	     (v // w)_(0,0) > 0
	     )
	 ) 
     );

hilbertBasis(Matrix,Thing) := Matrix => opts -> (C, notused) -> (
    transpose (normaliz(transpose C,"integral_closure"))#"gen")

isVeryAmple ToricDivisor := Boolean => D -> (
    if not isAmple D then return false;
    if isSmooth variety D then return true;    
    V := vertices D;
    n := numColumns V;
    d := numRows V;
    L := latticePoints D;
    m := numColumns L;
    all (n, 
	i -> (
	    H := hilbertBasis(V - matrix {toList(n:1)} ** V_{i}, "notused");
	    P := L - matrix {toList(m:1)} ** V_{i};
	    isSubset(set entries transpose H, set entries transpose P)
	    )
	)
    );
     
isFano = method ()
isFano NormalToricVariety := Boolean => X -> isAmple (- toricDivisor X)

vertices ToricDivisor := Matrix => D -> (
    if not isCartier D then 
	error "-- expected a Cartier divisor";
    X := variety D;
    if not isComplete X then 
    	error "-- expected a divisor on a complete toric variety";
    if not isEffective D then return null;
    d := dim X;
    V := transpose (matrix vector D | matrix rays variety D);
    V = V | transpose matrix {{1} | toList(d:0)};
    -( (fourierMotzkin V)#0)^{1..d} 
    );

latticePoints ToricDivisor := Matrix => D -> (
    V := vertices D;
    if V === null then return null;
    d := numRows V;
    V = transpose (normaliz (transpose V,"polytope"))#"gen";
    s := select (numColumns V, i -> V_(d,i) === 1);
    c := (V_s)^{0..d-1};
    c_(sortColumns c) 
    );

polytope ToricDivisor := Polyhedron => (
    cacheValue symbol polytope) (
    D -> polyhedronFromHData(-matrix rays variety D, matrix vector D)
    );

------------------------------------------------------------------------------
-- Total coordinate rings
ring NormalToricVariety := PolynomialRing => (
    cacheValue symbol ring) (
    X -> (
    	if isDegenerate X then 
      	    error "-- not yet implemented for degenerate varieties";
    	if not isFreeModule classGroup X then 
      	    error "-- gradings by torsion groups not yet implemented";
    	-- constructing ring
    	K := X.cache.CoefficientRing;	  
    	x := X.cache.Variable;	  
    	n := #rays X;
    	deg := entries transpose matrix fromWDivToCl X;
    	S := K (monoid [x_0..x_(n-1), Degrees => deg]);
    	S.variety = X;
    	S 
	)
    );

normalToricVariety Ring := NormalToricVariety => opts -> S -> variety S

ideal NormalToricVariety := Ideal => (
    cacheValue symbol ideal) (
    X -> (
    	S := ring X;
    	ideal apply (max X, L -> product (numgens S, 
		i -> if member (i,L) then 1_S else S_i) 
	    )
	)
    );
monomialIdeal NormalToricVariety := MonomialIdeal => X -> monomialIdeal ideal X

------------------------------------------------------------------------------
sheaf (NormalToricVariety, Module) := CoherentSheaf => (X,M) -> (
    if ring M =!= ring X then 
    	error "-- expected the module and the variety to have the same ring";
    if not isHomogeneous M then 
    	error "-- expected a homogeneous module";
    -- constructing coherent sheaf
    new CoherentSheaf from {
    	symbol module  => M,
    	symbol variety => X
	}
    );
sheaf (NormalToricVariety, Ring) := SheafOfRings => (X,R) -> (
    if ring X =!= R then 
	error "-- expected the ring of the variety";
    new SheafOfRings from { 
      	symbol variety => X, 
      	symbol ring    => R
	}
    );
sheaf NormalToricVariety := X -> sheaf_X ring X

CoherentSheaf#{Standard,AfterPrint} = F -> (
    X := variety F;
    M := module F;
    << endl;				  -- double space
    n := rank ambient F;
    << concatenate(interpreterDepth:"o") << lineNumber << " : coherent sheaf on " << X;
    if M.?generators then
    if M.?relations then << ", subquotient of " << ambient F
    else << ", subsheaf of " << ambient F
    else if M.?relations then << ", quotient of " << ambient F;
    << endl;)

installMethod(symbol _, OO, NormalToricVariety, (OO,X) -> sheaf(X, ring X))

CoherentSheaf Sequence := CoherentSheaf => (F,a) -> sheaf(variety F, 
    F.module ** (ring F)^{toList(a)})
   
sheafHom(CoherentSheaf,CoherentSheaf) := (F,G) -> (
    sheaf(variety F, Hom(module F, module G)) );
Hom(CoherentSheaf,CoherentSheaf) := Module => (F,G) -> HH^0(variety F, sheafHom(F,G))

CoherentSheaf.directSum = args -> (
    assert(#args > 0);
    X := variety args#0;
    if not all(args, F -> variety F === X) then (
    	error "-- expected all sheaves to be over the same ring");
    sheaf(X, directSum apply(args, F -> F.module)) );

SheafOfRings Sequence := CoherentSheaf => (O,a) -> O^1 a

super   CoherentSheaf := CoherentSheaf => F -> sheaf(variety F, super   module F)
ambient CoherentSheaf := CoherentSheaf => F -> sheaf(variety F, ambient module F)
cover   CoherentSheaf := CoherentSheaf => F -> sheaf(variety F, cover   module F)

minimalPresentation CoherentSheaf := 
prune CoherentSheaf := opts -> F -> (
    X := variety F;
    if instance(X, NormalToricVariety) then (
    	M := module F;
    	S := ring M;
    	B := ideal X;
    	N := saturate(image map(M,S^0,0), B);
    	if N != 0 then M = M/N;
    	C := res M;
    	-- is there a better bound?
    	a := max(1, max flatten flatten apply(length C +1, i -> degrees C#i));
    	return sheaf(X, minimalPresentation Hom(B^[a], M)) );
    sheaf minimalPresentation HH^0 F(>=0) );

cotangentSheaf NormalToricVariety := CoherentSheaf => opts -> (
    (cacheValue (symbol cotangentSheaf => opts)) (
	X -> (
      	    if isDegenerate X then 
		error "-- expect a non-degenerate toric variety";
      	    S := ring X;
      	    d := dim X;
      	    n := numgens S;
      	    nu := map (S^n, S^d, (matrix rays X) ** S);
      	    eta := map (directSum apply(n, i -> S^1 / ideal(S_i)), S^n, id_(S^n));
      	    om := sheaf (X, kernel (eta * nu));
      	    if opts.Minimize then om = minimalPresentation om;
      	    om 
	    )
	)
    );

cotangentSheaf(ZZ,NormalToricVariety) := CoherentSheaf => opts -> (i,X) -> 
  exteriorPower (i, cotangentSheaf (X,opts)) 


-- THIS FUNCTION IS NOT EXPORTED.  Given a normal toric variety, this function
-- creates a HashTable describing the cohomology of all twists of the
-- structure sheaf.  For more information, see Propositon~3.2 in
-- Maclagan-Smith "Multigraded regularity"
setupHHOO = X -> (
    X.cache.emsBound = new MutableHashTable;
    -- create a fine graded version of the total coordinate ring
    S := ring X;
    n := numgens S;
    fineDeg := entries id_(ZZ^n);
    h := toList (n:1);
    R := QQ (monoid [gens S, Degrees => fineDeg, Heft => h]);
    RfromS := map (R, S, gens R);
    B := RfromS ideal X;
    -- use simplicial cohomology find the support sets 
    quasiCech := Hom (res (R^1/B), R^1);
    supSets := delete ({}, subsets (toList (0..n-1)));
    d := dim X;
    sigma := new MutableHashTable;
    sigma#0 = {{{},1}};
    for i from 1 to d do (
    	sHH := prune HH^(i+1)(quasiCech);
    	sigma#i = for s in supSets list (
      	    m := product (s, j ->  R_j);
      	    b := rank source basis (-degree m, sHH);
      	    if b > 0 then {s,b} else continue
	    )
	);
    -- create rings
    degS := degrees S; 
    X.cache.rawHHOO = new HashTable from apply(d+1, 
	i -> {i, apply(sigma#i, s -> (
	  	    v := - degree product(n, 
			j -> if member(j,s#0) then S_j else 1_S
			);
	  	    degT := apply(n, 
	    		j -> if member(j,s#0) then -degS#j else degS#j
			);
	  	    T := (ZZ/2)(monoid [gens S, Degrees => degT]);
	  	    {v,T,s#0,s#1}
		    )
		)
	    }
	)
    );

-- Defines the Frobenius power of an ideal
Ideal ^ Array := (I,p) -> ideal apply (I_*, i -> i^(p#0))

-- THIS FUNCTION IS NOT EXPORTED.  This function creates a HastTable which
-- stores the data for determining the appropriate Frobenius power needed to
-- compute the cohomology of a general coherent sheaf; see Proposition 4.1 in
-- Eisenbud-Mustata-Stillman.
emsbound = (i, X, deg) -> (
    if not X.cache.emsBound#?{i,deg} then (
    	if i < 0 or i > dim X then X.cache.emsBound#{i,deg} = 1
    	else X.cache.emsBound#{i,deg} = max ( {1} | 
	    apply (X.cache.rawHHOO#i, 
		t -> # t#2 + max apply(first entries basis(deg-t#0,t#1),
	  	    m -> max (first exponents m)_(t#2)
		    )
		)
	    )
	);
    X.cache.emsBound#{i,deg}
    );

cohomology (ZZ, NormalToricVariety, CoherentSheaf) := Module => opts -> (i,X,F) -> (
    if ring F =!= ring X then 
    	error "-- expected a coherent sheaf on the toric variety";
    S := ring X;
    kk := coefficientRing S;
    if not isField kk then error "-- expected a toric variety over a field";
    if i < 0 or i > dim X then kk^0
    else (
    	if not X.cache.?rawHHOO then setupHHOO X;
    	M := module F;
    	if isFreeModule M then kk^(
      	    sum apply (degrees M, deg -> sum apply(X.cache.rawHHOO#i,
	  	    t -> rank source basis (-deg-t#0,(t#1)^(t#3))
		    )
		)
	    )
    	else (
      	    B := ideal X;
      	    C := res M;
      	    deg := toList (degreeLength S : 0);
      	    bettiNum := flatten apply (1+length C, 
		j -> apply (unique degrees C_j, alpha -> {j,alpha}));
      	    b1 := max apply (bettiNum, 
		beta -> emsbound(i+beta#0-1,X,deg-beta#1)
		);
      	    b2 := max apply(bettiNum, 
		beta -> emsbound(i+beta#0,X,deg-beta#1)
		);	       
      	    b := max(b1,b2);
      	    if i > 0 then 
		kk^(rank source basis (deg, Ext^(i+1)(S^1/B^[b],M)))
      	    else (
		h1 := rank source basis (deg, Ext^(i+1)(S^1/B^[b],M));
		h0 := rank source basis (deg, Ext^i(S^1/B^[b1],M));
		kk^(rank source basis (deg,M) + h1 - h0)
		)
	    )
	)
    );

cohomology (ZZ, NormalToricVariety, SheafOfRings) := Module => opts -> 
    (i, X ,O) -> HH^i (X, O^1)

euler CoherentSheaf := F -> (
    X := variety F;
    if class variety F === NormalToricVariety then 
    	return sum (1 + dim X, i -> (-1)^i * (rank HH^i(X,F)) );
    if class variety F === ProjectiveVariety then return euler module F;
    error "-- expected a sheaf on a ProjectiveVariety or NormalToricVariety");


------------------------------------------------------------------------------
-- abstract toric varieties
------------------------------------------------------------------------------

intersectionRing (NormalToricVariety, AbstractVariety) := Ring => (X, B) -> (
    if not isComplete X then 
        error "-- not yet implemented for a non-complete normal toric variety";    
    if not isSimplicial X then 
        error "-- not yet implemented for a non-simplicial normal toric variety";
    if not X.cache#?(intersectionRing, B) then X.cache#(intersectionRing, B) = (
    	kk := intersectionRing B;
    	n := # rays X;
    	t := getSymbol "t";
    	R := kk(monoid [t_0 .. t_(n-1), Join => false]);
    	M := dual monomialIdeal apply (max X, 
	    L -> product (n, i -> if member (i,L) then 1_R else R_i)
	    );
    	L := ideal (vars R * (matrix rays X) ** R);
    	R / (M + L)
    	);
    X.cache#(intersectionRing, B)
    );
intersectionRing NormalToricVariety := Ring => X -> intersectionRing (X, point)

abstractVariety (NormalToricVariety, AbstractVariety) := AbstractVariety => 
    opts -> (X, B) -> (
    	if not isComplete X then 
  	    error "-- not yet implemented for a non-complete normal toric variety";        
    	if not isSimplicial X then 
    	    error "-- not yet implemented for a non-simplicial normal toric variety";
    	if not X.cache#?(abstractVariety, B) then X.cache#(abstractVariety, B) = (
	    A := intersectionRing (X, B);
	    aX := abstractVariety (dim X, A);
	    aX.TangentBundle = abstractSheaf (aX, 
	    	Rank       => dim X, 
	    	ChernClass => product(gens A, x -> 1+x)
	    	);
	    -- Now we determine the mapping 'integral':
	    raysX := transpose matrix rays X;
	    onecone := first max X;
	    pt := (abs det raysX_onecone) * product(onecone, i -> A_i);
	    if size pt != 1 then 
	        error "-- cannot define integral: some strange error has occurred";
	    mon := leadMonomial pt;
	    cf := leadCoefficient pt;
	    if not liftable(cf, QQ) then 
	        error "-- cannot create integral function";
	    a := 1 / lift(cf, QQ);
	    integral A := f -> a * coefficient(mon, f);
	    -- a check:
	    assert all (max X, f -> integral(product(f, i -> A_i)) == 1 / (abs det raysX_f));
	    aX
	    );
    	X.cache#(abstractVariety, B)
    	);
abstractVariety NormalToricVariety := opts -> X -> abstractVariety (X, point)

abstractSheaf (NormalToricVariety, AbstractVariety, ToricDivisor) := opts -> 
    (X, B, D) -> (
    	aX := abstractVariety (X, B);
    	A := intersectionRing aX;
    	OO_aX ( ((vars A) * vector D)_0 )
    	);
abstractSheaf (NormalToricVariety, ToricDivisor) := opts -> (X, D) -> 
    abstractSheaf (X, point, D)

-- THIS FUNCTION IS NOT EXPORTED.  For a given normal toric variety, it
-- constructs a one-sided inverse for the map from the group of Weil divisors
-- to the class group.  It is used, in the next method, to choose an
-- equivariant representative for a given line bundle.
liftClToWDiv := X -> (
    A := fromWDivToCl X;
    I := id_(ZZ^(numRows A));
    if I % A != 0 then 
	error "-- cannot a lift from the class group to group of weil divisors?";
    I // A
    )
abstractSheaf (NormalToricVariety, AbstractVariety, CoherentSheaf) := opts -> 
    (X, B, F) -> (
      	if variety F =!= X then 
	    error "-- expected a coherent sheaf over the given variety";
      	M := module F;
        aX := abstractVariety (X, B);
       	A := intersectionRing aX;
      	lift := liftClToWDiv X;
	f := poincare M;
	p := pairs standardForm f;
	m := numgens ring f;
	r := sum (p, (d, c) -> (
		deg := matrix {apply (m, j -> if d#?j then -d#j else 0)};
		L := OO_aX first first entries (vars A * lift * transpose deg);
		c * ch L
		)
    	    );
	abstractSheaf (aX, ChernCharacter => r)
	);
abstractSheaf (NormalToricVariety, CoherentSheaf) := opts -> (X, F) -> 
    abstractSheaf (X, point, F)    	       
    
    
chern CoherentSheaf := RingElement => F -> chern abstractSheaf (variety F, F)
chern (ZZ, CoherentSheaf) := RingElement => (p, F) -> (
    chern (p, abstractSheaf (variety F, F)))
chern (ZZ, ZZ, CoherentSheaf) := RingElement => (p, q, F) -> (
    chern (p, q, abstractSheaf (variety F, F)))

ctop CoherentSheaf := RingElement => F -> ctop abstractSheaf (variety F, F)

ch CoherentSheaf := RingElement => F -> ch abstractSheaf (variety F, F)
ch (ZZ, CoherentSheaf) := RingElement => (n, F) -> ch (n, abstractSheaf (variety F, F))

todd CoherentSheaf := RingElement => F -> todd abstractSheaf (variety F, F)
todd NormalToricVariety := RingElement => X -> todd abstractVariety X

chi CoherentSheaf := RingElement => F -> chi abstractSheaf (variety F, F)

segre CoherentSheaf := RingElement => F -> segre abstractSheaf (variety F, F)
segre (ZZ, CoherentSheaf) := RingElement => (p, F) -> segre (p, abstractSheaf (variety F, F))


hilbertPolynomial NormalToricVariety := RingElement => opts -> (
    cacheValue symbol hilbertPolynomial) (
    X -> (
	if not isSmooth X then 
	error "-- not yet implemented for singular normal toric varieties";
	if not isComplete X then 
	error "-- not yet implemented for non-complete normal toric varieties";
	r := rank picardGroup X;
	i := getSymbol "i";	   
	pt := base (i_0 .. i_(r-1));
	V := abstractVariety (X, pt);
	S := intersectionRing V;
	R := coefficientRing S;
	sigma := first max X;
	sigmaVee := select (# rays X, j -> not member (j, sigma));
	f := ((vars S)_sigmaVee * 
	    ((fromCDivToPic X)_sigmaVee)^(-1) * transpose vars R)_(0,0);
	chi OO_V (f)
	)
    );

hilbertPolynomial (NormalToricVariety, Module) := RingElement => opts -> 
    (X,M) -> (
  	if not isHomogeneous M then error "-- expected a homogeneous module";
  	if ring X =!= ring M then 
	    error "-- expected a module over the total coordinate ring";
  	h := hilbertPolynomial X;
  	R := ring h;
  	r := numgens R;
  	f := poincare M;
  	p := pairs standardForm f;
  	if #p === 0 then 0_R
  	else sum (p, (d,c) -> (
      		shift := apply (r, j -> if d#?j then R_j - d#j else R_j);
      		c * sub (h, matrix{shift})
		)
	    )
	);

hilbertPolynomial (NormalToricVariety, Ring) := RingElement => opts -> 
    (X, S) -> hilbertPolynomial (X, S^1, opts)
hilbertPolynomial (NormalToricVariety, SheafOfRings) := RingElement => 
    opts -> (X, S) -> hilbertPolynomial (X, ring S, opts)    
hilbertPolynomial (NormalToricVariety, Ideal) := RingElement => opts -> 
    (X, I) -> hilbertPolynomial (X, (ring I)^1/I, opts)
hilbertPolynomial (NormalToricVariety, CoherentSheaf) := RingElement => 
    opts -> (X, F) -> hilbertPolynomial (X, module F, opts)



------------------------------------------------------------------------------
-- resolution of singularities
------------------------------------------------------------------------------

-- THIS METHOD IS NOT EXPORTED.  Given a normal toric variety 'X', a maximal
-- cone indexed by the list 's', and a weight vector encoded by the integer
-- list 'w', this method makes a new normal toric variety in which the maximal
-- cone corresponding to 's' has been replace by the regular subdivison
-- associated to the weight vector 'w'.  In particular, the entries in 'w' are
-- used as heights to lift the maximal cone corresponding to 's' into the next
-- dimension.  The lower faces (those for which the normal vector has negative
-- last coordinate) form a polyhedral complex and the regular subdivision is
-- the image of this complex.  For a generic weight vector, this subdivision
-- will be a triangulation.

regularSubdivisionLocal = method (TypicalValue => NormalToricVariety)
regularSubdivisionLocal (NormalToricVariety, List, List) := (X,s,w) -> (
    coneList := max X;
    rayList := rays X;
    rayMatrix := transpose matrix rayList;
    wtg := i -> if member(i,s) then w#(position (s, j -> i === j)) else 1;    
    for sigma in coneList do (
      	if #sigma === rank rayMatrix_sigma then continue;
      	w' := sigma / wtg;
      	if all (w', i -> i === 1) then continue;
      	C := rayMatrix_sigma || matrix {w'};
      	C' := fourierMotzkin C;
      	H := matrix select (entries transpose C'#0, r -> last r < 0);
      	if C'#1 !=0 then (  
  	    H' := select (entries transpose C'#1, r -> last r != 0);
  	    if #H' > 0 then H = H || matrix apply (H', 
	  	r -> if last r > 0 then -r else r)
	    );
      	inc := H * C;
      	coneList' := apply (apply (numRows inc, i -> 
		select (numColumns inc, j -> inc_(i,j) === 0)
		), 
	    t -> sigma_t
	    );
      	k := position (coneList, tau -> tau === sigma);
      	if all (coneList', tau -> #tau == rank rayMatrix_sigma) then (
	    coneList = drop (coneList,{k,k}) | coneList') 
	);
    if coneList == max X then return X;
    Y := normalToricVariety (rayList, coneList);
    Y.cache.Weights = apply (#rayList, i -> wtg i);
    Y 
    );    

makeSimplicial = method (
    TypicalValue => NormalToricVariety,
    Options => {Strategy => 0}
    )
makeSimplicial NormalToricVariety := opts -> X -> (
    Y := X;
    while true do (
	coneList := max Y;
	rayMatrix := transpose matrix rays Y;
	k := position (coneList, sigma -> #sigma =!= rank rayMatrix_sigma);
	if k === null then break
	else (
	    s := coneList#k;
	    if opts.Strategy === 1 then (
      		c := 1 + dim Y - rank rayMatrix_s;
      		i := 0;
      		edges := select (orbits (Y,c), r -> all(r, j -> member (j,s)));
      		while #select (edges, r -> not member (s#i,r)) === 1 do i = i+1;
      		Y = toricBlowup ({s#i},Y) )
    	    else (
      	    	s = coneList#k;
      	    	n := #s;
      	    	m := (n // 10) + 1;
      	    	w := apply (n, i -> random (2,100*m));
      	    	Y = regularSubdivisionLocal (Y,s,w) )));
    Y 
    );

-- THIS METHOD IS NOT EXPORTED.  Given a list 'w' of integers, this method
-- returns the associated primitive vector; it divides the entries by their
-- greatest common denominator
makePrimitive = method()
makePrimitive List := List => w -> (
   g := gcd w;
   if g === 1 then return w;
   apply(w, i -> i // g) 
   );

toricBlowup = method ()
toricBlowup (List, NormalToricVariety, List) := NormalToricVariety => (s, X, v) -> (
    coneList := max X;
    starIndex := positions (coneList, t -> all (s, i -> member (i,t)));
    star := coneList_starIndex;
    rayMatrix := transpose matrix rays X;
    d := dim X;
    clStar := {};
    for t in star do (
    	c := 1 + d - rank rayMatrix_t;
    	clStar = clStar | select (orbits(X,c), r -> all (r, j -> member(j,t)))
	);
    clStar = unique clStar;
    n := #rays X;
    coneList = coneList_(select (#coneList, i -> not member (i, starIndex)));
    if #s === 1 then (
    	coneList' := for t in clStar list (
      	    if member (s#0,t) then continue
      	    else sort (t | s)
	    );
    	return normalToricVariety (rays X, coneList | coneList') 
	);
    coneList' = for t in clStar list (
	if all (s, i -> member (i,t)) then continue
	else t | {n}
	);
    normalToricVariety (rays X | {v}, coneList | coneList') 
    );

toricBlowup (List, NormalToricVariety) := NormalToricVariety => (s,X) -> (
    v := makePrimitive sum ((rays X)_s);
    toricBlowup (s,X,v) 
    );

makeSmooth = method(
    TypicalValue => NormalToricVariety,
    Options => {Strategy => 0}
    )
makeSmooth NormalToricVariety := opts -> X -> (
    Y := X;
    while true do (
      	coneList := max Y;
      	rayMatrix := transpose matrix rays Y;
      	k := position (coneList, 
	    tau -> #tau =!= rank rayMatrix_tau or 1 != minors (#tau, rayMatrix_tau));
      	if k === null then break;
      	sigma := coneList#k;
      	tau := first select (select (subsets (sigma), t -> #t > 1), 
  	    f -> #f =!= rank rayMatrix_f or 1 != minors (#f,rayMatrix_f));
      	H := hilbertBasis (posHull rayMatrix_tau);
      	H = H / (v -> flatten entries v);
      	--time H := entries transpose hilbertBasis(Vt,"notused");
      	w := select(H, h -> not member (h, (rays Y)_sigma));
      	if w === {} then Y = makeSimplicial (Y, Strategy => opts.Strategy)
      	else Y = toricBlowup (tau,Y, first w)
	);
    Y 
    );

------------------------------------------------------------------------------
-- THINGS TO IMPLEMENT?
--   homology, NormalToricVariety
--   operational Chow rings
--   linear series
--   isSemiprojective
--   toric maps
--     pullback divisors
--     pushforward divisors
--     birational map with makeSimplicial & makeSmooth
--

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- DOCUMENTATION
------------------------------------------------------------------------------
------------------------------------------------------------------------------
beginDocumentation ()
    
doc ///
    Key
        NormalToricVarieties
    Headline
        a package for working with normal toric varieties
    Description
    	Text
            A toric variety is an integral scheme such that an algebraic torus
            forms a Zariski open subscheme and the natural action this torus
            on itself extends to an action on the entire scheme.  Normal toric
            varieties correspond to strongly convex rational polyhedral fans.
            This makes the theory of normal toric varieties very explicit and
            computable.
    	Text
            This {\em Macaulay2} package is designed to manipulate normal
            toric varieties and related geometric objects.  An introduction to
            the theory of normal toric varieties can be found in the following
            textbooks:
    	Text   
    	    @UL { 
                {"David A. Cox, John B. Little, Hal Schenck, ",
	            HREF("http://www.cs.amherst.edu/~dac/toric.html", 
			EM "Toric varieties"), ", Graduate Studies in
		    Mathematics 124. American Mathematical Society,
		    Providence RI, 2011.  ISBN: 978-0-8218-4817-7"},
                {"Günter Ewald, ", EM "Combinatorial convexity and algebraic
                    geometry", ", Graduate Texts in Mathematics 168.
                    Springer-Verlag, New York, 1996.  ISBN: 0-387-94755-8" },
                {"William Fulton, ", EM "Introduction to toric varieties", ",
                    Annals of Mathematics Studies 131, Princeton University
                    Press, Princeton, NJ, 1993. ISBN: 0-691-00049-2" },
                {"Tadao Oda, ", EM "Convex bodies and algebraic geometry, an
                    introduction to the theory of toric varieties", ",
                    Ergebnisse der Mathematik und ihrer Grenzgebiete (3) 15,
                    Springer-Verlag, Berlin, 1988. ISBN: 3-540-17600-4" },	 
    	    }@
	Text
            @SUBSECTION "Contributors"@
	Text
            The following people have generously contributed code or worked on
            our code.
    	Text
	    @UL {
		{HREF("http://www.math.duke.edu/~psa/","Paul Aspinwall")},
		{HREF("http://www-users.math.umn.edu/~cberkesc/","Christine
		    Berkesch")},
		{HREF("http://page.mi.fu-berlin.de/rbirkner/indexen.htm","René
		    Birkner")},
		{HREF("https://math.berkeley.edu/~ceur/index.html", "Chris
		    Eur")},
		{HREF("http://www.warwick.ac.uk/staff/D.Maclagan/","Diane
		    Maclagan")},
		{HREF("http://www.math.unl.edu/~aseceleanu2/","Alexandra
		    Seceleanu")},                
		{HREF("http://www.math.cornell.edu/~mike/","Mike Stillman")},
            }@
	Text
    	    @SUBSECTION "Menu"@
	Text
    	    @UL {
                {TO "Making normal toric varieties"},
		{TO "Basic invariants and properties of normal toric
		    varieties"},
        	{TO "Working with divisors and their associated groups"},
                {TO "Total coordinate rings and coherent sheaves"},
                {TO "Resolution of singularities"},
	    }@
///	

doc ///
    Key 
        "Making normal toric varieties"
    Description
        Text
            A normal toric variety corresponds to a strongly convex rational
            polyhedral fan in affine space.  In this package, the fan
            associated to a normal $d$-dimensional toric variety lies in the
            rational vector space $\QQ^d$ with underlying lattice $N :=
            \ZZ^d$.  The fan is encoded by the minimal nonzero lattice points
            on its rays and the set of rays defining the maximal cones (a
            maximal cone is not properly contained in another cone in the
            fan).
    	Text
    	    The general method for creating a normal toric variety is @TO
	    normalToricVariety@.  However, there are many additional methods
	    for constructing other specific types of normal toric varieties.
    	Text
    	    @SUBSECTION "Menu"@
	Text
    	    @UL {
                TO (normalToricVariety, List, List),
        	TO (normalToricVariety, Matrix),
        	TO NormalToricVariety,
        	TO (isWellDefined, NormalToricVariety),
        	TO (affineSpace, ZZ),
        	TO (toricProjectiveSpace, ZZ),
        	TO (weightedProjectiveSpace, List),
        	TO (hirzebruchSurface, ZZ),
        	TO (kleinschmidt, ZZ, List),
        	TO (symbol **, NormalToricVariety, NormalToricVariety),
        	TO (symbol ^**, NormalToricVariety, ZZ),
        	TO (smoothFanoToricVariety, ZZ, ZZ),
        	TO (normalToricVariety, Fan),
        	TO (normalToricVariety, Polyhedron)
    	    }@
	Text
	    Several methods for making new normal toric varieties from old
            ones are listed in the section on resolution of singularities.
    SeeAlso
        "Basic invariants and properties of normal toric varieties"
        "Working with divisors and their associated groups"
        "Total coordinate rings and coherent sheaves"
        "Resolution of singularities"
///	

doc /// 
  Key
      NormalToricVariety
  Headline 
      the class of all normal toric varieties
  Description
      Text  
	  A normal toric variety corresponds to a strongly convex rational
          polyhedral fan in affine space.  In this package, the fan associated
          to a normal $d$-dimensional toric variety lies in the rational
          vector space $\QQ^d$ with underlying lattice $N = \ZZ^d$.  The fan
          is encoded by the minimal nonzero lattice points on its rays and the
          set of rays defining the maximal cones (a maximal cone is not
          properly contained in another cone in the fan).
  Caveat 
      By assumption, all normal toric varieties in this package have positive
      dimension.
  SeeAlso
     "Making normal toric varieties"
     normalToricVariety
     (rays,NormalToricVariety)
     (max,NormalToricVariety)
     (expression,NormalToricVariety)
///

doc ///
    Key
        (normalToricVariety, List, List)
        normalToricVariety 	
        [normalToricVariety, CoefficientRing]
        [normalToricVariety, Variable]
        [normalToricVariety, WeilToClass]
        WeilToClass
    Headline 
        make a normal toric variety
    Usage 
        normalToricVariety (rayList, coneList)
    Inputs
        rayList : List
            of lists of integers; each entry is the minimal nonzero lattice
            point on a ray in the fan
	coneList : List	     
     	    of lists of nonnegative integers; each entry indexes the rays
            defining a maximal cone in the fan
        CoefficientRing => Ring
            that determines the coefficient ring of the total coordinate ring
        MinimalGenerators => Boolean 
            that specifies whether to compute minimal generators
        Variable => Symbol
	    that specifies the base symbol for the indexed variables in the
	    total coordinate ring	    
        WeilToClass => Matrix 
            that specifies the map from the group of torus-invariant Weil
            divisors to the class group	    
    Outputs
        : NormalToricVariety 
	    the normal toric variety determined by the fan
    Description
        Text	
            This is the general method for constructing a normal toric
            variety.

            A normal toric variety corresponds to a strongly convex rational
            polyhedral fan in affine space.  In this package, the fan
            associated to a normal $d$-dimensional toric variety lies in the
            rational vector space $\QQ^d$ with underlying lattice $N = \ZZ^d$.
            The fan is encoded by the minimal nonzero lattice points on its
            rays and the set of rays defining the maximal cones (meaning cones
            that are not proper subsets of another cone in the fan).  More
            precisely, {\tt rayList} lists the minimal nonzero lattice points
            on each ray (a.k.a. one-dimensional cone) in the fan. Each lattice
            point is a @TO2(List,"list")@ of @TO2(ZZ,"integers")@. The rays
            are ordered and indexed by nonnegative integers: $0,1,\dots,n$.
            Using this indexing, a maximal cone in the fan corresponds to a
            sublist of $\{0,1,\dots,n\}$.  All maximal cones are listed in
            {\tt coneList}.

            The first example is projective $2$-space blown up at two
            points.
        Example
            rayList = {{1,0},{0,1},{-1,1},{-1,0},{0,-1}}
            coneList = {{0,1},{1,2},{2,3},{3,4},{0,4}}
    	    X = normalToricVariety (rayList, coneList)
            rays X
            max X
            dim X
        Text
            The second example illustrates the data defining projective
            $4$-space.
        Example	    
            PP4 = toricProjectiveSpace 4;
            rays PP4
            max PP4
            dim PP4
            ring PP4
            PP4' = normalToricVariety (rays PP4, max PP4, CoefficientRing => ZZ/32003, Variable => y)
            ring PP4'
        Text
            The optional argument @TO WeilToClass@ allows one to specify the
            map from the group of torus-invariant Weil divisors to the class
            group.  In particular, this allows the user to choose her
            favourite basis for the class group.  This map also determines the
            grading on the total coordinate ring of the toric variety.  For
            example, we can choose the opposite generator for the class group
            of projective space as follows.
        Example  
            PP2 = toricProjectiveSpace 2;
            A = fromWDivToCl PP2
            source A == weilDivisorGroup PP2
            target A == classGroup PP2
            degrees ring PP2
            deg = matrix {toList (3:-1)}
            X = normalToricVariety (rays PP2, max PP2, WeilToClass => deg);
            A' = fromWDivToCl X
            source A' == weilDivisorGroup X
            target A' == classGroup X	  
            degrees ring X
            (matrix A')* (matrix rays X)
        Text
            The integer matrix {\tt A} should span the kernel of the matrix
            whose columns are the minimal nonzero lattice points on the rays
            of the fan.

            We can also choose a basis for the class group of a blow-up of the
            projective plane such that the nef cone is the positive quadrant.
        Example  
            rayList = {{1,0},{0,1},{-1,1},{-1,0},{0,-1}};
            coneList = {{0,1},{1,2},{2,3},{3,4},{0,4}};
            Y = normalToricVariety (rayList, coneList);
            fromWDivToCl Y
            nefGenerators Y
            deg = matrix{{1,-1,1,0,0},{0,1,-1,1,0},{0,0,1,-1,1}}
            Y' = normalToricVariety (rays Y, max Y, WeilToClass => deg);	  
            fromWDivToCl Y'
            nefGenerators Y'
    Caveat
        This method assumes that the lists {\tt rayList} and {\tt coneList}
        correctly encode a strongly convex rational polyhedral fan.  One can
        verify this by using @TO(isWellDefined, NormalToricVariety)@.
  SeeAlso 
    "Making normal toric varieties"
    (rays, NormalToricVariety)
    (max ,NormalToricVariety)
    (isWellDefined, NormalToricVariety)
///

doc ///
    Key 
        (normalToricVariety, Matrix)
    	[normalToricVariety, MinimalGenerators]
    Headline
        make a normal toric variety from a polytope
    Usage 
        normalToricVariety VertMat
    Inputs
        VerMat:Matrix
	    of integers; each column is the lattice vertex of the polytope		     
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the total coordinate ring
        MinimalGenerators => Boolean 
	    that specifies whether to compute minimal generators
    	Variable => Symbol 
	    that specifies the base symbol for the indexed variables in the
	    total coordinate ring
        WeilToClass => Matrix 
	    that specifies the map from the group of torus-invariant Weil
            divisors to the class group
    Outputs 
        :NormalToricVariety
            the normal toric variety determined by the polytope
    Description
        Text
            This method makes a @TO NormalToricVariety@ from the polytope with
            vertices corresponding to the columns of the matrix {\tt VertMat}.
            In particular, the associated fan is the INNER normal fan of the
            polytope.
    	Text  
            The first example shows how projective $2$-space is obtained from
            a triangle.
        Example
            PP2 = normalToricVariety matrix {{0,1,0},{0,0,1}};
            rays PP2
            max PP2
            PP2' = toricProjectiveSpace 2;
            set rays PP2 === set rays PP2'
            max PP2 === max PP2'
    	Text
            The second example makes the toric variety associated to the
            hypercube in affine $3$-space.
    	Example  
            X = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
            isSimplicial X
            transpose matrix rays X 
	    max X
        Text
            The optional argument {\tt MinimalGenerators} specifics whether to
            compute the vertices of the polytope defined as the convex hull of
            the columns of the matrix {\tt VertMat}.
    	Example  	    
            FF1 = normalToricVariety matrix {{0,1,0,2},{0,0,1,1}};
            rays FF1
            max FF1 
            FF1' = hirzebruchSurface 1;
            assert (rays FF1 === rays FF1' and max FF1 === max FF1')
    	    VertMat = matrix {{0,0,1,1,2},{0,1,0,1,1}}
    	    notFF1 = normalToricVariety VertMat;
    	    max notFF1
    	    isWellDefined notFF1
    	    FF1'' = normalToricVariety (VertMat, MinimalGenerators => true);
    	    assert (rays FF1'' == rays FF1 and max FF1'' == max FF1)
	    assert isWellDefined FF1''
    SeeAlso
        "Making normal toric varieties"
        (rays, NormalToricVariety)
        (max, NormalToricVariety)
        (isWellDefined, NormalToricVariety)
        (vertices, ToricDivisor)
        (latticePoints, ToricDivisor)
///

doc ///
    Key 
        (isWellDefined, NormalToricVariety)
    Headline 
        whether a toric variety is well-defined
    Usage
        isWellDefined X
    Inputs
        X:NormalToricVariety
    Outputs
        :Boolean
	    that is @TO true@ if the lists of rays and maximal cones
	    associated to {\tt X} determine a strongly convex rational
	    polyhedral fan
    Description
        Text
            A pair {\tt (rayList, coneList)} of lists correspond to a
            well-defined normal toric variety if the following conditions
            hold:
    	Text
    	    @UL {
                {"the union of the elements of ", TT "coneList", " equals the
          	    set of indices of elements of ", TT "rayList", ","},
                {"no element of ", TT "coneList", " is properly contained in
         	    another element of ", TT "coneList", ","},
                {"the rays indexed by an element of ", TT "coneList", "
         	    generate a strongly convex cone,"},
                {"the rays indexed by an element of ", TT "coneList", " are
         	    the unique minimal lattice points for the cone they
         	    generate,"},
                {"the intersection of the cones associated to two elements of ", 
		    TT "coneList", " is a face of each cone."}
    	    }@
	Text
            The first examples illustrate that small projective spaces are
            well-defined.
    	Example    
            assert all (5, d -> isWellDefined toricProjectiveSpace (d+1))
    	Text
            The second examples show that a randomly selected Kleinschmidt
            toric variety and a weighted projective space are also
            well-defined.
    	Example    
            setRandomSeed (currentTime ());
            a = sort apply (3, i -> random (7))
            assert isWellDefined kleinschmidt (4,a)
	Example
            q = sort apply (5, j -> random (1,9));
            while not all (subsets (q,#q-1), s -> gcd s === 1) do q = sort apply (5, j -> random (1,9));
            q
            assert isWellDefined weightedProjectiveSpace q
    	Text
            The next eight examples illustrate various ways that two lists can
            fail to define a normal toric variety.  By making the current
            debugging level greater than one, one gets some addition
            information about the nature of the failure.
    	Example    
	    X = new MutableHashTable;
            coneList = max toricProjectiveSpace 2;
            X#1 = normalToricVariety ({{-1,-1},{1,0},{0,1},{-1,0}}, coneList);
            isWellDefined X#1
            debugLevel = 1;
            isWellDefined X#1	  	  
            coneList' = {{0,1},{0,3},{1,2},{2,3},{3}};
            X#2 = normalToricVariety ({{-1,0},{0,-1},{1,-1},{0,1}}, coneList');
            isWellDefined X#2
            X#3 = normalToricVariety ({{-1,-1},{1,0},{0,1,1}},coneList);
            isWellDefined X#3
            X#4 = normalToricVariety ({{-1,-1/1},{1,0},{0,1}},coneList);
            isWellDefined X#4
            X#5 = normalToricVariety ({{1,0},{0,1},{-1,0}},{{0,1,2}});
            isWellDefined X#5
            X#6 = normalToricVariety ({{1,0,0},{0,1,0},{0,0,2}},{{0,1,2}});
            isWellDefined X#6
            X#7 = normalToricVariety ({{1,0},{0,1},{1,1}},{{0,1},{1,2}});
            isWellDefined X#7
	    debugLevel = 0;
	    assert all (keys X, k -> not isWellDefined X#k)
    	Text
            This method also checks that the following aspects of the data structure:
	Text
    	    @UL {
	        {"the underlying ", TO HashTable, " has the expected keys,
	    	    namely ", TT "rays", ", ", TT "max", ", and ", TT "cache",
	    	    ","},
       	        {"the value of the ", TT "rays", " key is a ", TO List, ","},
	        {"each entry in the ", TT "rays", " list is a ", TO List,
	            ","},
	        {"each entry in an entry of the ", TT "rays", " list is an ",
	            TO ZZ, ","},
                {"each entry in the ", TT "rays", " list as the same number of
                    entries,"},
	        {"the value of the ", TT "max", " key is a ", TO List, ","},	
                {"each entry in the ", TT "max", " list is a ", TO List, ","},
                {"each entry in an entry of the ", TT "max", " list is an ",
                    TO ZZ, ","},
                {"each entry in an entry of the ", TT "max", " list
                    corresponds to a ray,"},
                {"the value of the ", TT "cache", " key is a ", TO CacheTable,
                    "."}
	    }@
    SeeAlso
        "Making normal toric varieties"
        (normalToricVariety, List, List)
	"debugLevel"
///

doc ///
    Key
        (affineSpace, ZZ)
	affineSpace
        [affineSpace,CoefficientRing]
        [affineSpace,Variable]
    Headline 
        make an affine space
    Usage 
        affineSpace d
    Inputs 
        d:ZZ
	    a positive integer
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the total coordinate ring			       
        Variable => Symbol 
	    the base symbol for the indexed variables in the total coordinate
            ring
    Outputs 
        :NormalToricVariety 
	    affine $d$-space
    Description
        Text
            Affine $d$-space is a smooth normal toric variety.  The rays are
            generated by the standard basis $e_1, e_2, \dots, e_d$ of $\ZZ^d$,
            and the maximal cone in the fan correspond to the $d$-element
            subsets of $\{ 0, 1, \dots, d-1 \}$.
    	Text
            The examples illustrate the affine line and affine $3$-space.
    	Example  
    	    AA1 = affineSpace 1;
    	    rays AA1
    	    max AA1
    	    dim AA1
    	    assert (not isComplete AA1 and isSmooth AA1)
	Example
            AA3 = affineSpace (3, CoefficientRing => ZZ/32003, Variable => y);
    	    rays AA3
    	    max AA3
    	    dim AA3
    	    ring AA3
    	    assert (not isComplete AA3 and isSmooth AA3)
    SeeAlso
        "Making normal toric varieties"
        (isSmooth, NormalToricVariety)
        (isComplete, NormalToricVariety)
        (makeSmooth, NormalToricVariety)
///   

doc ///
    Key
        "projective space"
    Headline 
        various methods for constructing projective space
    Description
        Text
	    There are several different methods for creating projective
	    $n$-space in {\it Macaulay2}.   
    	Text
	    To generate projective space as a @TO ProjectiveVariety@,
	    we use the Proj-construction.
	Example
	    X0 = Proj (QQ[x_0..x_3])
	    assert (3 === dim X0)
	    ring X0
	    hilbertPolynomial(X0, Projective => false)
	    for d to 10 list rank HH^0 (OO_X0(d))
    	Text
	    To work with projective space as a @TO NormalToricVariety@,
	    we simply use the method @TO toricProjectiveSpace@.
	Example
	    X1 = toricProjectiveSpace 3
	    assert (3 === dim X1)
	    rays X1
	    max X1
	    ring X1
	    intersectionRing X1
	    hilbertPolynomial (X1)
	    for d to 10 list rank HH^0 (X1, OO_X1(d)) 
	    assert (X0 =!= X1)
    	Text
	    To manipulate projective space as an @TO AbstractVariety@,
	    we employ the method @TO abstractProjectiveSpace@.
	Example
	    X2 = abstractProjectiveSpace (3, base(symbol i))
	    assert (3 === dim X2)
    	    intersectionRing X2
	    chi (OO_X2(i))
	    assert (X2 =!= X0)
	    assert (X2 =!= X1)	    	    
	Text	    
	    If you prefer a shorter name for your favourite method of
	    constructing projective space, then make one.
	Example
	    projectiveSpace = n -> Proj (QQ[x_0..x_n]);
	    projectiveSpace 2
	Example	    
	    PP = toricProjectiveSpace;	    
	    PP 2
    SeeAlso
        "Making normal toric varieties"
        (Proj, Ring)
        toricProjectiveSpace
        abstractProjectiveSpace	
///

doc ///
    Key 
        (toricProjectiveSpace, ZZ)
	toricProjectiveSpace
        [toricProjectiveSpace,CoefficientRing]
        [toricProjectiveSpace,Variable]
    Headline 
        make a projective space
    Usage 
        toricProjectiveSpace d
    Inputs 
        d:ZZ
	    a positive integer
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the total coordinate ring
        Variable => Symbol 
	    the base symbol for the indexed variables in the total coordinate
            ring
    Outputs 
        :NormalToricVariety 	    
            projective $d$-space
    Description
        Text	    
            Projective $d$-space is a smooth complete normal toric variety.
            The rays are generated by the standard basis $e_1, e_2, \dots,e_d$
            of $\ZZ^d$ together with vector $-e_1-e_2-\dots-e_d$.  The maximal
            cones in the fan correspond to the $d$-element subsets of $\{ 0,1,
            \dots,d\}$.
    	Text
  	    The examples illustrate the projective line and projective $3$-space.
	Example
    	    PP1 = toricProjectiveSpace 1;
    	    rays PP1
    	    max PP1
    	    dim PP1
    	    ring PP1
    	    ideal PP1
    	    assert (isSmooth PP1 and isComplete PP1)
	Example	    
    	    PP3 = toricProjectiveSpace (3, CoefficientRing => ZZ/32003, Variable => y);
    	    rays PP3
    	    max PP3
    	    dim PP3
    	    ring PP3
    	    ideal PP3
    	    assert (isSmooth PP3 and isComplete PP3)    
    SeeAlso
        "Making normal toric varieties"
    	(isComplete, NormalToricVariety)
        (isSmooth, NormalToricVariety)
        (ring, NormalToricVariety)
        (ideal, NormalToricVariety)
///	

doc ///
    Key
        (hirzebruchSurface,ZZ)
	hirzebruchSurface
    	[hirzebruchSurface,CoefficientRing]
    	[hirzebruchSurface,Variable]
    Headline
        make a Hirzebruch surface
    Usage 
        hirzebruchSurface a
    Inputs
        a:ZZ
	    that determines which Hirzebruch surface
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the total coordinate ring
        Variable => Symbol 
	    that specifies the base symbol for the indexed variables in the
            total coordinate ring
    Outputs 
        :NormalToricVariety 
	    a Hirzebruch surface
    Description
        Text
            The $a$-{th} Hirzebruch surface is a smooth projective normal toric
            variety.  It can be defined as the $\PP^1$-bundle over $X = \PP^1$
            associated to the sheaf ${\mathcal O}_X(0) \oplus {\mathcal
            O}_X(a)$.  It is also the quotient of affine $4$-space by a rank
            two torus.  
	Example
            FF3 = hirzebruchSurface 3;
            rays FF3
            max FF3
            dim FF3
            ring FF3
            degrees ring FF3
            ideal FF3
	    assert (isProjective FF3 and isSmooth FF3)
    	Text
            When $a = 0$, we obtain $\PP^1 \times \PP^1$.
      	Example
            FF0 = hirzebruchSurface (0, CoefficientRing => ZZ/32003, Variable => y);
            rays FF0
            max FF0
            ring FF0
            degrees ring FF0
            I = ideal FF0
            decompose I
	    assert (isProjective FF3 and isSmooth FF3)	    
    	Text
            The map from the torus-invariant Weil divisors to the class group
            is chosen so that the positive orthant corresponds to the cone of
            nef line bundles.
	Example
	    nefGenerators FF3
    	    nefGenerators FF0	    
    SeeAlso
        "Making normal toric varieties"
    	(ring,NormalToricVariety)
	nefGenerators
///	     

doc ///
    Key
        (weightedProjectiveSpace,List)
	weightedProjectiveSpace 
        [weightedProjectiveSpace,CoefficientRing]
        [weightedProjectiveSpace,Variable]
    Headline 
        make a weighted projective space
    Usage 
        weightedProjectiveSpace q
    Inputs 
        q:List
            of relatively prime positive integers
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the total coordinate ring
        Variable => Symbol 
	    that specifies the base symbol for the indexed variables in the
            total coordinate ring
    Outputs 
        :NormalToricVariety 
	    a weighted projective space
    Description
        Text
	    The weighted projective space associated to a list $\{ q_0, q_1,
            \dots, q_d \}$, where no $d$-element subset of $q_0, q_1, \dots,
            q_d$ has a nontrivial common factor, is a projective simplicial
            normal toric variety built from a fan in $N = \ZZ^{d+1}/\ZZ(q_0,
            q_1, \dots,q_d)$.  The rays are generated by the images of the
            standard basis for $\ZZ^{d+1}$, and the maximal cones in the fan
            correspond to the $d$-element subsets of $\{ 0, 1, ..., d \}$.
	    A weighted projective space is typically not smooth.
    	Text  
  	    The first examples illustrate the defining data for three
            different weighted projective spaces.
    	Example
            PP4 = weightedProjectiveSpace {1,1,1,1};
            rays PP4
            max PP4
            dim PP4
	    assert (isProjective PP4 and isSmooth PP4)
	Example
            X = weightedProjectiveSpace {1,2,3};
            rays X
            max X
            dim X
            ring X
	    assert (isProjective X and isSimplicial X and not isSmooth X)
	Example
            Y = weightedProjectiveSpace ({1,2,2,3,4}, CoefficientRing => ZZ/32003, Variable => y);
            rays Y
            max Y
            dim Y
            ring Y
	    assert (isProjective Y and isSimplicial Y and not isSmooth Y)
    	Text
            The grading of the total coordinate ring for weighted projective
            space is determined by the weights.  In particular, the class
            group is $\ZZ$.
	Example
            classGroup PP4
            degrees ring PP4
            classGroup X
            degrees ring X
            classGroup Y
            degrees ring Y
    SeeAlso
        "Making normal toric varieties"
        (toricProjectiveSpace,ZZ)
        (ring,NormalToricVariety)
        (classGroup,NormalToricVariety)
        (isSimplicial,NormalToricVariety)
        (isSmooth,NormalToricVariety)
///	

doc ///
    Key
        (symbol **, NormalToricVariety, NormalToricVariety)
    Headline 
        make the Cartesian product of normal toric varieties
    Usage 
        X ** Y
    Inputs 
        X:NormalToricVariety
	Y:NormalToricVariety
    Outputs
        :NormalToricVariety
	    the product of {\tt X} and {\tt Y}
    Description
        Text	    
            The Cartesian product of two varieties $X$ and $Y$, both defined
            over the same ground field $k$, is the fiber product $X \times_k
            Y$.  For normal toric varieties, the fan of the product is given
            by the Cartesian product of each pair of cones in the fans of the
            factors.
    	Example  
    	    PP2 = toricProjectiveSpace 2;
            FF2 = hirzebruchSurface 2;
            X = FF2 ** PP2;
            assert (# rays X == # rays FF2 + # rays PP2)
            assert (matrix rays X == matrix rays FF2 ++ matrix rays PP2)
            primaryDecomposition ideal X 
	    flatten (primaryDecomposition \ {ideal FF2,ideal PP2})
    	Text
  	    The map from the torus-invariant Weil divisors to the class group
            is the direct sum of the maps for the factors.
      	Example
    	    assert (fromWDivToCl FF2 ++ fromWDivToCl PP2 == fromWDivToCl X)
    SeeAlso
        "Making normal toric varieties"
        (symbol ^**, NormalToricVariety, ZZ)
        normalToricVariety
///	

doc ///
    Key 
        (symbol ^**,NormalToricVariety,ZZ)
    Headline
        make the Cartesian power of a normal toric variety
    Usage 
        X ^** i
    Inputs
        X:NormalToricVariety
        i:ZZ
    Outputs 
        :NormalToricVariety
	    the {\tt i}-ary Cartesian product of {\tt X} with itself
    Description
        Text
            The $i$-ary Cartesian product of the variety $X$, defined over the
            ground field $k$, is the $i$-ary fiber product of $X$ with itself
            over $k$.  For a normal toric variety, the fan of the $i$-ary
            Cartesian product is given by the $i$-ary Cartesian product of the
            cones.
    	Example  
    	    PP2 = toricProjectiveSpace 2;
    	    X = PP2 ^** 4;
    	    fromWDivToCl X
	Example
    	    FF2 = hirzebruchSurface (2);
    	    Y = FF2 ^** 3;
    	    fromWDivToCl Y
	Example
    	    X' = PP2 ** PP2;
            X'' = PP2 ^** 2;
            assert (rays X' == rays X'' and  max X' == max X'')
    SeeAlso
        "Making normal toric varieties" 
        (symbol **, NormalToricVariety, NormalToricVariety)
        normalToricVariety
///	

doc ///
    Key 
    	(kleinschmidt, ZZ, List)
        kleinschmidt	
    	[kleinschmidt,CoefficientRing]
    	[kleinschmidt,Variable]
    Headline 
        make a smooth normal toric variety with Picard rank two
    Usage 
        kleinschmidt (d,a)
    Inputs
        d : ZZ 
	    that specifies the dimension of toric variety
        a : List
	    an increasing list of at most $d-1$ nonnegative integers
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the total coordinate ring
        Variable => Symbol 
	    that specifies the base symbol for the indexed variables in the
            total coordinate ring
    Outputs 
        : NormalToricVariety 
	    a smooth toric variety with Picard rank two
    Description
        Text
            Peter Kleinschmidt constructs (up to isomorphism) all smooth
            normal toric varieties with dimension $d$ and $d+2$ rays; see
            P. Kleinschmidt, A classification of toric varieties with few
            generators {\em Aequationes Mathematicae}, {\bf 35} (1998)
            254-266.
    	Text   
            When $d=2$, we obtain a variety isomorphic to a Hirzebruch
            surface.  By permuting the indexing of the rays and taking
	    an automorphism of the lattice, we produce an explicit 
	    isomorphism.
    	Example  
            X = kleinschmidt (2,{3});
            rays X
            max X
            FF3 = hirzebruchSurface 3;
            rays FF3
            max FF3
    	    permutingRays = matrix {{0,0,0,1},{0,1,0,0},{1,0,0,0},{0,0,1,0}}
	    latticeAutomorphism = matrix {{0,1},{1,0}}
	    assert (latticeAutomorphism * (matrix transpose rays X) * permutingRays == matrix transpose rays FF3)
    	Text
            The normal toric variety associated to the pair $(d,a)$ is Fano if
            and only if $\sum_{i=0}^{r-1} a_i < d-r+1$.
    	Example  
    	    X1 = kleinschmidt (3, {0,1});	  
    	    isFano X1
    	    X2 = kleinschmidt (4, {0,0});	  
    	    isFano X2
    	    ring X2
    	    X3 = kleinschmidt (9, {1,2,3}, CoefficientRing => ZZ/32003, Variable => y);
    	    isFano X3
    	    ring X3
    	Text
            The map from the torus-invariant Weil divisors to the class group
            is chosen so that the positive orthant corresponds to the cone of
            nef line bundles.
	Example
	    nefGenerators X
    	    nefGenerators X1
    	    nefGenerators X2
    	    nefGenerators X3	    	    
    SeeAlso
        "Making normal toric varieties"
        normalToricVariety
	hirzebruchSurface
///	

doc /// 
    Key 
        (smoothFanoToricVariety, ZZ, ZZ)
        smoothFanoToricVariety	
    	[smoothFanoToricVariety, CoefficientRing]
    	[smoothFanoToricVariety, Variable]
    Headline 
        get a smooth Fano toric variety from database
    Usage 
        smoothFanoToricVariety (d,i)
    Inputs
        d:ZZ 
	    equal to dimension of toric variety
        i:ZZ 
	    indexing a normal toric variety in database
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the total coordinate ring
        Variable => Symbol 
	    that specifies the base symbol for the indexed variables in the
	    total coordinate ring
    Outputs 
        :NormalToricVariety 
	    a smooth Fano toric variety
    Description
        Text
            This function accesses a database of all smooth Fano toric
            varieties of dimension at most $6$.  The enumeration of the toric
            varieties follows
            @HREF("http://www.mathematik.uni-tuebingen.de/~batyrev/batyrev.html.en",
            "Victor V. Batyrev's")@ classification (see
            @HREF("http://arxiv.org/abs/math/9801107",
            "arXiv:math/9801107v2")@ and
            @HREF("http://arxiv.org/abs/math/9911022", "arXiv:math/9011022")@)
            for dimension at most $4$ and Mikkel Øbro's classification (see
            @HREF("http://arxiv.org/abs/0704.0049", "arXiv:math/0704.0049v1")@
            for dimensions $5$ and $6$.  There is a unique smooth Fano toric
            curve, five smooth Fano toric surfaces, eighteen smooth Fano toric
            threefolds, $124$ smooth Fano toric fourfolds, $866$ smooth Fano
            toric fivefolds, and $7622$ smooth Fano toric sixfolds.
        Text
            For all $d$, {\tt smoothFanoToricVariety (d,0)} yields projective
            $d$-space.	    
      	Example
      	    PP1 = smoothFanoToricVariety (1,0);
            assert (rays PP1 === rays toricProjectiveSpace 1)
            assert (max PP1 === max toricProjectiveSpace 1)
    	    PP4 = smoothFanoToricVariety (4,0, CoefficientRing => ZZ/32003, Variable => y);
            assert (rays PP4 === rays toricProjectiveSpace 4)
            assert (max PP4 === max toricProjectiveSpace 4)	    
	Text
            The following example was missing from Batyrev's table.
	Example
            W = smoothFanoToricVariety (4,123);
            rays W
            max W
	Text
	    @SUBSECTION "Acknowledgements"@
    	Text
            We thank @HREF("http://homepages.warwick.ac.uk/staff/G.Brown/",
            "Gavin Brown")@ and
            @HREF("http://magma.maths.usyd.edu.au/users/kasprzyk/","Alexander
            Kasprzyk")@ for their help extracting the data for the smooth Fano
            toric five and sixfolds from their @HREF("http://www.grdb.co.uk",
            "Graded Rings Database")@.
    SeeAlso
        "Making normal toric varieties"
        normalToricVariety
        (isFano,NormalToricVariety)
///	

doc ///
    Key
        (normalToricVariety, Fan)
    Headline 
        make a normal toric variety from a 'Polyhedra' fan
    Usage 
        normalToricVariety F
    Inputs
        F:Fan
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the total coordinate ring
	MinimalGenerators => Boolean
	    that is ignored by this routine
        Variable => Symbol 
	    that specifies the base symbol for the indexed variables in the
            total coordinate ring
        WeilToClass => Matrix
	    that specifies the map from the group of torus-invariant Weil
	    divisors to the class group
    Outputs
        :NormalToricVariety
	    determined by the rational strongly convex polyhedral fan
    Description
        Text	    
            This method makes a @TO NormalToricVariety@ from a 
	    @TO "Polyhedra::Fan"@ as implemented in the 
	    @TO "Polyhedra::Polyhedra"@ package.
    	Example  
            F = faceFan convexHull (id_(ZZ^3) | -id_(ZZ^3))
    	    rays F
	    maxCones F
    	    X = normalToricVariety F;
    	    assert (transpose matrix rays X == rays F and max X == sort maxCones F)
    	Text
  	    The recommended method for creating a @TO NormalToricVariety@ from
            a fan is @TO (normalToricVariety,List,List)@.  In fact, this
            package avoids using objects from the @TO "Polyhedra::Polyhedra"@
            package whenever possible.   Here is a trivial example, namely
            projective 2-space, illustrating the substantial increase in time
            resulting from the use of a @TO "Polyhedra::Polyhedra"@ fan.
      	Example
            X1 = time normalToricVariety ({{-1,-1},{1,0},{0,1}}, {{0,1},{1,2},{0,2}})
            X2 = time normalToricVariety fan {posHull matrix {{-1,1},{-1,0}}, posHull matrix {{1,0},{0,1}}, posHull matrix{{-1,0},{-1,1}}};
	    assert (sort rays X1 == sort rays X2 and max X1 == max X2)
    SeeAlso
        "Making normal toric varieties"
        normalToricVariety
///	

doc ///
    Key 
        (normalToricVariety, Polyhedron)
    Headline 
        make a normal toric variety from a 'Polyhedra' polyhedron
    Usage 
        normalToricVariety P
    Inputs
        P:Polyhedron
	    whose vertices are lattice points
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the total coordinate ring
	MinimalGenerators => Boolean
	    that is ignored by this routine
        Variable => Symbol 
	    that specifies the base symbol for the indexed variables in the
            total coordinate ring
        WeilToClass => Matrix
	    that specifies the map from the group of torus-invariant Weil
	    divisors to the class group
    Outputs
        :NormalToricVariety
	    determined by the lattice polytope
    Description
        Text
            This method makes a @TO NormalToricVariety@ from a @TO
            "Polyhedra::Polyhedron"@ as implemented in the @TO
            "Polyhedra::Polyhedra"@ package.  In particular, the associated
            fan is inner normal fan to the polyhedron.
       	Example
            P = convexHull (id_(ZZ^3) | -id_(ZZ^3));
	    fVector P
	    vertices P
            X = normalToricVariety P;
            rays X
            max X
	    picardGroup X
	Text
	    When the polyhedron is not full-dimensional, restricting to the
	    smallest linear subspace that contains the polyhedron guarantees
	    that normal fan is strongly convex.
	Example
	    P = convexHull transpose matrix unique permutations {1,1,0,0};
	    assert not isFullDimensional P
	    fVector P
	    X = normalToricVariety P;
	    assert (dim P === dim X)
	    rays X
	    max X
	    assert (8 === #rays X)
    	    assert (6 === #max X)
	    picardGroup X
    	Text
            The recommended method for creating a @TO NormalToricVariety@ from
            a polytope is @TO (normalToricVariety,Matrix)@.  In fact, this
            package avoids using objects from the @TO "Polyhedra::Polyhedra"@
            whenever possible.  Here is a trivial example, namely projective
            2-space, illustrating the increase in time resulting from the use
            of a @TO "Polyhedra::Polyhedra"@ polyhedron.	    
	Example
    	    vertMatrix = matrix {{0,1,0},{0,0,1}}
            X1 = time normalToricVariety convexHull (vertMatrix);
            X2 = time normalToricVariety vertMatrix;
	    assert (set rays X2 === set rays X1 and max X1 === max X2)
    SeeAlso
        "Making normal toric varieties"
        (normalToricVariety, Matrix)
///	



------------------------------------------------------------------------------
-- basic properties and invariants
------------------------------------------------------------------------------

doc ///
    Key 
        "Basic invariants and properties of normal toric varieties"
    Description
        Text
            Having made a @TO NormalToricVariety@ one can access its basic
            invariants or test for some elementary properties by using the
            following methods:
    	Text
	    @SUBSECTION "Menu"@
	Text
            @UL {
        	TO (rays, NormalToricVariety),
        	TO (max, NormalToricVariety),    
        	TO (expression, NormalToricVariety),    
    		TO (dim, NormalToricVariety),
    		TO (orbits, NormalToricVariety, ZZ),
    		TO (isDegenerate, NormalToricVariety),
    		TO (isSimplicial, NormalToricVariety),
    		TO (isSmooth, NormalToricVariety),
    		TO (isComplete, NormalToricVariety),
    		TO (isProjective, NormalToricVariety),
    		TO (isFano, NormalToricVariety),
    		TO (fan, NormalToricVariety)
	    }@
    SeeAlso
        "Making normal toric varieties"
        "Working with divisors and their associated groups"
        "Total coordinate rings and coherent sheaves"
        "Resolution of singularities"
///

doc ///
    Key 
        (rays, NormalToricVariety)
    Headline 
        get the rays of the associated fan
    Usage 
        rays X
    Inputs 
        X:NormalToricVariety
    Outputs
        :List
	    of lists of integers; each entry corresponds to a minimal nonzero
            lattice point on the ray in the underlying fan
    Description
        Text
            A normal toric variety corresponds to a strongly convex rational
            polyhedral fan in affine space.  In this package, the fan
            associated to a normal $d$-dimensional toric variety lies in the
            rational vector space $\QQ^d$ with underlying lattice $N =
            {\ZZ}^d$.  As a result, each ray in the fan is determined by the
            minimal nonzero lattice point it contains.  Each such lattice
            point is given as a @TO2(List, "list")@ of $d$ @TO2(ZZ,
            "integers")@.
	Text
            The examples show the rays for the projective plane, projective
            $3$-space, a Hirzebruch surface, and a weighted projective space.
            There is a canonical bijection between the rays and
            torus-invariant Weil divisor on the toric variety.
	Example
    	    PP2 = toricProjectiveSpace 2;
            rays PP2
            dim PP2
            weilDivisorGroup PP2
	    PP2_0
	Example
            PP3 = toricProjectiveSpace 3;
            rays PP3
            dim PP3
            weilDivisorGroup PP3
	Example
            FF7 = hirzebruchSurface 7;
            rays FF7
            dim FF7
            weilDivisorGroup FF7
	Example
            X = weightedProjectiveSpace {1,2,3};
            rays X
            weilDivisorGroup X
    	Text
             When the normal toric variety is nondegerenate, the number of
             rays equals the number of variables in the total coordinate ring.
    	Example  
             #rays X == numgens ring X
    	Text
            In this package, an ordered list of the minimal nonzero lattice
            points on the rays in the fan is part of the defining data of a
            toric variety.
    SeeAlso
        "Making normal toric varieties"
        "Basic invariants and properties of normal toric varieties"
        (max, NormalToricVariety)
        (ring, NormalToricVariety)
///	

doc ///
    Key 
        (max, NormalToricVariety)
    Headline 
        get the maximal cones in the associated fan
    Usage 
        max X
    Inputs 
        X:NormalToricVariety
    Outputs 
        :List
	    of lists of nonnegative integers; each entry indexes the rays
            that generate a maximal cone in the fan
    Description
        Text
            A normal toric variety corresponds to a strongly convex rational
            polyhedral fan in affine space.  In this package, the fan
            associated to a normal $d$-dimensional toric variety lies in the
            rational vector space $\QQ^d$ with underlying lattice $N = \ZZ^d$.
            The fan is encoded by the minimal nonzero lattice points on its
            rays and the set of rays defining the maximal cones (where a
            maximal cone is not properly contained in another cone in the
            fan).  The rays are ordered and indexed by nonnegative integers:
            $0, 1, \dots, n-1$.  Using this indexing, a maximal cone in the fan
            corresponds to a sublist of $\{ 0, 1, \dots, n-1 \}$; the entries
            index the rays that generate the cone.
    	Text
            The examples show the maximal cones for the projective line,
            projective $3$-space, a Hirzebruch surface, and a weighted
            projective space.
    	Example  
    	    PP1 = toricProjectiveSpace 1;
            # rays PP1
            max PP1
    	Example
            PP3 = toricProjectiveSpace 3;
            # rays PP3
            max PP3
    	Example
            FF7 = hirzebruchSurface 7;
            # rays FF7
            max FF7
    	Example
            X = weightedProjectiveSpace {1,2,3};
            # rays X
            max X
    	Text
  	    In this package, a list corresponding to the maximal cones in the
            fan is part of the defining data of a normal toric variety.
    SeeAlso
        "Making normal toric varieties"
        "Basic invariants and properties of normal toric varieties"
        (rays, NormalToricVariety)
///	

doc ///
    Key
        (expression, NormalToricVariety)
    Headline 
        get the expression used to format for printing
    Usage 
        expression X
    Inputs
        X:NormalToricVariety
    Outputs 
        :Expression
	    used to format {\tt X} for printing
    Description
        Text	    
            This function is the primary function called upon by 
	    @TO(symbol <<)@ to format for printing.  It displays the minimal
            nonzero lattice points on each ray and the subsets of rays which
            determine the maximal cones in the fan.
    	Example
    	    toricProjectiveSpace 3
	    expression toricProjectiveSpace 3
    	    rays toricProjectiveSpace 3
    	    max toricProjectiveSpace 3
	Example
    	    hirzebruchSurface 7
	    expression hirzebruchSurface 7
    	    rays hirzebruchSurface 7
    	    max hirzebruchSurface 7
    	Text
  	    After assignment to a global variable {\em Macaulay2} knows the
            toric variety's name, and this name is used when printing.
    	Example  
    	    PP2 = toricProjectiveSpace 3
    	    expression PP2	    
    	    FF7 = hirzebruchSurface 7
	    expression FF7
    SeeAlso
        "Basic invariants and properties of normal toric varieties"
    	(rays, NormalToricVariety)
    	(max, NormalToricVariety)
///	

doc ///
    Key 
        (dim, NormalToricVariety)
    Headline 
        get the dimension of a normal toric variety
    Usage 
        dim X
    Inputs 
        X:NormalToricVariety
    Outputs 
        :ZZ 
	    the dimension of the normal toric variety
    Description
        Text
            The dimension of a normal toric variety equals the dimension of
            its dense algebraic torus.  In this package, the fan associated to
            a normal $d$-dimensional toric variety lies in the rational vector
            space $\QQ^d$ with underlying lattice $N = \ZZ^d$.  Hence, the
            dimension simply equals the number of entries in a minimal nonzero
            lattice point on a ray.
        Text
            The following examples illustrate normal toric varieties of
            various dimensions.
    	Example  
            dim toricProjectiveSpace 1
	    dim affineSpace 2
            dim toricProjectiveSpace 5
            dim hirzebruchSurface 7
            dim weightedProjectiveSpace {1,2,2,3,4}
            X = normalToricVariety ({{4,-1,0},{0,1,0}},{{0,1}})
            dim X
            isDegenerate X
    SeeAlso
        "Basic invariants and properties of normal toric varieties"
        (rays, NormalToricVariety)
///	
 
doc ///
    Key
        (orbits, NormalToricVariety)
        orbits	
    Headline 
        make a hashtable indexing the torus orbits (a.k.a. cones in the fan)
    Usage 
        orbits X
    Inputs 
        X:NormalToricVariety
    Outputs 
        :HashTable 
	    whose keys are the dimensions of the torus orbits in {\tt X} and
            whose values are lists of lists of integers indexing the torus
            orbits
    Description
        Text			   
            A normal toric variety is a disjoint union of its orbits under the
            action of its algebraic torus.  These orbits are in bijection with
            the cones in the associated fan.  Each cone is determined by the
            rays it contains.  In this package, the rays are ordered and
            indexed by nonnegative integers: $0, 1, \dots,n$.  Using this
            indexing, an orbit or cone corresponds to a sublist of
            $\{0,\dots,n\}$; the entries index the rays that generate the
            cone.
        Text	    
            The rojective plane has three fixed points and three fixed curves
            (under the action of its torus), and projective $3$-space has four
            fixed points, six fixed curves, and four divisors.  More
            generally, the orbits of projective $(n-1)$-space are enumerated
            by the $n$-th row of Pascal's triangle.
	Example
    	    O2 = orbits toricProjectiveSpace 2
            (#O2#0, #O2#1, #O2#2)
    	    O3 = orbits toricProjectiveSpace 3     
            apply (4, k -> #O3#k)
            apply (5, k -> # (orbits toricProjectiveSpace 4)#k)
            apply (6, k -> # (orbits toricProjectiveSpace 5)#k)    
    	Text
  	    Here is a non-simplicial example.
    	Example
      	    X = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
    	    assert not isSimplicial X
    	    OX = orbits X
	    apply (1+dim X, k -> #OX#k)
    	Text
  	    The following degenerate example has no fixed points.
    	Example
    	    U = normalToricVariety ({{4,-1,0},{0,1,0}},{{0,1}});
    	    assert isDegenerate U
    	    OU = orbits U
	    apply (4, k -> #OU#k)
	    assert (#OU#0 == 0)
    SeeAlso
        "Basic invariants and properties of normal toric varieties"
        (rays, NormalToricVariety)
        (orbits, NormalToricVariety,ZZ)
///	

doc ///
    Key 
        (orbits, NormalToricVariety, ZZ)
    Headline 
        get a list of the torus orbits (a.k.a. cones in the fan) of a given dimension
    Usage 
        orbits (X, i)
    Inputs
        X:NormalToricVariety
        i:ZZ
	    determining the dimension of the orbits
    Outputs 
        :List
	    of lists of integers indexing the torus orbits
    Description
        Text			   
            A normal toric variety is a disjoint union of its orbits under the
            action of its algebraic torus.  These orbits are in bijection with
            the cones in the associated fan.  Each cone is determined by the
            rays it contains.  In this package, the rays are ordered and
            indexed by nonnegative integers: $0, 1, \dots, n$.  Using this
            indexing, an orbit or cone corresponds to a sublist of
            $\{ 0, 1, \dots, n \}$; the entries index the rays that generate
            the cone.
        Text	    
            The projective plane has three fixed points and three fixed curves
            (under the action of its torus), and projective $3$-space has four
            fixed points, six fixed curves, and four divisors.
    	Example  
    	    PP2 = toricProjectiveSpace 2;
    	    orbits (PP2,0)
    	    orbits (PP2,1)
	    orbits (PP2,2)
    	    PP3 = toricProjectiveSpace 3;
    	    orbits (PP3,0)
    	    orbits (PP3,1)
    	    orbits (PP3,2)
    	    orbits (PP3,3)	    
    	Text
	    Here is a non-simplicial example.  Since it is nondegenerate, the
            fixed points correspond to the maximal cones in the fan.  The rays
            always correspond to the divisors.
      	Example
    	    X = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
	    orbits (X,0)
            assert (orbits (X,0) === max X)
    	    orbits (X,1)
	    orbits (X,2)
    	    assert (orbits (X,2) === apply (#rays X, i -> {i}))
	    orbits (X,3)
	    assert (orbits (X,3) === {{}})
    	Text
  	    The following degenerate example has no fixed points.
    	Example
    	    U = normalToricVariety ({{4,-1,0},{0,1,0}},{{0,1}});
    	    assert isDegenerate U
    	    orbits (U,0)
    	    orbits (U,1)
    	    orbits (U,2)
	    orbits (U,3)
    	    dim U
    SeeAlso
        "Basic invariants and properties of normal toric varieties"
        (rays, NormalToricVariety)
        (orbits, NormalToricVariety)
///

doc ///
    Key
        (isDegenerate, NormalToricVariety)
        isDegenerate	
    Headline 
        whether a toric variety is degenerate
    Usage 
        isDegenerate X
    Inputs 
        X : NormalToricVariety
    Outputs
        : Boolean 
 	    that is @TO true@ if the fan of {\tt X} is contained in a proper
	    linear subspace of its ambient space
    Description
        Text	
            A $d$-dimensional normal toric variety is degenerate if its rays
            do not span $\QQ^d$.  For example, projective spaces and
            Hirzebruch surfaces are not degenerate.
    	Example  
            assert not isDegenerate toricProjectiveSpace 3
            assert not isDegenerate hirzebruchSurface 7
    	Text
            Although one typically works with non-degenerate toric varieties,
            not all normal toric varieties are non-degenerate.
    	Example  
            U = normalToricVariety ({{4,-1,0},{0,1,0}},{{0,1}});
            isDegenerate U
    SeeAlso
        "Basic invariants and properties of normal toric varieties"
        (rays, NormalToricVariety)
///

doc ///
    Key
        (isSimplicial, NormalToricVariety)
    Headline
        whether a normal toric variety is simplicial
    Usage
        isSimplicial X
    Inputs
        X:NormalToricVariety
    Outputs
        :Boolean 
	    that is @TO true@ if the minimal nonzero lattice points on the
            rays in each maximal cone in the associated fan of form part of a
            $\QQ$-basis
    Description
    	Text		
            A normal toric variety is simplicial if every cone in its fan is
            simplicial and a cone is simplicial if its minimal generators are
            linearly independent over $\QQ$.  In fact, the following
            conditions on a normal toric variety $X$ are equivalent:
    	Text
    	    @UL {
        	{EM "X", " is simplicial,"},
        	{"every torus-invariant Weil divisor on ", EM "X",
         	    " has a positive integer multiple that is Cartier,"},
        	{"the Picard group of ", EM "X", " has finite index in
         	    the class group of ", EM "X", ","},
                {EM "X", " has only finite quotient singularities."}
	    }@
	Text	
            Projective spaces, weighted projective spaces, and Hirzebruch
    	    surfaces are simplicial.
    	Example    
	    PP1 = toricProjectiveSpace 1;
            assert (isSimplicial PP1 and isProjective PP1)
	    FF7 = hirzebruchSurface 7;
            assert (isSimplicial FF7 and isProjective FF7)
	    AA3 = affineSpace 3;
	    assert (isSimplicial AA3 and not isComplete AA3 and # max AA3 === 1)	
	    P12234 = weightedProjectiveSpace {1,2,2,3,4};
            assert (isSimplicial P12234 and isProjective P12234)
            U = normalToricVariety ({{4,-1},{0,1}},{{0,1}});	
	    assert (isSimplicial U and not isSmooth U)
    	Text
            However, not all normal toric varieties are simplicial.
    	Example
	    Q = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	    assert (not isSmooth Q and not isSimplicial Q and not isComplete Q)	    
	    Y = normalToricVariety ( id_(ZZ^3) | - id_(ZZ^3));
	    assert (not isSimplicial Y and isProjective Y)	    
    SeeAlso
        "Basic invariants and properties of normal toric varieties"
        (rays, NormalToricVariety)
     	(max, NormalToricVariety) 
    	(isSmooth, NormalToricVariety)
    	(makeSimplicial, NormalToricVariety)
///

doc ///
    Key 
        (isSmooth, NormalToricVariety)
    Headline 
        whether a normal toric variety is smooth
    Usage
        isSmooth X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : Boolean 
	    that is @TO true@ if the minimal nonzero lattice points on the
            rays in each maximal cone in the associated fan of form part of a
            $\ZZ$-basis
    Description
    	Text		
            A normal toric variety is smooth if every cone in its fan is
            smooth and a cone is smooth if its minimal generators are linearly
            independent over $\ZZ$.  In fact, the following conditions on a
            normal toric variety $X$ are equivalent:
    	Text
	    @UL {
                {EM "X", " is smooth,"},
                {"every torus-invariant Weil divisor on ", EM "X", " is
                    Cartier,"},
                {"the Picard group of ", EM "X", " equals the class group of ",
		    EM "X", ","},
                {EM "X", " has no singularities."}
	    }@
	Text
            Many of our favourite normal toric varieties are smooth.
    	Example
	    PP1 = toricProjectiveSpace 1;
            assert (isSmooth PP1 and isProjective PP1)
	    FF7 = hirzebruchSurface 7;
            assert (isSmooth FF7 and isProjective FF7)
	    AA3 = affineSpace 3;
	    assert (isSmooth AA3 and not isComplete AA3 and # max AA3 === 1)
	    X = smoothFanoToricVariety (4,120);
	    assert (isSmooth X and isProjective X and isFano X)
	    U = normalToricVariety ({{4,-1},{0,1}},{{0},{1}});
	    assert (isSmooth U and not isComplete U)	    
    	Text
            However, not all normal toric varieties are smooth.
    	Example
	    P12234 = weightedProjectiveSpace {1,2,2,3,4};
            assert (not isSmooth P12234 and isSimplicial P12234 and isProjective P12234)
            C = normalToricVariety ({{4,-1},{0,1}},{{0,1}});
    	    assert (not isSmooth C and isSimplicial C and # max C === 1) 
	    Q = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	    assert (not isSmooth Q and not isSimplicial Q and not isComplete Q)
	    Y = normalToricVariety ( id_(ZZ^3) | - id_(ZZ^3));
	    assert (not isSmooth Y and not isSimplicial Y and isProjective Y)
    SeeAlso
        "Basic invariants and properties of normal toric varieties"
        (rays, NormalToricVariety)
        (max, NormalToricVariety)
        (isSimplicial, NormalToricVariety)
///

doc ///
    Key 
	(isFano, NormalToricVariety)
        isFano 	
    Headline 
        whether a normal toric variety is Fano
    Usage 
        isFano X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : Boolean 
	    that is @TO true@ if the normal toric variety is Fano
    Description
        Text
            A normal toric variety is Fano if its anticanonical divisor,
            namely the sum of all the torus-invariant irreducible divisors, is
            ample.  This is equivalent to saying that the polyhedron
            associated to the anticanonical divisor is a reflexive polytope.
        Text
            Projective space is Fano.
    	Example
    	    PP3 = toricProjectiveSpace 3;
            assert isFano PP3
    	    K = toricDivisor PP3
            isAmple (-K)
            assert all (5, d -> isFano toricProjectiveSpace (d+1))
	Text
    	    There are eighteen smooth Fano toric threefolds.
    	Example
    	    assert all (18, i -> (X := smoothFanoToricVariety (3,i); isSmooth X and isFano X))
    	Text
            There are also many singular Fano toric varieties.
    	Example
            X = normalToricVariety matrix {{1,0,-1},{0,1,-1}};
            assert (not isSmooth X and isFano X)
            Y = normalToricVariety matrix {{1,1,-1,-1},{0,1,1,-1}};
    	    assert (not isSmooth Y and isFano Y)
    	    Z = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
    	    assert (not isSmooth Z and isFano Z)
    SeeAlso
        "Basic invariants and properties of normal toric varieties"   
        (toricDivisor, NormalToricVariety)
        (isAmple, ToricDivisor)
        (smoothFanoToricVariety, ZZ, ZZ)
///

doc ///
    Key 
        (isComplete, NormalToricVariety)
    Headline 
        whether a toric variety is complete
    Usage 
        isComplete X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : Boolean
	    that is @TO true@ if the normal toric variety is complete
    Description
        Text
            A normal toric variety is complete if any of the following
            equivalent conditions hold:
    	Text    
    	    @UL {
	        {"the associated complex variety is compact in its classical
	 	    topology,"},
                {"the constant map from the normal toric variety to space
         	    consisting of a single point is proper,"},
                {"every one-parameter subgroup of the torus has a limit in the
        	    toric variety,"},
       	        {"the union of all the cones in the associated fan equals the
        	    entire vector space containing it,"},
    	        {"every torus-invariant curve lying in the normal toric
        	    variety is projective."}
	    }@
	Text
            Affine varieties are not complete.
	Example
	    AA1 = affineSpace 1
	    assert (not isComplete AA1 and isSmooth AA1 and # max AA1 === 1)
	    AA3 = affineSpace 3
	    assert (not isComplete AA3 and isSmooth AA3 and # max AA3 === 1)
      	    U = normalToricVariety ({{4,-1,0},{0,1,0}},{{0,1}});
            assert (not isComplete U and isDegenerate U and # max U === 1)
	    Q = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}})
	    assert (not isComplete Q and not isSmooth Q and # max Q === 1)	    	    
    	Text
    	    Projective varieties are complete.
    	Example
	    PP1 = toricProjectiveSpace 1;
            assert (isComplete PP1 and isProjective PP1 and isSmooth PP1)
	    FF7 = hirzebruchSurface 7;
            assert (isComplete FF7 and isProjective FF7 and isSmooth FF7 and not isFano FF7)	    
	    X = smoothFanoToricVariety (4,120);
            assert (isComplete X and isProjective X and isSmooth X and isFano X)	    	    
	    P12234 = weightedProjectiveSpace {1,2,2,3,4};
            assert (isComplete P12234 and isProjective P12234 and not isSmooth P12234 and isSimplicial P12234)
	    Y = normalToricVariety ( id_(ZZ^3) | - id_(ZZ^3));
	    assert (isComplete Y and isProjective Y and not isSmooth Y and not isSimplicial Y)
    	Text
            There are also complete non-projective normal toric varieties.
    	Example
            X1 = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{-1,0,-1},{-2,-1,0}},{{0,1,2},{0,1,3},{1,3,4},{1,2,4},{2,4,5},{0,2,5},{0,3,5},{3,4,5}});
            assert (isComplete X1 and not isProjective X1 and not isSmooth X1 and isWellDefined X1)
            X2 = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{0,-1,2},{0,0,-1},{-1,1,-1},{-1,0,-1},{-1,-1,0}},{{0,1,2},{0,2,3},{0,3,4},{0,4,5},{0,1,5},{1,2,7},{2,3,7},{3,4,7},{4,5,6},{4,6,7},{5,6,7},{1,5,7}});    
            assert (isComplete X2 and not isProjective X2 and isSmooth X2 and isWellDefined X2)
            X3 = normalToricVariety ({{-1,2,0},{0,-1,0},{1,-1,0},{-1,0,-1},{0,0,-1},{0,1,0},{0,0,1},{1,0,-2}},{{0,1,3},{1,2,3},{2,3,4},{3,4,5},{0,3,5},{0,5,6},{0,1,6},{1,2,6},{2,4,7},{4,5,7},{2,6,7},{5,6,7}});    
            assert (isComplete X3 and not isProjective X3 and isSmooth X3 and isWellDefined X3)
    	Text
	    @SUBSECTION "Reference"@
	Text
            The nonprojective examples are taken from [Osamu Fujino and Sam
            Payne,
            @HREF("http://projecteuclid.org/euclid.pja/1135791770", "Smooth
            complete toric threefolds with non nontrivial nef line bundles")@,
            {\em Proc. Japan Acad. Ser. A Math. Sci.} {\bf 81} (2005), no. 10,
            174-179].
    SeeAlso    
        "Basic invariants and properties of normal toric varieties"
        (isProjective, NormalToricVariety)
///	
  
doc ///
    Key 
        (isProjective, NormalToricVariety)
        isProjective	
    Headline 
        whether a toric variety is projective
    Usage 
        isProjective X
    Inputs
        X : NormalToricVariety
    Outputs
        : Boolean 
	    that is @TO true@ if {\tt X} is a projective variety
    Description
        Text
	    A variety is projective if it can be realized as a closed
            subvariety of some projective space.  For an normal toric variety,
            this is equivalent to saying that the associated fan is the normal
            fan of a polytope.
    	Text
            Nontrivial affine varieties are not projective.
    	Example
    	    assert not isProjective affineSpace 1
    	    assert not isProjective affineSpace 3
    	    U = normalToricVariety ({{4,-1,0},{0,1,0}},{{0,1}});
    	    assert (not isProjective U and isDegenerate U)
    	Text
  	    Many of our favour toric varieties are projective.
    	Example
    	    assert isProjective toricProjectiveSpace 1
    	    assert isProjective toricProjectiveSpace 3
    	    assert isProjective hirzebruchSurface 7
    	    assert isProjective smoothFanoToricVariety (3,3)
    	    assert isProjective normalToricVariety (id_(ZZ^3) | -id_(ZZ^3))
    	Text
  	    There are complete non-projective normal toric varieties.
  	Example
            X1 = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{-1,0,-1},{-2,-1,0}},{{0,1,2},{0,1,3},{1,3,4},{1,2,4},{2,4,5},{0,2,5},{0,3,5},{3,4,5}});
            assert (isComplete X1 and not isProjective X1 and not isSmooth X1)
    	Example
            X2 = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{0,-1,2},{0,0,-1},{-1,1,-1},{-1,0,-1},{-1,-1,0}},{{0,1,2},{0,2,3},{0,3,4},{0,4,5},{0,1,5},{1,2,7},{2,3,7},{3,4,7},{4,5,6},{4,6,7},{5,6,7},{1,5,7}});    
            assert (isComplete X2 and not isProjective X2 and isSmooth X2)
	Text
            To determine if a normal toric variety is projective, we use the
            Gale dual vector configuration associated to the rays; see Theorem
            V.4.8 in Ewald's book {\em Combinatorial convexity and algebraic
            geometry} for more information.  
    SeeAlso
        "Basic invariants and properties of normal toric varieties"
        (isComplete, NormalToricVariety)
        (isAmple,ToricDivisor)
///	
     
doc ///     
    Key 
        (fan, NormalToricVariety)
    Headline
        make the 'Polyhedra' fan associated to the normal toric variety
    Usage 
        fan X
    Inputs 
         X : NormalToricVariety
    Outputs
        : Fan
	    the underlying fan of the normal toric variety
    Description
        Text
            This methods returns the @TO Polyhedra@ fan associated to a normal
            toric variety.  
    	Example  
    	    PP3 = toricProjectiveSpace 3;
    	    F1 = fan PP3
    	    rays F1
    	    maxCones F1
    	    assert (set rays PP3 === set rays normalToricVariety F1 and max PP3 === max normalToricVariety F1)
    	Example
    	    F2 = fan hirzebruchSurface 3;
    	    rays F2
    	    maxCones F2
    SeeAlso
        "Basic invariants and properties of normal toric varieties"
	(normalToricVariety, Fan)
///	
   
   
------------------------------------------------------------------------------
-- working with divisors
------------------------------------------------------------------------------
   
   
doc ///
    Key
        "Working with divisors and their associated groups"
    Description
        Text
            The following methods allows one to make and manipulate
            torus-invariant Weil divisors on a normal toric variety.
    	Text
	    @SUBSECTION "Menu"@
	Text
            @UL {
    	        TO ToricDivisor,	
        	TO (toricDivisor, List, NormalToricVariety),
    		TO (toricDivisor, NormalToricVariety),
		TO (toricDivisor, Polyhedron),
		TO (smallAmpleToricDivisor, ZZ, ZZ),
    		TO (symbol _, NormalToricVariety, ZZ),
    		TO (normalToricVariety, ToricDivisor),
    		TO (expression, ToricDivisor),	
    		TO (support, ToricDivisor),
    		TO (entries, ToricDivisor),
    		TO (symbol +, ToricDivisor, ToricDivisor),
    		TO (symbol SPACE, OO, ToricDivisor),
    		TO (isEffective, ToricDivisor),
    		TO (isCartier, ToricDivisor),    
    		TO (isQQCartier, ToricDivisor), 
    		TO (isNef, ToricDivisor),  
		TO (nefGenerators, NormalToricVariety),     
    		TO (isAmple, ToricDivisor),
    		TO (isVeryAmple, ToricDivisor),
    		TO (vertices, ToricDivisor),
    		TO (latticePoints, ToricDivisor),
		TO (monomials, ToricDivisor),
    		TO (polytope, ToricDivisor)
	    }@
	Text
            One can also work with the various groups arising from
            torus-invariant and the canonical maps between them.
    	Text
	    @SUBSECTION "Menu"@
	Text
            @UL {	    
    		TO (weilDivisorGroup, NormalToricVariety),
    		TO (fromWDivToCl, NormalToricVariety),
    		TO (classGroup, NormalToricVariety),
    		TO (cartierDivisorGroup, NormalToricVariety),
    		TO (fromCDivToWDiv, NormalToricVariety),
    		TO (fromCDivToPic, NormalToricVariety),
    		TO (picardGroup, NormalToricVariety),
    		TO (fromPicToCl, NormalToricVariety)
	    }@
    SeeAlso
        "Making normal toric varieties"
        "Basic invariants and properties of normal toric varieties"
        "Total coordinate rings and coherent sheaves"
        "Resolution of singularities"
///

doc ///
    Key
        (weilDivisorGroup, NormalToricVariety)
        weilDivisorGroup	
    Headline 
        make the group of torus-invariant Weil divisors
    Usage 
        weilDivisorGroup X
    Inputs 
        X : NormalToricVariety 
    Outputs 
        : Module 
	    a finitely generated free abelian group
    Description
        Text
            The group of torus-invariant Weil divisors on a normal toric
            variety is the free abelian group generated by the torus-invariant
            irreducible divisors.  The irreducible divisors correspond
            bijectively to rays in the associated fan.  Since the rays are
            indexed in this package by $0, 1, \dots, n-1$ the group of
            torus-invariant Weil divisors is canonically isomorphic to
            $\ZZ^n$.
    	Text
  	    The examples illustrate various possible Weil groups.
    	Example
     	    PP2 = toricProjectiveSpace 2;
    	    # rays PP2
    	    weilDivisorGroup PP2
    	Example
    	    FF7 = hirzebruchSurface 7;
    	    # rays FF7
    	    weilDivisorGroup FF7
    	Example
    	    U = normalToricVariety ({{4,-1},{0,1}},{{0,1}});
    	    # rays U
    	    weilDivisorGroup U
    SeeAlso
        "Working with divisors and their associated groups"
	(symbol _, NormalToricVariety, ZZ)
        (fromCDivToWDiv, NormalToricVariety)
    	(fromWDivToCl, NormalToricVariety)
    	ToricDivisor
///

doc ///
    Key 
        (fromWDivToCl, NormalToricVariety)
        fromWDivToCl	
    Headline 
        get the map from the group of Weil divisors to the class group	
    Usage 
        fromWDivToCl X
    Inputs 
        X : NormalToricVariety
    Outputs
        : Matrix 
	    defining the surjection from the torus-invariant Weil divisors to
     	    the class group
    Description
        Text
            For a normal toric variety, the class group has a presentation
            defined by the map from the group of torus-characters to group of
            torus-invariant Weil divisors induced by minimal nonzero lattice
            points on the rays of the associated fan.  Hence, there is a
            surjective map from the group of torus-invariant Weil divisors to
            the class group.  This method returns a matrix representing this
            map.  Since the ordering on the rays of the toric variety
            determines a basis for the group of torus-invariant Weil divisors,
            this matrix is determined by a choice of basis for the class
            group.
    	Text
            The examples illustrate some of the possible maps from the group
            of torus-invariant Weil divisors to the class group.
	Example
    	    PP2 = toricProjectiveSpace 2;
    	    A1 = fromWDivToCl PP2
    	    assert ( (target A1, source A1) === (classGroup PP2, weilDivisorGroup PP2) )
	    assert ( A1 * matrix rays PP2 == 0)
    	Example
    	    X = weightedProjectiveSpace {1,2,2,3,4};
    	    A2 = fromWDivToCl X
    	    assert ( (target A2, source A2) === (classGroup X, weilDivisorGroup X) )	    
	    assert ( A2 * matrix rays X == 0)
    	Example
    	    Y = normalToricVariety ( id_(ZZ^3) | - id_(ZZ^3));
    	    A3 = fromWDivToCl Y
	    classGroup Y
    	    assert ( (target A3, source A3) === (classGroup Y, weilDivisorGroup Y) )	    
	    assert ( A3 * matrix rays Y == 0)	    
    	Example
    	    U = normalToricVariety ({{4,-1},{0,1}},{{0,1}});
    	    A4 = fromWDivToCl U
    	    classGroup U
    	    assert ( (target A4, source A4) === (classGroup U, weilDivisorGroup U) )	    
	    assert ( A4 * matrix rays U == 0)	  	    
    	Text
  	    This matrix also induces the grading on the total coordinate ring
            of toric variety.
    	Example  
    	    assert ( transpose matrix degrees ring PP2 === fromWDivToCl PP2)
    	    assert ( transpose matrix degrees ring X === fromWDivToCl X)
    	Text
	    The optional argument @TO WeilToClass@ for the constructor @TO
	    normalToricVariety@ allows one to specify a basis of the class
	    group.
    SeeAlso
        "Working with divisors and their associated groups"
        "Making normal toric varieties"
        (weilDivisorGroup, NormalToricVariety)
    	(classGroup, NormalToricVariety)
    	(ring, NormalToricVariety)
///	

doc ///
    Key 
        (classGroup, NormalToricVariety)
        classGroup	
    Headline 
        make the class group
    Usage 
        classGroup X
    Inputs 
        X : NormalToricVariety
    Outputs
        : Module 
	    a finitely generated abelian group
    Description
        Text
            The class group of a variety is the group of Weil divisors divided
            by the subgroup of principal divisors.  For a normal toric
            variety, the class group has a presentation defined by the map
            from the group of torus-characters to group of torus-invariant
            Weil divisors induced by minimal nonzero lattice points on the
            rays of the associated fan.
    	Text  
            The following examples illustrate some possible class groups.
	Example
    	    classGroup toricProjectiveSpace 1
    	    classGroup hirzebruchSurface 7
	    classGroup affineSpace 3	    
            classGroup normalToricVariety ({{4,-1},{0,1}},{{0,1}})
	    classGroup normalToricVariety ( id_(ZZ^3) | - id_(ZZ^3))
    	Text
            The total coordinate ring of a toric variety is graded by its class group.
	Example
	    degrees ring toricProjectiveSpace 1
	    degrees ring hirzebruchSurface 7	    
	    degrees ring affineSpace 3
    SeeAlso
        "Working with divisors and their associated groups"
        (rays, NormalToricVariety)
        (weilDivisorGroup, NormalToricVariety)
    	(ring, NormalToricVariety)
     	(fromPicToCl, NormalToricVariety)
    	(fromWDivToCl, NormalToricVariety)
///	


doc ///
    Key 
        (cartierDivisorGroup, NormalToricVariety)
        cartierDivisorGroup	
    Headline 
        compute the group of torus-invariant Cartier divisors
    Usage 
        cartierDivisorGroup X
    Inputs 
        X : NormalToricVariety
    Outputs
        : Module
	    a finitely generated abelian group
    Description
        Text	    
            The group of torus-invariant Cartier divisors on $X$ is the
            subgroup of all locally principal torus-invarient Weil divisors.
            On a normal toric variety, the group of torus-invariant Cartier
            divisors can be computed as an inverse limit.  More precisely, if
            $M$ denotes the lattice of characters on $X$ and the maximal cones
            in the fan of $X$ are $sigma_0, sigma_1, \dots, sigma_{r-1}$, then
            we have $CDiv(X) = ker( \oplus_{i} M/M(sigma_i{}) \to{}
            \oplus_{i<j} M/M(sigma_i \cap sigma_j{})$.
	Text
            When $X$ is smooth, every torus-invariant Weil divisor is Cartier.
      	Example
    	    PP2 = toricProjectiveSpace 2;
	    cartierDivisorGroup PP2
    	    assert (isSmooth PP2 and weilDivisorGroup PP2 === cartierDivisorGroup PP2)
    	    assert (id_(cartierDivisorGroup PP2) == fromCDivToWDiv PP2)
    	Example
    	    FF7 = hirzebruchSurface 7;
    	    cartierDivisorGroup FF7
   	    assert (isSmooth FF7 and weilDivisorGroup FF7 === cartierDivisorGroup FF7)
    	    assert (id_(cartierDivisorGroup FF7) == fromCDivToWDiv FF7)	    
    	Text
            On a simplicial toric variety, every torus-invariant Weil divisor
            is $\QQ$-Cartier; every torus-invariant Weil divisor has a
            positive integer multiple that is Cartier.
    	Example  
    	    U = normalToricVariety ({{4,-1},{0,1}},{{0,1}});
	    assert (isSimplicial U and not isSmooth U and not isComplete U)
    	    cartierDivisorGroup U
	    weilDivisorGroup U
	    prune coker fromCDivToWDiv U
    	    assert ( (coker fromCDivToWDiv U) ** QQ == 0)
	Example
	    X = weightedProjectiveSpace {1,2,2,3,4};
	    assert (isSimplicial X and not isSmooth X and isComplete X)
   	    cartierDivisorGroup X
	    weilDivisorGroup X
	    prune coker fromCDivToWDiv X
    	    assert (rank coker fromCDivToWDiv X === 0)
	Text	    
            In general, the Cartier divisors are only a subgroup of the Weil divisors.
	Example
    	    Q = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	    assert (not isSimplicial Q and not isComplete Q)
    	    cartierDivisorGroup Q
    	    weilDivisorGroup Q
	    prune coker fromCDivToWDiv Q
	    assert (rank coker fromCDivToWDiv Q === 1)
    	Example
    	    Y = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
	    assert (not isSimplicial Y and isComplete Y)
    	    cartierDivisorGroup Y
    	    weilDivisorGroup Y
    	    prune cokernel fromCDivToWDiv Y
	    assert (rank coker fromCDivToWDiv Y === 4)
    SeeAlso
        "Working with divisors and their associated groups"
        (fromCDivToWDiv,NormalToricVariety)
    	(fromCDivToPic,NormalToricVariety)
    	(isCartier,ToricDivisor)
///	

doc ///
    Key
        (fromCDivToWDiv, NormalToricVariety)
        fromCDivToWDiv	
    Headline 
        get the map from Cartier divisors to Weil divisors
    Usage 
        fromCDivToWDiv X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : Matrix 
	    representing the inclusion map from the group of torus-invariant
	    Cartier divisors to the group of torus-invariant Weil divisors
    Description
        Text
            The group of torus-invariant Cartier divisors is the subgroup of
            all locally principal torus-invariant Weil divisors.  This
            function produces the inclusion map with respect to the chosen
            bases for the two finitely-generated abelian groups.
    	Text  
            On a smooth normal toric variety, every torus-invariant Weil
            divisor is Cartier, so the inclusion map is simply the identity
            map.
    	Example  
    	    PP2 = toricProjectiveSpace 2;
	    assert (isSmooth PP2 and isProjective PP2)
    	    fromCDivToWDiv PP2
    	    assert (fromCDivToWDiv PP2 === id_(weilDivisorGroup PP2))
    	Example
	    X = smoothFanoToricVariety (4,20);
	    assert (isSmooth X and isProjective X and isFano X)
	    fromCDivToWDiv X
	    assert (fromCDivToWDiv X === id_(weilDivisorGroup X))
	Example
    	    U = normalToricVariety ({{4,-1},{0,1}},{{0},{1}});
	    assert (isSmooth U and not isComplete U)
    	    fromCDivToWDiv U
	    assert (fromCDivToWDiv U === id_(weilDivisorGroup U))	    
    	Text
            On a simplicial normal toric variety, every torus-invariant Weil
            divisor is $\QQ$-Cartier; every torus-invariant Weil divisor has a
            positive integer multiple that is Cartier.
    	Example  
    	    C = normalToricVariety ({{4,-1},{0,1}},{{0,1}});
    	    fromCDivToWDiv C
    	    prune cokernel fromCDivToWDiv C
    	    assert (rank cokernel fromCDivToWDiv C === 0)
    	Text
	    In general, the Cartier divisors are only a subgroup of the Weil
	    divisors.
	Example
    	    Q = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	    assert (not isSimplicial Q and not isComplete Q)
    	    fromCDivToWDiv Q
	    prune coker fromCDivToWDiv Q
	    assert (rank coker fromCDivToWDiv Q === 1)
    	Example
    	    Y = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
	    assert (not isSimplicial Y and isComplete Y)
    	    fromCDivToWDiv Y
    	    prune cokernel fromCDivToWDiv Y
	    assert (rank coker fromCDivToWDiv Y === 4)	    
    SeeAlso
        "Working with divisors and their associated groups"
        (weilDivisorGroup ,NormalToricVariety)
        (cartierDivisorGroup, NormalToricVariety)
        (isCartier, ToricDivisor)
///	

doc ///
    Key 
    	(fromCDivToPic, NormalToricVariety)
        fromCDivToPic	
    Headline 
        get the map from Cartier divisors to the Picard group
    Usage 
        fromCDivToPic X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : Matrix
	    representing the surjective map from the group of torus-invariant
            Cartier divisors to the Picard group
    Description
        Text
            The Picard group of a variety is the group of Cartier divisors
            divided by the subgroup of principal divisors.  For a normal toric
            variety , the Picard group has a presentation defined by the map
            from the group of torus-characters to the group of torus-invariant
            Cartier divisors.  Hence, there is a surjective map from the group
            of torus-invariant Cartier divisors to the Picard group.  This
            function returns a matrix representing this map with respect to
            the chosen bases.
	Text
            On a smooth normal toric variety, the map from the torus-invariant
            Cartier divisors to the Picard group is the same as the map from
            the Weil divisors to the class group.
    	Example
    	    PP2 = toricProjectiveSpace 2;
	    assert (isSmooth PP2 and isProjective PP2)
    	    fromCDivToPic PP2
    	    assert (fromCDivToPic PP2 === fromWDivToCl PP2)
    	Example
	    X = smoothFanoToricVariety (4,20);
	    assert (isSmooth X and isProjective X and isFano X)
	    fromCDivToPic X
	    assert (fromCDivToPic X === fromWDivToCl X)
	Example
    	    U = normalToricVariety ({{4,-1},{0,1}},{{0},{1}});
	    assert (isSmooth U and not isComplete U)
    	    fromCDivToPic U
	    assert (fromCDivToPic U === fromWDivToCl U)
    	Text	    
            In general, there is a commutative diagram relating the map from
            the group of torus-invariant Cartier divisors to the Picard group
            and the map from the group of torus-invariant Weil divisors to the
            class group.
      	Example
    	    Q = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	    assert (not isSimplicial Q and not isComplete Q)
    	    fromCDivToPic Q
    	    assert (fromWDivToCl Q * fromCDivToWDiv Q == fromPicToCl Q * fromCDivToPic Q)
    	Example
    	    Y = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
	    assert (not isSimplicial Y and isProjective Y)
    	    fromCDivToPic Y
	    fromPicToCl Y
	    fromPicToCl Y * fromCDivToPic Y
	    fromCDivToWDiv Y
	    fromWDivToCl Y
    	    assert (fromWDivToCl Y * fromCDivToWDiv Y == fromPicToCl Y * fromCDivToPic Y)
    SeeAlso
        "Working with divisors and their associated groups"
        (cartierDivisorGroup, NormalToricVariety)
        (picardGroup, NormalToricVariety)
        (fromWDivToCl, NormalToricVariety)
///	

doc ///
    Key 
    	(picardGroup, NormalToricVariety)
	picardGroup	
    Headline 
        make the Picard group
    Usage 
        picardGroup X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : Module 
	    a finitely generated abelian group
    Description
        Text
            The Picard group of a variety is the group of Cartier divisors
            divided by the subgroup of principal divisors.  For a normal toric
            variety, the Picard group has a presentation defined by the map
            from the group of torus-characters to the group of torus-invariant
            Cartier divisors.
    	Text
            When the normal toric variety is smooth, the Picard group is
            isomorphic to the class group.
      	Example
    	    PP3 = toricProjectiveSpace 3;
	    assert (isSmooth PP3 and isProjective PP3)
    	    picardGroup PP3
    	    assert (picardGroup PP3 === classGroup PP3 and isFreeModule picardGroup PP3)
    	Example
	    X = smoothFanoToricVariety (4,90);
	    assert (isSmooth X and isProjective X and isFano X)
	    picardGroup X
	    assert (fromCDivToPic X === fromWDivToCl X and isFreeModule picardGroup X)	
   	Example
    	    U = normalToricVariety ({{4,-1},{0,1}},{{0},{1}});
	    assert (isSmooth U and not isComplete U and # max U =!= 1)
    	    picardGroup U
    	    assert (classGroup U	=== picardGroup U and not isFreeModule picardGroup U)
    	Text
            For an affine toric variety, the Picard group is trivial.
	Example
	    AA3 = affineSpace 3
	    assert (isSimplicial AA3 and isSmooth AA3 and # max AA3 === 1)
	    picardGroup AA3
	    assert (picardGroup AA3 == 0 and isFreeModule picardGroup AA3)
    	Example
    	    Q = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	    assert (not isSimplicial Q and not isComplete Q and # max Q === 1)
    	    picardGroup Q
	    assert (picardGroup Q == 0 and isFreeModule picardGroup Q)
	Text
	    If the fan associated to $X$ contains a cone of dimension
	    $dim(X)$, then the Picard group is free.
    	Example
    	    Y = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
	    assert (not isSimplicial Y and isProjective Y)
    	    picardGroup Y
	    assert (rank picardGroup Y === 1 and isFreeModule picardGroup Y)
    SeeAlso
        "Working with divisors and their associated groups"
        (classGroup, NormalToricVariety)
        (cartierDivisorGroup, NormalToricVariety)
        (fromCDivToPic, NormalToricVariety)
        (fromPicToCl, NormalToricVariety)
///	

doc ///
    Key
	(nefGenerators, NormalToricVariety)
        nefGenerators	
    Headline
        compute generators of the nef cone
    Usage
        nefGenerators X
    Inputs
        X : NormalToricVariety
    Outputs
        : Matrix
	    whose columns generate the nef cone of {\tt X} as a convex cone in
	    the Picard group
    Description
        Text
	    The nef cone of a variety is the cone generated by classes of nef
	    Cartier divisors in vector space of Cartier divisors modulo
	    numerical equivalence.  On a normal toric variety, numerical
	    equivalence and linear equivalence coincide, so the nef cone lies
	    in the Picard group.  Assume that the normal toric variety is
	    non-degenerate, its nef cone is a rational polyhedral cone in the
	    Picard group; see Theorem 6.3.20 in Cox-Little-Schenck.  This
	    function calculates generators for the rays of this cone, and
	    returns a matrix whose columns correspond to these generates
	    (expressed as vectors in the chosen basis for the Picard group).
	Text
	    For some of our favourite normal toric varieties, we choose a
	    basis for the Picard group which makes the nef cone into the
	    positive orthant.
	Example
	    nefGenerators toricProjectiveSpace 1
	    nefGenerators toricProjectiveSpace 3	    
	    nefGenerators normalToricVariety ( id_(ZZ^3) | - id_(ZZ^3))
	    nefGenerators hirzebruchSurface 7
    	    nefGenerators kleinschmidt (3,{0,1})	    
	    nefGenerators smoothFanoToricVariety (2,3)
	    nefGenerators smoothFanoToricVariety (3,12)
	    nefGenerators smoothFanoToricVariety (4,90)
	Text
	    In general, the nef cone need not even be simplicial.
	Example
	    nefGenerators smoothFanoToricVariety (2,4)	    
	    nefGenerators smoothFanoToricVariety (3,16)	    	    
	    nefGenerators smoothFanoToricVariety (4,120)
	Text
	    There are smooth complete normal toric varieties with no
            nontrivial nef divisors.
    	Example    
            X = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{0,-1,2},{0,0,-1},{-1,1,-1},{-1,0,-1},{-1,-1,0}},{{0,1,2},{0,2,3},{0,3,4},{0,4,5},{0,1,5},{1,2,7},{2,3,7},{3,4,7},{4,5,6},{4,6,7},{5,6,7},{1,5,7}});    
	    assert (isComplete X and not isProjective X and isSmooth X)
	    picardGroup X
	    assert (nefGenerators X == 0)
    SeeAlso
        "Working with divisors and their associated groups"
	(isNef, ToricDivisor)
///	        

doc ///
    Key
        (fromPicToCl, NormalToricVariety)
        fromPicToCl	
    Headline 
        get the map from Picard group to class group
    Usage 
        fromPicToCl X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : Matrix 
	    representing the inclusion map from the Picard group to the class
	    group
    Description
        Text			
	    The Picard group of a normal toric variety is a subgroup of the
            class group.  This function returns a matrix representing this map
            with respect to the chosen bases.
    	Text
            On a smooth normal toric variety, the Picard group is isomorphic
            to the class group, so the inclusion map is the identity.
    	Example  
    	    PP3 = toricProjectiveSpace 3;
	    assert (isSmooth PP3 and isProjective PP3)
    	    fromPicToCl PP3
	    assert (fromPicToCl PP3 === id_(classGroup PP3))
    	Example
	    X = smoothFanoToricVariety (4,90);
	    assert (isSmooth X and isProjective X and isFano X)
    	    fromPicToCl X
	    assert (fromPicToCl X === id_(classGroup X))	    
   	Example
    	    U = normalToricVariety ({{4,-1},{0,1}},{{0},{1}});
	    assert (isSmooth U and not isComplete U and # max U =!= 1)
	    fromPicToCl U
	    assert (fromPicToCl U === id_(classGroup U))	   	    
    	Text
            For weighted projective space, the inclusion corresponds to 
	    $l \ZZ$ in $\ZZ$ where $l = lcm(q_0, q_1, \dots, q_d {})$.
    	Example
    	    P123 = weightedProjectiveSpace {1,2,3};
	    assert (isSimplicial P123 and isProjective P123)
	    fromPicToCl P123
	    assert (fromPicToCl P123 === lcm (1,2,3) * id_(classGroup P123))	   	    
    	Example
    	    P12234 = weightedProjectiveSpace {1,2,2,3,4};
	    assert (isSimplicial P12234 and isProjective P12234)
	    fromPicToCl P12234
	    assert (fromPicToCl P12234 === lcm (1,2,2,3,4) * id_(classGroup P12234))	
	Text
            The following examples illustrate some other possibilities.
	Example
    	    Q = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	    assert (not isSimplicial Q and not isComplete Q and # max Q === 1)
	    fromPicToCl Q
    	    assert (fromPicToCl Q == 0)
    	Example
   	    Y = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
	    assert (not isSimplicial Y and isProjective Y)
    	    fromPicToCl Y
    SeeAlso
        "Working with divisors and their associated groups"
        (picardGroup, NormalToricVariety)
    	(classGroup, NormalToricVariety)
///	

doc ///
    Key
        ToricDivisor
    Headline 
        the class of all torus-invariant Weil divisors
    Description
        Text
            A torus-invariant Weil divisor on a normal toric variety is an
            integral linear combination of the irreducible torus-invariant
            divisors.  The irreducible torus-invariant divisors correspond to
            the rays.  In this package, the rays are ordered and indexed by
            the nonnegative integers.
    	Text  
            The first examples illustrates some torus-invariant Weil divisors
            on projective $2$-space.
    	Example  
    	    PP2 = toricProjectiveSpace 2;
            D1 = toricDivisor ({2,-7,3}, PP2) 
            D2 = 2*PP2_0 + 4*PP2_2
            D1+D2
            D1-D2
            K = toricDivisor PP2  
	Text
            One can easily extract individual coefficients or the list of
            coefficients.
    	Example  
    	    D1#0
            D1#1
            D1#2
            entries D1
            entries K
    SeeAlso
        "Working with divisors and their associated groups"
        (weilDivisorGroup, NormalToricVariety)
        (toricDivisor, List, NormalToricVariety)
        (toricDivisor, NormalToricVariety)
        (symbol _, NormalToricVariety, ZZ)
        (expression, ToricDivisor)
        (normalToricVariety, ToricDivisor)
        (support, ToricDivisor)
        (symbol +, ToricDivisor, ToricDivisor)
///

doc ///
    Key 
        (isWellDefined, ToricDivisor)
    Headline 
        whether a toric divisor is well-defined
    Usage
        isWellDefined D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Boolean 
	    that is @TO true@ if underlying data structure is correctly
	    formatted
    Description
        Text			  
            This function checks that the following aspects of the data
            structure:
        Text	    
            @UL {
		{"the underlying ", TO HashTable, " has the expected keys,
	          namely the integers from ", TT "0", " to ", TT "n-1", "
	          where ", TT "n = # rays variety D", ", ", TT "variety", ",
	          and ", TT "cache", ","},
	        {"the value of each integer key is an ", TO ZZ, ","},
		{"the value of the ", TT "variety", " key is a ", 
	    	  TO NormalToricVariety, ","},
	        {"the value of the ", TT "cache", " key is a ", 
		  TO CacheTable, "."}
	    }@
        Example
      	    PP2 = toricProjectiveSpace 2
	    D1 = toricDivisor ({2,-7,3}, PP2)
	    assert isWellDefined D1
        Example	    
    	    debugLevel = 1;
    	    D2 = new ToricDivisor from hashTable { 0 => 2, symbol variety => PP2, symbol cache => new CacheTable};
	    assert not isWellDefined D2
	    D3 = new ToricDivisor from hashTable { 0 => 2, 1 => x, 2 => 3, symbol variety => PP2, symbol cache => new CacheTable};
    	    assert not isWellDefined D3	
	    D4 = new ToricDivisor from hashTable { 0 => 2, 1 => -7, 2 => 3, symbol variety => 7, symbol cache => new CacheTable};	
	    assert not isWellDefined D4	
        Text	
            The function @TO(expression, ToricDivisor)@ assumes that the input
            toric divisor is well-defined.
    SeeAlso
	"Working with divisors and their associated groups"
        (toricDivisor, List, NormalToricVariety)
	"debugLevel"
///	   	

doc ///
    Key
        (expression, ToricDivisor)
    Headline 
        get the expression used to format for printing
    Usage 
        expression D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Expression
	    used to format {\tt D} for printing
    Description
        Text	    
            This method is the primary function called upon by @TO(symbol <<)@
            to format for printing.  It assumes that {\tt D} is well-defined.
      	Text
	    When the underlying normal toric variety has not been assigned a
            global variable, the $i$-th irreducible torus-invariant Weil
            divisor is displayed as $D_i$.  However, if the underlying normal
            toric variety has been assigned a global variable $X$, the $i$-th
            irreducible torus-invariant Weil divisor is displayed as $X_i$.
            In either case, an arbitrary torus-invariant Weil divisor is
            displayed as an integral linear combination of these
            expressions.
    	Example
	    toricDivisor({2,-7,3}, toricProjectiveSpace 2)  
    	Example	    
	    toricDivisor convexHull (id_(ZZ^3) | - id_(ZZ^3))
    	Example	    
	    PP2 = toricProjectiveSpace 2;
	    D1 = toricDivisor({2,-7,3}, PP2)  
	    D2 = 2 * PP2_0 - 7 * PP2_1 + 3 * PP2_2
	    assert(D1 == D1)
    SeeAlso
        "Working with divisors and their associated groups"
	(isWellDefined, ToricDivisor)
        (symbol _, NormalToricVariety, ZZ)
///

undocumented { (net,ToricDivisor) }

doc ///
    Key 
        (vector, ToricDivisor)
    Headline 
        make the vector of coefficients
    Usage 
        vector D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Vector 
	    whose entries are the coefficients of {\tt D}
    Description
        Text	  
            This function returns the @TO Vector@ whose $i$-th entry is the
            coefficient of $i$-th irreducible torus-invariant divisor.  The
            indexing of the irreducible torus-invariant divisors is inherited
            from the indexing of the rays in the associated fan.  This list
            can be viewed as an element of the group of torus-invariant Weil
            divisors.
	Text
	    Here are two simple examples.
	Example
	    PP2 = toricProjectiveSpace 2;
	    D1 = 2*PP2_0 - 7*PP2_1 + 3*PP2_2
	    vector D1
	    assert(entries vector D1 === entries D1)
	Example
	    D2 = toricDivisor convexHull (id_(ZZ^3) | - id_(ZZ^3))
	    vector D2
	    assert(entries vector D2 === entries D2)
    SeeAlso
        "Working with divisors and their associated groups"
	ToricDivisor
	(entries, ToricDivisor)
	(rays, NormalToricVariety)
///

doc ///
    Key
        (entries, ToricDivisor)
    Headline
        get the list of coefficients 
    Usage
        entries D
    Inputs
        D : ToricDivisor
    Outputs
        : List
	    of @TO2 (ZZ, "integers")@ that are the coefficients of the corresponding
	    irreducible torus-invariant divisors
    Description
        Text	   
            This function returns the @TO List@ whose $i$-th entry is the
            coefficient of $i$-th irreducible torus-invariant divisor.  The
            indexing of the irreducible torus-invariant divisors is inherited
            from the indexing of the rays in the associated fan.  This list
            can be viewed as an element of the group of torus-invariant Weil
            divisors.
	Text
	    Here are two simple examples.
	Example
	    PP2 = toricProjectiveSpace 2;
	    D1 = 2*PP2_0 - 7*PP2_1 + 3*PP2_2
	    entries D1
	    assert( D1 == toricDivisor(entries D1, variety D1) )
	    assert all(entries toricDivisor PP2, i -> i === -1)
	Example
	    D2 = toricDivisor convexHull (id_(ZZ^3) | - id_(ZZ^3))
	    entries D2
    	    assert all(entries D2, i -> i === 1)
    SeeAlso
        "Working with divisors and their associated groups"
	(rays, NormalToricVariety)		
	(toricDivisor, List, NormalToricVariety)
	(toricDivisor, Polyhedron)
///

doc ///
    Key
        (degree, ToricDivisor)
    Headline
        make the degree of the associated rank-one reflexive sheaf
    Usage
        degree D
    Inputs
        D : ToricDivisor
    Outputs
        : List	
	    of @TO2 (ZZ, "integers")@ corresponding to the degree of the
	    rank-one reflexive sheaf
    Description
        Text	   
            This function returns the @TO List@ representing an element of the
	    Picard group corresponding to the associated rank-one reflexive
	    sheaf.
	Text
	    Here are two simple examples.
	Example
	    PP2 = toricProjectiveSpace 2;
	    D1 = PP2_0
	    degree D1
	    OO D1
	    D2 = 3*PP2_1
	    degree D2
	    OO D2
	Example
	    FF2 = hirzebruchSurface 2;
	    D3 = -1*FF2_2 + 3*FF2_3
	    degree D3
    	    OO D3	    
    SeeAlso
        "Working with divisors and their associated groups"
	(symbol SPACE, OO, ToricDivisor)
	(picardGroup, NormalToricVariety)
///

doc ///     	    
    Key 
        (support, ToricDivisor)
    Headline 
        make the list of irreducible divisors with nonzero coefficients
    Usage 
        support D
    Inputs 
        D : ToricDivisor
    Outputs 
        : List 
	    indexing the irreducible torus-invariant divisors whose
            coefficient in {\tt D} are nonzero
    Description
        Text			
            The support of a torus-invariant Weil divisor is the set of
            irreducible torus-invariant divisors which appear with nonzero
            coefficients in the unique expression for this divisor.  In this
            package, we encode this information by indexing the irreducible
            torus-invariantdivisors that appear with a nonzero coefficient.
            The indexing of the irreducible torus-invariant divisors is
            inherited from the indexing of the rays in the associated fan.
    	Example	    
    	    PP2 = toricProjectiveSpace 2;
            D1 = 2*PP2_0 - 7*PP2_1 + 3*PP2_2    
    	    support D1
    	    D2 = PP2_0-5*PP2_2
    	    support D2
    	    support (6*PP2_1)
    SeeAlso
        "Working with divisors and their associated groups"
        (rays,NormalToricVariety)
	(entries, ToricDivisor)
///

doc ///
    Key 
        (variety, ToricDivisor)
        (normalToricVariety, ToricDivisor)
    Headline 
        get the underlying normal toric variety
    Usage 
        variety D 
	normalToricVariety D
    Inputs 
        D : ToricDivisor
    Outputs 
        : NormalToricVariety
	    namely the underlying variety for {\tt D}
    Description
        Text	    
            This function allows one to easily access the normal toric variety
            over which the torus-invariant Weil divisor is defined.
    	Example  
    	    PP2 = toricProjectiveSpace 2;
    	    D1 = 2*PP2_0 - 7*PP2_1 + 3*PP2_2
    	    variety D1
    	    normalToricVariety D1
	    assert(variety D1 === PP2 and normalToricVariety D1 === PP2)
    	Example
    	    X = normalToricVariety(id_(ZZ^3) | - id_(ZZ^3));
    	    D2 = X_0 - 5 * X_3
	    variety D2
    	    assert(X === variety D2 and X === normalToricVariety D2)
    SeeAlso
        "Working with divisors and their associated groups"
	NormalToricVariety
///	

doc ///
    Key
        (toricDivisor, List, NormalToricVariety)
        toricDivisor	
    Headline 
        make a torus-invariant Weil divisor
    Usage 
        toricDivisor(L, X)
    Inputs
        L : List
   	    of integers coefficients for the irreducible torus-invariant
	    divisors on {\tt X}
	X : NormalToricVariety
    Outputs 
        : ToricDivisor
    Description
        Text
            Given a list of integers and a normal toric variety, this method
            returns the torus-invariant Weil divisor such the coefficient of
            the $i$-th irreducible torus-invariant divisor is the $i$-th entry
            in the list.  The indexing of the irreducible torus-invariant
            divisors is inherited from the indexing of the rays in the
            associated fan.  In this package, the rays are ordered and indexed
            by the nonnegative integers.
	Example
            PP2 = toricProjectiveSpace 2;
    	    D = toricDivisor({2,-7,3},PP2)
            assert(D == 2* PP2_0 - 7*PP2_1 + 3*PP2_2)
            assert(D == toricDivisor(entries D, variety D))
    	Text
            Although this is a general method for making a torus-invariant
            Weil divisor, it is typically more convenient to simple enter the
            appropriate linear combination of torus-invariant Weil divisors.
    SeeAlso
        "Working with divisors and their associated groups"
        (toricDivisor, NormalToricVariety)
        (symbol _, NormalToricVariety, ZZ)
///	

doc ///
    Key 
        (toricDivisor, NormalToricVariety)
    Headline
        make the canonical divisor
    Usage 
        toricDivisor X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : ToricDivisor
	    specifically minus the sum of all the irreducible torus-invariant
            divisors
    Description
        Text			       
            On a smooth normal toric variety, the canonical divisor equals
            minus the sum of all the torus-invariant irreducible divisors.
            For a singular toric variety, this divisor may not be Cartier or
            even $\QQ$-Cartier.  Nevertheless, the associated coherent sheaf,
            whose local sections are rational functions with at least simple
            zeros along the irreducible divisors, is the dualizing sheaf.
	Text
	    The first example illustrates the canonical divisor on projective
	    space.
    	Example	    
    	    PP3 = toricProjectiveSpace 3;
	    assert(isSmooth PP3 and isProjective PP3)
    	    K = toricDivisor PP3
	    assert(all(entries K, i -> i === -1) and isWellDefined K)
    	    omega = OO K
    	    assert(HH^3(PP3, OO_PP3(-7) ** omega) === HH^0(PP3, OO_PP3(7)))
	Text
            The second example illustrates that duality also holds on complete
            singular nonprojective toric varieties.
	Example 
            X = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{-1,0,-1},{-2,-1,0}},{{0,1,2},{0,1,3},{1,3,4},{1,2,4},{2,4,5},{0,2,5},{0,3,5},{3,4,5}});
            assert(isComplete X and not isProjective X and not isSmooth X)	
    	    KX = toricDivisor X	
	    assert(all(entries KX, i -> i === -1) and isWellDefined KX)
	    isCartier KX
	    omegaX = OO KX
	    assert( HH^0(X, OO_X(1,2,5)) === HH^3(X, OO_X(-1,-2,-5) ** omegaX) )
    SeeAlso
        "Working with divisors and their associated groups"
    	toricDivisor
    	(symbol SPACE, OO, ToricDivisor)
    	(cohomology, ZZ, NormalToricVariety, CoherentSheaf)
///	

doc ///
    Key
        (toricDivisor, Polyhedron)
	[toricDivisor, CoefficientRing]
	[toricDivisor, Variable]
	[toricDivisor, WeilToClass]
    Headline
        make the toric divisor associated to a polyhedron
    Usage
        toricDivisor P
    Inputs
        P:Polyhedron
	    whose vertices are lattice points
        CoefficientRing => Ring
	    that determines the coefficient ring of the total coordinate ring
	    of the underlying normal toric variety
        Variable => Symbol
	    that specifies the base symbol for the indexed variables in the
	    total coordinate ring of the underlying normal toric variety	    
        WeilToClass => Matrix 
            that specifies the map from the group of torus-invariant Weil
            divisors to the class group of the underlying normal toric variety
    Outputs
       :ToricDivisor
           corresponding to the given convex lattice polytope on the normal
	   toric variety defined by the inner normal fan
    Description
        Text
	    A convex lattice polytope corresponds to a pair: the normal toric
	    variety determined by its normal fan and toric divisor.  The
	    coefficient of the $i$-th irreducible torus-invariant divisor is
	    determined by the supporting hyperplane to the polytope whose
	    normal vector is the minimal lattice point on the $i$-th ray.
	Text
	   Our example demonstrates how different triangles correspond
	   to toric divisors on the projective plane.
    	Example
	    P1 = convexHull matrix{{0,1,0},{0,0,1}};
	    D1 = toricDivisor P1
	    X = variety D1;
	    D1
	    P2 = convexHull matrix{{-1,0,-1},{0,0,1}};
	    D2 = toricDivisor P2
	    P3 = convexHull matrix{{0,1,0},{-1,-1,0}};
	    D3 = toricDivisor P3
	    P4 = convexHull matrix{{-1,2,-1},{-1,-1,2}};
	    D4 = toricDivisor(P4, CoefficientRing => ZZ/2)
	    ring variety D4
	Text
	    This method function creates both the toric divisor and the
	    underlying normal toric variety.
    SeeAlso
        "Working with divisors and their associated groups"
	(normalToricVariety, Polyhedron)
///	   
	   
doc ///
    Key 
        (symbol _, NormalToricVariety, ZZ)
    Headline 
        make an irreducible torus-invariant divisor
    Usage 
        X_i
    Inputs
        X : NormalToricVariety
        i : ZZ
	    indexing a ray in the fan associated to {\tt X}
    Outputs 
        : ToricDivisor 
	    namely the irreducible torus-invariant divisor associated to the
	    {\tt i}-th ray in the fan of {\tt X}
    Description
        Text	    
            The irreducible torus-invariant divisors on a normal toric variety
            correspond to the rays in the associated fan.  In this package,
            the rays are ordered and indexed by the nonnegative integers.
            Given a normal toric variety and nonnegative integer, this method
            returns the corresponding irreducible torus-invariant divisor.
            The most convenient way to make a general torus-invariant Weil
            divisor is to simply write the appropriate linear combination of
            these torus-invariant Weil divisors.
	Text
	    There are three irreducible torus-invariant divisors on the
	    projective plane.
    	Example	    
    	    PP2 = toricProjectiveSpace 2;
    	    PP2_0
    	    PP2_1
    	    PP2_2
    	    assert (- PP2_0 - PP2_1 - PP2_2 === toricDivisor PP2)
    	Text
	    A torus-invariant Weil divisor is irreducible if and only if its
            support has a single element.
    	Example  
     	    X = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
    	    X_0
    	    support X_0
	    assert( # support X_0 === 1)
    	    K = toricDivisor X	    
    	    support K
    SeeAlso
        "Working with divisors and their associated groups"
	(support, ToricDivisor)
    	(variety, ToricDivisor)
///

doc /// 
    Key 
        (smallAmpleToricDivisor, ZZ, ZZ)    
        smallAmpleToricDivisor	    
    	[smallAmpleToricDivisor, CoefficientRing]
    	[smallAmpleToricDivisor, Variable]
	[smallAmpleToricDivisor, WeilToClass]
    Headline 
        get a very ample toric divisor from the database
    Usage 
        smallAmpleToricDivisor (d, i)
    Inputs
        d : ZZ 
	    equal to dimension of the underlying toric variety
        i : ZZ 
	    indexing the toric divisor in the database
        CoefficientRing => Ring 
	    that specifies the coefficient ring of the total coordinate ring
        Variable => Symbol 
	    that specifies the base symbol for the indexed variables in the
	    total coordinate ring
        WeilToClass => Matrix 
            that specifies the map from the group of torus-invariant Weil
            divisors to the class group of the underlying normal toric variety
    Outputs
       : ToricDivisor
           representing a very ample divisor that embeds the underlying smooth
           toric variety into a low-dimensional projective space
    Description
        Text
            This method function accesses a database of equivalence classes of very
            ample divisors that embed their underlying smooth toric varieties
            into low-dimensional projective spaces.  
	Text
	    The enumeration of the $41$ smooth projective toric surfaces
            embedding into at most projective $11$-space follows the
            classification in [Tristram Bogart, Christian Haase, Milena Hering,
            Benjamin Lorenz, Benjamin Nill Andreas Paffenholz, Günter Rote,
            Francisco Santos, and Hal Schenck,
            @HREF("https://dx.doi.org/10.1007/s11856-015-1175-7", "Finitely
            many smooth d-polytopes with n lattice points")@, {\em Israel
            J. Math.}, {\bf 207} (2015) 301-329].
	Text
	    The enumeration of the $103$ smooth projective toric threefolds
	    embedding into at most projective $15$-space follows [Anders
	    Lundman,
	    @HREF("https://dx.doi.org/10.1007/10.1007/s10801-012-0363-3", "A
	    classification of smooth convex 3-polytopes with at most 16
	    lattice points")@, {\em J. Algebr. Comb.}, {\bf 37} (2013) 139-165].
	Text
    	    The first $2$ toric divisors over a surface lie over a product of
    	    projective lines.
      	Example
	    D1 = smallAmpleToricDivisor(2,0)
	    assert isVeryAmple D1
	    X1 = variety D1;
	    assert (isSmooth X1 and isProjective X1)
	    rays X1
	    D1
	    latticePoints D1
	    D2 = smallAmpleToricDivisor (2,1);
	    assert isVeryAmple D2
	    X2 = variety D2;
	    assert (isSmooth X2 and isProjective X2)
	    rays X2
	    D2
	    latticePoints D2
	Text
	    The $15$-th toric divisors on a surface lies over normal toric
	    varieties with $8$ irreducible torus-invariant divisors.
	Example
	    D3 = smallAmpleToricDivisor (2,15);
	    assert isVeryAmple D3
	    X3 = variety D3;
	    assert (isSmooth X3 and isProjective X3)
	    rays X3
	    D3
	    latticePoints D3
	Text
	    Last, $25$ toric divisors on a surface lies over Hirzebruch
	    surfaces.
	Example
	    D4 = smallAmpleToricDivisor (2,30);
	    assert isVeryAmple D4
	    X4 = variety D4;
	    assert (isSmooth X4 and isProjective X4)
	    rays X4
	    D4
	    latticePoints D4
	Text  
	    The first $99$ toric divisors on a threefold embed a projective
	    bundle into projective space.
	Example
	    D5 = smallAmpleToricDivisor(3,75);
	    assert isVeryAmple D5
	    X5 = variety D5;
	    assert (isSmooth X5 and isProjective X5)
	    assert (# rays X5 === 8)
	    D5
	    latticePoints D5	
	Text
	    The last $4$ toric divisors on a threefold embed a blow-up of a
	    projective bundle at few points into projective space.
	Example
	    D6 = smallAmpleToricDivisor (3,102);
	    assert(isVeryAmple D6)
	    X6 = variety D6;
	    assert (isSmooth X6 and isProjective X6)
	    assert (# rays X6 === 7)
	    D6
	    latticePoints D6	    
	Text
	    @SUBSECTION "Acknowledgements"@
    	Text
            We thank @HREF("http://www.maths.ed.ac.uk/~mhering/", "Milena
            Hering")@ for her help creating the database.
    SeeAlso
        "Working with divisors and their associated groups"
	(toricDivisor, List, NormalToricVariety)
    	(variety, ToricDivisor)
///	

doc ///
    Key 
        (symbol +, ToricDivisor, ToricDivisor)
    	(symbol -, ToricDivisor, ToricDivisor)
    	(symbol -, ToricDivisor)
    	(symbol *, ZZ, ToricDivisor)
    Headline 
        perform arithmetic on toric divisors
    Usage 
        D1 + D2
    Inputs 
        D : ToricDivisor
        E : ToricDivisor
    Outputs 
        : ToricDivisor
    Description
        Text	    
            The set of torus-invariant Weil divisors forms an abelian group
            under addition.  The basic operations arising from this structure,
            including addition, substraction, negation, and scalar
            multplication by integers, are available.
	Text
	    We illustrate a few of the possibilities on one variety.
      	Example
    	    X = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
    	    # rays X
    	    D = toricDivisor({2,-7,3,0,7,5,8,-8}, X)
    	    K = toricDivisor X
    	    D + K
	    assert(D + K == K + D)
    	    D - K
    	    assert(D - K == -(K-D))
    	    - K
    	    assert(-K == (-1)*K)
    	    7*D
    	    assert(7*D == (3+4)*D)
	    assert(7*D == 3*D + 4*D)
    	    -3*D + 7*K    
    	    assert(-3*D+7*K == (-2*D+8*K) + (-D-K))
    SeeAlso
        "Working with divisors and their associated groups"
	(symbol ==, ToricDivisor, ToricDivisor)
///

doc ///
    Key
        (symbol ==, ToricDivisor, ToricDivisor)
    Headline
        equality of toric divisors
    Usage
        D1 == D2
    Inputs
        D1 : ToricDivisor
	D2 : ToricDivisor
    Outputs
        : Boolean
	    that is @TO true@ if the underlying varieties are equal and lists
	    of coefficents are equal
    Description
        Text
	    Two torus-invariant Weil divisors are equal when their underlying
	    normal toric varieties are equal and, for each irreducible
	    torus-invariant divisor, the corresponding coefficients are equal.
	Example
	    X = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
	    D1 = toricDivisor({2,-7,3,0,7,5,8,-8}, X)
	    D2 = 2 * X_0 - 7 * X_1 + 3 * X_2 + 7 * X_4 + 5 * X_5 + 8 * X_6 - 8 * X_7
	    D1 == D2
	    assert(D1 == D2 and D2 == D1)
	    D1 == - D2
	    assert(D1 =!= - D2)
    SeeAlso
        "Working with divisors and their associated groups"
	(symbol +, ToricDivisor, ToricDivisor)    
///
	
doc ///
    Key
        (monomials, ToricDivisor)
    Headline
       list the monomials that span the linear series
    Usage
        monomials D
    Inputs
        D : ToricDivisor
    Outputs
        : List
	    of torus-invariant @TO2 (RingElement, "RingElements")@ that appear
	    in the complete linear system of the given divisor
    Description
        Text
	    By identifying the coefficients of an effective irreducible
	    torus-invariant divisors with exponents of the generators of the
	    @TO2( (ring, NormalToricVariety), "total coordinate ring")@, each
	    toric divisor on a @TO NormalToricVariety@ corresponds to a
	    monomial.  This method function returns all of the monomials
	    corresponding to linear equivalent toric divisors.
	Text
	    This method function assumes that the underlying toric variety is
	    projective.	    
	Text
	    Projective space is especially simple.
	Example
	    PP2 = toricProjectiveSpace 2;
	    D1 = 5*PP2_0
	    M1 = elapsedTime monomials D1
	    elapsedTime assert (set M1 === set first entries basis(degree D1, ring variety D1))
	Text
	    Toric varieties of Picard-rank 2 are slightly more interesting.
	Example
	    FF2 = hirzebruchSurface 2;
	    D2 = 2*FF2_0 + 3 * FF2_1
	    M2 = elapsedTime monomials D2
	    elapsedTime assert (set M2 === set first entries basis (degree D2, ring variety D2))	
	    X = kleinschmidt (5, {1,2,3});     
	    D3 = 3*X_0 + 5*X_1
	    m3 = elapsedTime # monomials D3
	    elapsedTime assert (m3 === #first entries basis (degree D3, ring variety D3))
	Text
	    By exploiting @TO "Polyhedra::latticePoints"@, this method function
	    avoids using the @TO basis@ function.	    
    SeeAlso
        "Working with divisors and their associated groups"
	(ring, NormalToricVariety)    
	(vector, ToricDivisor)
///

doc ///
    Key 
        (symbol SPACE, OO, ToricDivisor)
    Headline 
        make the associated rank-one reflexive sheaf
    Usage 
        OO D
    Inputs 
        D : ToricDivisor
    Outputs 
        : CoherentSheaf 
	    the associated rank-one reflexive sheaf
    Description
        Text
            For a Weil divisor $D$ on a normal variety $X$ the associated
  	    sheaf ${\cal O}_X(D)$ is defined by 
	    $H^0(U, {\cal O}_X(D)) = \{ f \in {\mathbb C}(X)^* | (div(f)+D)|_U \geq 0 \} \cup \{0\}$.
            The sheaf associated to a Weil divisor is reflexive; it is equal
  	    to its bidual.  A divisor is Cartier if and only if the associated
  	    sheaf is a line bundle
	Text
            The first examples show that the associated sheaves are reflexive.
	Example
            PP3 = toricProjectiveSpace 3;
            K = toricDivisor PP3
            omega = OO K
            omegaVee = prune sheafHom (omega, OO_PP3)
            omega === prune sheafHom (omegaVee, OO_PP3)
        Example
            X = hirzebruchSurface 2;
            D = X_0 + X_1
            L = OO D
            LVee = prune sheafHom (L, OO_X)
            L === prune sheafHom (LVee, OO_X)
	Example
            rayList = {{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{-1,0,-1},{-2,-1,0}};
            coneList = {{0,1,2},{0,1,3},{1,3,4},{1,2,4},{2,4,5},{0,2,5},{0,3,5},{3,4,5}};
            Y = normalToricVariety(rayList,coneList);
    	    isSmooth Y
    	    isProjective Y
    	    E = Y_0 + Y_2 + Y_4
    	    isCartier E
    	    F = OO E
    	    FVee = prune sheafHom(F, OO_Y)
    	    F === prune sheafHom(FVee, OO_Y)
        Text
            Two Weil divisors $D$ and $E$ are linearly equivalent if 
	    $D = E + div(f)$, for some $f \in {\mathbb C}(X)^*$. Linearly
            equivalent divisors produce isomorphic sheaves.
	Example
       	    PP3 = toricProjectiveSpace 3;
    	    D1 = PP3_0
    	    E1 = PP3_1
    	    OO D1 === OO E1
    	    X = hirzebruchSurface 2;
    	    D2 = X_2 + X_3    
    	    E2 = 3*X_0 + X_1
    	    OO D2 === OO E2
    SeeAlso 
        "Working with divisors and their associated groups"
        "Total coordinate rings and coherent sheaves"
///

doc ///
    Key 
	(isEffective,ToricDivisor)
        isEffective 	
    Headline 
        whether a torus-invariant Weil divisor is effective
    Usage 
        isEffective D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Boolean 
	    that is @TO true@ if all the coefficients of the irreducible
            torus-invariant divisors are nonnegative
    Description
        Text    			  
            A torus-invariant Weil divisor is effective if all the
            coefficients of the torus-invariant irreducible divisors are
            nonnegative.
        Text
	    The canonical divisor is not effective, but the anticanonical
	    divisor is.
        Example	    
      	    PP3 = toricProjectiveSpace 3;
    	    K = toricDivisor PP3
    	    isEffective K
    	    isEffective (-K)
        Text
            The torus-invariant irreducible divisors generate the cone of
            effective divisors.
    SeeAlso
        "Working with divisors and their associated groups"
	(symbol _, NormalToricVariety, ZZ)
///  

doc ///
    Key 
	(isCartier, ToricDivisor)
        isCartier 	
    Headline 
        whether a torus-invariant Weil divisor is Cartier
    Usage 
        isCartier D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Boolean 
	    that is @TO true@ if the divisor is Cartier
    Description
        Text
            A torus-invariant Weil divisor $D$ on a normal toric variety $X$
            is Cartier if it is locally principal, meaning that $X$ has an
            open cover $\{U_i\}$ such that $D|_{U_i}$ is principal in $U_i$
            for every $i$.
        Text  
            On a smooth variety, every Weil divisor is Cartier.
	Example
      	    PP3 = toricProjectiveSpace 3;
    	    assert all (3, i -> isCartier PP3_i)
        Text
            On a simplicial toric variety, every torus-invariant Weil divisor
            is $\QQ$-Cartier, which means that every torus-invariant Weil
            divisor has a positive integer multiple that is Cartier.
        Example
            W = weightedProjectiveSpace {2,5,7};
    	    assert isSimplicial W
    	    assert not isCartier W_0    
    	    assert isQQCartier W_0
    	    assert isCartier (35*W_0)      
        Text
  	    In general, the Cartier divisors are only a subgroup of the Weil
            divisors.
        Example  
            X = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
    	    assert not isCartier X_0
    	    assert not isQQCartier X_0
    	    K = toricDivisor X;
    	    assert isCartier K
    SeeAlso
        "Working with divisors and their associated groups"
        (cartierDivisorGroup,NormalToricVariety)
        (isSimplicial,NormalToricVariety)
    	(isQQCartier,ToricDivisor)
///	

doc ///
    Key 
	(isQQCartier,ToricDivisor)
        isQQCartier 	
    Headline 
        whether a torus-invariant Weil divisor is QQ-Cartier
    Usage 
        isCartier D
    Inputs 
        D : ToricDivisor
    Outputs 
       : Boolean 
           that is @TO true@ if the divisor is $\QQ$-Cartier
    Description
        Text
            A Weil divisor is $\QQ$-Cartier if some positive integer multiple
            is Cartier.
        Text	    
            On a simplicial toric variety, every torus-invariant Weil divisor
            is $\QQ$-Cartier.
        Example  
            W = weightedProjectiveSpace {2,5,7};
    	    assert isSimplicial W
            assert not isCartier W_0    
            assert isQQCartier W_0
            assert isCartier (35*W_0)    
        Text	    	      
            In general, the $\QQ$-Cartier divisors form a proper subgroup of
            the Weil divisors.
        Example 	    
            X = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
    	    assert not isCartier X_0
            assert not isQQCartier X_0
            K = toricDivisor X;
    	    assert isCartier K
    SeeAlso
        "Working with divisors and their associated groups"
        (cartierDivisorGroup,NormalToricVariety)
        (isSimplicial,NormalToricVariety)
        (isCartier,ToricDivisor)
///	

doc ///
    Key 
        (isNef, ToricDivisor)
        isNef
    Headline 
        whether a torus-invariant Weil divisor is nef
    Usage 
        isNef D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Boolean 
	    that is @TO true@ if the divisor is nef
    Description
        Text	    
            A $\QQ$-Cartier divisor is nef (short for numerically
            effective or numerically eventually free) if the
            intersection product of the divisor with every complete
            irreducible curve is nonnegative.  The definition depends only on
            the numerical equivalence class of the divisor.  For a
            torus-invariant $\QQ$-Cartier divisor on a complete normal
            toric variety, the following are equivalent:
        Text    
            @UL { 
        	{"the divisor is nef;"},
		{"some positive integer multiply of the divisor is Cartier and
		  basepoint free;"},
		{"the real piecewise linear support function associated to 
		  the divisor is convex."}
	    }@
        Text
            A torus-invariant Cartier divisor is nef if and only if it is
            basepoint free; in other words, the associated line bundle is
            generated by its global sections.
        Text		    
            On a Hirzebruch surface, three of the four torus-invariant
    	    irreducible divisors are nef, and none are ample.
        Example
            X1 = hirzebruchSurface 2;
            assert (isNef X1_0 and not isAmple X1_0)
            assert not isNef X1_1
            assert (isNef X1_2 and not isAmple X1_2)
            assert (isNef X1_3 and not isAmple X1_3)
        Text
            Not every $\QQ$-Cartier nef divisor is basepoint free.
        Example	    
            X2 = weightedProjectiveSpace {2,3,5};
            D = X2_1 - X2_0
            assert (isNef D and HH^0(X2, OO D) == 0)
    	    assert all (dim X2, i -> HH^i(X2, OO D) == 0)
    	    assert not isCartier D    
            assert isCartier (30*D)
            HH^0 (X2, OO (30*D))
            assert all (dim X2 -1, i -> HH^(i+1)(X2, OO (30*D)) == 0)
        Text	
            There are smooth complete normal toric varieties with no
    	    nontrivial nef divisors.
        Example	     
            X3 = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{0,-1,2},{0,0,-1},{-1,1,-1},{-1,0,-1},{-1,-1,0}},{{0,1,2},{0,2,3},{0,3,4},{0,4,5},{0,1,5},{1,2,7},{2,3,7},{3,4,7},{4,5,6},{4,6,7},{5,6,7},{1,5,7}});    
    	    assert (isComplete X3 and not isProjective X3)
            assert not any (#rays X3, i -> isNef X3_i)
            assert isNef (0*X3_1)    
	    assert (nefGenerators X3 == 0)
        Text
            The most basic vanishing theorem for normal toric varieties states
            that the higher cohomology of coherent sheaf associated to a nef
            divisor is zero.
        Example	       
            X4 = kleinschmidt (9,{1,2,3});
            assert (isNef X4_0 and not isAmple X4_0)
	    assert all (dim X4 - 1, i -> HH^(i+1)(X4, OO X4_0) == 0)
    	    D = X4_0 + X4_4
	    assert (isNef D and isAmple D)
    	    assert all (dim X4 - 1, i -> HH^(i+1)(X4, OO D) == 0)
    SeeAlso
        "Working with divisors and their associated groups"
	(nefGenerators, NormalToricVariety)
        (isComplete, NormalToricVariety)
        (kleinschmidt, ZZ, List)
        (isQQCartier, ToricDivisor)
        (isAmple, ToricDivisor)
///	

doc ///
    Key 
	(isAmple, ToricDivisor)
        isAmple	
    Headline 
        whether a torus-invariant Weil divisor is ample
    Usage 
        isAmple D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Boolean 
	    that is @TO true@ if the divisor is ample
    Description
        Text		    
            A Cartier divisor is very ample when it is basepoint free and the
            map arising from its complete linear series is a closed embedding.
            A Cartier divisor is ample when some positive integer multiple is
            very ample.  For a torus-invariant Cartier divisor on a complete
            normal toric variety, the following conditions are equivalent:
        Text	  
	    @UL {
		{"the divisor is ample;"},
		{"the real piecewise linear support function associated to
    		  the divisor is strictly convex;"},
    		{"the lattice polytope corresponding to the divisor is
    		  full-dimensional and its normal fan equals the fan
    	          associated to the underlying toric variety;"},
                {"the intersection product of the divisor with every
      		  torus-invariant irreducible curve is positive."}
      	    }@
        Text      
            On projective space, every torus-invariant irreducible divisor is
            ample.
        Example	   
            PP3 = toricProjectiveSpace 3;
    	    assert all (# rays PP3, i -> isAmple PP3_i)
        Text  
            On a Hirzebruch surface, none of the torus-invariant irreducible
   	    divisors are ample.
        Example	  
            X1 = hirzebruchSurface 2;
    	    assert not any (# rays X1, i -> isAmple X1_i)
    	    D = X1_2 + X1_3
    	    assert isAmple D  
    	    assert isProjective X1  
        Text 	       
	    A normal toric variety is Fano if and only if its anticanonical
  	    divisors, namely minus the sum of its torus-invariant irreducible
  	    divisors, is ample.
        Example	    
    	    X2 = smoothFanoToricVariety (3,5);
    	    K = toricDivisor X2
    	    assert isAmple (- K)
    	    X3 = kleinschmidt (9,{1,2,3});
    	    K = toricDivisor X3
    	    assert isAmple (-K)  
    SeeAlso 
        "Working with divisors and their associated groups"
	(isVeryAmple, ToricDivisor)
    	(isProjective, NormalToricVariety)
    	(isNef, ToricDivisor)
    	(isFano, NormalToricVariety)
///	

doc ///
    Key 
        (isVeryAmple, ToricDivisor)
    Headline 
        whether a torus-invariant Weil divisor is very ample
    Usage 
        isVeryAmple D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Boolean 
	    that is @TO true@ if the divisor is very ample
    Description
        Text
            A Cartier divisor is very ample when it is basepoint free and the
            map arising from its complete linear series is a closed embedding.
            On a normal toric variety, the following are equivalent:
        Text
            @UL {
    		{"the divisor is a very ample divisor;"},
    		{"for every vertex of the associated lattice polytope
    		  associated to the divsor, the corresponding semigroup is
    		  saturated in the group characters."}
	    }@
        Text    
            On a smooth normal toric variety every ample divisor is very ample.
        Example		    
            PP3 = toricProjectiveSpace 3;
    	    assert isAmple PP3_0
    	    assert isVeryAmple PP3_0
    	    FF2 = hirzebruchSurface 2;
    	    assert isAmple (FF2_2 + FF2_3)
    	    assert isVeryAmple (FF2_2 + FF2_3)
        Text
            A Cartier divisor is ample when some positive integer multiple is
            very ample.  On a normal toric variety of dimension $d$, the
            $(d-1)$ multiple of any ample divisor is always very ample.
        Example	  
    	    X = normalToricVariety matrix {{0,1,0,0,1},{0,0,1,0,1},{0,0,0,1,1},{0,0,0,0,3}};
    	    assert (dim X === 4)
    	    D = 3*X_0
    	    assert isAmple D
    	    assert not isVeryAmple D
    	    assert not isVeryAmple (2*D)
    	    assert isVeryAmple (3*D)    
    SeeAlso
        "Working with divisors and their associated groups"
        (isProjective,NormalToricVariety)
        (isAmple,ToricDivisor)
///	

doc ///
    Key 
        (vertices, ToricDivisor)
    Headline 
        compute the vertices of the associated polytope
    Usage 
        vertices D
    Inputs
        D : ToricDivisor
    Outputs 
        : Matrix 
	    whose columns are the vertices of the associated polytope
    Description
        Text	    
            On a complete normal toric variety, the polyhedron associated to a
            Cartier divisor is a lattice polytope.  Given a torus-invariant
            Cartier divisor on a normal toric variety, this method returns an
            integer matrix whose columns correspond to the vertices of the
            associated lattice polytope.  For a non-effective Cartier divisor,
            this methods returns @TO null@.  When the divisor is ample,
            the normal fan the corresponding polytope equals the fan
            associated to the normal toric variety.
        Text
            On the projective plane, the associate polytope is either empty,
	    a point, or a triangle.
        Example  
    	    PP2 = toricProjectiveSpace 2;
    	    assert (null === vertices (-PP2_0))
    	    vertices (0*PP2_0)
    	    assert isAmple PP2_0
    	    V1 = vertices (PP2_0)
    	    X1 = normalToricVariety V1;
    	    assert (set rays X1 === set rays PP2 and max X1 === max PP2)
    	    assert isAmple (2*PP2_0)
    	    V2 = vertices (2*PP2_0)
    	    X2 = normalToricVariety V2;
    	    assert (rays X2 === rays X1 and max X2 === max X1)
        Text
            On a Hirzebruch surface, the polytopes associated to non-ample
  	    Cartier divisors give rise to other normal toric varieties.
        Example   
    	    FF2 = hirzebruchSurface 2;
    	    assert not isAmple FF2_2
    	    V3 = vertices FF2_2
    	    normalToricVariety V3  -- a degenerated version of the projective line
    	    assert isDegenerate normalToricVariety V3  
    	    assert not isAmple FF2_3
    	    V4 = vertices FF2_3
    	    normalToricVariety V4 -- a weighted projective space
    	    vertices FF2_1
    	    assert isAmple (FF2_2 + FF2_3)
    	    V5 = vertices (FF2_2 + FF2_3)
    	    X3 = normalToricVariety V5 -- isomorphic Hirzebruch surface
	    assert (set rays X3 === set rays FF2)
    SeeAlso
        "Working with divisors and their associated groups"
    	(isComplete, NormalToricVariety)
    	(isCartier, ToricDivisor)
    	(isEffective, ToricDivisor)
    	(normalToricVariety, Matrix)
    	(latticePoints, ToricDivisor)
///	

doc ///
    Key 
        (latticePoints, ToricDivisor)
    Headline 
        compute the lattice points in the associated polytope
    Usage 
        latticePoints D
    Inputs 
        D : ToricDivisor
    Outputs 
        : Matrix 
	    whose columns are the lattice points in the associated polytope
    Description
        Text	  
            On a complete normal toric variety, the polyhedron associated to a
  	    Cartier divisor is a lattice polytope.  Given a torus-invariant
  	    Cartier divisor on a normal toric variety, this method returns an
  	    integer matrix whose columns correspond to the lattices points
  	    contained in the associated polytope.  For a non-effective Cartier
  	    divisor, this method returns @TO null@.
        Text  
            On the projective plane, the associate polytope is either empty, a
   	    point, or a triangle.
        Example
            PP2 = toricProjectiveSpace 2;
    	    assert (null === vertices (-PP2_0))
    	    latticePoints (0*PP2_0)
    	    assert isAmple PP2_0
    	    V1 = latticePoints (PP2_0)
    	    X1 = normalToricVariety V1;
    	    assert (set rays X1 === set rays PP2 and  max X1 === max PP2)
    	    assert isAmple (2*PP2_0)
    	    V2 = latticePoints (2*PP2_0)
    	    X2 = normalToricVariety(V2, MinimalGenerators => true);
    	    assert (rays X2 === rays X1 and max X2 === max X1)
        Text
            In this singular example, we see that all the lattice points in
  	    the polytope arising from a divisor $2D$ do not come from the
  	    lattice points in the polytope arising from $D$.
        Example  
            Y = normalToricVariety matrix {{0,1,0,0,1},{0,0,1,0,1},{0,0,0,1,1},{0,0,0,0,3}};
    	    D = 3*Y_0;
    	    latticePoints D
    	    latticePoints (2*D)
    SeeAlso
        "Working with divisors and their associated groups"
    	(normalToricVariety, Matrix)
    	(vertices, ToricDivisor)
///

doc ///	
    Key
        (polytope, ToricDivisor)
    Headline 
        makes the associated 'Polyhedra' polyhedron
    Usage 
        polytope D
    Inputs
        D : ToricDivisor
    Outputs 
        : Polyhedron
    Description
        Text
            For a torus-invariant Weil divisors $D = \sum_i a_i D_i$ the
            associated polyhedron is 
	    $\{ m \in M : (m, v_i) \geq -a_i \forall i \}$.	    
            Given a torus-invariant Weil divisor, this methods makes the
  	    associated polyhedra as an object in @TO Polyhedra@.
        Example  
    	    PP2 = toricProjectiveSpace 2;
    	    P0 = polytope (-PP2_0)
	    assert (dim P0 === -1)
    	    P1 = polytope (0*PP2_0)
	    assert (dim P1 == 0)
	    assert (vertices P1 == 0)
    	    P2 = polytope (PP2_0)
    	    vertices P2
	    halfspaces P2
        Text	    
            This method works with $\QQ$-Cartier divisors.
        Example  	    
    	    Y = normalToricVariety matrix {{0,1,0,0,1},{0,0,1,0,1},{0,0,0,1,1},{0,0,0,0,3}};
    	    assert not isCartier Y_0
    	    assert isQQCartier Y_0
    	    P3 = polytope Y_0;
	    vertices P3
    	    vertices polytope Y_0
	    halfspaces P3
        Text	    
  	    It also works divisors on non-complete toric varieties.
       Example	    
           Z = normalToricVariety ({{1,0},{1,1},{0,1}}, {{0,1},{1,2}});
    	   assert not isComplete Z
    	   D = - toricDivisor Z
    	   P4 = polytope D;
	   rays P4
    	   vertices P4
	   halfspaces P4
    SeeAlso
        "Working with divisors and their associated groups"
    	(normalToricVariety, Matrix)
    	(vertices, ToricDivisor)
///	

--------------------------------------------------------------------------------
-- total coordinate rings and coherent sheaves
------------------------------------------------------------------------------

doc ///
    Key 
        "Total coordinate rings and coherent sheaves"
    Description
        Text	
            @HREF("http://www3.amherst.edu/~dacox/", "David A. Cox")@
  	    introduced the total coordinate ring $S$ of a normal toric variety
  	    $X$ and the irrelevant ideal $B$.  The polynomial ring $S$ has one
  	    variable for each ray in the associated fan and a natural grading
  	    by the class group.  The monomial ideal $B$ encodes the maximal
  	    cones.  The following results of Cox indicate the significance of
  	    the pair $(S,B)$.
        Text	  
  	    @UL {
    		{"The variety ", EM "X", " is a good categorial ",
    		 "quotient of Spec(", EM "S", ") -  V(", EM "B",
		 ") by a suitable group action."},
                {"The category of coherent sheaves on ", EM "X", " is
    		   equivalent to the quotient of the category of finitely
    		   generated graded ", EM "S", "-modules by the full
    		   subcategory of ", EM "B", "-torsion modules."}
	    }@
        Text 
            In particular, we may represent any coherent sheaf on $X$ by
  	    giving a finitely generated graded $S$-module.
        Text   
            The following methods allow one to make and manipulate coherent
  	    sheaves on normal toric varieties.
        Text     
            @SUBSECTION "Menu"@  
        Text     
  	    @UL {
    		TO (ring, NormalToricVariety),
    		TO (ideal, NormalToricVariety),
    		TO (sheaf, NormalToricVariety, Ring),	  	  
    		TO (sheaf ,NormalToricVariety, Module),
    		TO (symbol SPACE, OO, ToricDivisor),
    		TO (cotangentSheaf, NormalToricVariety),	  
    		TO (cohomology, ZZ, NormalToricVariety, CoherentSheaf),
		TO (intersectionRing, NormalToricVariety),
		TO (chern, CoherentSheaf),
		TO (ctop, CoherentSheaf),		
		TO (ch, CoherentSheaf),		
		TO (chi, CoherentSheaf),			       		
		TO (todd, CoherentSheaf),			       				
		TO (hilbertPolynomial, NormalToricVariety, CoherentSheaf)
	    }@
    SeeAlso
        "Making normal toric varieties"
        "Basic invariants and properties of normal toric varieties"
        "Total coordinate rings and coherent sheaves"
    	"Resolution of singularities"
///	

doc ///
    Key 
        (ring, NormalToricVariety) 
	"Cox ring"
    Headline
        make the total coordinate ring (a.k.a. Cox ring)
    Usage 
        ring X
    Inputs 
        X : NormalToricVariety
    Outputs
        : PolynomialRing
	    the total coordinate ring
    Description
        Text
            The total coordinate ring, which is also known as the Cox ring, of
  	    a normal toric variety is a polynomial ring in which the variables
  	    correspond to the rays in the fan.  The map from the group of
  	    torus-invarient Weil divisors to the class group endows this ring
  	    with a grading by the @TO2(classGroup,"class group")@.	    
        Text	    	
            The total coordinate ring for projective space is the standard
  	    graded polynomial ring.
        Example	      
            PP3 = toricProjectiveSpace 3;
    	    S = ring PP3;
    	    assert (isPolynomialRing S and isCommutative S)
    	    gens S
    	    degrees S
    	    assert (numgens S == #rays PP3)
    	    coefficientRing S
        Text	    
            For a product of projective spaces, the total coordinate ring has
  	    a bigrading.
        Example	    
     	    X = toricProjectiveSpace(2) ** toricProjectiveSpace(3);
    	    gens ring X
    	    degrees ring X
        Text
  	    A Hirzebruch surface also has a $\ZZ^2$-grading.
        Example		    
    	    FF3 = hirzebruchSurface 3;
    	    gens ring FF3
    	    degrees ring FF3
    Caveat     
        The total coordinate ring is not yet implemented when the toric
	variety is degenerate or the class group has torsion.
    SeeAlso
        "Total coordinate rings and coherent sheaves"
    	(rays, NormalToricVariety)
    	classGroup
    	WeilToClass
    	(fromWDivToCl, NormalToricVariety)
    	(ideal, NormalToricVariety)
    	(sheaf, NormalToricVariety, Module)
///	

doc ///
    Key
        (normalToricVariety, Ring)
    Headline 
        get the associated normal toric variety
    Usage
        normalToricVariety S
    Inputs
        S : Ring
	CoefficientRing => Ring
            not used
        MinimalGenerators => Boolean 
            not used
        Variable => Symbol
	    not used	    
        WeilToClass => Matrix 
            not used   
    Outputs 
        : NormalToricVariety
    Description
        Text
            If a polynomial ring is made as the total coordinate ring of
  	    normal toric variety, then this method returns the associated
  	    variety.
        Example	  
    	    PP3 = toricProjectiveSpace 3;
    	    S = ring PP3
    	    gens S
    	    degrees S
    	    normalToricVariety S
	    assert (PP3 === normalToricVariety S)	    
    	    variety S
	    assert (PP3 === variety S)
        Text
            If the polynomial ring is not constructed from a variety, then
  	    this method produces an error: "no variety associated with ring".
        Example  
     	   S = QQ[x_0..x_2];
    	   gens S
    	   degrees S
    	   assert (try (normalToricVariety S; false) else true)
    	   assert (try (variety S; false) else true)
    SeeAlso
        "Total coordinate rings and coherent sheaves"
       	(ring, NormalToricVariety)
///	

doc ///
    Key 
        (ideal, NormalToricVariety)
    	(monomialIdeal, NormalToricVariety)
    Headline
        make the irrelevant ideal
    Usage 
        ideal X
    Inputs 
        X : NormalToricVariety
    Outputs 
        : Ideal 
	    in the total coordinate ring of $X$
    Description
        Text	    
            The irrelevant ideal is a reduced monomial ideal in the total
  	    coordinate ring that encodes the combinatorics of the fan.  For
  	    each maximal cone in the fan, it has a minimal generator, namely
  	    the product of the variables not indexed by elements of the list
  	    corresponding to the maximal cone.
        Text
            For projective space, the irrelevant ideal is generated by the
            variables.
        Example 	    
    	    PP4 = toricProjectiveSpace 4;
    	    B = ideal PP4
    	    assert (isMonomialIdeal B and B == radical B)
	    monomialIdeal PP4
	    assert (B == monomialIdeal PP4)
        Text
  	    For an affine toric variety, the irrelevant ideal is the unit
  	    ideal.
        Example 	    
    	    C = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}}, {{0,1,2,3}});
    	    ideal C
	    assert (monomialIdeal C == 1)
	    monomialIdeal affineSpace 3
	    assert (ideal affineSpace 3 == 1)
        Text	    
            The irrelevant ideal for a product of toric varieties is
  	    intersection of the irrelevant ideal of the factors.
        Example 	    
    	    X = toricProjectiveSpace (2) ** toricProjectiveSpace (3);
    	    S = ring X;
    	    B = ideal X
    	    primaryDecomposition B
    	    dual monomialIdeal B
        Text
            For a complete simplicial toric variety, the irrelevant ideal is
  	    the Alexander dual of the Stanley-Reisner ideal of the fan.
        Example   
    	    Y = smoothFanoToricVariety (2,3);
    	    dual monomialIdeal Y
    	    sort apply (max Y, s -> select (# rays Y, i -> not member (i,s)))	    
	    primaryDecomposition dual monomialIdeal Y
        Text
            Since the irrelevent ideal is a monomial ideal, the command @TO
            monomialIdeal@ also produces the irrelevant ideal.
        Example  
    	    code (monomialIdeal, NormalToricVariety)
    SeeAlso
        "Total coordinate rings and coherent sheaves"
    	(max, NormalToricVariety)
    	(ring, NormalToricVariety)
///	

doc ///
    Key
        (sheaf, NormalToricVariety, Module)
    Headline 
        make a coherent sheaf
    Usage
        sheaf (X, M)
    Inputs 
        X : NormalToricVariety
        M : Module
	    a graded module over the total coordinate ring
    Outputs 
        : CoherentSheaf
	    the coherent sheaf on {\tt X} corresponding to {\tt M}
    Description
        Text	   	    
            The category of coherent sheaves on a normal toric variety is
  	    equivalent to the quotient category of finitely generated modules
  	    over the total coordinate ring by the full subcategory of torsion
  	    modules with respect to the irrelevant ideal.  In particular, each
  	    finitely generated module over the total coordinate ring
  	    corresponds to coherent sheaf on the normal toric variety and
  	    every coherent sheaf arises in this manner.
        Text  
            Free modules correspond to reflexive sheaves.
        Example  	    
     	    PP3 = toricProjectiveSpace 3;
            F = sheaf (PP3, (ring PP3)^{{1},{2},{3}})
    	    FF7 = hirzebruchSurface 7;
    	    G = sheaf (FF7, (ring FF7)^{{1,0},{0,1}})
    SeeAlso
        "Total coordinate rings and coherent sheaves"
    	(ring, NormalToricVariety)
    	(ideal, NormalToricVariety)
    	(sheaf, NormalToricVariety)
///	

doc ///
    Key 
        (sheaf, NormalToricVariety, Ring)
     	(symbol _, OO, NormalToricVariety)
    	(sheaf, NormalToricVariety)
    Headline
        make a coherent sheaf of rings
    Usage
        sheaf (X, S)
    Inputs 
        X : NormalToricVariety
        S : Ring 
	    the total coordinate ring of {\tt X}
    Outputs
        : SheafOfRings
	    the structure sheaf on {\tt X}
    Description
        Text
            The category of coherent sheaves on a normal toric variety is
  	    equivalent to the quotient category of finitely generated modules
  	    over the total coordinate ring by the full subcategory of torsion
  	    modules with respect to the irrelevant ideal.  In particular, the
  	    total coordinate ring corresponds to the structure sheaf.
        Text  
            On projective space, we can make the structure sheaf in a few
            ways.
        Example  
            PP3 = toricProjectiveSpace 3;
    	    F = sheaf (PP3, ring PP3)
    	    G = sheaf PP3
    	    assert (F === G)
    	    H = OO_PP3
    	    assert (F === H)
    SeeAlso
        "Total coordinate rings and coherent sheaves"
    	(ring, NormalToricVariety)
    	(sheaf, NormalToricVariety, Module)
///

doc ///	
    Key 
        (cotangentSheaf, NormalToricVariety)
	(cotangentSheaf, ZZ, NormalToricVariety)
    Headline
        make the sheaf of Zariski 1-forms
    Usage 
        cotangentSheaf X
    Inputs
        X : NormalToricVariety
        Minimize => Boolean 	
	    that specifies whether to apply @TO minimalPresentation@ to the
	    result before returning it
    Outputs
        : CoherentSheaf 
	    the sheaf of Zariski 1-forms on {\tt X}
    Description
        Text	    
            For a normal variety, the sheaf of Zariski 1-forms is defined to
  	    be the double dual of the cotangent bundle or equivalently the
  	    extension of the sheaf of 1-forms on the smooth locus to the
  	    entire variety (the complement of the smooth locus has codimension
  	    at least two because the variety is normal).  By construction,
  	    this sheaf is reflexive with rank equal to the dimension of the
  	    variety.  When the underlying variety is smooth, this is simple
  	    the sheaf of 1-forms or the cotangent bundle.
        Text	    
            On a non-degenerate normal toric variety, the sheaf of Zariski
  	    1-forms is associated to the kernel of a map from the character
	    lattice tensor the total coordinate ring to the direct sum over
	    the rays of the quotient of the total coordinate ring by the
	    ideal generated by the corresponding variable.
        Example  
    	    PP3 = toricProjectiveSpace 3;
    	    OmegaPP3 = cotangentSheaf PP3
	    assert (prune cotangentSheaf PP3 === cotangentSheaf (PP3, Minimize => true))	    
    	    L = prune exteriorPower (3, OmegaPP3)
    	    assert (L === OO toricDivisor PP3)
	    assert (L === prune cotangentSheaf (dim PP3, PP3))
        Example	    
    	    X = hirzebruchSurface 2;
    	    OmegaX = cotangentSheaf X
    	    L = prune exteriorPower(dim X, OmegaX)
    	    assert (L === OO toricDivisor X)
	    assert (L === prune cotangentSheaf(dim X, X))
        Example
    	    Y = normalToricVariety ({{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{-1,0,-1},{-2,-1,0}}, {{0,1,2},{0,1,3},{1,3,4},{1,2,4},{2,4,5},{0,2,5},{0,3,5},{3,4,5}});
    	    assert (not isSmooth Y and not isProjective Y)
    	    OmegaY = cotangentSheaf Y
    	    prune exteriorPower(dim Y, OmegaY)
    	    assert (prune exteriorPower(dim Y, OmegaY) === OO toricDivisor Y)
    SeeAlso
        "Total coordinate rings and coherent sheaves"
    	(sheaf, NormalToricVariety, Module) 
///	   

doc ///
    Key 
        (cohomology, ZZ, NormalToricVariety, CoherentSheaf)
     	(cohomology, ZZ, NormalToricVariety, SheafOfRings)
    Headline 
        compute the cohomology of a coherent sheaf
    Usage
        HH^i (X, F)
    Inputs
        i : ZZ
        X : NormalToricVariety
        F : CoherentSheaf
	    on {\tt X}
    Outputs 
        : Module
            the {\tt i}-th cohomology group of {\tt F}
    Description
        Text	 	    
            The cohomology functor $HH^i (X,-)$ from the category of sheaves
  	    of abelian groups to the category of abelian groups is the right
  	    derived functor of the global sections functor.
        Text
            As a simple example, we compute the dimensions of the cohomology
  	    groups for some line bundles on the projective plane.
        Example  
    	    PP2 = toricProjectiveSpace 2;
    	    HH^0 (PP2, OO_PP2(1))
	    matrix table (reverse toList (0..2), toList (-10..5),  (i,j) -> rank HH^i (PP2, OO_PP2(j-i)))
        Text
            For a second example, we compute the dimensions of the cohomology
  	    groups for some line bundles on a Hirzebruch surface.
        Example  
  	    FF2 = hirzebruchSurface 2;
	    HH^0 (FF2, OO_FF2(1,1))
	    matrix table (reverse toList (-7..7), toList (-7..7),  (i,j) -> rank HH^0 (FF2, OO_FF2(j,i)))
	    matrix table (reverse toList (-7..7), toList (-7..7),  (i,j) -> rank HH^1 (FF2, OO_FF2(j,i)))	    
	    matrix table (reverse toList (-7..7), toList (-7..7),  (i,j) -> rank HH^2 (FF2, OO_FF2(j,i)))	    
        Text
            When {\it F} is free, the algorithm based on [Diane Maclagan and
  	    Gregory G. Smith, 
	    @HREF("http://arxiv.org/abs/math.AC/0305214", "Multigraded Castelnuovo-Mumford regularity")@, 
	    {\it J. Reine Angew. Math.} {\bf 571} (2004), 179-212].  The
  	    general case uses the methods described in [David Eisenbud, Mircea
  	    Mustata, and Mike Stillman, 
  	    @HREF("http://arxiv.org/abs/math.AG/0001159", "Cohomology on toric varieties and local cohomology with monomial supports")@,
	    {\it J. Symbolic Comput.} {\bf 29} (2000), 583-600].
    SeeAlso
        "Total coordinate rings and coherent sheaves"
    	(sheaf, NormalToricVariety, Module)
    	(sheaf, NormalToricVariety, Ring)
///

doc ///
    Key
        (abstractVariety, NormalToricVariety, AbstractVariety)
        (abstractVariety, NormalToricVariety)
    Headline
        make the corresponding abstract variety
    Usage 
        abstractVariety (X, B)
    Inputs 
        X : NormalToricVariety
	    that is complete and simplicial
	B : AbstractVariety
	    the base variety, when omitted the 
	    @TO2( "Schubert2::point", "default base variety of dimension 0")@ is used
	ReturnType => 
	    this option is ignored by this method
	DefaultIntegral => 
	    this option is ignored by this method
    Outputs 
        : AbstractVariety 
	    determined by the @TO2( (dim, NormalToricVariety), "dimension")@ of
	    {\tt X} and its @TO2( (intersectionRing, NormalToricVariety),
	    "rational Chow ring")@
    Description
        Text  	    
	    This method converts a @TO NormalToricVariety@ into an 
	    @TO "Schubert2::AbstractVariety"@, as defined in the @TO Schubert2@
	    package.
	Text
	    Since many routines from the @TO Schubert2@ package have been
	    overloaded so that they apply directly to normal toric varieties,
	    this method is primarily of interest to developers.
	Text
	    Projective space can be constructed as an AbstractVariety in a few
	    equivalent, but not identical, ways.
	Example
	    tPP2 = toricProjectiveSpace 2;
	    aPP2 = abstractVariety tPP2 
	    assert (dim tPP2 === dim aPP2)
	    intersectionRing aPP2 
	    intersectionRing tPP2
	    intersectionRing abstractVariety (tPP2, base())
	    intersectionRing abstractVariety (tPP2, base(a))
	    PP2 = toricProjectiveSpace 2
	    intersectionRing PP2
	    minimalPresentation intersectionRing PP2
	    minimalPresentation intersectionRing tPP2
    SeeAlso
        "Total coordinate rings and coherent sheaves"
	(intersectionRing, NormalToricVariety)
	(abstractSheaf, NormalToricVariety, CoherentSheaf) 
	base        	
///

doc ///
    Key
        (abstractSheaf, NormalToricVariety, AbstractVariety, CoherentSheaf)
        (abstractSheaf, NormalToricVariety, CoherentSheaf)    
    Headline
        make the corresponding abstract sheaf
    Usage 
        abstractSheaf (X, B, F)
    Inputs 
        X : NormalToricVariety
	    that is complete and simplicial
	B : AbstractVariety
	    the base variety, when omitted the 
	    @TO2( "Schubert2::point", "default base variety of dimension 0")@ is used
	F : CoherentSheaf
	    on {\tt X}
	Rank => 
	    this option is ignored by this method
	ChernCharacter => 
	    this option is ignored by this method
	ChernClass => 
	    this option is ignored by this method	
	Name => 
	    this option is ignored by this method		
    Outputs 
        : AbstractSheaf 
    Description
        Text  
	    This method converts a @TO CoherentSheaf@ over a 
	    @TO NormalToricVariety@ into an @TO "Schubert2::AbstractSheaf"@
	    over the corresponding @TO "Schubert2::AbstractVariety"@, as
	    defined in the @TO Schubert2@ package.
	Text
	    Since many routines from the @TO Schubert2@ package have been
	    overloaded so that they apply directly to coherent sheaves on
	    normal toric varieties, this method is primarily of interest to
	    developers. 
	Text
	    We illustrate this methods with some classic coherent sheaves on
	    projective 3-space.
	Example
	    PP3 = toricProjectiveSpace 3;
	    F = OO_PP3 (2) ++ OO_PP3 (5);
	    aF = abstractSheaf (PP3, F);
	    chern aF
	    assert (chern aF === chern (OO_PP3 (2)) * chern (OO_PP3 (5)))
	    assert (chern aF == chern F)
            Omega = cotangentSheaf PP3;
	    aOmega = abstractSheaf (PP3, Omega);
	    chern aOmega
	    assert (aOmega === cotangentBundle abstractVariety PP3)
	Text
	    We also provide a similar collection of examples on a larger
	    variety.
	Example
	    X = smoothFanoToricVariety (5, 100);
	    rank picardGroup X
	    nefGenerators X
	    G = OO_X (1,1,-1,1,1,2) ++ OO_X (1,1,-1,0,0,1);
	    aG = abstractSheaf (X, G);
	    A = intersectionRing variety aG;
	    chern aG
	    assert (chern aG === chern (OO_X (1,1,-1,1,1,2)) * chern (OO_X (1,1,-1,0,0,1)))
	    assert (chern aG == chern G)
            Omega = cotangentSheaf X;
	    aOmega = abstractSheaf (X, Omega);
	    chern aOmega
	    assert (aOmega === cotangentBundle abstractVariety X)		    
    SeeAlso
        "Total coordinate rings and coherent sheaves"
	(abstractSheaf, NormalToricVariety, ToricDivisor)    	
	(abstractVariety, NormalToricVariety)        	   	    
///
    
doc ///
    Key    
        (abstractSheaf, NormalToricVariety, AbstractVariety, ToricDivisor)
    	(abstractSheaf, NormalToricVariety, ToricDivisor)
    Headline
        make the corresponding abstract sheaf
    Usage 
        abstractVariety (X, B, D)
    Inputs 
        X : NormalToricVariety
	    that is complete and simplicial
	B : AbstractVariety
	    the base variety, when omitted the 
	    @TO2( "Schubert2::point", "default base variety of dimension 0")@ is used 
	D : ToricDivisor
	    on {\tt X}
	Rank => 
	    this option is ignored by this method
	ChernCharacter => 
	    this option is ignored by this method
	ChernClass => 
	    this option is ignored by this method	
	Name => 
	    this option is ignored by this method		
    Outputs 
        : AbstractSheaf 
    Description
        Text  
	    This method converts a @TO ToricDivisor@ on a 
	    @TO NormalToricVariety@ into an @TO "Schubert2::AbstractSheaf"@
	    over the corresponding @TO "Schubert2::AbstractVariety"@, as
	    defined in the @TO Schubert2@ package.	
	Text
	    Since many routines from the @TO Schubert2@ package have been
	    overloaded so that they apply directly to toric divisors, this
	    method is primarily of interest to developers.
	Example
	    PP2 = toricProjectiveSpace 3;
	    D1 = abstractSheaf (PP2, PP2_0);
	    assert (rank D1 === 1 and variety D1 === abstractVariety PP2)
    	    chern D1
	    ch D1
	    D2 = abstractSheaf (PP2, PP2_1);
	    assert (D2 === D1)
	Example       
	    FF2 = hirzebruchSurface 2
	    D3 = abstractSheaf (FF2, 2*FF2_2 + FF2_3)
	    assert (rank D3 === 1 and variety D3 === abstractVariety FF2)
	    chern D3
	    D4 = abstractSheaf (FF2, 4*FF2_0 + FF2_1)
	    assert (D4 === D3)
    SeeAlso
        "Total coordinate rings and coherent sheaves"
	(abstractSheaf, NormalToricVariety, CoherentSheaf)    	
	(abstractVariety, NormalToricVariety)    
///

doc ///
    Key
        (intersectionRing, NormalToricVariety, AbstractVariety)
        (intersectionRing, NormalToricVariety)
	"Chow ring"
    Headline
        make the rational Chow ring 
    Usage 
        intersectionRing (X, B)
    Inputs 
        X : NormalToricVariety
	    that is complete and simplicial
	B : AbstractVariety
	    the base variety, when omitted the 
	    @TO2( "Schubert2::point", "default base variety of dimension 0")@ is used 
    Outputs 
        : Ring 
	    the rational Chow ring of {\tt X} over {\tt B}
    Description
        Text    
            The rational Chow ring of a variety $X$ is an associative
            commutative ring graded by codimension.  The $k$-th graded
            component of this ring is the rational vector space spanned by the
            rational equivalence classes of subvarieties in $X$ having
            codimension $k$.  For generically transverse subvarieties $Y$ and
            $Z$ in $X$, the product satisfies $[Y][Z] = [Y \cap Z]$.
        Text 
	    For a complete simplicial normal toric variety, the rational Chow
	    ring has an explicit presentation.  Specifically, it is the
	    quotient of the polynomial ring with variables indexed by the set
	    of rays in the underlying fan by the sum of two ideals.  The first
	    ideal is the Stanley-Reisner ideal of the fan (equivalently
	    the Alexander dual of the 
	    @TO2( (ideal, NormalToricVariety), "irrelevant ideal")@) 
	    and the second ideal is generated by linear forms that encode
	    the @TO(rays, NormalToricVariety)@ in the fan.  In this context,
	    the rational Chow ring is isomorphic to the rational cohomology of
	    $X$.
	Text
	    The rational Chow ring of projective space is generated by the
	    rational equivalence class of a hyperplane.
	Example
	    PP3 = toricProjectiveSpace 3;
	    A0 = intersectionRing PP3
	    assert (# rays PP3 === numgens A0)	    
	    ideal A0
	    dual monomialIdeal PP3 + ideal ((vars ring PP3) * matrix rays PP3)
	    minimalPresentation A0
	    for i to dim PP3 list hilbertFunction (i, A0)
	Text
	    The rational Chow ring for the product of two projective spaces is
	    the tensor product of the rational Chow rings of the factors.
	Example
	    X = toricProjectiveSpace (2) ** toricProjectiveSpace (3);
	    A1 = intersectionRing X
	    assert (# rays X === numgens A1)	    
	    ideal A1
	    minimalPresentation A1
	    for i to dim X list hilbertFunction (i, A1)	    	  
	Text
            We end with a slightly larger example.
	Example
	    Y = time smoothFanoToricVariety(5,100);
	    A2 = intersectionRing Y;
	    assert (# rays Y === numgens A2)	    
            ideal A2
	    minimalPresentation A2
	    for i to dim Y list time hilbertFunction (i, A2)
    SeeAlso
        "Total coordinate rings and coherent sheaves"    
	(abstractVariety, NormalToricVariety)        	    
///	

doc ///
    Key    
        (chern, ZZ, CoherentSheaf)
        (chern, CoherentSheaf)
    Headline
        compute the Chern class of a coherent sheaf
    Usage 
        chern (i, F)
    Inputs 
        i : ZZ
	    that determines a component of the Chern class (optional)
        F : CoherentSheaf
	    on a complete simplicial normal toric variety
    Outputs 
        : RingElement
	    in the intersection ring of the underlying variety
    Description
        Text
	    The total Chern class of a coherent sheaf {\tt F} on a variety
	    {\tt X} is defined axiomatically by an element {\tt chern F} in
	    the @TO "Chow ring"@ of {\tt X} such that the following three
	    conditions hold:
	Text
	    @UL {
		{"For any morphism ", TT "f : X -> Y", ", we have ", 
		  TT "chern f^*(F) == f^*(chern F)", "."},
	        {"For each exact sequence ", TT "0 -> F' -> F -> F'' -> 0",
		 ", we have ", TT "chern F == (chern F') * (chern F'')", "."},
	        {"For any divisor ", TT "D", " on ", TT "X", ", we have ",
		  TT "chern OO_X (D) == 1 + [D]", " where ", TT "[D]", 
		 " denotes the divisor class in ", TT "picardGroup X",
		 " and we identify the Picard group of ", TT "X", " with
		   the graded component of the intersection ring having
		   codimenion 1."}
	    }@
	Text
	    The {\tt i}-th component of {\tt chern F} is {\tt chern (i, F)},
	    so we obtain {\tt chern F == chern (0, F) + chern (1, F) + ... +
	    chern (dim X, F)}.	    
        Text  
	    The total Chern class for the tangent bundle on projective space
	    is particularly simple; the coefficient of {\tt i}-th component is
	    a binomial coefficient.	    
	Example
	    d = 3;
	    PPd = toricProjectiveSpace d;
	    A0 = intersectionRing PPd;
	    TP = dual cotangentSheaf PPd
	    f0 = chern TP
    	    assert (A0 === ring f0)
	    assert (f0 === (1+A0_0)^(d+1))
	    assert all(d, i -> leadCoefficient chern (i, TP) == binomial(d+1,i))
	    assert (chern TP === sum (d+1, i -> chern (i, TP)))
        Text 
	    On a complete smooth normal toric variety, the total Chern class
	    of the cotangent sheaf is a product over the torus-invariant
	    divisors of the total Chern classes of the inverse of the
	    corresponding line bundles.
	Example       
	    X = smoothFanoToricVariety (4, 50);
	    A = intersectionRing X;
	    Omega = cotangentSheaf X
    	    f1 = chern Omega
	    assert (f1 ===  product (# rays X, i -> chern OO (-X_i)))
	    f3 = chern (2, Omega)
	    assert (f3 == sum (orbits (X, 2), s -> product (s, i -> A_i)))
    SeeAlso
        "Total coordinate rings and coherent sheaves"
	(intersectionRing, NormalToricVariety)    	
///

doc ///
    Key    
        (ctop, CoherentSheaf)
    Headline
        compute the top Chern class of a coherent sheaf
    Usage 
        ctop F
    Inputs 
        F : CoherentSheaf
	    on a complete simplicial normal toric variety
    Outputs 
        : RingElement
	    in the intersection ring of the underlying variety
    Description
        Text
	    For a coherent sheaf {\tt F} on a variety {\tt X}, the top Chern
	    class is {\tt chern (dim X, F)}.
        Text  
	    On projective space, the coefficient of the top Chern class of the
	    tangent bundle is the 1 plus dimension of the variety.
	Example
	    X0 = toricProjectiveSpace 5;
	    A0 = intersectionRing X0;
	    ctop dual cotangentSheaf X0
	    assert all (5, d -> (leadCoefficient ctop dual cotangentSheaf toricProjectiveSpace (d+1)) == d+2)
	    assert all (5, d -> (
		    F := dual cotangentSheaf toricProjectiveSpace (d+1);
		    chern (d+1, F) === ctop F
		    )
		)
        Text 
	    On a complete smooth normal toric variety, the top Chern class is
	    a sum of the classes corresponding to maximal cones in the
	    underlying fan.
	Example       
	    X1 = smoothFanoToricVariety (4, 50);
	    A1 = intersectionRing X1;
	    E1 = cotangentSheaf X1
	    f1 = ctop E1
	    assert (f1 === sum(max X1, s -> product(s, i -> -A1_i)))
	    assert (f1 === chern (dim X1, E1))
    SeeAlso
        "Total coordinate rings and coherent sheaves"
	(chern, CoherentSheaf)    		
	(intersectionRing, NormalToricVariety)    	
///

doc ///
    Key    
        (ch, ZZ, CoherentSheaf)    
        (ch, CoherentSheaf)
    Headline
        compute the Chern character of a coherent sheaf
    Usage 
        ch (i, F)
    Inputs 
        i : ZZ
	    that determines a component of the Chern character (optional)    
        F : CoherentSheaf
	    on a complete simplicial normal toric variety
    Outputs 
        : RingElement
	    in the intersection ring of the underlying variety
    Description
        Text
	    Given a locally-free sheaf $E$ of rank $r$ on a smooth
	    variety such that its Chern class formally factor as
	    {\tt chern} $E = \prod_{j=1}^r (1 + \alpha_j)$,
	    we define its Chern character to be 
	    {\tt ch} $E := \sum_{j=1}^r exp(\alpha_j)$.
	    The $i$-th graded piece of this power series is symmetric in the
	    $\alpha_j$ and, hence, expressible as a polynomial in the
	    elementary symmetric polynomials in the $\alpha_j$; we set 
	    {\tt ch} $(i, E)$ to be this polynomial.  
	    Because the Chern character is additive on exact sequences of
	    vector bundles and every coherent sheaf can be resolved by
	    locally-free sheaves, we can extend this definition to all
	    coherent sheaves.
	Text
	    The first few components of the Chern character are easily related
	    to other invariants.
	Example
	    X0 = kleinschmidt(4, {1,2,3});
	    E0 = cotangentSheaf X0
	    A0 = intersectionRing X0;
	    ch E0
	    assert (ch (0, E0) == rank E0 )
	    assert (ch (1, E0) === chern (1, E0))
	    assert (ch (2, E0) === (1/2)*((chern (1, E0))^2 - 2 * chern (2, E0)))	
        Text  
	    On a complete smooth normal toric variety, the Chern class of the
	    cotangent bundle factors as a product over the irreducible
	    torus-invariant divisors, so we can express the Chern character as
	    a sum.
	Example
	    X1 = smoothFanoToricVariety (4, 100);
	    E1 = dual cotangentSheaf X1
	    A1 = intersectionRing X1;
	    f1 = ch E1
	    n = # rays X1
	    assert (f1 === (sum(n, i -> ch OO (X1_i)) - (n - dim X1)))
    SeeAlso
        "Total coordinate rings and coherent sheaves"
	(chern, CoherentSheaf)    		
	(intersectionRing, NormalToricVariety)    	
///

doc ///
    Key    
        (chi, CoherentSheaf)
    Headline
        compute the Euler characteristic of a coherent sheaf
    Usage 
        chi F
    Inputs 
        F : CoherentSheaf
	    on a complete simplicial normal toric variety
    Outputs 
        : RingElement
	    in the coefficient ring of intersection ring of the underlying
	    variety	   
    Description
        Text
	    By definition, the Euler characteristic of coherent sheaf $F$ on a
	    variety $X$ is $\sum_i (-1)^i$ dim $HH^i (X, F)$.  However, this
	    methods uses the Hirzebruch-Riemann-Roch theorem to calculate the
	    Euler characteristic.
        Text 
	    For a nef line bundle on a normal toric variety, the Euler
	    characteristic equals the number of lattice points in the
	    corresponding polytope.	    
	Example
            X0 = hirzebruchSurface 2;
	    degrees ring X0
	    chi OO X0_2
	    latticePoints X0_2
	    assert all ({{1,0},{0,1},{1,1},{2,1},{1,2}}, p -> (
		    D := p#0 * X0_2 + p#1 * X0_3;
		    isNef D and chi OO D == rank source latticePoints D
		    )
		)
	    chi OO (2 * X0_1)
	    assert not isNef (2 * X0_1) 
    SeeAlso
        "Total coordinate rings and coherent sheaves"
	(todd, CoherentSheaf)	
	(hilbertPolynomial, NormalToricVariety, CoherentSheaf)
	(intersectionRing, NormalToricVariety)   	
///

doc ///
    Key    
        (todd, CoherentSheaf)
    Headline
        compute the Todd class of a coherent sheaf
    Usage 
        todd F
    Inputs 
        F : CoherentSheaf
	    on a complete simplicial normal toric variety
    Outputs 
        : RingElement
	    in the intersection ring of the underlying variety	   
    Description
        Text
	    Given a locally-free sheaf $E$ of rank $r$ on a smooth
	    variety such that its Chern class formally factor as
	    {\tt chern} $E = \prod_{j=1}^r (1 + \alpha_j)$,
	    we define its Todd class to be 
	    {\tt todd} $E := \prod_{j=1}^r \alpha_j / [1- exp(-\alpha_j)]$
	    written as a polynomial in the elementary symmetric functions
	    {\tt chern} $(i, E)$ of the $\alpha_j$.
	Text
	    The first few components of the Todd class are easily related
	    to Chern classes.
	Example
	    X0 = kleinschmidt(4, {1,3,5});
	    E0 = cotangentSheaf X0
	    A0 = intersectionRing X0;
	    todd E0
	    assert (part (0, todd E0) == 1)
	    assert (part (1, todd E0) === (1/2) * chern (1, E0))
	    assert (part (2, todd E0) === (1/12)*((chern (1, E0))^2 + chern (2, E0)))	
        Text  
	    On a complete smooth normal toric variety, the Todd class of the
	    tangent bundle factors as a product over the irreducible
	    torus-invariant divisors.
	Example
	    X1 = smoothFanoToricVariety (3, 12);
	    E1 = dual cotangentSheaf X1
	    A1 = intersectionRing X1;
	    f1 = todd E1
	    assert (f1 === product(# rays X1, i -> todd OO (X1_i)))
    SeeAlso
        "Total coordinate rings and coherent sheaves"
	(chi, CoherentSheaf)	
	(hilbertPolynomial, NormalToricVariety, CoherentSheaf)
	(intersectionRing, NormalToricVariety)   	
///
 
 
doc ///
    Key
        (hilbertPolynomial, NormalToricVariety)
	(hilbertPolynomial, NormalToricVariety, Ring)
	(hilbertPolynomial, NormalToricVariety, SheafOfRings)
    Headline 
        compute the multivariate Hilbert polynomial
    Usage 
        hilbertPolynomial X
    Inputs 
        X : NormalToricVariety
	    that is smooth and complete
	Projective => 
	    this option is ignored
    Outputs 
        : RingElement 
	    the Hilbert polynomial of {\tt X}
    Description
        Text
            The Hilbert polynomial of a smooth projective toric variety $X$ is
    	    the Euler characteristic of $OO_X(i_0,i_1,...,i_r)$ where $r$ is
    	    the rank of the Picard group of $X$ and $i_0,i_1,...,i_r$ are
    	    formal variables.  The Hilbert polynomial agrees with the Hilbert
    	    function when evaluated at any point in the nef cone.
    	Text
            On projective space, one recovers the standard Hilbert polynomial.	    
    	Example  
    	    PP2 = toricProjectiveSpace 2;
	    h0 = hilbertPolynomial PP2
	    factor h0
	    h1 = hilbertPolynomial (ring PP2, Projective => false)
	    assert (h0 === sub(h1, (ring h1)_0 => (ring h0)_0))
	    PP3 = toricProjectiveSpace 3;
	    h2 = hilbertPolynomial PP3
	    factor h2
	    h3 = hilbertPolynomial (ring PP3, Projective => false)
	    assert (h2 === sub(h3, (ring h3)_0 => (ring h2)_0))
	Text
	    The Hilbert polynomial of a product of normal toric varieties is
	    simply the product of the Hilbert polynomials of the factors.	    
	Example
            X = toricProjectiveSpace (2) ** toricProjectiveSpace (3);
	    h3 = hilbertPolynomial X
	    factor h3
	Text
	    Example 2.9 in [Diane Maclagan and Gregory G. Smith,
	    @HREF("http://arxiv.org/abs/math.AC/0305215", 
		"Uniform bounds on multigraded regularity")@, 
	    {\it J. Algebraic Geom.} {\bf 14} (2005), 137-164] describes the
	    Hilbert polynomials on a Hirzebruch surface.	    
    	Example
	    a = random (9)
	    FFa = hirzebruchSurface a;
	    h4 = hilbertPolynomial FFa
	    R = ring h4;
	    assert (h4 == R_0 * R_1 + (a/2)*R_1^2 + R_0 + ((a+2)/2)*R_1 + 1)
	Text
	    The Hilbert polynomial is computed using the
	    Hirzebruch-Riemann-Roch Theorem.  In particular, this method
	    depends on the @TO Schubert2@ package.
    SeeAlso
        "Total coordinate rings and coherent sheaves"    
	(hilbertPolynomial, NormalToricVariety, CoherentSheaf)
        (isSmooth, NormalToricVariety)
///   

doc ///
    Key
        (hilbertPolynomial, NormalToricVariety, CoherentSheaf)
	(hilbertPolynomial, NormalToricVariety, Module)
	(hilbertPolynomial, NormalToricVariety, Ideal)
    Headline 
        compute the multivariate Hilbert polynomial
    Usage 
        hilbertPolynomial (X, F)
    Inputs 
        X : NormalToricVariety
	    that is smooth and complete
	F : CoherentSheaf
	    on {\tt X}
	Projective => 
	    this option is ignored
    Outputs 
        : RingElement 
	    the Hilbert polynomial of {\tt X}
    Description
        Text
            The Hilbert polynomial of a coherent sheaf $F$ on smooth normal
    	    toric variety $X$ is the Euler characteristic of $F **
    	    OO_X(i_0,i_1,...,i_r)$ where $r$ is the rank of the Picard group
    	    of $X$ and $i_0,i_1,...,i_r$ are formal variables.  The Hilbert
    	    polynomial agrees with the Hilbert function when evaluated at any
    	    point sufficiently far in the interior of in the nef cone.
	Text
	    For a graded module over the total coordinate ring of $X$, this
	    method computes the Hilbert polynomial of the corresponding
	    coherent sheaf.  Given an ideal $I$ in the total coordinate ring,
	    it computes the Hilbert polynomial of the coherent sheaf
	    assoicated to $S^1/I$.
    	Text
            The cotangent bundle on a smooth surface provides simple examples.
    	Example  
    	    PP2 = toricProjectiveSpace 2;
	    OmegaPP2 = cotangentSheaf PP2
	    h0 = hilbertPolynomial (PP2, OmegaPP2)
	    for i to 10 list hilbertFunction(i, module OmegaPP2)
	    R = ring h0;
	    for i to 10 list sub(h0, R_0 => i)
    	Example  
    	    FF3 = hirzebruchSurface 3;
	    OmegaFF3 = cotangentSheaf FF3
	    h1 = hilbertPolynomial (FF3, OmegaFF3)
	    matrix table(5,5, (i,j) -> hilbertFunction({j,4-i}, module OmegaFF3))
	    R = ring h1;
	    matrix table(5,5, (i,j) -> sub(h1, {R_0 => j, R_1 => 4-i}))
	Text
	    Example 2.16 in [Diane Maclagan, Gregory G. Smith, 
	    @HREF("http://arxiv.org/abs/math.AC/0305215", 
		"Uniform bounds on multigraded regularity")@, 
	    {\it J. Algebraic Geom.} {\bf 14} (2005), 137-164] shows that the
	    Hilbert function for a set of points agrees with the Hilbert
	    polynomial precisedly on the multigraded regularity.
    	Example
            X = toricProjectiveSpace (1) ** toricProjectiveSpace (1);
	    S = ring X;
	    I = intersect (ideal (S_1-S_0, S_3-S_2), ideal (S_1-S_0, S_3-2*S_2), ideal (S_1-2*S_0, S_3-S_2), ideal (S_1-2*S_0, S_3-2*S_2), ideal (S_1-3*S_0, S_3-3*S_2), ideal (S_1-4*S_0, S_3-4*S_2))
	    assert (I == saturate (I, ideal X))
	    hilbertPolynomial (X, I)
	    matrix table(5,5, (i,j) -> hilbertFunction({j,4-i}, I))
    SeeAlso
        "Total coordinate rings and coherent sheaves"    
	(hilbertPolynomial, NormalToricVariety)
        (isSmooth, NormalToricVariety)
///   


------------------------------------------------------------------------------
-- Resolution of singularities    
------------------------------------------------------------------------------

doc ///
    Key
        "Resolution of singularities"
    Description
        Text	
            A variety $X$ has a resolution of singularities if one can find a
  	    nonsingular variety $Y$ and a proper birational map from $Y$ to
  	    $X$.  Every normal toric variety has a resolution of singularities
  	    given by another normal toric variety.
        Text	  
            The following methods related to resolutions of singularities are
  	    currently available in this package.
        Text  
  	    @SUBSECTION "Menu"@
        Text	    
  	    @UL {
    		TO (makeSmooth, NormalToricVariety),
    		TO (makeSimplicial, NormalToricVariety),
    		TO (toricBlowup, List, NormalToricVariety)
	    }@
    SeeAlso
        "Making normal toric varieties"
        "Basic invariants and properties of normal toric varieties"
        "Working with divisors and their associated groups"
        "Total coordinate rings and coherent sheaves"
///	

doc ///
    Key 
    	(makeSimplicial, NormalToricVariety)    
        makeSimplicial
    	[makeSimplicial,Strategy]
    Headline 
        make a birational simplicial toric variety
    Usage 
        makeSimplical X
    Inputs
        X : NormalToricVariety
        Strategy => ZZ
	    either {\tt 0} or {\tt 1}
    Outputs 
        : NormalToricVariety 
	    that is simplicial
    Description
        Text
            A normal toric variety is simplicial if every cone in its fan is
  	    simplicial and a cone is simplicial if its minimal generators are
  	    linearly independent over $\QQ$.  In fact, the following
  	    conditions on a normal toric variety $X$ are equivalent:
        Text	    
            @UL{
                {EM "X", " is simplicial;"},
                {"every Weil divisor on ", EM "X", " has a positive integer
     		  multiple that is Cartier;"},
                {"the Picard group of ", EM "X", " has finite index in the
     		  class group of ", EM "X", ";"},
                {EM "X", " has only finite quotient singularities."}
            }@
        Text	
            Given a normal toric variety, this method makes a simplicial toric
  	    variety with the same rays by triangulating the non-simplicial
  	    maximal cones.  For the {\tt 0} strategy, the triangulation is
  	    constructed by repeated regular subdivisions using random integral
  	    weight vectors.  For the {\tt 1} strategy, the triangulation is
  	    constructed by repeated pushing subdivisions (i.e. toricBlowups at a
  	    given ray).
        Example  
            X = normalToricVariety (id_(ZZ^3) | - id_(ZZ^3));
    	    assert not isSimplicial X
    	    Y1 = makeSimplicial X;
    	    assert isSimplicial Y1
    	    assert (rays Y1 === rays X)
    	    max Y1 
	    max X
    	    Y2 = makeSimplicial(X, Strategy => 1);
    	    assert isSimplicial Y2
    	    assert (rays Y2 === rays X)
    	    max Y2
	    max Y1 == max Y2
        Text
            If the initial toric variety is simplicial, then this method
  	    simply returns it.
        Example	    
    	    PP3 = toricProjectiveSpace 3;
    	    assert isSimplicial PP3
    	    Z = makeSimplicial PP3;
    	    assert (rays Z === rays PP3 and max Z === max PP3)
    SeeAlso
        "Resolution of singularities"
        (isSimplicial, NormalToricVariety)
///	

doc ///
    Key 
    	(toricBlowup, List, NormalToricVariety, List)    
    	(toricBlowup, List, NormalToricVariety)    
        toricBlowup
    Headline 
        makes the toricBlowup of a normal toric variety along a torus orbit closure	
    Usage 
        toricBlowup (s, X, v)
    Inputs
        s : List 
	    of integers indexing a proper torus orbit
        X : NormalToricVariety
        v : List 
	    of integers giving a vector in the relative interior of the cone
	    corresponding to {\tt s} (optional)		    
    Outputs 
        : NormalToricVariety 
	    obtained by blowing up {\tt X} along the torus orbit indexed by
	    {\tt s}
    Description
        Text	    
            Roughly speaking, the toricBlowup replaces a subspace of a given space
  	    with all the directions pointing out of that subspace.  The
  	    metaphor is inflation of a balloon rather than an explosion.  A
  	    toricBlowup is the universal way to turn a subvariety into a Cartier
  	    divisor.
        Text  
            The toricBlowup of a normal toric variety along a torus orbit closure
  	    is also a normal toric variety.  The fan associated to the toricBlowup
  	    is star subdivision or stellar subdivision of the fan of the
  	    original toric variety.  More precisely, we throw out the star of
  	    the cone corresponding to {\tt s} and join a vector {\tt v} lying
  	    the relative interior to the boundary of the star.  When the
  	    vector {\tt v} is not specified, the ray corresponding to the sum
  	    of all rays in the cone corresponding to {\tt s} is used.
        Text  
            The simplest example is toricBlowup of the origin in the affine plane.
  	    Note that the new ray has the largest index.
        Example    
            AA2 = affineSpace 2;
    	    rays AA2
    	    max AA2
    	    Bl0 = toricBlowup ({0,1}, AA2);
    	    rays Bl0
    	    max Bl0
        Text 
  	    Here are a few different toricBlowups of a non-simplicial affine toric
  	    variety
        Example 	    
    	    C = normalToricVariety ({{1,0,0},{1,1,0},{1,0,1},{1,1,1}}, {{0,1,2,3}});
    	    assert not isSimplicial C
    	    Bl1 = toricBlowup ({0,1,2,3}, C);
    	    rays Bl1
    	    max Bl1
	    assert isSimplicial Bl1
    	    Bl2 = toricBlowup ({0,1}, C);
    	    rays Bl2
    	    max Bl2
	    assert isSimplicial Bl2
	    assert (rays Bl1 =!= rays Bl2 and max Bl1 =!= max Bl2)
    	    Bl3 = toricBlowup ({0,1,2,3}, C, {5,3,4});
    	    rays Bl3
    	    max Bl3
	    assert isSimplicial Bl3	    
    	    Bl4 = toricBlowup ({0}, C);
    	    rays Bl4
    	    max Bl4
    	    assert isSimplicial Bl4	    
        Text 
            The third collection of examples illustrate some toricBlowups of a
  	    non-simplicial projective toric variety.
        Example 	    
    	    X = normalToricVariety (id_(ZZ^3) | (-id_(ZZ^3)));
    	    rays X
    	    max X
    	    assert (not isSimplicial X and isProjective X)
    	    orbits (X,1)
    	    Bl5 = toricBlowup ({0,2}, X);
    	    Bl6 = toricBlowup ({6,7}, Bl5);
    	    Bl7 = toricBlowup ({1,5}, Bl6);
    	    rays Bl7
    	    max Bl7
    	    assert (isSimplicial Bl7 and isProjective Bl7)
    	    Bl8 = toricBlowup ({0}, X);
    	    Bl9 = toricBlowup ({7}, Bl8);
    	    assert (rays Bl9 === rays X)
    	    assert (isSimplicial Bl9 and isProjective Bl9)
    Caveat 
        The method assumes that the list {\tt v} corresponds to a primitive
  	vector.  In other words, the greatest common divisor of its entries is
  	one.  The method also assumes that {\tt v} lies in the relative
  	interior of the cone corresponding to {\tt s}.  If either of these
  	conditions fail, then the output will not necessarily be a
  	well-defined normal toric variety.
    SeeAlso
        "Resolution of singularities"
    	(orbits, NormalToricVariety)
    	(isWellDefined, NormalToricVariety)
    	(makeSmooth, NormalToricVariety)
///	

doc ///
    Key 
    	(makeSmooth, NormalToricVariety)
        makeSmooth 	
    	[makeSmooth, Strategy]
    Headline 
        make a birational smooth toric variety 
    Usage 
        makeSmooth X
    Inputs 
        X : NormalToricVariety
        Strategy => ZZ
	    either {\tt 0} or {\tt 1}	
    Outputs 
        : NormalToricVariety
	    that is smooth and birational to {\tt X} 
    Description
        Text	
            Every normal toric variety has a resolution of singularities given
  	    by another normal toric variety.  Given a normal toric variety $X$
  	    this method makes a new smooth toric variety $Y$ which has a
  	    proper birational map to $X$.  The normal toric variety $Y$ is
  	    obtained from $X$ by repeatedly blowing up appropriate torus orbit
  	    closures (if necessary the @TO makeSimplicial@ method is also used
  	    with the specified strategy).  A minimal number of blow-ups are
  	    used.
        Text
            As a simple example, we can resolve a simplicial affine
            singularity.
        Example 	    
    	    U = normalToricVariety ({{4,-1},{0,1}}, {{0,1}});
    	    assert not isSmooth U
    	    V = makeSmooth U;
    	    assert isSmooth V
    	    rays V, max V
    	    toList (set rays V - set rays U)
        Text 
            There is one additional rays, so only one toricBlowup was needed.
        Text 
  	    To resolve the singularities of this simplicial projective
  	    fourfold, we need eleven toricBlowups.
        Example  
    	    W = weightedProjectiveSpace {1,2,3,4,5};
    	    assert (dim W === 4)
    	    assert (isSimplicial W and not isSmooth W)
    	    W' = makeSmooth W;
    	    assert isSmooth W'
    	    # (set rays W' - set rays W)
        Text
            If the initial toric variety is smooth, then this method simply
  	    returns it.
        Example   
    	    AA1 = affineSpace 1;
    	    assert (AA1 === makeSmooth AA1)
    	    PP2 = toricProjectiveSpace 2;
    	    assert (PP2 === makeSmooth PP2)
        Text
            In the next example, we resolve the singularities of a
  	    non-simplicial projective threefold.
        Example 	    
    	    X = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
    	    assert (not isSimplicial X and not isSmooth X)
    	    X' = makeSmooth X;
    	    assert isSmooth X'
    	    # (set rays X' - set rays X)
        Text	    
  	    We also demonstrate this method on a complete simplicial
  	    non-projective threefold.
        Example  
    	    Z = normalToricVariety ({{-1,-1,1},{3,-1,1},{0,0,1},{1,0,1},{0,1,1},{-1,3,1},{0,0,-1}}, {{0,1,3},{0,1,6},{0,2,3},{0,2,5},{0,5,6},{1,3,4},{1,4,5},{1,5,6},{2,3,4},{2,4,5}});
    	    assert (isSimplicial Z and not isSmooth Z)
	    assert (isComplete Z and not isProjective Z)
    	    Z' = makeSmooth Z;
    	    assert isSmooth Z'
    	    # (set rays Z' - set rays Z)
        Text
  	    We end with a degenerate example.
        Example	    
            Y = normalToricVariety ({{1,0,0,0},{0,1,0,0},{0,0,1,0},{1,-1,1,0},{1,0,-2,0}}, {{0,1,2,3},{0,4}});
    	    assert (isDegenerate Y and not isSimplicial Y and not isComplete Y)
    	    Y' = makeSmooth Y;
    	    assert isSmooth Y'
	    # (set rays Y' - set rays Y)
    Caveat 
        A singular normal toric variety almost never has a unique minimal
	resolution.  This method returns only of one of the many minimal
	resolutions.
    SeeAlso
        "Resolution of singularities"
    	(isSmooth, NormalToricVariety)
    	(makeSimplicial, NormalToricVariety)
///	



------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- test 0
TEST ///
X = affineSpace 1;
assert isWellDefined X 
assert (rays X == {{1}})
assert (max X == {{0}})
assert (dim X == 1)
assert (orbits (X,0) === max X)
assert (orbits (X,1) === {{}})
assert not isDegenerate X
assert isSmooth X
assert not isComplete X
assert not isFano X
assert (weilDivisorGroup X == ZZ^1) 
assert (fromWDivToCl X == 0)
assert (classGroup X == ZZ^0)
assert (cartierDivisorGroup X == ZZ^1)
assert (fromCDivToWDiv X == id_(ZZ^1))
assert (fromCDivToPic X == 0)
assert (picardGroup X == ZZ^0)
assert (fromPicToCl X == 0)
assert (nefGenerators X == 0)
assert isEffective X_0
assert try (monomials X_0; false) else true
assert not isEffective (-X_0) 
assert isCartier X_0
assert not isNef X_0
assert (OO X_0 === OO_X^1)
assert (degrees ring X === {{}})
assert (ideal X == 1)
assert (cotangentSheaf X === OO_X^1)
assert (makeSimplicial X === X)
assert (makeSmooth X === X)
assert try (affineSpace 0; false) else true
///

-- test 1
TEST ///
X = toricProjectiveSpace 1;
assert isWellDefined X
assert (set rays X === set ({-1},{1}))
assert (max X == sort subsets (2,1))
assert (dim X === 1)
assert (orbits (X,0) === max X)
assert (orbits (X,1) === {{}})
assert not isDegenerate X
assert isSmooth X
assert isProjective X
assert isFano X
assert (weilDivisorGroup X == ZZ^2) 
assert (fromWDivToCl X == map (ZZ^1,ZZ^2, i -> 1_ZZ))
assert (classGroup X == ZZ^1)
assert (cartierDivisorGroup X == ZZ^2)
assert (fromCDivToWDiv X == id_(ZZ^2))
assert (fromCDivToPic X == map (ZZ^1,ZZ^2, i -> 1_ZZ))
assert (picardGroup X == ZZ^1)
assert (fromPicToCl X == id_(ZZ^1))
assert (nefGenerators X == 1)
assert (isNef toricDivisor (flatten entries  ( (fromCDivToWDiv X) * (nefGenerators X // fromCDivToPic X)), X))
assert isEffective X_0
assert (monomials (4*X_0)  == sort first entries basis (degree (4*X_0), ring variety X_0))
assert (X_0 + X_1 === toricDivisor ({1,1},X))
assert (2*X_0 === X_0 + X_0)
assert isVeryAmple X_0
assert (vertices (2*X_0) == matrix {{0,2}})
assert (latticePoints (2*X_0) == matrix {{0,1,2}})
assert (degrees ring X === {{1},{1}})
assert (ideal X == ideal gens ring X)
assert (cotangentSheaf X === OO_X(-2))
assert (all (5, i -> rank HH^0(X,OO_X(i)) == binomial(1+i,i)))
assert (makeSimplicial X === X)
assert (makeSmooth X === X)
assert (null == try toricProjectiveSpace 0)
///

-- test 2
TEST ///
n = 4;
X = toricProjectiveSpace n;
assert isWellDefined X
assert (rays X === {toList (n:-1)} | entries id_(ZZ^n))
assert (max X === subsets (n+1,n))
assert (dim X === n)
assert (orbits (X,1) === sort subsets (n+1,n-1))
assert (orbits (X,2) === sort subsets (n+1,n-2))
assert (orbits (X,4) === {{}})
assert not isDegenerate X
assert isSmooth X
assert isProjective X
assert isFano X
assert (weilDivisorGroup X == ZZ^(n+1)) 
assert (fromWDivToCl X == map (ZZ^1,ZZ^(n+1), i -> 1_ZZ))
assert (classGroup X == ZZ^1)
assert (cartierDivisorGroup X == ZZ^(n+1))
assert (fromCDivToWDiv X == id_(ZZ^(n+1)))
assert (fromCDivToPic X == map (ZZ^1,ZZ^(n+1), i -> 1_ZZ))
assert (picardGroup X == ZZ^1)
assert (fromPicToCl X == id_(ZZ^1))
assert (nefGenerators X == 1)
assert (isNef toricDivisor (flatten entries ( (fromCDivToWDiv X) * (nefGenerators X // fromCDivToPic X)), X))
assert (isEffective X_0 === true)
assert (monomials (2*X_0)  == sort first entries basis(degree (2*X_0), ring variety X_0))
assert (X_0 + X_1 === toricDivisor ({1,1} | toList (n-1:0),X))
assert (2*X_0 === X_0 + X_0)
assert isVeryAmple X_0
assert (degree X_0 === {sum entries X_0})
assert (degree (2*X_0+X_3) === {sum entries (2*X_0+X_3)})
assert (vertices (2*X_0) == map (ZZ^n,ZZ^1,i -> 0) | 2*id_(ZZ^n))
assert (degrees ring X === toList (n+1 : {1}))
assert (ideal X == ideal gens ring X)
assert (cotangentSheaf (X, Minimize => true) === prune sheaf (X, 
	homology (vars ring X,jacobian ring X)))
assert ({degree (-X_0+3*X_2)} === - degrees OO (-X_0+3*X_2))
assert (all (5, i -> rank HH^0(X,OO_X(i)) == binomial(n+i,i)))
assert (all (5, i -> HH^1(X,OO_X(i)) == 0))
assert (all (5, i -> rank HH^n(X,OO_X(-i-n-1)) == binomial(n+i,i)))
assert (makeSimplicial X === X)
assert (makeSmooth X === X)
///

-- test 3
TEST ///
X = hirzebruchSurface 2;
assert isWellDefined X
assert (rays X == {{1,0},{0,1},{-1,2},{0,-1}})
assert (max X == {{0,1},{0,3},{1,2},{2,3}})	  
assert (dim X == 2)	  
assert (orbits (X,0) === max X)
assert (orbits (X,1) === apply (4, i -> {i}))
assert (orbits (X,2) === {{}})
assert not isDegenerate X
assert isSmooth X
assert isProjective X
assert not isFano X
assert (weilDivisorGroup X == ZZ^4) 
assert (fromWDivToCl X == map (ZZ^2,ZZ^4, matrix{{1,-2,1,0},{0,1,0,1}}))
assert (classGroup X == ZZ^2)
assert (cartierDivisorGroup X == ZZ^4)
assert (fromCDivToWDiv X == id_(ZZ^4))
assert (fromCDivToPic X == map (ZZ^2,ZZ^4, matrix{{1,-2,1,0},{0,1,0,1}}))
assert (picardGroup X == ZZ^2)
assert (fromPicToCl X == id_(ZZ^2))
assert (nefGenerators X == 1)
assert all (entries transpose ( (fromCDivToWDiv X) * (nefGenerators X // fromCDivToPic X)),
    coeffs -> isNef toricDivisor (coeffs, X))
assert isEffective X_0
assert (monomials (2*X_0 + X_1) == sort first entries basis (degree (2*X_0 + X_1), ring variety X_0))
assert isWellDefined X_0
assert (X_0 + X_1 === toricDivisor ({1,1,0,0},X))
assert (2*X_0 === X_0 + X_0)
assert (degree (-3*X_0 + 7*X_1) === - first degrees OO (-3*X_0 + 7*X_1))
assert isNef X_0
assert not isNef X_1
assert isVeryAmple (X_2+X_3)
assert (vertices (X_2+X_3) === matrix{{0,1,0,3},{0,0,1,1}})
assert (latticePoints (X_2+X_3) === matrix {{0, 1, 0, 1, 2, 3}, {0, 0, 1, 1, 1, 1}})
assert (degrees ring X === {{1,0},{-2,1},{1,0},{0,1}})
S = ring X;
assert (ideal X == intersect (ideal (S_0,S_2),ideal (S_1,S_3)))
assert (makeSimplicial X === X)
assert (makeSmooth X === X)
///

-- test 4
TEST ///
X = weightedProjectiveSpace {1,2,3};
assert isWellDefined X
assert (rays X == {{-2,-3},{1,0},{0,1}})
assert (max X == {{0,1},{0,2},{1,2}})
assert (dim X == 2)	  
assert (orbits (X,0) === max X)
assert (orbits (X,1) === apply (3, i -> {i}))
assert (orbits (X,2) === {{}})
assert not isDegenerate X
assert isSimplicial X
assert not isSmooth X
assert isProjective X
assert isFano X
assert (weilDivisorGroup X == ZZ^3) 
assert (fromWDivToCl X == map (ZZ^1,ZZ^3, matrix{{1,2,3}}))
assert (classGroup X == ZZ^1)
assert (cartierDivisorGroup X == ZZ^3)
assert (rank fromCDivToPic X === 1)
assert (picardGroup X == ZZ^1)
assert (fromPicToCl X == map (ZZ^1,ZZ^1, {{6}}))
assert (nefGenerators X == 1)
assert all (entries transpose  ( (fromCDivToWDiv X) * (nefGenerators X // fromCDivToPic X)),
    coeffs -> isNef toricDivisor (coeffs, X))
assert isEffective X_0
assert (X_0 + X_1 === toricDivisor ({1,1,0},X))
assert (2*X_0 === X_0 + X_0)
assert isNef X_0
assert not isCartier X_0
assert isQQCartier X_0
assert not isAmple X_1
assert isAmple  (3*X_1)
assert isVeryAmple (3*X_1)
assert (vertices (6*X_0) === matrix{{0,3,0},{0,0,2}})
assert (OO (6*X_0) === OO (3*X_1))
assert (degrees ring X === apply (3, i -> {i+1}))
assert (ideal X == ideal gens ring X)
Y = makeSmooth X;
assert isWellDefined Y
assert isSmooth Y
assert (set rays Y === set {{-2,-3},{1,0},{0,1},{-1,-2},{-1,-1},{0,-1}})
assert (sort max Y === sort {{0,5},{0,4},{1,2},{1,3},{2,4},{3,5}})
///

-- test 5
TEST ///
X = kleinschmidt (9,{1,2,3});
assert (orbits (X, dim X) === {{}})
assert isWellDefined X
assert isFano X
assert isSmooth X
assert (picardGroup X === ZZ^2)
assert (nefGenerators X == 1)
assert all (entries transpose ( (fromCDivToWDiv X) * (nefGenerators X // fromCDivToPic X)),
    coeffs -> isNef toricDivisor (coeffs, X))
///

-- test 6
TEST ///
assert (all (5, i -> (
    X := smoothFanoToricVariety (2,i);
    isWellDefined X and isSmooth X and isFano X)))
assert (all (18, i -> (
    X := smoothFanoToricVariety (3,i);
    isWellDefined X and isSmooth X and isFano X)))
X = smoothFanoToricVariety (2,4);
assert (HH^1 (X,OO_X (-2,1,1,-2)) == QQ^2)
///

-- test 7
TEST ///
rayList = {{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{-1,0,-1},{-2,-1,0}};
coneList = {{0,1,2},{0,1,3},{1,3,4},{1,2,4},{2,4,5},{0,2,5},{0,3,5},{3,4,5}};
X = normalToricVariety (rayList,coneList);
assert isWellDefined X
assert (dim X === 3)
assert (orbits (X,0) === max X)
assert (orbits (X,2) === apply (6, i -> {i}))
assert not isDegenerate X
assert isSimplicial X
assert not isSmooth X
assert isComplete X
assert not isProjective X
assert not isFano X
assert (weilDivisorGroup X == ZZ^6)
assert (classGroup X === ZZ^3)
assert (picardGroup X === ZZ^3)
assert (nefGenerators X == 0)
assert all (entries transpose ( (fromCDivToWDiv X) * (nefGenerators X // fromCDivToPic X)),
    coeffs -> isNef toricDivisor (coeffs, X))
assert isNef (0*X_0)
assert all (6, i -> not isAmple (X_i))
Y = makeSmooth X;
assert isWellDefined Y
assert isSmooth Y
///

-- test 8
TEST ///
X = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
assert isWellDefined X
assert (set rays X === set entries matrix{{1,1,1},{-1,1,1},{1,-1,1},{-1,-1,1},
    {1,1,-1},{-1,1,-1},{1,-1,-1},{-1,-1,-1}})
assert (dim X === 3)
assert (orbits (X,0) === max X)
assert (orbits (X,2) === apply (8, i -> {i}))
assert not isDegenerate X
assert not isSimplicial X
assert not isSmooth X
assert isProjective X   
assert isFano X
assert (weilDivisorGroup X == ZZ^8) 
assert (classGroup X ==  (cokernel matrix{{2}})^2 ++ ZZ^5)
assert (picardGroup X == ZZ^1)
assert (fromWDivToCl X * fromCDivToWDiv X == fromPicToCl X * fromCDivToPic X)
assert (nefGenerators X == 1)
assert all (entries transpose ( (fromCDivToWDiv X) * (nefGenerators X // fromCDivToPic X)),
    coeffs -> isNef toricDivisor (coeffs, X))
assert isEffective X_0
assert not isCartier X_0
K = toricDivisor X;
assert isCartier K
assert not isNef K
Y = makeSimplicial X;
assert isWellDefined Y
assert isSimplicial Y
assert not isSmooth Y
Y = makeSimplicial (X, Strategy => 1);
assert isWellDefined Y
assert isSimplicial Y
assert not isSmooth Y
Z = makeSmooth X;
assert isWellDefined Z
assert isSmooth Z 
///

-- test 9
TEST ///
X = normalToricVariety ({{1,0,0,0},{0,1,0,0},{0,0,1,0},{1,-1,1,0},{1,0,-2,0}},
  {{0,1,2,3},{0,4}});
assert isWellDefined X
assert (dim X === 4)
assert (orbits (X,0) === {})
assert (orbits (X,1) === {{0,1,2,3}})
assert (orbits (X,2) === {{0,1},{0,3},{0,4},{1,2},{2,3}})
assert (orbits (X,3) === apply (5, i -> {i}))
assert isDegenerate X
assert not isSimplicial X
assert not isSmooth X
assert not isComplete X
assert (weilDivisorGroup X == ZZ^5)
assert (classGroup X == ZZ^2)
assert (picardGroup X == ZZ^1)
assert isEffective X_0
Y = makeSimplicial X;
assert isWellDefined Y
assert isSimplicial Y
assert not isSmooth Y
Y = makeSimplicial (X, Strategy => 0);
assert isWellDefined Y
assert isSimplicial Y
assert not isSmooth Y
Z = makeSmooth X;
assert isWellDefined Z
assert isSmooth Z
///

-- test 10
TEST ///
C = normalToricVariety ({{1,0,0},{1,1,0},{1,0,1},{1,1,1}},{{0,1,2,3}});
Bl1 = toricBlowup ({0,1,2,3},C);
assert (rays Bl1 === {{1,0,0},{1,1,0},{1,0,1},{1,1,1},{2,1,1}})
assert (max Bl1 === {{0,1,4},{0,2,4},{1,3,4},{2,3,4}})
Bl2 = toricBlowup ({0,1},C);
assert (rays Bl2 === {{1,0,0},{1,1,0},{1,0,1},{1,1,1},{2,1,0}})
assert (max Bl2 === {{0,2,4},{1,3,4},{2,3,4}})
Bl3 = toricBlowup ({0,1,2,3},C,{5,3,4});
assert (rays Bl3 === {{1,0,0},{1,1,0},{1,0,1},{1,1,1},{5,3,4}})
assert (max Bl3 === {{0,1,4},{0,2,4},{1,3,4},{2,3,4}})
Bl4 = toricBlowup ({0},C);
assert isSimplicial Bl4
assert (rays Bl4 === {{1,0,0},{1,1,0},{1,0,1},{1,1,1}})
assert (max Bl4 === {{0,1,3},{0,2,3}})
X = normalToricVariety (id_(ZZ^3) | (-id_(ZZ^3)));
Bl5 = toricBlowup ({0,2},X);
assert (rays Bl5 === rays X | {{1,0,1}})
assert not isSimplicial Bl5
assert isProjective Bl5 
assert isWellDefined Bl5
Bl6 = toricBlowup ({0},X);
assert (rays Bl6 === rays X)
assert not isSimplicial Bl6
assert isProjective Bl6 
Bl7 = toricBlowup ({7},Bl6);
assert (rays Bl7 === rays X)
assert isSimplicial Bl7
assert isProjective Bl7 
assert isWellDefined Bl7
///

-- test 11
TEST ///
rayList = {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1},{0,1,1,1},{1,0,1,-1},
  {1,-1,0,1},{1,1,-1,0},{0,0,0,-1},{-1,0,-1,1},{0,-1,0,0},{-1,1,0,-1},
  {0,0,-1,0},{-1,-1,1,0}};
coneList = {{0,5},{0,6},{0,7},{1,4},{1,7},{1,11},{2,4},{2,5},{2,13},{3,4},{3,6},
  {3,9},{5,8},{6,10},{7,12},{8,9},{10,11},{12,13}};
X = normalToricVariety (rayList,coneList);
assert not isComplete X
///

-- test 12
TEST ///
-- examples provided by Claudiu Raicu, Mike Stillman, and me to illustrate an
-- earlier bug in 'isProjective'
X = normalToricVariety ({{1,2,3},{-1,1,1},{1,-1,1},{-1,-1,1},{1,1,-1},{-1,1,-1},
	{1,-1,-1},{-1,-1,-1}},{{0,1,2,3},{0,1,4,5},{0,2,4,6},{1,3,5,7},
	{2,3,6,7},{4,5,6,7}});
assert isWellDefined X
assert not isSmooth X
assert not isSimplicial X
assert not isProjective X
assert (nefGenerators X == 0)
Y = normalToricVariety ({{-1,-1,-1,-1},{-1,-1,-1,0},{-1,-1,0,2},{-1,0,-1,-1},
	{0,-1,-1,-1},{1,-1,0,-1},{1,2,2,2}},{{0,1,2,5},{0,1,2,6},{0,1,3,5},
	{0,1,3,6},{0,2,5,6},{0,3,5,6},{1,2,4,5},{1,2,4,6},{1,3,4,5},
	{1,3,4,6},{2,4,5,6},{3,4,5,6}});
assert isWellDefined Y
assert not isSmooth Y
assert isSimplicial Y
assert not isProjective Y
Z = normalToricVariety ({{-1,-1,1},{3,-1,1},{0,0,1},{1,0,1},{0,1,1},{-1,3,1},
	{0,0,-1}},{{0,1,3},{0,1,6},{0,2,3},{0,2,5},{0,5,6},{1,3,4},{1,4,5},
	{1,5,6},{2,3,4},{2,4,5}});
assert isWellDefined Z
assert isComplete Z
assert isSimplicial Z
assert not isProjective Z
///

-- test 13
TEST ///
-- examples provided by Mike Stillman to illustrate an earlier bug in
-- 'cartierDivisorGroup'
X = normalToricVariety ({{-1,7,-5,-4},{-1,-1,1,1},{-1,-1,3,1},{-1,-1,1,2},
	{1,-1,0,0},{-1,3,-2,-1},{-1,-1,2,1},{-1,1,0,0}},{{0,1,4,5},{0,1,4,6},
	{0,1,5,7},{0,1,6,7},{0,2,4,5},{0,2,4,6},{0,2,5,7},{0,2,6,7},{1,3,4,5},
	{1,3,4,6},{1,3,5,7},{1,3,6,7},{2,3,4,5},{2,3,4,6},{2,3,5,7},{2,3,6,7}});
assert isWellDefined X 
assert isSimplicial X
assert (cartierDivisorGroup X == ZZ^8)
assert (weilDivisorGroup X == ZZ^8)
assert isCartier toricDivisor X
assert all (# rays X, i -> isCartier  (2*X_i))
assert all (# rays X, i -> not isCartier X_i)
assert all (entries transpose ( (fromCDivToWDiv X) * (nefGenerators X // fromCDivToPic X)),
    coeffs -> isNef toricDivisor (coeffs, X))
///


-- test 14
TEST ///
-- first example provided by Chris Eur to illustrate an earlier bug in
-- 'normalToricVariety'
P = convexHull transpose matrix unique permutations {1,1,0,0};
X = normalToricVariety P;
assert isWellDefined X
assert (8 === #rays X)
assert (dim P === dim X)
assert (1 === rank picardGroup X)
Q = convexHull ((transpose matrix unique permutations {1,1,0,0}) || matrix {toList{6:1}});
Y = normalToricVariety Q;
assert isWellDefined Y
assert (dim Q === dim Y)
assert (1 === rank picardGroup Y)
assert (rays X == rays Y)
assert (max X == max X)
///

end---------------------------------------------------------------------------     

------------------------------------------------------------------------------
-- SCRATCH SPACE
------------------------------------------------------------------------------

-- XXX
uninstallPackage "NormalToricVarieties"
restart
installPackage "NormalToricVarieties"
check NormalToricVarieties



------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- interesting test but it is currently to slow to be on the basic list.
-- test 
TEST ///
rayList = {{1,0,0,0,0,0},{0,1,0,0,0,0},{0,0,1,0,0,0},{0,0,0,1,0,0},{0,0,0,0,1,0},
  {0,0,0,0,0,1},{-1,-1,-1,-1,-1,-1},{1,1,1,0,0,0},{1,0,0,1,1,0},
  {0,-1,-1,-1,-1,0},{0,1,0,1,0,1},{0,0,1,0,1,1},{-1,-1,0,0,-1,-1},
  {-1,0,-1,-1,0,-1}};
coneList = {{0,1,3,6,8,9,10,13},{0,1,3,6,8,12,13},{0,1,3,6,9,10,12},{0,1,3,7,8,10},
  {0,1,3,7,8,12},{0,1,3,7,10,12},{0,1,4,5,8,11},{0,1,4,5,8,13},{0,1,4,5,11,13},
  {0,1,4,7,8,11},{0,1,4,7,8,13},{0,1,4,7,11,13},{0,1,5,7,8,10,11},
  {0,1,5,7,9,11,13},{0,1,5,7,9,12},{0,1,5,7,10,12},{0,1,5,8,9,10,13},
  {0,1,5,9,10,12},{0,1,6,7,9,12},{0,1,6,7,9,13},{0,1,6,7,12,13},{0,1,7,8,12,13},
  {0,2,3,5,7,8,10,11},{0,2,3,5,7,10,12},{0,2,3,5,8,11,12},{0,2,3,7,8,12},
  {0,2,4,7,8,11},{0,2,4,7,8,12,13},{0,2,4,7,11,13},{0,2,4,8,11,12},
  {0,2,4,11,12,13},{0,2,5,7,9,11},{0,2,5,7,9,12},{0,2,5,9,11,12},{0,2,6,7,9,11,13},
  {0,2,6,7,9,12},{0,2,6,7,12,13},{0,2,6,9,11,12},{0,2,6,11,12,13},
  {0,3,5,8,9,10},{0,3,5,8,9,12},{0,3,5,9,10,12},{0,3,6,8,9,12},{0,4,5,8,9,11},
  {0,4,5,8,9,13},{0,4,5,9,11,13},{0,4,6,8,9,11,12},{0,4,6,8,9,13},
  {0,4,6,8,12,13},{0,4,6,9,11,13},{0,4,6,11,12,13},{0,5,8,9,11,12},
  {1,2,3,7,8,10,11},{1,2,3,7,8,12},{1,2,3,7,10,12},{1,2,3,8,11,12},
  {1,2,3,10,11,12},{1,2,4,7,8,11},{1,2,4,7,8,12,13},{1,2,4,7,11,13},
  {1,2,4,8,11,12},{1,2,4,11,12,13},{1,2,5,6,7,9,11,13},{1,2,5,6,7,9,12},
  {1,2,5,6,11,12,13},{1,2,5,7,10,11},{1,2,5,7,10,12},{1,2,5,10,11,12},
  {1,2,6,7,12,13},{1,3,4,8,10,11},{1,3,4,8,10,13},{1,3,4,8,11,12},
  {1,3,4,8,12,13},{1,3,4,10,11,12,13},{1,3,6,10,12,13},{1,4,5,8,10,11},
  {1,4,5,8,10,13},{1,4,5,10,11,13},{1,5,6,9,10,12},{1,5,6,9,10,13},
  {1,5,6,10,12,13},{1,5,10,11,12,13},{2,3,5,10,11,12},{2,5,6,9,11,12},
  {3,4,5,8,10,11},{3,4,5,8,10,13},{3,4,5,8,11,12},{3,4,5,8,12,13},
  {3,4,5,10,11,12,13},{3,5,6,8,9,10,13},{3,5,6,8,9,12},{3,5,6,8,12,13},
  {3,5,6,9,10,12},{3,5,6,10,12,13},{4,5,6,8,9,11,12},{4,5,6,8,9,13},
  {4,5,6,8,12,13},{4,5,6,9,11,13},{4,5,6,11,12,13}};
X = normalToricVariety (rayList, coneList);
Y = makeSimplicial X;
debugLevel = 2;
assert (isWellDefined Y === true)
assert (isSimplicial Y === true)
assert (isProjective Y === true)
Y = makeSimplicial (X, Strategy => 0);
assert (isWellDefined Y === true)
assert (isSimplicial Y === true)
assert (isProjective Y === true)

///

