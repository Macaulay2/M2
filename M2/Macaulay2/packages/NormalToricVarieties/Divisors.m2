------------------------------------------------------------------------------
-- Toric Divisors and Related Groups
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


------------------------------------------------------------------------------
-- Database of some ample divisors
------------------------------------------------------------------------------
-- THIS FUNCTION IS NOT EXPORTED. By reading an auxiliary file, this function
-- creates a HashTable with the defining data for the small smooth projective
-- toric varieties.
smallSmoothFile := currentFileDirectory | "smallSmoothProjectiveToricVarieties.txt"
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


------------------------------------------------------------------------------
-- Arithmetic of toric divisors
------------------------------------------------------------------------------
ToricDivisor == ToricDivisor := Boolean => (D, E) -> 
    variety D === variety E and entries D === entries E;
ToricDivisor == ZZ := Boolean => (D, m) -> (
    if m =!= 0 then 
	error "attempted to compare a divisor with a nonzero integer";
    all(entries D, e -> e === 0)
    );
ZZ == ToricDivisor := Boolean => (m, D) -> D == m   

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


------------------------------------------------------------------------------
-- Line bundles
------------------------------------------------------------------------------
installMethod(symbol SPACE, OO, ToricDivisor, (OO, D) -> (
  	X := variety D;
  	a := toSequence entries (fromWDivToCl X * vector D);
  	OO_X a
	)
    );


------------------------------------------------------------------------------
-- Properties of toric divisors
------------------------------------------------------------------------------
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
     -- ample if and only if the intersection with every torus-invariant
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


------------------------------------------------------------------------------
-- Polyhedral features of a toric divisor
------------------------------------------------------------------------------
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
