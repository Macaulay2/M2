-- -*- coding: utf-8 -*-
------------------------------------------------------------------------------
-- Simplicial Complexes CODE
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Basic features of the simplicial complex datatype
------------------------------------------------------------------------------
SimplicialComplex = new Type of HashTable
SimplicialComplex.synonym = "abstract simplicial complex"

-- 'facets' method defined in Polyhedra
facets SimplicialComplex := List => D -> D.facets

expression SimplicialComplex := D -> (expression simplicialComplex) expression matrix {facets D}
net SimplicialComplex := net @@ expression
texMath SimplicialComplex := D -> texMath expression D

ideal SimplicialComplex := Ideal => D -> ideal D.monomialIdeal
monomialIdeal SimplicialComplex := MonomialIdeal => D -> D.monomialIdeal
ring SimplicialComplex := PolynomialRing => D -> D.ring
coefficientRing SimplicialComplex := Ring => D -> coefficientRing ring D

dim SimplicialComplex := ZZ => (cacheValue symbol dim) (
    D -> max apply(facets D, s -> # support(s)) - 1
    )
   
simplicialComplex = method()   
simplicialComplex List := SimplicialComplex => L -> (
    facetsList := select(L, r -> r =!= 0);
    -- need at least one facet to determine the ring   
    if # facetsList === 0 then error "-- expected at least one facet";
    if not same apply(facetsList, class) then error "-- expect all elements in the list to have same type";
    if class L#0 === Face then 
	facetsList = apply(facetsList, j -> product vertices j);
    S := ring (facetsList#0);
    G := matrix {{product gens S}};
    -- the monomialIdeal constructor verifies that the elements all lie in the
    -- same commutative polynomial ring
    I := monomialIdeal contract (matrix{facetsList}, G);
    -- we need this ideal to be squarefree
    if not isSquareFree I then error "-- expected squarefree inputs";
    -- remove any non-maximal faces from 'facetsList'
    I = monomialIdeal mingens I;
    -- contracting with G complements the support of the monomials
    F := first entries sort contract (gens I, G);
    -- Alexander duality for monomial ideals in part of the 'Core'
    I = dual I;
    new SimplicialComplex from {
	symbol ring           => S,
	symbol monomialIdeal  => I,
	symbol facets         => F,
	symbol cache          => new CacheTable
	}
    )
simplicialComplex Matrix := SimplicialComplex => f -> (
    if numrows f =!= 1 then error "-- expected a matrix with 1 row";
    simplicialComplex first entries f
    )

simplicialComplex MonomialIdeal := SimplicialComplex => I -> (
    S := ring I;
    -- we need a squarefree ideal
    if not isSquareFree I then error "-- expected a squarefree ideal";
    -- Alexander duality for monomial ideals in part of the 'Core'
    J := dual I;
    -- the void complex is the special case that has no facets
    if J == 0 then (
	return new SimplicialComplex from {
	    symbol ring           => S,
	    symbol monomialIdeal  => monomialIdeal 1_S,
	    symbol facets         => first entries map(S^1, S^0, 0),
	    symbol cache          => new CacheTable
	    }
	);    
    G := matrix {{product gens S}};
    -- contracting with G complements the support of the monomials    
    F := first entries sort contract (gens J, G);
    new SimplicialComplex from {
	symbol ring           => S,
	symbol monomialIdeal  => I,
	symbol facets         => F,
	symbol cache          => new CacheTable
	}       
    )

simplicialComplex Ideal := SimplicialComplex => I -> (
    if not isMonomialIdeal I then error "-- expected a monomial ideal";
    simplicialComplex monomialIdeal I
    )

isWellDefined SimplicialComplex := Boolean => D -> (
    -- CHECK DATA STRUCTURE
    -- check keys
    K := keys D;
    expectedKeys := set {symbol ring, symbol monomialIdeal, symbol facets, symbol cache};
    if set K =!= expectedKeys then (
	if debugLevel > 0 then (
	    added := toList (K - expectedKeys);
	    missing := toList (expectedKeys - K);
	    if # added > 0 then 
	        << "-- unexpected key(s): " << toString added << endl;
	    if # missing > 0 then 
	        << "-- missing key(s): " << toString missing << endl;
	    );
	return false
	);
    -- check types
    if not instance (D.ring, PolynomialRing) then (
	if debugLevel > 0 then 
	    << "-- expected `.ring' to be a PolynomialRing" << endl;
	return false
	);
    if not instance (D.monomialIdeal, MonomialIdeal) then (
	if debugLevel > 0 then 
	    << "-- expected `.monomialIdeal' to be a MonomialIdeal" << endl;
	return false
	);
    if ring D.monomialIdeal =!= D.ring then (
	if debugLevel > 0 then 
	    << "-- expected the ring of `.monomialIdeal' to be `.ring'" << endl;
	return false
	);     
    if not instance (D.facets, List) then (
	if debugLevel > 0 then 
	    << "-- expected `.facets' to be a List" << endl;
	return false
	);        
    if any(D.facets, F -> ring F =!= D.ring) then (
	if debugLevel > 0 then 
	    << "-- expected the entries in `.facets' to be elements of `.ring'" << endl;
	return false
	); 
    if not instance (D.cache, CacheTable) then (
    	if debugLevel > 0 then 
	    << "-- expected `.cache' to be a CacheTable" << endl;
    	return false
	);     
    -- CHECK MATHEMATICAL STRUCTURE
    -- check whether the monomialIdeal is square free
    if not isSquareFree D.monomialIdeal then (
	if debugLevel > 0 then 
	    << "-- expected `.monomialIdeal' to be square free" << endl;
	return false
	);   
    -- check whether the facets correspond to the monomialIdeal
    S := ring D;
    G := matrix {{product gens S}};
    if D.facets =!= first entries sort contract (gens dual monomialIdeal D, G) then (
	if debugLevel > 0 then
	    << "-- expected '.facets' to list the facets corresponding to `.monomialIdeal' << "endl;
    	return false
	);	    
    true
    )

------------------------------------------------------------------------------
-- constructors for classic examples
------------------------------------------------------------------------------
simplexComplex = method()
simplexComplex (ZZ, PolynomialRing) := SimplicialComplex => (n, S) -> (
    if n === -1 then 
	return simplicialComplex {1_S};
    if n < -1 then 
        error "-- expected integer greater than -1";
    if numgens S < n + 1 then 
	error concatenate("-- expected a polynomial ring with at least ", 
	    toString(n+1), " generators");
    simplicialComplex {product(n+1, i -> S_i)}
    )

-- Masahiro Hachimori created a library of interesting simplicial complexes; see
--   http://infoshako.sk.tsukuba.ac.jp/~hachi/math/library/index_eng.html
hachimoriFile := currentFileDirectory | "hachimoriLibrary.txt"
hachimoriTable := hashTable apply (lines get hachimoriFile, x -> (
    	if notify then stderr << "--loading file " << hachimoriFile << endl;
    	value x
	)
    )

bartnetteSphereComplex = method()
bartnetteSphereComplex PolynomialRing := SimplicialComplex => S -> (
    if numgens S < 8 then 
	error "-- expected a polynomial ring with at least 8 generators";
    simplicialComplex apply(hachimoriTable#-1, f -> product(f, i -> S_i))
    )

poincareSphereComplex = method()
poincareSphereComplex PolynomialRing := SimplicialComplex => S -> (
    if numgens S < 16 then
        error "-- expected a polynomial ring with at least 16 generators";    
    simplicialComplex apply(hachimoriTable#0, f -> product(f, i -> S_i))
    )

nonPiecewiseLinearSphereComplex = method()
nonPiecewiseLinearSphereComplex PolynomialRing := SimplicialComplex => S -> (
    if numgens S < 18 then
        error "-- expected a polynomial ring with at least 18 generators";    
    simplicialComplex apply(hachimoriTable#1, f -> product(f, i -> S_i))
    )

rudinBallComplex = method()
rudinBallComplex PolynomialRing := SimplicialComplex => S -> (
    if numgens S < 14 then
        error "-- expected a polynomial ring with at least 14 generators";    
    simplicialComplex apply(hachimoriTable#5, f -> product(f, i -> S_i))
    )

grunbaumBallComplex = method()
grunbaumBallComplex PolynomialRing := SimplicialComplex => S -> (
    if numgens S < 14 then
        error "-- expected a polynomial ring with at least 14 generators";    
    simplicialComplex apply(hachimoriTable#6, f -> product(f, i -> S_i))
    )

zieglerBallComplex = method()
zieglerBallComplex PolynomialRing := SimplicialComplex => S -> (
    if numgens S < 10 then
        error "-- expected a polynomial ring with at least 10 generators";    
    simplicialComplex apply(hachimoriTable#7, f -> product(f, i -> S_i))
    )

dunceHatComplex = method()
dunceHatComplex PolynomialRing := SimplicialComplex => S -> (
    if numgens S < 8 then
        error "-- expected a polynomial ring with at least 8 generators";    
    simplicialComplex apply(hachimoriTable#12, f -> product(f, i -> S_i))
    )

bjornerComplex = method()
bjornerComplex PolynomialRing := SimplicialComplex => S -> (
    if numgens S < 6 then
        error "-- expected a polynomial ring with at least 6 generators";    
    simplicialComplex apply(hachimoriTable#13, f -> product(f, i -> S_i))
    )

-- Frank Lutz has enumerated all 2 and 3-manifolds with less than 10 vertices;
-- see http://page.math.tu-berlin.de/~lutz/stellar/3-manifolds.html
small2ManifoldsFile := currentFileDirectory | "small2ManifoldsLibrary.txt"
small210ManifoldsFile := currentFileDirectory | "small210ManifoldsLibrary.txt"
small3ManifoldsFile := currentFileDirectory | "small3ManifoldsLibrary.txt"
smallManifoldsTable := memoize((d,v) -> (
	local manifoldsFile;
	if (d === 2 and v < 10) then manifoldsFile = small2ManifoldsFile
	else if (d === 2 and v === 10) then manifoldsFile = small210ManifoldsFile
	else if d === 3 then manifoldsFile = small3ManifoldsFile;
	if notify then stderr << "--loading file " << manifoldsFile << endl;
	hashTable apply( lines get manifoldsFile, x -> (
		x = value x;
        	(x#0,x#1) => x#2
		)
	    )
	)
    );
smallManifold = method()
smallManifold (ZZ,ZZ,ZZ,PolynomialRing) := SimplicialComplex => (d,v,i,S) -> (
    if d < 2 or d > 3 then
    	error "-- expected dimension two or three";
    if v < 4 or i < 0 then
        error "-- expected at least four vertices or nonnegative index";
    if d === 2 and v === 4 and i > 0 then
    	error "-- there is only one 2-manifold with four vertices.";
    if d === 2 and v === 5 and i > 0 then
    	error "-- there is only one 2-manifold with five vertices.";
    if d === 2 and v === 6 and i > 2 then
    	error "-- there are only three 2-manifolds with six vertices.";
    if d === 2 and v === 7 and i > 8 then
    	error "-- there are only nine 2-manifolds with seven vertices.";
    if d === 2 and v === 8 and i > 42 then
    	error "-- there are only 43 2-manifolds with eight vertices.";
    if d === 2 and v === 9 and i > 654 then
    	error "-- there are only 655 2-manifolds with nine vertices.";
    if d === 2 and v === 10 and i > 42425 then
    	error "-- there are only 42426 2-manifolds with ten vertices.";
    if d === 3 and v === 5 and i > 0 then
    	error "-- there is only one 3-manifold with five vertices.";
    if d === 3 and v === 6 and i > 1 then
    	error "-- there are only two 3-manifolds with six vertices.";
    if d === 3 and v === 7 and i > 4 then
    	error "-- there are only five 3-manifolds with seven vertices.";
    if d === 3 and v === 8 and i > 38 then
    	error "-- there are only 39 3-manifolds with eight vertices.";
    if d === 3 and v === 9 and i > 1296 then
    	error "-- there are only 1297 3-manifolds with nine vertices.";
    if d === 3 and v === 10 then
    	error "-- the database for 3-manifolds with ten vertices hasn't been included yet.";
    if v > 10 then 
        error "-- database doesn't include manifolds with more than ten vertices";
    if numgens S < v then 
        error ("-- expected a polynomial ring with at least " | toString v | " generators");
    -- Since Frank Lutz starts counting at 1 instead of 0, there is
    -- some appropriate index shifting.
    simplicialComplex apply((smallManifoldsTable(d,v))#(v,i+1), f -> product(f, i -> S_(i-1)))
    )

kleinBottleComplex = method()
kleinBottleComplex PolynomialRing := SimplicialComplex => S -> (
    if numgens S < 8 then
        error "-- expected a polynomial ring with at least 8 generators";
    smallManifold(2, 8, 12, S)
    )

realProjectiveSpaceComplex = method()
realProjectiveSpaceComplex (ZZ, PolynomialRing) := SimplicialComplex => (n, S) -> (
    if n == 0 and numgens S < 1 then
    	error "-- expected a polynomial ring with at least 1 generator";
    if n == 1 and numgens S < 3 then
    	error "-- expected a polynomial ring with at least 3 generators";
    if n == 2 and numgens S < 6 then
    	error "-- expected a polynomial ring with at least 6 generators";
    if n == 3 and numgens S < 11 then
    	error "-- expected a polynomial ring with at least 11 generators";
    if n == 4 and numgens S < 16 then
    	error "-- expected a polynomial ring with at least 16 generators";
    --if n > 4 and numgens S < 2^(n+1)-1 then
    --	error ("-- expected a polynomial ring with at least " | toString (2^(n+1)-1) | " generators");
    if n == 0 then return simplicialComplex {S_0};
    if n == 1 then return simplicialComplex {S_0*S_1, S_0*S_2, S_1*S_2};
    if n == 2 then return smallManifold(2, 6, 1, S);
    if n == 3 then (
	return simplicialComplex apply({{1,2,3,7},{1,4,7,9},{2,3,4,8},{2,5,8,10},{3,6,7,10},
		{1,2,3,11},{1,4,7,10},{2,3,4,11},{2,5,9,10},{3,6,8,9},{1,2,6,9},{1,4,8,9},
		{2,3,7,8},{2,6,9,10},{3,6,9,10},{1,2,6,11},{1,4,8,10},{2,4,6,10},{3,4,5,9},
		{4,5,6,7},{1,2,7,9},{1,5,6,8},{2,4,6,11},{3,4,5,11},{4,5,6,11},{1,3,5,10},
		{1,5,6,11},{2,4,8,10},{3,4,8,9},{4,5,7,9},{1,3,5,11},{1,5,8,10},{2,5,7,8},
		{3,5,9,10},{4,6,7,10},{1,3,7,10},{1,6,8,9},{2,5,7,9},{3,6,7,8},{5,6,7,8}}, f -> product(f, i -> S_(i-1)))
	);
    if n == 4 then (
        return simplicialComplex apply({{2,5,6,13,14},{0,7,8,11,14},{0,2,7,11,12},{2,3,5,12,14},{0,3,9,12,13},{0,6,9,11,12},{5,7,9,13,15},{0,4,8,10,14},{0,4,8,10,15},{4,6,9,10,11},{2,3,5,10,12},{1,5,8,11,15},
		{2,4,7,11,12},{2,4,7,11,13},{6,8,9,12,13},{1,7,9,10,14},{2,6,8,10,13},{0,1,6,12,13},{0,3,4,10,15},{2,6,8,10,15},{3,7,8,11,13},{3,7,8,11,14},{1,2,6,13,14},{6,8,9,10,13},{2,3,6,14,15},
		{4,6,7,11,12},{2,3,5,6,10},{2,4,5,12,14},{2,3,5,6,14},{3,7,8,9,13},{3,7,8,9,14},{1,2,9,14,15},{0,2,8,11,15},{3,6,7,14,15},{1,3,8,12,13},{0,1,4,13,14},{0,5,6,13,14},{1,5,9,11,15},{2,3,6,10,15},
		{3,7,9,13,15},{2,7,8,10,13},{3,5,8,12,14},{0,2,3,9,12},{0,5,6,11,14},{0,2,3,9,15},{0,5,6,9,11},{4,8,9,12,14},{1,5,7,12,15},{0,5,6,9,13},{0,1,7,10,12},{3,4,6,7,11},{0,1,7,10,14},{0,2,9,11,12},
		{3,4,6,7,15},{1,5,7,10,12},{0,2,9,11,15},{4,6,8,9,10},{4,8,9,10,14},{4,6,8,9,12},{3,4,7,13,15},{3,5,6,11,14},{5,6,9,10,11},{1,6,7,14,15},{5,6,9,10,13},{3,4,7,11,13},{2,4,9,11,12},{4,5,7,12,15},
		{1,3,4,10,11},{0,3,9,13,15},{0,6,9,12,13},{1,6,7,12,15},{0,2,7,10,12},{7,8,9,10,13},{0,7,8,10,14},{0,4,5,8,14},{0,4,5,8,15},{7,8,9,10,14},{1,5,8,12,15},{4,6,9,11,12},{0,3,4,13,15},{0,1,3,12,13},
		{0,2,7,8,10},{1,2,8,11,13},{2,5,6,10,13},{0,2,7,8,11},{1,2,8,11,15},{0,1,3,10,12},{0,1,6,13,14},{1,4,9,10,11},{5,7,9,10,13},{1,4,9,10,14},{1,2,6,14,15},{0,6,7,11,12},{4,6,7,12,15},{2,4,5,13,14},
		{0,6,7,11,14},{4,5,8,12,14},{4,5,8,12,15},{0,5,8,11,14},{1,3,5,10,11},{0,5,8,11,15},{1,3,5,10,12},{1,6,8,12,13},{1,6,8,12,15},{2,3,9,14,15},{0,2,8,10,15},{1,3,8,11,13},{1,3,5,8,11},{1,3,5,8,12},
		{3,7,9,14,15},{2,7,8,11,13},{1,2,4,13,14},{0,1,6,7,12},{0,1,3,4,10},{0,2,3,10,12},{0,1,6,7,14},{1,5,9,10,11},{2,5,7,10,12},{0,1,3,4,13},{2,5,7,10,13},{0,2,3,10,15},{1,2,6,8,13},{1,2,9,11,15},
		{2,3,9,12,14},{1,2,6,8,15},{3,6,7,11,14},{3,4,6,10,11},{0,1,4,10,14},{3,4,6,10,15},{2,4,5,7,12},{2,4,5,7,13},{1,2,4,11,13},{3,5,8,11,14},{0,5,9,13,15},{4,6,8,12,15},{1,2,4,9,11},{1,2,4,9,14},
		{0,5,9,11,15},{4,6,8,10,15},{0,4,5,13,14},{0,4,5,13,15},{1,5,7,9,10},{1,5,7,9,15},{2,4,9,12,14},{3,5,6,10,11},{4,5,7,13,15},{3,8,9,12,13},{3,8,9,12,14},{1,3,4,11,13},{1,7,9,14,15}}, 
	    f -> product(f, i-> S_i))
	);
    if n > 4 then error "--not yet implemented";
    )
------------------------------------------------------------------------------
-- more advanced constructors 
------------------------------------------------------------------------------
prune SimplicialComplex := SimplicialComplex => opts -> D -> (
    V := vertices D;
    if gens ring D === V then return D;
    R := (coefficientRing D)(monoid[V]);
    if dim D < -1 then return simplicialComplex monomialIdeal(1_R);
    if dim D === 1 then return simplicialComplex {1_R};
    simplicialComplex for x in facets D list (
	if sum first exponents x =!= 1 then substitute(x, R) else continue)
    )

inducedSubcomplex = method()
inducedSubcomplex (SimplicialComplex,List) := SimplicialComplex => (D, V) -> (
    if any(V, v -> not member(v, vertices D)) then 
	error "expected vertices of the simplicial complex";
    if dim D < -1 then return D;
    S := ring D;
    phi := map(S, S, for x in gens S list if member(x, V) then x else 1_S);
    -- although map(D, phi) is not a well-defined simplicial map, its image is
    -- nevertheless the induced complex
    image map(D, phi)
    )

dual SimplicialComplex := SimplicialComplex => {} >> opts -> D -> (
    -- Alexander duality for monomial ideals in part of the 'Core'    
    simplicialComplex dual monomialIdeal D)

link = method()
link (SimplicialComplex, RingElement) := SimplicialComplex => (D, f) -> (
    if isUnit f then return D;
    simplicialComplex ((monomialIdeal support f) + (monomialIdeal D : monomialIdeal f))
    )

-- 'skeleton' method defined in the `Polyhedra' package
skeleton (ZZ, SimplicialComplex) := SimplicialComplex => (n, D) -> (
    S := ring D;
    if n < -1 then return simplicialComplex monomialIdeal 1_S;
    if n === -1 then return simplicialComplex {1_S};
    if n >= dim D then return D;
    simplicialComplex flatten for i to n list faces (i,D)
    )

star = method ()
star (SimplicialComplex, RingElement) := (S, f) -> (simplicialComplex(monomialIdeal(S):monomialIdeal(f)))

-- The simplicial join of two simplicial complexes defined over different rings 
SimplicialComplex * SimplicialComplex := (D, D') -> (
     S := ring D ** ring D';
     fromD := map(S, ring D);
     fromD' := map(S, ring D');
     simplicialComplex monomialIdeal(fromD ideal D + fromD' ideal D')
     )

wedge = method(Options => true);
wedge (SimplicialComplex, SimplicialComplex, RingElement, RingElement) := SimplicialComplex => {Variables => {}} >> opts -> (D,E,u,v) -> (
    if not coefficientRing D === coefficientRing E then 
        error "expected the simplicial complexes to have the same coefficient rings";
    if not member(u, vertices D) or not member(v,vertices E) then 
        error ("expected " | toString u | " to be a vertex in " | toString D);
    if not member(u, vertices D) or not member(v,vertices E) then 
        error ("expected " | toString v | " to be a vertex in " | toString E);  
    RD := ring D;
    nD := numgens RD;
    RE := ring E;  
    nE := numgens RE;
    RW := if # opts.Variables > 0 then (
	      n := nD + nE - 1;
	      if # opts.Variables < n then
	          error ("expected the optional list to have at least " | toString n | " variables");
              (coefficientRing RD)(monoid[opts.Variables])
	      ) 
    	  else (coefficientRing RD)(monoid[join(gens RD, delete(v, gens RE))]);
    uIndex := position(gens RD, x -> x == u);
    vIndex := position(gens RE, y -> y == v);
    includeD := map(RW, RD, (gens RW)_{0..nD-1});
    includeE := map(RW, ring E, for i to nE - 1 list (
	            if i < vIndex then RW_(nD + i)
	            else if i == vIndex then RW_uIndex
	            else RW_(nD + i - 1))
		);
    facetsD := apply(facets D, F -> includeD(F));
    facetsE := apply(facets E, F -> includeE(F));
    simplicialComplex(facetsD | facetsE)
    )

------------------------------------------------------------------------------
-- basic properties and invariants
------------------------------------------------------------------------------
-- 'vertices' method is defined in 'Polyhedra" package
vertices SimplicialComplex := D -> (
    if dim D < 0 then {}
    else support product facets D
    )
-- vertices Face := F -> F.vertices


-- 'faces' method defined in Polyhedra
faces (ZZ, SimplicialComplex) := List => (i, D) -> (
    S := ring D;
    if not D.cache.?faces then (
	D.cache.faces = new MutableHashTable;
	E := (coefficientRing S)(monoid[gens S, SkewCommutative => true]);
	phi := map(E, S, gens E);
	J := phi(ideal D);
	D.cache.faces.ring = E/J;
	);
    if not D.cache.faces#?i then (
    	D.cache.faces#i = first entries( 
	    if dim D < -1 or i < -1 or i > dim D then 
	        matrix(S, {{}})
    	    else if i === -1 then matrix {{1_S}}
    	    else (
	    	R := D.cache.faces.ring;
	    	sub(matrix basis(i+1, R), vars S)
	    	)
	    )
	);
    D.cache.faces#i
    )
faces SimplicialComplex := HashTable => D -> (
    hashTable apply(toList(-1..dim D), i -> i => faces(i, D))
    )

-- method defined in the Polyhedra package
isPure SimplicialComplex := Boolean => D -> (
     F := facets D;
     L := unique apply(F, m -> # support m);
     #L <= 1
     )
 
connectedComponents = method()
connectedComponents SimplicialComplex := D -> ( 
    if dim D < 0 then return {D};
    R := ring D;
    H := new MutableHashTable;
    for F in facets D do (
    	if all(keys H, h -> gcd(F, H#h) == 1_R) then H#F = F
    	else (
	    L := {};
	    for k in keys H do if not (gcd(F, H#k) == 1_R) then L = append(L, k);
	    m := lcm append(L, F);
	    for l in L do remove(H, l);
	    H#m = m;
	    )
	);
    for k in keys H list inducedSubcomplex(D, support H#k)
    ) 

-- 'fVector' method defined in Polyhedra
fVector SimplicialComplex := List => D -> (
    -- the `faces' methods creates the face ring as a quotient of the exterior algebra
    if not D.cache.?faces then faces(-1,D);
    d := dim D;
    -- the void complex is exceptional
    if d < -1 then return {0};
    apply(toList(0..1+dim D), i -> hilbertFunction(i, D.cache.faces.ring))
    )
    
flagfVector = method()
flagfVector SimplicialComplex := HashTable => D -> (
    T := newRing(ring D/ideal D, SkewCommutative => true);
    multidegrees := sort apply(flatten entries basis T, m -> degree m);
    hashTable for m in multidegrees list m => hilbertFunction(m, T)
    )

flagfVector (List, SimplicialComplex) := ZZ => (m, D) -> (
    T := newRing(ring D/ideal D, SkewCommutative => true);
    hilbertFunction(m, T)
    )

isProper = method()
isProper SimplicialComplex := Boolean => D -> (
    M := matrix unique degrees ring D;
    if numRows M =!= rank M then return false;
    newDegrees := (matrix degrees ring D)*(inverse M);
    E := sub(D, newRing(ring D, Degrees => entries newDegrees));
    all(flatten for F in faces(1,E) list degree F, i -> i < 2)
    )

------------------------------------------------------------------------------
-- Associated chain complexes
------------------------------------------------------------------------------
importFrom_Core { "raw", "rawIndices", "rawKoszulMonomials" }

-- local function
lcmM = L -> (
    -- lcmM finds the lcm of a list of monomials; the quickest method Sorin knows
    m := intersect toList (L/(i -> monomialIdeal(i)));
    m_0)

-- local function
makeLabels = (D, L, i, S) -> (
    -- D is a simplicial complex
    -- L is a list of monomials in S
    -- i is an integer
    -- S is a ring
    Vertices := vertices D;
    F := faces(i,D);
    if #F === 0 then matrix{{1_S}} 
    else matrix {apply(F, m -> (
		s := rawIndices raw m;
		lcmM L_(apply(s, i -> position(Vertices, j -> index j == i)))
		))}
    )

boundaryMap = method(Options => {Labels => {}})
boundaryMap (ZZ, SimplicialComplex) := opts -> (r, D) -> (
    L := opts.Labels;
    if L =!= {} then (
	if #L =!= # vertices D then 
	    error "-- expected labels to correspond to vertices";
	if any(L, m -> size m > 1) then 
	    error "-- expected Labels to be a list of monomials";
	S := ring L#0;
	Sext := S(monoid [Variables => #L]);
	L = apply(#L, i -> L_i*Sext_i);
	ones := map(S, Sext, toList(#L:1_S));
	m1 := makeLabels(D, L, r, Sext);
	m2 := if r =!= 0 then makeLabels(D, L, r-1, Sext)
	      else matrix{{1_Sext}};
	F := source map(S^1, , ones m2);
	bd := ones map(Sext, rawKoszulMonomials(numgens Sext, raw m2,raw m1));
	bd = map(F, , bd);
	bd
	)
    else (
    	R := ring D;	
	b1 := sub(matrix{faces(r, D)}, R);
	b2 := sub(matrix{faces(r-1, D)}, R);
	ones = map(coefficientRing R, R, toList(numgens R:1));
	ones map(R, rawKoszulMonomials(numgens R, raw b2, raw b1))
	)
    )

complex SimplicialComplex := Complex => {Labels => {}} >> opts -> (
    cacheValue(symbol complex => opts)) (D -> (
    	d := dim D;
    	C := if d < -1 then (complex(coefficientRing D)^0)[-1]
    	     else if d === -1 then complex (coefficientRing D)^1
    	     else complex apply(toList(0..d), r -> boundaryMap(r, D, Labels => opts.Labels));
    	if opts.Labels === {} then C[1] 
	else C
    	)
    )

homology(ZZ, SimplicialComplex, Ring) := Module => opts -> (i, Delta, R) -> (
    homology(i, (complex Delta) ** R)
    )
 
homology(ZZ, SimplicialComplex) := Module => opts -> (i, Delta) -> (
    homology(i, complex Delta)
    )
 
homology(Nothing, SimplicialComplex, Ring) :=
homology(SimplicialComplex, Ring) := Complex => opts -> (Delta, R) -> (
    homology(complex Delta ** R)
    )

homology(Nothing, SimplicialComplex) :=
homology SimplicialComplex := Complex => opts -> Delta -> (
    homology(complex Delta)
    )

cohomology(ZZ, SimplicialComplex, Ring) := Module => opts -> (i, Delta, R) -> (
    cohomology(i, Hom(complex Delta ** R, module R))
    )

cohomology(ZZ, SimplicialComplex) := Module => opts -> (i,Delta) -> (
    cohomology(i, Hom(complex Delta, module coefficientRing Delta))
    )



-- helper functions for algebraicShifting. Not exported.
shiftMonomial = (m) -> (
    variables := flatten entries vars ring m;
    D := unique degrees ring m;
    P := apply(D, d -> flatten entries basis(d, ring m));
    f := (Q, v) -> {position(Q, q -> member(v, q)), position(Q_(position(Q, q -> member(v,q))), b -> b == v)};
    multisupp := MultiSupp(m);
    deg := degree(m);
    auxlist := flatten apply(deg, d -> toList(0..d-1));
    sm := 1;
    apply(auxlist, multisupp, (i, j) -> sm = sm * (P_((f(P, j))_0))_((f(P, j))_1+i) );
    return sm
    );

MultiSupp = (m) -> (
    multisupp := {};
    while m != 1 do (multisupp = append(multisupp, (support(m))_0);
        m=m//((support(m))_0););
    return multisupp
    );


shift = (I) -> (
    shiftgens := apply( I_*, g -> shiftMonomial(g));
    return ideal shiftgens
    );


-- Compute the algebraic shifting of a simplicial complex and the
-- colored shifting if the ring is multigraded.
algebraicShifting = method (Options => {Multigrading => false})
algebraicShifting SimplicialComplex := opts -> S -> (
    if not opts.Multigrading then (
    	R := newRing(ring S, Degrees => {#(gens ring S):1});
    	f := map(R, ring S);
    	g := map(ring S, R);
    	J := g(shift(gin(f(ideal S), Multigraded => opts.Multigrading)));
    	return simplicialComplex monomialIdeal J
    	)
    else (
	M := matrix unique degrees ring S;
    	newDegrees := (matrix degrees ring S)*(inverse M);
    	E := sub(S, newRing(ring S, Degrees => entries newDegrees));
        sI := monomialIdeal shift(gin(ideal E, Multigraded => opts.Multigrading));
    	return sub(simplicialComplex sI, ring S)
    	)
    )

-- substitute a complex to another ring
substitute(SimplicialComplex, PolynomialRing) := SimplicialComplex => (D, R) -> (
    if ideal D === ideal(1_(ring D))
    then (
	I := sub(ideal D, R);
	simplicialComplex monomialIdeal I
	)
    else (
	n := numgens ring D;
    	simplicialComplex for F in facets D list sub(F, (vars R)_{0..n-1})
	)
    )

--------------------------------------------------------------------------
-- monomial resolutions        
--------------------------------------------------------------------------
-- lcmMRed and faceBuchberger are specific to buchbergerComplex
lcmMRed = method()
lcmMRed List := (L) -> (
-- lcmMRed finds the lcm of a list of monomial and drops the exponent on each
-- variable by one (if that exponent in nonzero).
    m := intersect toSequence apply(L, i-> monomialIdeal(i));
    m_0//(product support m_0)
    )
faceBuchberger = (m, L) -> (
-- true iff the monomial m defines a face in the Buchberger complex.  if x has
-- a variable with index greater than #L-1, then this code produces an error
     x := rawIndices raw m;
     mon := lcmMRed(L_x);
     all(L, n -> mon//n == 0)
     )
-- requires numgens R == #L. I dislike this. Currently not exported.
buchbergerComplex = method()
buchbergerComplex(List,Ring) := (L,R) -> (
    P := ideal apply(gens R, x -> x^2);
    nonfaces := {};
    d := 1;
    while (L1 := flatten entries basis(d,coker gens P); #L1 > 0) do (
	L1 = select(L1, m -> not faceBuchberger(m,L));
	-- I do not like this output to the screen
	-- << "new nonfaces in degree " << d << ": " << L1 << endl;	  
	nonfaces = join(nonfaces,L1);
	if #nonfaces > 0 then
	P = P + ideal nonfaces;
	d = d+1;
	);
    simplicialComplex monomialIdeal matrix(R, {nonfaces})
    )
buchbergerSimplicialComplex = method()
buchbergerSimplicialComplex(List, Ring) := (L,R) -> (
    if numgens R < #L 
    then error concatenate("expect ring to have at least ",toString(#L)," generators");
    -- trim squares and excess variables for quicker basis computations later on
    P := ideal join(apply(#L, i -> R_i^2),apply(#L..numgens R-1, i -> R_i));
    -- excess variables in R are always nonfaces
    nonfaces := toList apply(#L..numgens R-1, i -> R_i);
    d := 2;
    -- We find the nonfaces in each degree
    while (L1 := flatten entries basis(d,coker gens P); #L1 > 0) do (
	L1 = select(L1, m -> not faceBuchberger(m,L));
	nonfaces = join(nonfaces,L1);
	if #nonfaces > 0 then
	P = P + ideal nonfaces;
	d = d+1;
	);    
    simplicialComplex monomialIdeal matrix(R, {nonfaces})
    )
buchbergerSimplicialComplex(MonomialIdeal, Ring) := (I,R) -> (
    buchbergerSimplicialComplex(flatten entries mingens I, R)
    )
buchbergerResolution = method()
buchbergerResolution List := Complex => M -> (
    -- handle degenerate cases first
    if monomialIdeal M == 0
    then return (ring(monomialIdeal M))^1[0];
    -- Construct the buchbergerSimplicial Complex and homogenize
    R := ZZ(monoid[vars(0..#M-1)]);
    B := buchbergerSimplicialComplex(M,R);
    complex(B,Labels=>M)
    )
buchbergerResolution MonomialIdeal := Complex => M -> (
    if M == 0
    then (ring M)^1[0]
    else buchbergerResolution(first entries mingens M)
    )

taylorResolution = method();
taylorResolution List := Complex => M -> (
    -- The Taylor resolution is the homogenization of the
    -- (numgens M - 1)-simplex. The implementation is
    -- straightforward once we've dealt with degenerate
    -- cases.
    if monomialIdeal M == 0
    then return (ring(monomialIdeal M))^1[0];
    if not all(M, m -> size m == 1) then 
    error "-- expected a list of monomials";
    if not all(M, m -> member(m,flatten entries mingens ideal M)) then 
    error "-- expected minimal generators of a monomial ideal";
    R := ZZ(monoid[vars(0..#M-1)]);
    Simplex := simplexComplex(#M-1,R);
    complex(Simplex,Labels=>M)
    )
taylorResolution MonomialIdeal := Complex => M -> (
    if M == 0
    then (ring M)^1[0]
    else taylorResolution(first entries mingens M)
    )


hasRoot = (F,L) -> (
    faceLabel := lcm L_(indices F);
    root := position(L,m -> faceLabel % m == 0);    
    member(root,indices F)
    )
lyubeznikSimplicialComplex = method(Options => {MonomialOrder => {}})
lyubeznikSimplicialComplex (List,Ring) := opts -> (M,A) -> (
    -- Deal with degenerate case first
    if M == {}
    then return simplicialComplex({1_A});
    -- Straightforward error checks
    if not all(M, m -> size m == 1) then 
    error "-- expected a list of monomials";
    if not all(M, m -> member(m,flatten entries mingens ideal M)) then 
    error "-- expected minimal generators of a monomial ideal";
    if not class A === PolynomialRing then
    error"-- expected a polynomial ring";
    L := M;
    if not opts.MonomialOrder == {} 
    then(
	if not sort opts.MonomialOrder == toList(0..#M-1) then
	error concatenate("-- MonomialOrder should be a permutation of {0,...,",toString(#M-1),"}");
 	L = M_(opts.MonomialOrder)
	);
    P := ideal join(apply(#L, i -> A_i^2),apply(#L..numgens A-1, i -> A_i));
    -- excess variables in A are always nonfaces
    nonfaces := toList apply(#L..numgens A-1, i -> A_i);
    d := 2;
    -- We look for nonrooted faces. The nonfaces also define the simplicial 
    -- complex and adding the nonfaces to P and taking the basis mod P, we
    -- reduce the number of faces we need to check for rootedness.
    while (dBasis := flatten entries basis(d, coker gens P); #dBasis > 0) do (
	newNonfaces := select(dBasis, F -> not hasRoot(F,L));
	nonfaces = join(nonfaces, newNonfaces);
	if #nonfaces > 0 then P = P + ideal(nonfaces);
	d = d+1;
	);
    simplicialComplex monomialIdeal matrix(A,{nonfaces})
    )
-- Natural implementation for a monomial ideal.
lyubeznikSimplicialComplex(MonomialIdeal,Ring) := opts -> (I,R) -> (
    minGens := first entries mingens I;
    lyubeznikSimplicialComplex(minGens, R, MonomialOrder => opts.MonomialOrder)
    )
-- We can construct the lyubeznik Resolution by first building the
-- lyubeznikSimplicialComplex and then homogenizing. The MonomialOrder option
-- seems silly, as lists are implicitly ordered, but this option is needed
-- when we give a method for the MonomialIdeal class.
lyubeznikResolution = method(Options => {MonomialOrder => {}})
lyubeznikResolution List := opts -> L -> (
    MO := opts.MonomialOrder;
    R := QQ(monoid[vars(0..#L-1)]);
    if opts.MonomialOrder == {}
    then complex(lyubeznikSimplicialComplex(L,R),Labels=>L)
    else complex(lyubeznikSimplicialComplex(L,R),Labels=>L_MO)
    )
lyubeznikResolution MonomialIdeal := opts -> I -> (
    -- if I == monomialIdeal(0), then the set of mingens in empty and M2
    -- doesn't know what ring to use when constructing the
    -- resolution. Otherwise straightforward.
    if numgens I == 0 
    then return ((ring I)^1)[0];
    MinGens := flatten entries mingens I; 
    MO := opts.MonomialOrder;
    R := QQ(monoid[vars(0..#(mingens I)-1)]);
    if opts.MonomialOrder == {}
    then return(
	complex(lyubeznikSimplicialComplex(I,R,MonomialOrder=>MO),Labels=>MinGens)
	)
    else return(
	 complex(lyubeznikSimplicialComplex(I,R,MonomialOrder=>MO),Labels=>MinGens_MO)
	 )
     )

scarfSimplicialComplex = method()
scarfSimplicialComplex (List,Ring) := (L,A) -> (
    -- The scarfSimplicialComplex is the subcomplex of the (#L-1)-simplex
    -- whose faces have unique multidegrees. These unique multidegrees
    -- are called Scarf multidegrees.
    faceList := drop(subsets L, 1);
    faceLabels := for F in faceList list lcm F;
    scarfMultidegrees := for m in faceLabels list(
    	if #positions(faceLabels, k -> k == m) > 1
    	then continue
    	else m
    	);
    scarfFaces := for F in faceList list(
    	if member(lcm F, scarfMultidegrees)
    	then F
    	else continue
    	);
    simplicialComplex(for F in scarfFaces list(
	    vertexLabel := m -> position(L,k -> k == m);
	    product(for m in F list A_(vertexLabel m))
	    )
    	)
    )
-- natural functionality for a MonomialIdeal
scarfSimplicialComplex (MonomialIdeal,Ring) := (I,A) -> (
    if numgens I == 0 
    then return ((coefficientRing(ring I))^1)[0];
    scarfSimplicialComplex(first entries mingens I,A)
    )
-- The scarfChainComplex is the homogenization of the scarfSimplicial complex.
scarfChainComplex = method()
scarfChainComplex List := L ->(
    A := QQ(monoid[vars(0..#L-1)]);
    complex(scarfSimplicialComplex(L,A), Labels=>L)
    )
scarfChainComplex MonomialIdeal := I -> (
    if numgens I == 0 
    then return ((ring I)^1)[0];
    A := QQ(monoid[vars(0..#(mingens I)-1)]);
    complex(scarfSimplicialComplex(I,A), Labels=>(first entries mingens I))
    )


------------------------------------------------------------------------------
-- Simplicial Maps
------------------------------------------------------------------------------
SimplicialMap = new Type of HashTable
SimplicialMap.synonym = "map of abstract simplicial complexes"
source SimplicialMap := SimplicialComplex => f -> f.source
target SimplicialMap := SimplicialComplex => f -> f.target
map SimplicialMap := RingMap => opts -> f -> f.map
matrix SimplicialMap := Matrix => opts -> f -> matrix map f

expression SimplicialMap := f -> (expression map) (expression (target f, source f, first entries matrix f))
toString SimplicialMap := f -> toString expression f
net SimplicialMap := f ->  net matrix f
texMath SimplicialMap := f -> texMath expression f

SimplicialMap#{Standard,AfterPrint} = SimplicialMap#{Standard,AfterNoPrint} = f -> (
    << endl;	-- double space
    << concatenate(interpreterDepth:"o") << lineNumber << " : SimplicialMap ";
    << net target f << " <--- " << net source f << endl;
    )

map(SimplicialComplex, SimplicialComplex, Matrix) := SimplicialMap => opts -> (E, D, A) -> (
    if ring A =!= ring E then 
        error "-- expected a matrix over the ring of the target";
    if rank target A =!= 1 then 
        error "-- expected the matrix to have 1 row"; 
    n := numgens ring D; 
    if rank source A =!= n then 
        error("-- expected the matrix to have " | n | " columns");	
    if coefficientRing D =!= coefficientRing E then
        error("-- expected the source and target to have the same coefficient ring");
    new SimplicialMap from {
    	symbol source => D,
    	symbol target => E,
    	symbol map => map(ring E, ring D, A),
    	symbol cache => new CacheTable}
    )
map(SimplicialComplex, SimplicialComplex, List) := SimplicialMap => opts -> (E, D, A) -> (
    map(E, D, matrix {A})
    )
map(SimplicialComplex, SimplicialComplex, RingMap) := SimplicialMap => opts -> (E,D,phi) -> (
    map(E, D, matrix phi)
    )
map(SimplicialComplex, Matrix) := SimplicialMap => opts -> (D,A) -> (
    phi := map(ring D, A);
    Image := simplicialComplex(for F in facets D list phi(F));
    map(Image,D,A)
    )
map(SimplicialComplex, List) := SimplicialMap => opts -> (D,A) -> (
    map(D, matrix A)
    )
map(SimplicialComplex, RingMap) := SimplicialMap => opts -> (D,phi) -> (
    map(D,matrix phi)
    )

SimplicialComplex#id = D -> map(D, D, vars ring D)

isWellDefined SimplicialMap := Boolean => f -> (
    -- CHECK DATA STRUCTURE
    -- check keys
    K := keys f;
    expectedKeys := set{symbol source, symbol target, symbol map, symbol cache};
    if set K =!= expectedKeys then (
	if debugLevel > 0 then (
	    added := toList(K - expectedKeys);
	    missing := toList(expectedKeys - K);
	    if #added > 0 then 
	        << "-- unexpected key(s): " << toString added << endl;
	    if #missing > 0 then 
	        << "-- missing keys(s): " << toString missing << endl);
    	return false
	);
    --Check types
    if not instance(f.source, SimplicialComplex) then (
	if debugLevel > 0 then (
	    << "-- expected the source to be a SimplicialComplex" << endl);
	return false	);
    if not instance(f.target, SimplicialComplex) then (
	if debugLevel > 0 then (
	    << "-- expected the target to be a SimplicialComplex" << endl);
	return false
	);
    if not instance(f.map, RingMap) then (
	if debugLevel > 0 then (
	    << "-- expected the map to be a RingMap" << endl);
	return false
	);
    if not instance(f.cache, CacheTable) then (
    	if debugLevel > 0 then (
	    << "-- expected cache to be a CacheTable" << endl);
    	return false
	);    
    --Check mathematical structure
    D := source f;
    E := target f;
    g := map f;
    -- check ring map
    if source g =!= ring D then (
    	if debugLevel > 0 then (
	    << "-- expected source of the underlying ring map to be the ring of the source");
	return false	
	);
    if target g =!= ring E then (
    	if debugLevel > 0 then (
	    << "-- expected target of the underlying ring map to be the ring of the target");
	return false	
	);
    -- check that coefficient rings agree
    if coefficientRing ring D =!= coefficientRing ring E then (
	if debugLevel > 0 then (
	    << "-- expected the coefficient rings of the Stanley-Reisner rings to be equal");
	return false
	);
    -- check that vertices map to vertices
    if not all(vertices D, m -> member (g m, vertices E)) then (
    	if debugLevel > 0 then (
	    << "-- expected image of a vertex to be a vertex");
	return false	
	);
    -- check that the image of a face is a face
    if not all(facets D, m -> any(facets E, e -> e % (product support g m) == 0))
    then (
    	if debugLevel > 0 then (
	    << "-- expected image of a face to be a face");
	return false
	);
    true    
    )

complex SimplicialMap := ComplexMap => {} >> opts -> f -> (
    D := source f;
    E := target f;
    CD := complex D;
    CE := complex E;
    kk := coefficientRing D;
    EE := kk(monoid[gens ring E, SkewCommutative => true]);
    ED := kk(monoid[gens ring D, SkewCommutative => true]);
    coefEE := map(kk,EE, for i in gens EE list 1);
    phi := map f;
    psi := map(EE,ED,sub(matrix f,EE));
    g := i -> (
	if i === -1 then map(CE_(-1), CD_(-1), 1) else (
	    map(CE_i, CD_i, matrix table(faces(i, E), faces(i, D),
		    (n,m) -> (
			if phi m == n
			then coefEE(psi(sub(m,ED)))
			else 0
			)
		    )     
		)
	    )
	);
    map(CE, CD, g)
    )

-- TODO: If we have constructed the barycentric subdivision, but want to
-- change rings, can we access some cached data here to make the computation
-- faster?
barycentricSubdivision = method();
barycentricSubdivision (SimplicialComplex, Ring) := SimplicialComplex => (D,S) -> (
    if dim D == -infinity then return simplicialComplex(monomialIdeal(1_S));
    if numgens S < numFaces D - 1
    then error(" -- expected the ring to have at least " | numFaces D - 1 | " generators");
    faceList := flatten for i to dim D + 1 list faces(i, D);
    baryFacets := flatten for F in facets D list(
	for vertexList in permutations(support F) list(
	    L := apply(#vertexList, i -> product vertexList_{0..i});
    	    if L === {} then 1_S
	    else product apply(L, l -> S_(position(faceList, j -> j == l)))
	    )
	);
    simplicialComplex baryFacets
    )

-- TODO: Unexported? I forgot if that's intended
numFaces = method()
numFaces SimplicialComplex := ZZ => D -> (
    sum(-1 .. dim D, i -> #faces(i,D))
    )
-- TODO: Are we using cached data to construct the barycentric subdivisions for
-- the source and target?
barycentricSubdivision (SimplicialMap, Ring, Ring) := SimplicialMap => (f,S,R) -> (
    D := source f;
    E := target f;
    faceListSource := flatten for i to dim D + 1 list faces(i,D);
    faceListTarget := flatten for i to dim E + 1 list faces(i,E);
    variableList := for F in faceListSource list(
       	S_(position(faceListTarget, G -> G == product support (map f)(F)))
	);
    map(barycentricSubdivision(target f, S), barycentricSubdivision(source f, R), 
	variableList | toList(numgens R - #variableList : 1_S)
	)
    )

isInjective SimplicialMap := Boolean => f -> (
    #vertices(source f) == #unique for x in vertices source f list (map f)(x)
    )
isSurjective SimplicialMap := Boolean => f -> facets image f === facets target f

image SimplicialMap := SimplicialComplex => f -> (
    simplicialComplex for F in facets(source f) list 
        sub(product support (map f)(F), ring target f)
    )

homology(Nothing, SimplicialMap) :=
homology SimplicialMap := ComplexMap => opts -> f -> (
    homology(complex f)
    )
homology(ZZ, SimplicialMap) := Matrix => opts -> (i,f) -> homology(i, complex f)

homology(ZZ, SimplicialComplex, SimplicialComplex) := Module => opts -> (i,D,E) -> (
    inclusion := map(D, E, gens ring D);
    C := coker complex inclusion;
    homology(i,C)
    )

homology(Nothing, SimplicialComplex, SimplicialComplex) :=
homology(SimplicialComplex, SimplicialComplex) := Complex => opts -> (D,E) -> (
    inclusion := map(D, E, gens ring D);
    C := coker complex inclusion;
    homology C
    )

cohomology(ZZ, SimplicialMap) := Matrix => opts -> (i,f) -> (
    cohomology(i, Hom(complex f, module coefficientRing source f))
    )

cohomology(ZZ, SimplicialComplex, SimplicialComplex) := Module => opts -> (i,D,E) -> (
    inclusion := map(D, E, gens ring D);
    C := coker complex inclusion;
    cohomology(i, Hom(C, module coefficientRing D))
    )

elementaryCollapse = method();
elementaryCollapse (SimplicialComplex,RingElement) := SimplicialComplex => (D,F) -> (
    if not size F == 1 then error "The second argument should be a monomial representing a face";
    facetsContainingF := {};
    for G in facets D do(
	if G % F == 0 then facetsContainingF = append(facetsContainingF,G)
	);
    if #facetsContainingF === 0 then error "this face does not belong to the simplicial complex";
    if #facetsContainingF > 1 then error "can not collapse by this face";
    G := first facetsContainingF;
    if first degree F =!= first degree G - 1 then error "this face is not a maximal proper subface of a facet";
    newFacetList := delete(G, facets D);
    for m in subsets(support G, first degree G - 1) do (
	if product m =!= F 
	then newFacetList = append(newFacetList, product m)
	else continue
	);
    simplicialComplex newFacetList
    )



-----------------------------------------------------------------------
-- Defining a class Face
-- to be used in other package in particular in the KustinMiller package
-- additions by Janko
-----------------------------------------------------------------------

Face = new Type of MutableHashTable
viewHelp
-- vertices of a face
-- vertices=method()

-- pretty print
net Face := (f)-> (
v:=vertices(f);
if #v==0 then return(net({}));
horizontalJoin(apply(v,j->net(j)|net(" "))))

-- after print
Face#{Standard,AfterPrint} = m -> (
  n:=#vertices(m);
  if n==0 then vstr:="empty face";
  if n==1 then vstr="face with "|n|" vertex";
  if n>1 then vstr="face with "|n|" vertices";
      << endl;
      << concatenate(interpreterDepth:"o") << lineNumber << " : "
      << vstr|" in "|net(ring m)
      << endl;)

-- dimension
dim Face := F->-1+#(vertices F)

-- ring of a face
ring Face :=F->F.ring;

-- construct a face from a List of vertices
face=method()
face(List):= (L)-> new Face from {symbol vertices => L, symbol ring=> ring L#0}
face(List,PolynomialRing):= (L,R)-> new Face from {symbol vertices => L, symbol ring=>R}

-- construct a face from a monomial
face(RingElement):= (m)-> face(support m,ring m)

-- compare two faces
Face == Face := (F,G)->(
(#(vertices F))==(#(vertices G)) and set vertices F === set vertices G)

-- test if a face is a subface of another
isSubface=method()
isSubface(Face,Face):=(F,G)->(
isSubset(set vertices F,set vertices G))

-- test if a face is a face of a complex
isFaceOf = method()
isFaceOf (Face,SimplicialComplex) := (F,C) -> (
    fc := facets(C);    
    #(select(1,fc, G -> isSubface(F,G)))>0
    )

-- substitute a face to another ring
substitute(Face,PolynomialRing):=(F,R)->(
v:=vertices(F);
face(apply(v,j->sub(j,R)),R))


    
    
    
-*    

simplicialComplex for F in facets void list sub(F, (vars R)_{0..4})

ideal void
ideal irrelevant


sub(ideal void, newRing(R, Degrees => {1,1,1,1,1}))
needsPackage "SimplicialComplexes"
R = QQ[a..e]
D = simplicialComplex monomialIdeal(a*b*c*d*e)
substitute(D,R)
faces(1,D)
F = (first entries faces(1,D, Output => List))#0
isFaceOf(F,D)
*-


