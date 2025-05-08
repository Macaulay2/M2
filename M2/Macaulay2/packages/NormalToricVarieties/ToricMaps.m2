------------------------------------------------------------------------------
-- Toric Maps
------------------------------------------------------------------------------
ToricMap = new Type of HashTable
ToricMap.synonym = "toric map"
source ToricMap := NormalToricVariety => f -> f.source
target ToricMap := NormalToricVariety => f -> f.target
matrix ToricMap := Matrix => opts -> f -> f.matrix

net ToricMap := f -> net matrix f
ToricMap#{Standard,AfterPrint} = ToricMap#{Standard,AfterNoPrint} = f -> (
    << endl;	-- double space
    << concatenate(interpreterDepth:"o") << lineNumber << " : ToricMap ";
    << net target f << " <--- " << net source f << endl;
    )

map(NormalToricVariety, NormalToricVariety, Matrix) := ToricMap => opts -> (Y, X, A) -> (
    if ring A =!= ZZ then error "-- expected an integer matrix";
    if rank source A != dim X then 
        error("-- expected source to be the lattice of rank " | dim X);
    if rank target A != dim Y then 
        error("-- expected target to be the lattice of rank " | dim Y);  
    new ToricMap from {
    	symbol source => X,
    	symbol target => Y,
    	symbol matrix => A,
    	symbol cache => new CacheTable}
    )

map(NormalToricVariety, NormalToricVariety, ZZ) := ToricMap => opts -> (Y, X, i) -> (
    m := dim Y;
    n := dim X;
    if i === 0 then return map(Y, X, map(ZZ^m, ZZ^n, 0))
    else if m === n then return map(Y, X, map(ZZ^m, ZZ^n, i))
    else error "expect zero map, or the source and target to have same dimension"
    )
NormalToricVariety#id = X -> map(X,X,1)



-- THIS METHOD IS NOT EXPORTED.
-- computes and caches the supporting hyperplanes with outer normal vectors
-- of each cone
outerNormals = method()
outerNormals (NormalToricVariety, List) := List => (X, sigma) -> (
    if not X.cache.?outerNormals then (
    	X.cache.outerNormals = new MutableHashTable);
    if not X.cache.outerNormals#?sigma then (
    	V := transpose matrix rays X;
	(H0, H1) := fourierMotzkin V_sigma;
    	X.cache.outerNormals#sigma = {H0, H1};
	);
    X.cache.outerNormals#sigma
    )

-- THIS METHOD IS NOT EXPORTED.
-- covers the pair returned by 'fourierMotzkin' into a single matrix
outerMatrix = method()
outerMatrix List := Matrix => pair -> (
    if pair#1 == 0 then transpose pair#0 else 
	transpose (pair#0 | pair#1 | - pair#1)
    )
------------------------------------------------------------------------------

isWellDefined ToricMap := Boolean => f -> (
    -- CHECK DATA STRUCTURE
    -- check keys
    K := keys f;
    expectedKeys := set{symbol source, symbol target, symbol matrix, symbol cache};
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
    if not instance(f.source, NormalToricVariety) then (
	if debugLevel > 0 then (
	    << "-- expected `source' to be a NormalToricVariety" << endl);
	return false	);
    if not instance(f.target, NormalToricVariety) then (
	if debugLevel > 0 then (
	    << "-- expected `target' to be a NormalToricVariety" << endl);
	return false
	);
    if not instance(f.matrix, Matrix) then (
	if debugLevel > 0 then (
	    << "-- expected `matrix' to be a Matrix" << endl);
	return false
	);
    if ring matrix f =!= ZZ then (
    	if debugLevel > 0 then (
	    << "-- expected `matrix' over the integers" << endl);
	return false
	);	 
    if not instance(f.cache, CacheTable) then (
    	if debugLevel > 0 then (
	    << "-- expected `f.cache' to be a CacheTable" << endl);
    	return false
	);
    --Check mathematical structure
    X := source f;
    Y := target f;
    A := matrix f;
    if rank source A =!= dim X then (
    	if debugLevel > 0 then (
	    << "-- expected number of columns of the matrix to equal the dimension of the source variety"
	    );
	return false
	);
    if rank target A =!= dim Y then (
    	if debugLevel > 0 then (
	    << "-- expected number of rows of the matrix to equal the dimension of the target variety"
	    );
	return false
	);
    V := transpose matrix rays X;
    if not all(max X, sigma -> any(max Y, tau -> (
		normals := outerMatrix outerNormals(Y, tau);
		innerProducts := normals * A * V_sigma;		
		all(flatten entries innerProducts, b -> b <= 0)))) then (
    	if debugLevel > 0 then (
	    << "-- expected image of each maximal cone to be contained in some maximal cone");
	return false
	);
    true
    )

ToricMap * ToricMap := ToricMap => (g, f) -> (
    if target f =!= source g then error "-- expected composable maps";
    new ToricMap from {
    	symbol source => source f,
    	symbol target => target g,
    	symbol matrix => (matrix g) * (matrix f),
    	symbol cache => new CacheTable
	}
    )

ToricMap == ToricMap := Boolean => (f, g) -> (
    source f === source g and target f === target g and matrix f == matrix g
    )

NormalToricVariety ^ Array := ToricMap => (X, A) -> (
    if A === [] then (
	if not X.cache.?toricBlowup then error "expected a variety to be a blowup";
	return map(X.cache.toricBlowup, X, 1)	
	);
    if not X.cache.?components then (
	if A =!= [0] then error "expected a product variety"
	else return id_X
	);
    factors := X.cache.components;
    factorList := toList(0 .. # factors - 1);
    if not all(A, i -> member(i, factorList)) then error "expected the array to index factors";
    targetVariety := cartesianProduct apply(toSequence A, i -> factors#i);
    latticeMap := directSum apply(# factors, i ->     
	if member(i, A) then id_(ZZ^(dim factors#i))
	else 0 * id_(ZZ^(dim factors#i))
	);
    map(targetVariety, X, latticeMap ^ A)        
    )

NormalToricVariety _ Array := ToricMap => (X, A) -> (
    if A === [] then error "expect the array to index factors";
    if not X.cache.?components then (
	if A =!= [0] then error "expected a product variety"
	else return id_X
	);
    factors := X.cache.components;
    factorList := toList (0 .. # factors - 1);
    if not all(A, i -> member (i, factorList)) then error "expected the array to index factors";
    sourceVariety := cartesianProduct apply(toSequence A, i -> factors#i);
    latticeMap := directSum apply(# factors, i ->     
	if member(i, A) then id_(ZZ^(dim factors#i))
	else 0 * id_(ZZ^(dim factors#i))
	);
    map(X, sourceVariety, latticeMap _ A)        
    )

diagonalToricMap = method()
diagonalToricMap (NormalToricVariety, ZZ, Array) := ToricMap => (X, m, A) -> (
    factorList := toList(0 .. m-1);
    if A === [] then error "expect the array to index factors";    
    if not all(A, i -> member(i, factorList)) then error "expect the array to index factors";
    latticeMatrix := transpose matrix {
      	apply(m, i -> if member(i,A) then id_(ZZ^(dim X)) else 0 * id_(ZZ^(dim X)))
      	};
    map(X ^** m, X, latticeMatrix)
    )
diagonalToricMap (NormalToricVariety, ZZ) := ToricMap => (X, m) -> (
    A := new Array from (0..m-1);
    diagonalToricMap(X, m, A)
    )
diagonalToricMap NormalToricVariety := ToricMap => X -> diagonalToricMap(X, 2)

------------------------------------------------------------------------------
-- properties of toric maps
------------------------------------------------------------------------------



isProper = method()
------------------------------------------------------------------------------
isProper ToricMap := Boolean => (cacheValue symbol isProper) (f -> (
    	X := source f;
    	Y := target f;
    	if isComplete X then return true;
    	if (isComplete Y and not isComplete X) then return false;
    	rayMatrixX := transpose matrix rays X;
    	rayMatrixY := transpose matrix rays Y;
    	A := matrix f;
    	-- based on the idea that the map should be proper if and only if all
    	-- torus invariant curves in X are PP^1 or are contained in the torus
    	-- invariant curves of Y.	    
    	for tau in max Y do (
	    -- dimension of tau cap image A and computing potential cones over tau
	    d := dim Y - rank (gens ker transpose A | gens ker transpose rayMatrixY_tau);
	    maxConesWithRightDimension := select(max X, 
	    	sigma -> member(sigma, orbits(X, rank A - d))
	    	);
	    -- compute the cones over tau
	    conesOverTau := select(maxConesWithRightDimension, sigma -> (
		    normals := outerMatrix outerNormals(Y, tau);
		    innerProducts := normals * A * rayMatrixX_sigma;
		    all(flatten entries innerProducts, b -> b <= 0)
		    )
	    	);
    	    -- if no cones over tau, not proper
	    if (#conesOverTau === 0) then return false;
    	    -- compute facets of the cones over tau
            facesOverTau := select(orbits(X, rank A - d + 1), 
	    	alpha-> any(conesOverTau, sigma -> isSubset(alpha, sigma))
	    	);
    	    -- pick which faces appear only once
	    facesCount := hashTable apply(facesOverTau,
	    	alpha -> {alpha, #select(conesOverTau, sigma -> isSubset(alpha, sigma))}
	    	);
	    uniqueFaces := select(facesOverTau, i -> facesCount#i < 2);
    	    -- faces of tau
    	    facesOfTau := select(orbits(Y, dim Y - d + 1), beta -> isSubset(beta,tau));
	    -- check if the faces appearing only once are contained in faces of tau
	    if not all(uniqueFaces, alpha -> any(facesOfTau,
		    beta -> coker (A * rayMatrixX_alpha) == coker rayMatrixY_beta)
	    	) then return false;
	    );
    	true
    	)
    )

isFibration = method()
isFibration ToricMap := Boolean => (
    cacheValue symbol isFibration) (f -> (
    isProper f and gens gb matrix f == id_(ZZ^(dim target f))))

isDominant = method()
isDominant ToricMap := Boolean => (
    cacheValue symbol isDominant) (
    f -> rank matrix f == dim target f)


-- THIS METHOD IS NOT EXPORTED
-- determines if a vector lies in the relative interior of a cone
isRelativeInterior = method()
isRelativeInterior (NormalToricVariety, List, Matrix) := Boolean => (X, sigma, v) -> (
    H := outerNormals(X, sigma);
    H0 := transpose H#0;
    H1 := transpose H#1;
    all (flatten entries (H0 * v), i -> i < 0) and 
	all (flatten entries(H1 * v), i -> i === 0)
    )

isSurjective ToricMap := Boolean => (cacheValue symbol isSurjective) (f -> (
    	if not isDominant f then return false;
	X := source f;
	Y := target f;
	A := matrix f;
	-- we remove the cones {} corresponding to the dense tori
    	sourceCones := flatten drop (values orbits X, -1);	
    	targetCones := reverse flatten drop (values orbits Y, -1);
	raysX := rays X;
    	for sigma in sourceCones do (
	    if targetCones === {} then return true;
	    v := A * (transpose matrix {sum raysX_sigma});
	    for tau in targetCones do (
	    	if isRelativeInterior(Y, tau, v) then (
		    targetCones = delete(tau, targetCones);
		    break);
		);
	    );
    	targetCones === {}
    	)
    )

------------------------------------------------------------------------------
-- making various associated groups into functors
------------------------------------------------------------------------------
weilDivisorGroup ToricMap := Matrix => (cacheValue symbol weilDivisorGroup) (
    f -> ( 
    	Y := target f;
    	map(weilDivisorGroup source f, weilDivisorGroup Y, 
	    transpose matrix apply(# rays Y, i -> entries pullback(f, Y_i)))
    	)
    )

classGroup ToricMap := Matrix => (cacheValue symbol classGroup) (f -> (
    	X := source f;
    	Y := target f;
    	divisorMap := weilDivisorGroup f;
    	map(classGroup X, classGroup Y, transpose (
	    	(transpose (fromWDivToCl(X) * divisorMap)) // transpose fromWDivToCl(Y)
	    	))
    	)
    )

cartierDivisorGroup ToricMap := Matrix => (
    cacheValue symbol cartierDivisorGroup) (
    f -> ( 
    	X := source f;
    	Y := target f;
	toWDivX := transpose matrix apply(entries transpose fromCDivToWDiv Y, 
	    c -> entries pullback(f, toricDivisor(c,Y)));
	inducedMatrix := toWDivX // fromCDivToWDiv X;
    	map(cartierDivisorGroup X, cartierDivisorGroup Y, inducedMatrix)
    	)
    )

picardGroup ToricMap := Matrix => (cacheValue symbol picardGroup) (f -> ( 
    	X := source f;
    	Y := target f;
    	divisorMap := cartierDivisorGroup f;
    	map(classGroup X, classGroup Y, transpose (
	    	(transpose (fromCDivToPic(X) * divisorMap)) // transpose fromCDivToPic(Y)
	    	))
    	)
    )


------------------------------------------------------------------------------
-- operations related to toric divisors and sheaves
------------------------------------------------------------------------------
-- THIS METHOD IS NOT EXPORTED
-- this method caches the index of a maximal cone in the target which contains
-- the image of each ray of the source.
rayMaxList = method()
rayMaxList ToricMap := List => (cacheValue symbol maxRayList) (f -> (
    X := source f;
    Y := target f;
    A := matrix f;	
    maxY := max Y;
    -- find a maximal cone containing the image of each ray
    normals := new MutableHashTable;
    for ray in rays X list (
	rho := A * transpose matrix {ray};
	position(max Y, sigma -> (
		if not normals#?sigma then (
		    normals#sigma = outerMatrix outerNormals(Y, sigma);
		    );
		innerProducts := normals#sigma * rho;
		all(flatten entries innerProducts, b -> b <= 0)
		)
	    )
	)
    )
)		    

pullback (ToricMap, ToricDivisor) := ToricDivisor => {} >> o -> (f, D) -> (
    if not isCartier D then error "-- expected a Cartier divisor";
    cartierData := cartierCoefficients D;
    A := matrix f;
    X := source f;
    raysX := rays X;
    maxConeIndices := rayMaxList f;
    sum for i to # raysX - 1 list (
	rho := A * transpose matrix {raysX_i};
	-- see Thm 4.2.12.b and Prop 6.2.7 in Cox-Little-Schenck (Prop 6.1.20
	-- in the preprint) note: in CLS they use inner normals, whereas we
	-- use outer normals, hence the different sign
	(transpose cartierData_(maxConeIndices_i) * rho)_(0,0) * X_i
	)
    )
pullback (ToricMap, Module) := Module => {} >> o -> (f, M) -> (
    if ring target f =!= ring M then 
	error "-- expected module over the Cox ring of the target";
    (inducedMap f) ** M
    )
pullback (ToricMap, CoherentSheaf) := CoherentSheaf => {} >> o -> (f, F) -> (
    sheaf (source f, pullback(f, module F))
    )

inducedMap ToricMap := RingMap => o -> (cacheValue symbol inducedMap) (f -> (
    	Y := target f;
    	S := ring Y;
    	R := ring source f;
    	m := picardGroup f; -- degree map
    	map(R, S, apply(numgens S, i -> (
		    exps := entries pullback(f, Y_i);
		    product(numgens R, j -> R_j^(exps#j)))),
	    DegreeMap => (deg -> first entries (matrix{deg} * transpose m)))
	)
    )

ideal ToricMap := Ideal => f -> (
    -- First find the ideal in K[T_Y] of image of the map from T_X to T_Y The
    -- map K[T_Y] -> K[T_X] is given by t_i mapsto prod_j t_j^{a_ij}, where 
    -- A = matrix(f), and the ideal is the kernel of this, which is generated
    -- by t^u - t^v where u-v lies in ker(A^T) (We just list the vectors u-v
    -- generating this ideal)
    R := ring target f;
    kernelGenerators := kernelLLL transpose matrix f;
    if rank source kernelGenerators === 0 then return ideal (0_R);
    -- Then map this to the Cox ring, using the isomorphism K[T] cong
    -- (Cox_prod x_i)_0. We have to make the substitutions, and clear
    -- denominators, and then saturate by the product of the variables. The
    -- map sends t_i to prod_j x_j^{(v_j)_i}, where v_i is the first lattice
    -- point on the ray of Sigma.
    raysY := matrix rays target f;
    binomialExps := entries transpose (raysY * kernelGenerators);
    I := ideal apply(binomialExps, u -> (
	    posMonomial := 1_R;
	    negMonomial := 1_R;
	    apply(#u, i -> if u_i > 0 then posMonomial = posMonomial * R_i^(u_i) 
		else if u_i < 0 then negMonomial = negMonomial * R_i^(-u_i)
		);    
    	    posMonomial - negMonomial
    	    )
	);
    scan(gens R, r -> I = saturate(I, r));
    I
    )
