
-- Matching Fields package for Macaulay2 by Oliver Clarke

newPackage(
    "MatchingFields",
    Version => "1.2",
    Date => "November 23, 2023",
    Authors => {
	{Name => "Oliver Clarke", Email => "oliver.clarke@ed.ac.uk", HomePage => "https://www.oliverclarkemath.com/"}
	},
    Headline => "Toric degenerations of flag varieties via matching fields",
    Keywords => {"Flag Varieties"},
    DebuggingMode => false,
    PackageExports => {"Polyhedra", "SubalgebraBases", "Matroids", "FourTiTwo", "Graphs"}
    )  

-- ###########
-- # Exports #
-- ###########

export {
    "GrMatchingField",
    "grMatchingField",
    "FlMatchingField",
    "flMatchingField",
    "getTuples",
    "getWeightMatrix",
    "getWeightPluecker",
    "getGrMatchingFields",
    "matchingFieldPolytope",
    "ExtraZeroRows",
    "diagonalMatchingField",
    "matchingFieldRingMap",
    "matchingFieldIdeal",
    "plueckerIdeal",
    "plueckerMap",
    "plueckerAlgebra",
    "matchingFieldFromPermutation",
    "RowNum",
    "UsePrimePowers",
    "ScalingCoefficient",
    "PowerValue",
    "isToricDegeneration",
    "NOBody",
    "matroidSubdivision",
    "weightMatrixCone",
    "isCoherent",
    "linearSpanTropCone",
    "VerifyToricDegeneration",
    "algebraicMatroid",
    "algebraicMatroidBases",
    "algebraicMatroidCircuits",
    "TopeField",
    "topeField",
    "isLinkage",
    "amalgamation"
    }



-- #############
-- # Main Code #
-- #############


-- Matching Field Types
GrMatchingField = new Type of HashTable;
-- Grassmannian Matching Fields MF have:
-- MF.n
-- MF.k
-- MF.tuples = List of k-subets of {1 .. n}  
-- MF.cache

FlMatchingField = new Type of HashTable;
-- Flag Matching Fields MF have:
-- MF.kList
-- MF.grMatchingFieldList
-- MF.cache

-- The cache table contains some of the following (or eventually computed):
-- weightMatrix
-- weightPluecker
-- mfPolytopePoints
-- mfPolytope 
-- matchingFieldIdeal
-- matchingFieldRingMap
-- ringP
-- ringX
-- X
-- mfSubring
-- mfNOBody
-- mfPlueckerIdeal

protect symbol tuples
protect symbol n
protect symbol k
protect symbol kList
protect symbol grMatchingFieldList

protect symbol weightMatrix
protect symbol weightPluecker
protect symbol mfPolytope
protect symbol mfPolytopePoints

protect symbol ringP -- Polynomial ring in variables P_I, I in subsets(n, k)
protect symbol ringX -- Polynomial ring in variables x_(i,j), 1 <= i <= k, 1 <= j <= n
protect symbol X     -- matrix of ringX variables
protect symbol mfRingMap
protect symbol plueckerRingMap
protect symbol mfIdeal
protect symbol projMFIdeal
protect symbol mfPlueckerIdeal
protect symbol mfSubring

protect symbol mfNOBody

protect symbol computedMatroidSubdivision
protect symbol computedWeightMatrixCone
protect symbol computedLinearSpanTropCone
protect symbol computedAlgebraicMatroid

---------------------------------------------
-- Matching Field constructor
grMatchingField = method(
    TypicalValue => GrMatchingField
    )

-- MF from weight matrix
grMatchingField(Matrix) := M -> (
    -- uses min convention   
    -- returns matching f ield for weight matrix along with the weight for the Plucker variables
    -- NB we assume the MF is well defined from the weight matrix
    --    i.e. the minimum was uniquely attained for each plucker form
    Mk := numRows M;
    Mn := numColumns M;
    local subsetOrder;
    local subsetWeight;
    local weight;
    L := {};
    W := {};
    for I in subsets(Mn, Mk) do (
	subsetWeight = infinity;
	for ordering in permutations(I) do (
	    weight = sum for i from 0 to Mk-1 list M_(i, ordering_i);
	    if weight < subsetWeight then (
		subsetOrder = apply(ordering, i -> i + 1);
 		subsetWeight = weight;
 		);
	    );
	L = append(L, subsetOrder);
	W = append(W, subsetWeight);
	);
    MF := new GrMatchingField from {
	n => Mn, 
	k => Mk, 
	tuples => L, 
	cache => new CacheTable from {
	    weightMatrix => M,
	    weightPluecker => W
	    }
	}
    )

-- Matching Field from list of tuples
grMatchingField(ZZ, ZZ, List) := (Lk , Ln, L) -> (
    -- check user input:
    sortedTuples := sort (sort \ L);
    usualSubsets := subsets(1 .. Ln, Lk);
    if not (sortedTuples == sort usualSubsets) then (
	error("Unexpected tuples.");
	);
    -- put the subsets in the correct order:
    lookupPosition := new HashTable from for i from 0 to #usualSubsets - 1 list (usualSubsets_i => i);
    tupleList := sort(L, x -> lookupPosition#(sort(x)));
    new GrMatchingField from {
	n => Ln,
	k => Lk,
	tuples => tupleList,
	cache => new CacheTable from {}
	}
    )

-- Constructor for partial Flag Matching Fields
flMatchingField = method(
    TypicalValue => FlMatchingField
    )
flMatchingField(List, Matrix) := (inputKList, inputWeightMatrix) -> (
    if numRows inputWeightMatrix < max inputKList then (
	error("expected a matrix with at least " | toString max inputKList | " rows");
	);
    grMatchingFields := for Mk in inputKList list grMatchingField(inputWeightMatrix^(toList(0 .. Mk - 1)));
    new FlMatchingField from {
	n => numColumns inputWeightMatrix, 
	kList => inputKList, 
	grMatchingFieldList => grMatchingFields, 
	cache => new CacheTable from {
	    weightMatrix => inputWeightMatrix,
	    weightPluecker => flatten for grMF in grMatchingFields list grMF.cache.weightPluecker
	    }
	}
    )

flMatchingField(Matrix) := inputWeightMatrix -> (
    flMatchingField(toList(1 .. numRows inputWeightMatrix), inputWeightMatrix)
    )

-- Flag matching field from list of tuples
-- tuple list should be a list of lists
flMatchingField(List, ZZ, List) := (LkList, Ln, L) -> (
    grMatchingFields := for kIndex from 0 to #LkList - 1 list (
	grMatchingField(LkList_kIndex, Ln, L_kIndex) 
	);
    new FlMatchingField from {
	n => Ln,
	kList => LkList,
	grMatchingFieldList => grMatchingFields,
	cache => new CacheTable from {}
	}
    )

net(GrMatchingField) := MF -> (
    "Grassmannian Matching Field for Gr(" | toString MF.k | ", " | toString MF.n | ")"  
    )

net(FlMatchingField) := MF -> (
    s := toString MF.kList;
    "Flag Matching Field for Fl(" | s_(1, #s - 2) | "; " | toString MF.n | ")"
    )

-----------------------
-- Setup weight vectors
-- unexported 
-- Called by weight-getters
setupWeights = method()
setupWeights(GrMatchingField) := MF -> (
    if not MF.cache.?weightMatrix then (
    	MF.cache.weightMatrix = computeWeightMatrix MF;
	);
    if not MF.cache.?weightPluecker then (
	MF.cache.weightPluecker = for tuple in MF.tuples list sum(for i from 0 to MF.k - 1 list MF.cache.weightMatrix_(i, tuple_i - 1));
	);
    )

setupWeights(FlMatchingField) := MF -> (
    if not MF.cache.?weightMatrix then (
    	MF.cache.weightMatrix = computeWeightMatrix MF;
	);
    if not MF.cache.?weightPluecker then (
	MF.cache.weightPluecker = flatten for grMF in MF.grMatchingFieldList list (
	    for tuple in grMF.tuples list sum(
		for i from 0 to grMF.k - 1 list MF.cache.weightMatrix_(i, tuple_i - 1)
		)
	    );
	);
    )


-- basic getters:
getTuples = method()
getTuples(GrMatchingField) := MF -> (
    MF.tuples
    )
getTuples(FlMatchingField) := MF -> (
    for grMF in MF.grMatchingFieldList list grMF.tuples
    )


getGrMatchingFields = method()
getGrMatchingFields(FlMatchingField) := MF -> (
    MF.grMatchingFieldList
    )


getWeightMatrix = method()
getWeightMatrix(GrMatchingField) := MF -> (
    if not MF.cache.?weightMatrix then (
    	setupWeights MF;
	);
    MF.cache.weightMatrix
    )

getWeightMatrix(FlMatchingField) := MF -> (
    if not MF.cache.?weightMatrix then (
	setupWeights MF;
	);
    MF.cache.weightMatrix
    )


getWeightPluecker = method()
getWeightPluecker(GrMatchingField) := MF -> (
    if not MF.cache.?weightPluecker then (
    	setupWeights MF;
	);
    MF.cache.weightPluecker
    )

getWeightPluecker(FlMatchingField) := MF -> (
    if not MF.cache.?weightPluecker then (
	setupWeights MF;
	);
    MF.cache.weightPluecker
    )


-- Comparison operators: (note that tuples are always listed in revlex order)
GrMatchingField == GrMatchingField := (MF1, MF2) -> (
    MF1.n == MF2.n and
    MF1.k == MF2.k and
    getTuples MF1 == getTuples MF2
    )

FlMatchingField == FlMatchingField := (MF1, MF2) -> (
    MF1.n == MF2.n and
    MF1.kList == MF2.kList and
    getTuples MF1 == getTuples MF2
    )

---------------------------------------
-- Matching Field Polytope Points
-- The vertices of the matching field polytope
--
-- Note that the package Polyhedra will compute its own vertices
-- for the matching field polytope. So the order of 
-- the columns is not guaranteed when calling 'vertices' on a Polyhedron 
-- so we use our own function since we know that all supplied points are vertices
--
-- This function is for internal use only (unexported)
-- It is only required for Grassmannian matching fields
-- We make use of this matrix in 'matchingFieldIdeal'
matchingFieldPolytopePoints = method(
    Options => {
	ExtraZeroRows => 0
	}
    )

matchingFieldPolytopePoints(GrMatchingField) := opts -> MF -> (
    if opts.ExtraZeroRows == 0 and MF.cache.?mfPolytopePoints then (
	MF.cache.mfPolytopePoints
	) else (
	-- construct a matching field polytope P_L from 
	-- L a matching field for Gr(k, n) Grassmannian 
    	points := {};
    	for I in MF.tuples do (
	    -- construct the point corresponding to I 
	    point := ();
	    for i in I do (
	    	point = point | (i - 1 : 0) | (1 : (1)) | (MF.n - i : 0);
	    	);
	    point = point | (MF.n * opts.ExtraZeroRows : 0);
	    point = toList point;
	    points = append(points, point);
	    ); 
    	points = transpose matrix points;
	if opts.ExtraZeroRows == 0 then MF.cache.mfPolytopePoints = points;
	points
	)
    )


---------------------------
-- Matching Field polytope
-- The polytope with one vertex for each tuple of the matching field
-- given by the convex hull of the exponent vectors of the monomial map
--
-- Options:
-- ExtraZeroRows: adds this many rows of 0's to each vertex (thought of as a k by n matrix) 
--     	       	  of the polytope (used for constructing flag polytopes)
-- 
matchingFieldPolytope = method(
    Options => {
	ExtraZeroRows => 0 
	}
    )
matchingFieldPolytope(GrMatchingField) := opts -> MF -> (
    if opts.ExtraZeroRows == 0 and MF.cache.?mfPolytope then (
	MF.cache.mfPolytope
	) else ( 
	P := convexHull matchingFieldPolytopePoints(MF, opts);
    	if opts.ExtraZeroRows == 0 then MF.cache.mfPolytope = P;
	P    
	)
    )

matchingFieldPolytope(FlMatchingField) := opts -> MF -> (
    if opts.ExtraZeroRows == 0 and MF.cache.?mfPolytope then (
	MF.cache.mfPolytope
	) else (
	P := sum for grMF in MF.grMatchingFieldList list matchingFieldPolytope(grMF, 
	    ExtraZeroRows => (max MF.kList - grMF.k + opts.ExtraZeroRows)
	    );
	if opts.ExtraZeroRows == 0 then MF.cache.mfPolytope = P;
	P 
	)
    )

--------------------------
-- Diagonal matching field
-- Corresponds to the Gelfand-Tsetlin Cone of the Flag Variety / Grassmannian
diagonalMatchingField = method()
diagonalMatchingField(ZZ, ZZ) := (Lk, Ln) -> (
    M := matrix for i from 0 to Lk - 1 list for j from 0 to Ln - 1 list i*(Ln - j);
    grMatchingField(M)
    )

-- partial flag variety
diagonalMatchingField(List, ZZ) := (LkList, Ln) -> (
    M := matrix for i from 0 to max LkList - 1 list for j from 0 to Ln - 1 list i*(Ln - j);
    flMatchingField(LkList, M)
    )

-- full-flag variety 
-- by convention this is a (n-1) by (n) matrix
diagonalMatchingField(ZZ) := Ln -> (
    M := matrix for i from 0 to Ln - 2 list for j from 0 to Ln - 1 list i*(Ln - j);
    flMatchingField(M)
    )

------------------------
-- setting up the polynomials rings for the matching field
-- unexported method
-- The weight vector v / matrix m stored in the matching field uses min convention
-- so use weight vector {max v .. max v} - v for the M2 weight vector
--  and the same for each row of m
--
-- MonomialOrder => "default" or "none"
setupMatchingFieldRings = method(
    Options => {
	MonomialOrder => "default"
	}
    )
setupMatchingFieldRings(GrMatchingField) := opts -> MF -> (
    local monomialOrder;
    if not MF.cache.?ringP then (
	p := symbol p;
	variables := for s in subsets(toList(1 .. MF.n), MF.k) list p_(if #s == 1 then s_0 else toSequence s);
	if opts.MonomialOrder == "default" then (
	    monomialOrder = (
	    	maxVal := max (getWeightPluecker MF);
	    	{Weights => for val in (getWeightPluecker MF) list maxVal - val} 
	    	);
	    ) else if opts.MonomialOrder == "none" then (
	    monomialOrder = GRevLex;
	    ) else (
	    error("Unknown option: MonomialOrder => " | toString opts.MonomialOrder);
	    );
	MF.cache.ringP = QQ[variables, MonomialOrder => monomialOrder];
	);
    if not MF.cache.?ringX then (
	x := symbol x;
	if opts.MonomialOrder == "default" then (
	    monomialOrder = (
	    	weights := for wRow in entries getWeightMatrix MF list (
		    bigVal := toList(MF.n : (max wRow));
		    bigVal - wRow
		    ); 
	    	{Weights => flatten weights} 
	    	);
	    ) else if opts.MonomialOrder == "none" then (
	    monomialOrder = GRevLex;
	    ) else (
	    error("Unknown option: MonomialOrder => " | toString opts.MonomialOrder);
	    );
	MF.cache.ringX = QQ[x_(1,1) .. x_(MF.k, MF.n), MonomialOrder => monomialOrder];
	);
    if not MF.cache.?X then (
	MF.cache.X = transpose genericMatrix(MF.cache.ringX, MF.n, MF.k);
	);
    )

setupMatchingFieldRings(FlMatchingField) := opts -> MF -> (
    local monomialOrder;
    if not MF.cache.?ringP then (
	p := symbol p;
	variables := flatten for Lk in MF.kList list for s in subsets(toList(1 .. MF.n), Lk) list p_(if #s == 1 then s_0 else toSequence s);
	if opts.MonomialOrder == "default" then (
	    monomialOrder = (
	    	bigVals := flatten (
		    currentIndex := 0;
		    for grMF in MF.grMatchingFieldList list (
		    	numberEntries := binomial(grMF.n, grMF.k);
		    	currentIndex = currentIndex + numberEntries;
		    	toList(numberEntries : max ((getWeightPluecker MF)_{currentIndex - numberEntries .. currentIndex - 1}))
		    	)
	    	    );
	    	{Weights => (bigVals - (getWeightPluecker MF))} 
	    	);
	    ) else if opts.MonomialOrder == "none" then (
	    monomialOrder = GRevLex;
	    ) else (
	    error("Unknown option: MonomialOrder => " | toString opts.MonomialOrder);
	    );
	MF.cache.ringP = QQ[variables, MonomialOrder => monomialOrder];
	);
    if not MF.cache.?ringX then (
	x := symbol x;
	if opts.MonomialOrder == "default" then (
	    monomialOrder = (
	    	weights := for wRow in entries getWeightMatrix MF list (
		    bigVal := toList(MF.n : (max wRow));
		    bigVal - wRow
		    ); 
	    	{Weights => flatten weights} 
	    	);
	    ) else if opts.MonomialOrder == "non" then (
	    monomialOrder = GRevLex;
	    ) else (
	    error("Unknown option: MonomialOrder => " | toString opts.MonomialOrder);
	    );
	MF.cache.ringX = QQ[x_(1,1) .. x_(max MF.kList, MF.n), MonomialOrder => monomialOrder];
	);
    if not MF.cache.?X then (
	MF.cache.X = transpose genericMatrix(MF.cache.ringX, MF.n, max MF.kList);
	);
    )


-- gets the sign of a tuple
-- which is (-1) to power the number of descets modulo 2
-- 1 means even tuple, -1 means odd tuple
tupleSign = method()
tupleSign(List) := I -> (
    if #I <= 1 then 1 else (
    	(-1)^((sum for s in subsets(I, 2) list if s_0 > s_1 then 1 else 0) % 2)
	)
    )

-- matching field ring map: P_I -> x_(1,I_1) * x_(2, I_2) ... x_(k, I_k), for each tuple I
matchingFieldRingMap = method(
    Options => {
	MonomialOrder => "default" -- monomial order for the ambient rings (default or none)
	}
    )
matchingFieldRingMap(GrMatchingField) := opts -> MF -> (
    setupMatchingFieldRings(opts, MF);
    if not MF.cache.?mfRingMap then (
    	R := MF.cache.ringP;
    	S := MF.cache.ringX;
        MF.cache.mfRingMap = map(S, R, 
	    for tuple in MF.tuples list tupleSign(tuple) * (product for i from 0 to MF.k - 1 list (MF.cache.X)_(i, tuple_i - 1))
	    );
	);
    MF.cache.mfRingMap
    )

matchingFieldRingMap(FlMatchingField) := opts -> MF -> (
    setupMatchingFieldRings(opts, MF);
    if not MF.cache.?mfRingMap then (
    	R := MF.cache.ringP;
    	S := MF.cache.ringX;
    	MF.cache.mfRingMap = map(S, R, 
	    flatten for grMF in MF.grMatchingFieldList list (
		for tuple in grMF.tuples list tupleSign(tuple) * (product for i from 0 to grMF.k - 1 list (MF.cache.X)_(i, tuple_i - 1))
	    	)
	    );
	);
    MF.cache.mfRingMap
    )	     

-- matching field ideal
-- compute using M2 or FourTiTwo methods
matchingFieldIdeal = method(
    Options => {
	Strategy => "4ti2", -- "FourTiTwo" or "M2"
	MonomialOrder => "default" -- monomial order for ambient rings (default or none)
	}
    )
matchingFieldIdeal(GrMatchingField) := opts -> MF -> (
    -- setting up MF rings is done by grMatchingFieldRingMap if necessary
    if not MF.cache.?mfIdeal then (
	if opts.Strategy == "M2" then (
	    MF.cache.mfIdeal = kernel matchingFieldRingMap(MF, MonomialOrder => opts.MonomialOrder);
    	    )
	else if opts.Strategy == "4ti2" then (
	    local gensMatrix;
	    setupMatchingFieldRings(MF, MonomialOrder => opts.MonomialOrder);
	    V := matchingFieldPolytopePoints MF;
	    if opts.MonomialOrder == "default" then (
	    	gensMatrix = gens toricGroebner(V, MF.cache.ringP, Weights => getWeightPluecker MF);
		) else if opts.MonomialOrder == "none" then (
	    	gensMatrix = gens toricGroebner(V, MF.cache.ringP);
		) else (
		error("Unknown option: MonomialOrder => " | toString opts.MonomialOrder);
		);
	    -- adjust the signs of the variables
	    signChange := map(MF.cache.ringP, MF.cache.ringP, matrix {
		    for i from 0 to #MF.tuples - 1 list (tupleSign (MF.tuples)_i)*(MF.cache.ringP)_i
		    });
	    gensMatrix = signChange gensMatrix;
	    MF.cache.mfIdeal = ideal gensMatrix;
	    -- sometimes the Weights might not work (depends on 4ti2 version) see docs 
	    forceGB gens MF.cache.mfIdeal; 
	    )
       	else (
	    error("unknown Strategy: " | toString opts.Strategy | " for matchingFieldIdeal");
	    );
	);
    MF.cache.mfIdeal
    )

matchingFieldIdeal(FlMatchingField) := opts -> MF -> (
    -- setting up MF rings is done by grMatchingFieldRingMap if necessary
    if not MF.cache.?mfIdeal then (
    	if opts.Strategy == "M2" then (
	    MF.cache.mfIdeal = kernel matchingFieldRingMap(MF, MonomialOrder => opts.MonomialOrder);
    	    )
	else if opts.Strategy == "4ti2" then (
	    local gensMatrix;
	    setupMatchingFieldRings(MF, MonomialOrder => opts.MonomialOrder);
	    VList := for grMF in MF.grMatchingFieldList list (
		matchingFieldPolytopePoints(grMF, ExtraZeroRows => (max MF.kList - grMF.k))
		);
	    V := fold(VList, (V1, V2) -> V1 | V2);
	    if opts.MonomialOrder == "default" then (
	    	gensMatrix = gens toricGroebner(V, MF.cache.ringP, Weights => getWeightPluecker MF);
		) else if opts.MonomialOrder == "none" then (
	    	gensMatrix = gens toricGroebner(V, MF.cache.ringP);
		) else (
		error("Unknown option: MonomialOrder => " | toString opts.MonomialOrder);
		);
	    -- adjust the signs of the variables
	    signChange := map(MF.cache.ringP, MF.cache.ringP, matrix {
		    variableIndex := -1;
		    flatten for grMF in MF.grMatchingFieldList list (
			for i from 0 to #grMF.tuples - 1 list (
			    variableIndex = variableIndex + 1;
			    (tupleSign (grMF.tuples)_i)*(MF.cache.ringP)_variableIndex
			    )
			)
		    });
	    gensMatrix = signChange gensMatrix;
	    MF.cache.mfIdeal = ideal gensMatrix;
	    -- sometimes the Weights might not work (depends on 4ti2 version) see docs
	    forceGB gens MF.cache.mfIdeal; 
	    )
       	else (
	    error("unknown Strategy" | toString opts.Strategy | " for matchingFieldIdeal");
	    );
    	);
    MF.cache.mfIdeal
    )

---------------------------------------------------
-- projective matching field ideal (unexported)
-- this is the ideal obtained by composing the 
-- matching field rings map and the segre embedding
-- 
-- Note: the signs are not correct
-- 4ti2 Strategy only
-- assumes that the weight matrices of the grMatchingFields
--   are sub weight matrices of the flMatchingField

projectiveMatchingFieldIdeal = method(
    Options => {
	Strategy => "4ti2", -- "FourTiTwo"
	MonomialOrder => "default" -- monomial order for ambient rings (default or none)
	}
    )

projectiveMatchingFieldIdeal(GrMatchingField) := opts -> MF -> (
    matchingFieldIdeal(opts, MF)
    )

projectiveMatchingFieldIdeal(FlMatchingField) := opts -> MF -> (
    if not MF.cache.?projMFIdeal then (
	local monomialOrder;
        if opts.MonomialOrder == "none" then (
	    monomialOrder = GRevLex;
	    ) else if opts.MonomialOrder == "default" then (
	    plueckerWeights := for grMF in MF.grMatchingFieldList list (
		plueckerWeight := getWeightPluecker grMF;
		maxWeight := max plueckerWeight;
		for weight in plueckerWeight list maxWeight - weight
		);
	    monomialOrder = (Weights => fold(plueckerWeights, (W1, W2) -> flatten for w1 in W1 list for w2 in W2 list w1+w2));
	    ) else (
	    error("Unknown option: MonomialOrder => " | toString opts.MonomialOrder);
	    );
	p := symbol p;
	subsetList := for k in MF.kList list subsets(1 .. MF.n, k);
	variableIndices := fold(subsetList, (S1, S2) -> flatten for s1 in S1 list for s2 in S2 list s1 | s2);  
	R := QQ[for variableIndex in variableIndices list p_(toSequence variableIndex), MonomialOrder => monomialOrder];
	if opts.Strategy == "4ti2" then (
	    VList := for grMF in MF.grMatchingFieldList list (
		pointsMatrix := matchingFieldPolytopePoints(grMF, ExtraZeroRows => (max MF.kList - grMF.k));
		for columnIndex from 0 to numColumns pointsMatrix -1 list pointsMatrix_{columnIndex}
		);
	    VCols := fold(VList, (V1, V2) -> flatten for v1 in V1 list for v2 in V2 list v1+v2);
    	    V := fold(VCols, (col1, col2) -> col1 | col2);
	    gensMatrix := gens toricGroebner(V, R);
	    MF.cache.projMFIdeal = ideal gensMatrix;
	    forceGB gens MF.cache.projMFIdeal;
	    ) else (
	    error("Unknown option: Strategy => " | toString opts.Strategy);
	    );
	);
    MF.cache.projMFIdeal
    )


-- Grassmannian ideal using the constructed pluecker variable ring
-- Sets the weight of the polynomial ring to be the MF pluecker weight
-- 
-- the matching field must be coherent for this shortcut to work 
Grassmannian(GrMatchingField) := opts -> MF -> (
    plueckerIdeal MF
    )


plueckerIdeal = method(
    Options => {
	MonomialOrder => "default" -- default or none; see setupMatchingFieldRings(.., MonomialOrder => [option])
	}
    )
plueckerIdeal(GrMatchingField) := opts -> MF -> (
    if not MF.cache.?mfPlueckerIdeal then (
    	setupMatchingFieldRings(MF, MonomialOrder => opts.MonomialOrder);
    	R := MF.cache.ringP;
    	MF.cache.mfPlueckerIdeal = Grassmannian(MF.k - 1, MF.n - 1, R);
	);
    MF.cache.mfPlueckerIdeal
    )

plueckerIdeal(FlMatchingField) := opts -> MF -> (
    if not MF.cache.?mfPlueckerIdeal then (
    	setupMatchingFieldRings(opts, MF);
    	local i;
	local variableFromSubset;
	local generatorList;
	i = 0;
	variableFromSubset = new HashTable from flatten (
	    varsMatrix := vars MF.cache.ringP;
	    for grMF in MF.grMatchingFieldList list (
	    	for s in subsets(toList(1 .. grMF.n), grMF.k) list (
		    i = i + 1;
		    s => varsMatrix_(0, i-1)
		    )
	    	)
	    );
	--------------------------------
    	-- Grassmannian relations
	-- 
    	generatorList = flatten for grMF in MF.grMatchingFieldList list (
	    if grMF.k >= 2 and grMF.n - grMF.k >= 2 then (
	    	flatten for I in subsets(1 .. grMF.n, grMF.k - 1) list (
		    for J in subsets(1 .. grMF.n, grMF.k + 1) list (
			newGenerator := sum for jPosition from 0 to #J - 1 list (
			    j := J_jPosition;
			    if not member(j, I) then (
			    	IIndex := sort(I | {j});
			    	JIndex := delete(j, J);
			    	swapsToSortI := # for i in I list if i > j then i else continue;
				pI := variableFromSubset#IIndex;
			    	pJ := variableFromSubset#JIndex;
			    	(-1)^(jPosition + swapsToSortI)*pI*pJ
				) else continue
			    );
			if not zero(newGenerator) then (
			    newGenerator
			    ) else continue
			)
		    ) 
		) else continue
	    );
    	---------------------
	-- Incident Relations
	--
	generatorList = generatorList | flatten for grMFs in subsets(MF.grMatchingFieldList, 2) list (
	    grMF0 := grMFs_0;
	    grMF1 := grMFs_1;
	    flatten for I in subsets(1 .. grMF0.n, grMF0.k - 1) list (
		for J in subsets(1 .. grMF1.n, grMF1.k + 1) list (
		    sum for jPosition from 0 to #J - 1 list (
			j := J_jPosition;
			if not member(j, I) then (
			    IIndex := sort(I | {j});
			    JIndex := delete(j, J);
			    swapsToSortI := # for i in I list if i > j then i else continue;
			    pI := variableFromSubset#IIndex;
			    pJ := variableFromSubset#JIndex;
			    (-1)^(jPosition + swapsToSortI)*pI*pJ 
			    ) else continue
			)
		    )
		) 
	    );
	MF.cache.mfPlueckerIdeal = ideal(generatorList);
    	);    
    MF.cache.mfPlueckerIdeal
    )

----------------
-- Pluecker map is the determinantal map associated to Grassmannian / Flag variety
-- its kernel coincides with the pluecker ideal defined above 

plueckerMap = method(
    Options => {
	MonomialOrder => "default" -- default or none; see setupMatchingFieldRings(.., MonomialOrder => [option])
	}
    )
plueckerMap(GrMatchingField) := opts -> MF -> (
    setupMatchingFieldRings(opts, MF);
    if not MF.cache.?plueckerRingMap then (
	R := MF.cache.ringP;
	S := MF.cache.ringX;
	matX := MF.cache.X;
	MF.cache.plueckerRingMap = map(S, R, for s in subsets(MF.n, MF.k) list det(matX_s));
	);
    MF.cache.plueckerRingMap
    )

plueckerMap(FlMatchingField) := opts -> MF -> (
    setupMatchingFieldRings(opts, MF);
    if not MF.cache.?plueckerRingMap then (
	R := MF.cache.ringP;
	S := MF.cache.ringX;
	matX := MF.cache.X;
	MF.cache.plueckerRingMap = map(S, R, flatten for grMF in MF.grMatchingFieldList list (
		for s in subsets(grMF.n, grMF.k) list det(matX_s^(toList(0 .. grMF.k - 1))))
		);
	);
    MF.cache.plueckerRingMap
    )


----------------------------------
-- matching field from permutation
-- Fix a permutation S, a 'generic' weight matrix M
-- that induces the diagonal matching field 
-- Permute the 2nd row of M using S
-- See the paper: Clarke-Mohammadi-Zaffalon 2022
--
matchingFieldFromPermutation = method(
    Options => {
	RowNum => 2, -- which row to permute
	UsePrimePowers => false, -- Take N (in the definition of the weight matrix) to be a prime number
	ScalingCoefficient => 1, -- scale the permuted row by this coefficient, if > 2 then matrix may be non-generic unless prime power is true
	PowerValue => 0 -- Value of N in weight matrix, if supplied 0 then choose N to be n or nextPrime n depending on above options
	})
matchingFieldFromPermutation(ZZ, ZZ, List) := opts -> (Lk, Ln, S) -> (
    if # S != Ln or # set S < Ln then (
	error("expected a permutation of " | toString Ln | " distinct values");
	);
    if opts.ScalingCoefficient == 1 then (
	matchingFieldFromPermutationNoScaling(Lk, Ln, S, opts)
	) else (
	local N;
	local M;
	local W;
	if opts.PowerValue > 0 then (
	    N = opts.PowerValue;
	    ) else if opts.UsePrimePowers then (
	    N = nextPrime Ln;
	    ) else (
	    N = Ln
	    );
	if Lk == 1 then (
	    M = matrix {toList {Ln : 0}};
	    ) else (
    	    M = matrix {toList{Ln : 0}} || matrix for i from 1 to Lk - 1 list for j from 1 to Ln list (
	    	if i + 1 == opts.RowNum then (
	    	    (S_(j - 1))*opts.ScalingCoefficient*N^(i - 1)
	    	    ) else (
	    	    (Ln - j)*N^(i - 1)
	    	    )
	    	);
	    );
	grMatchingField M
	)
    );

-- The Flag matching field from permuting the second row
matchingFieldFromPermutation(List, ZZ, List) := opts -> (LkList, Ln, S) -> (
    if # S != Ln or # set S < Ln then (
	error("expected a permutation of " | toString Ln | " distinct values");
	);
    sortedLkList := sort LkList;
    grMatchingFields := for Lk in sortedLkList list matchingFieldFromPermutation(Lk, Ln, S, opts);
    lastGrMatchingField := grMatchingFields_(#sortedLkList - 1);
    new FlMatchingField from {
	n => Ln, 
	kList => sortedLkList, 
	grMatchingFieldList => grMatchingFields, 
	cache => new CacheTable from {
	    weightMatrix => lastGrMatchingField.cache.weightMatrix,
	    weightPluecker => flatten for grMF in grMatchingFields list grMF.cache.weightPluecker
	    }
	}
    );

-- matching field from permutation 
-- assume that there is no scaling coefficient so the tuples of the matching field can be written down quickly
-- unexported method (used by matchingFieldFromPermutation)
matchingFieldFromPermutationNoScaling = method(
    Options => {
	RowNum => 2, -- which row to permute
	UsePrimePowers => false, -- Take N (in the definition of the weight matrix) to be a prime number
	ScalingCoefficient => 1, -- scale the permuted row by this coefficient, if > 2 then matrix may be non-generic unless prime power is true
	PowerValue => 0 -- Value of N in weight matrix, if supplied 0 then choose N to be n or nextPrime n depending on above options
	})
matchingFieldFromPermutationNoScaling(ZZ, ZZ, List) := opts -> (Lk, Ln, S) -> (
    local IOrdered;
    local minIndex;
    local N;
    local M;
    local W;
    L := {};
    for I in subsets(Ln, Lk) do (
	if opts.RowNum <= Lk then (
	    -- find i in 0 .. rowNum-1 such that S_(I_i) is minimum
	    minIndex = 0;
	    for i from 1 to opts.RowNum - 1 do (
	    	if S_(I_i) < S_(I_minIndex) then (
		    minIndex = i;
		    );
	    	);
	    -- The elements I_0 .. I_(minIndex-1) are ordered in increasing order
	    IOrdered = for i from 0 to minIndex-1 list I_i + 1;
	    -- The next elements are I_(minIndex+1) .. I_(rowNum-1)
	    IOrdered = IOrdered | for i from minIndex+1 to opts.RowNum-1 list I_i + 1;
	    -- Then we get I_minIndex
	    IOrdered = append(IOrdered, I_minIndex + 1);
	    -- Then the rest of I in order
	    IOrdered = IOrdered | for i from opts.RowNum to Lk - 1 list I_i + 1;
	    L = append(L, IOrdered);
	    ) else (
	    L = append(L, apply(I, i -> i+1));
	    );
	);
    if opts.PowerValue > 0 then (
	N = opts.PowerValue;
	) else if opts.UsePrimePowers then (
	N = nextPrime Ln;
	) else (
	N = Ln
	);
    if Lk  == 1 then (
	M = matrix {toList {Ln : 0}};
	) else (
    	M = matrix {toList{Ln : 0}} || matrix for i from 1 to Lk - 1 list for j from 1 to Ln list (
	    if i + 1 == opts.RowNum then (
	    	(S_(j - 1))*N^(i - 1)
	    	) else (
	    	(Ln - j)*N^(i - 1)
	    	)
	    );
	);
    W = for I in L list (
	sum for i from 0 to Lk - 1 list M_(i, I_i - 1)
	);
    new GrMatchingField from {
	n => Ln, 
	k => Lk, 
	tuples => L, 
	cache => new CacheTable from {
	    weightMatrix => M,
	    weightPluecker => W
	    }
	}
    );


-------------------------------------------
-- isToricDegeneration for a Matching Field
-- checks if the matching field ideal is equal to the initial ideal of the Grassmannian
-- we already have that leadTerm(pluecker ideal) is a subset of matching field ideal
-- so it suffices to reduce the generators of the matching field ideal modulo the
-- leadTerm of the pluecker ideal
--
-- The algorithm of inhomogeneous ideals is sketched below:
-- 1) get matching field (toric) ideal and record largest degree generator d
-- 2) get partial GB of plueckerIdeal up to DegreeLimit d
--    If partial GB is actually a complete GB then go to (8)
-- 3) take the lead terms of the partial GB and forceGB
-- 4) reduce matching field ideal generators modulo the forced GB
-- 5) remove any generators that are reduced to zero
-- 6) if matching field generator list is zero then we have a toric degeneration so return true
-- 7) if not then d = d+1 and go back to step 2
-- 8) reduce the matching field ideal gens modulo the full GB and check if the result is zero
--
-- In the homogeneous case, it suffices to compute a GB up to degree limit d (step 1)
-- so we can forgo the while loop

isToricDegeneration = method ()
isToricDegeneration(GrMatchingField) := MF -> (
    matchingFieldIdealGens := gens matchingFieldIdeal MF;
    maxDegree := max flatten flatten degrees matchingFieldIdealGens;
    plueckerGB := gb(plueckerIdeal MF, Algorithm => Homogeneous, DegreeLimit => maxDegree);
    inPluecker := forceGB leadTerm(1, gens plueckerGB);
    zero(matchingFieldIdealGens % inPluecker)
    )

isToricDegeneration(FlMatchingField) := MF -> (
    matchingFieldIdealGens := gens matchingFieldIdeal MF;
    maxDegree := max flatten flatten degrees matchingFieldIdealGens;
    plueckerGB := gb(plueckerIdeal MF, Algorithm => Homogeneous, DegreeLimit => maxDegree);
    inPluecker := forceGB leadTerm(1, gens plueckerGB);
    zero(matchingFieldIdealGens % inPluecker)
    )

-------------------------------
-- plueckerAlgebra of a matching field
-- in older versions this method overloaded the method subring from the package SubalgebraBases
-- the pluecker algebra inside inside a ring with term order
-- given by the weightMatrix
-- 
-- MonomialOrder is the monomial order of the ambient rings; see setupMatchingFieldRings(.., MonomialOrder => [option])
plueckerAlgebra = method(
    Options => {
	MonomialOrder => "default" -- default or none
	}
    )
plueckerAlgebra(GrMatchingField) := opts -> MF -> (
    if not MF.cache.?mfSubring then (
    	setupMatchingFieldRings(MF, MonomialOrder => opts.MonomialOrder);
    	matX := MF.cache.X;
    	MF.cache.mfSubring = subring for s in subsets(MF.n, MF.k) list det(matX_s);
	);
    MF.cache.mfSubring
    )

plueckerAlgebra(FlMatchingField) := opts -> MF -> (
    if not MF.cache.?mfSubring then (
    	setupMatchingFieldRings(MF, MonomialOrder => opts.MonomialOrder);
    	matX := MF.cache.X;
    	MF.cache.mfSubring = subring flatten for grMF in MF.grMatchingFieldList list (
	    for s in subsets(grMF.n, grMF.k) list det(matX_s^(toList(0 .. grMF.k - 1)))
	    );
	);
    MF.cache.mfSubring
    )


---------------------------------------------
-- Newton-Okounkov body for a matching field
-- 
NOBody = method()
-- Note: it is okay to always set 
-- sagbi( .. AutoSubduce => false .. )
--

-- Grassmannian matching fields
-- the NO body has vertices that are directly read from the initial algebra (using Sagbi basis)
-- So, the lead terms can be simply scaled and the NO body is the convex hull of exponent vectors
NOBody(GrMatchingField) := MF -> (
    if not MF.cache.?mfNOBody then ( 
    	-- compute the initial algbera of the Pluecker algebra wrt the weight term order
    	initialAlgberaGens := first entries leadTerm subalgebraBasis(plueckerAlgebra MF, AutoSubduce => false); 
    	generatorExponents := apply(initialAlgberaGens, f -> (exponents(f))_0);
    	NOBodyVertices := apply(generatorExponents, v -> ((MF.k) / sum(v))*v); -- normalize the vertices
    	MF.cache.mfNOBody = convexHull transpose matrix NOBodyVertices;
	);
    MF.cache.mfNOBody
    )

-- Flag matching fields
-- each Pluecker form has a specific grading
-- the sagbi generators are lifted by their grading
-- and we compute the NO body by taking the slice of the cone 
-- that corresponds to the grading (1 .. 1)
NOBody(FlMatchingField) := MF -> (
    if not MF.cache.?mfNOBody then (
	kmax := max MF.kList;
	initialAlgebraGens := first entries leadTerm subalgebraBasis(plueckerAlgebra MF, AutoSubduce => false);
	generatorExponents := matrix apply(initialAlgebraGens, f -> (exponents(f))_0);
	gradingMap := matrix for gradingRow from 0 to #MF.kList -1 list(
	    flatten for kIndex from 0 to kmax-1 list (
	        if MF.kList_gradingRow - 1 == kIndex then (
		    toList(MF.n : 1)
		    ) else if MF.kList_gradingRow == kIndex then (
		    toList(MF.n : -1)
		    ) else (
		    toList(MF.n : 0)
		    )
		)
	    );
	coneRays := (gradingMap * transpose generatorExponents) || (transpose generatorExponents); 
	polyCone := coneFromVData coneRays;
	-- take the part of the cone with grading (1 .. 1)
	slice := polyhedronFromHData (
	    matrix {toList(#MF.kList + kmax * MF.n : 0)},
	    matrix {{0}},
	    id_(ZZ^(#MF.kList)) | matrix for row in MF.kList list for col from 1 to (MF.n * kmax) list 0,
	    transpose matrix {toList(#MF.kList : 1)}
	    );
	NOBodyAsIntersection := intersection(polyCone, slice);
	-- simplify by removing the grading part
	NOBodyVertices := (vertices NOBodyAsIntersection)^{#MF.kList .. #MF.kList + MF.n * kmax - 1};
	MF.cache.mfNOBody = convexHull NOBodyVertices;
	);
    MF.cache.mfNOBody
    )

-----------------------
-- Regular Subdivision of a set of points
-- code is copied and modified from "Polyhedra" Package 
-- not exported 
-- Why is code copied:
-- >> See previous comment about the permutation of vertex names in Polyhedra package
--    the same bug is present in the regular subdivision method
-- >> Sent a message to the authors of Polyhedra package explaining the problem but no response yet  
--
pointRegularSubdivision = method()
pointRegularSubdivision(Matrix, Matrix) := (points, weight) -> (
    -- Checking for input errors
    if numColumns weight != numColumns points or numRows weight != 1 then error("The weight must be a one row matrix with number of points many entries");
    P := convexHull(points || weight, matrix (toList(numRows points : {0}) | {{1}} ));
    F := select(faces (1,P), f -> #(f#1) == 0);
    V := vertices P;
    apply (F, f -> V_(f#0)^(toList(0 .. (numRows points - 1))))
    )
 
---------------------------------
-- matroidal subdivision from matching field
-- Take the Pluecker weight w of a matching field
-- Note that w lies in the Dressian
-- Compute the regular subdivision of the hypersimplex wrt w
matroidSubdivision = method()
matroidSubdivision(ZZ, ZZ, List) := (k, n, L) -> (
    assert(#L == binomial(n, k));
    SS := subsets(toList(1 .. n), k);
    hyperSimplex := sub(transpose matrix for s in SS list for i from 1 to n list if member(i, s) then 1 else 0, QQ); -- sub to avoid entries in ZZ
    subdivisionPieces := pointRegularSubdivision(hyperSimplex, matrix {L});
    vertexLookup := new HashTable from for i from 0 to binomial(n, k) - 1 list hyperSimplex_{i} => SS_i;
    for piece in subdivisionPieces list for c from 0 to numColumns piece - 1 list vertexLookup#(piece_{c})
    )

matroidSubdivision(GrMatchingField) := MF -> (
    if not MF.cache.?computedMatroidSubdivision then (
    	MF.cache.computedMatroidSubdivision = matroidSubdivision(MF.k, MF.n, getWeightPluecker MF);
    	);
    MF.cache.computedMatroidSubdivision
    )


----------------------
-- weightMatrixCone
-- the cone whose interior points are weight matrices that induce the given matching field
weightMatrixCone = method(
    Options => {
	ExtraZeroRows => 0 -- adds this many rows of 0 to each inequality, used for FlMatchingField cone
	}
    )

weightMatrixCone(GrMatchingField) := opts -> MF -> (
    if opts.ExtraZeroRows == 0 and MF.cache.?computedWeightMatrixCone then (
	MF.cache.computedWeightMatrixCone
	) else (
	-- form the matrix of inequalities A: such that the cone is Ax >= 0
	local inequalities;
	if MF.k > 1 then (
            inequalities = matrix ( 
	    	subsetList := subsets(1 .. MF.n, MF.k);
	    	flatten for i from 0 to binomial(MF.n, MF.k) - 1 list (
	    	    columnIndices := subsetList_i;
	    	    minimalTuple := MF.tuples_i;
		    for p in delete(minimalTuple, permutations columnIndices) list (
		    	-- the row vector the encodes: minimalTuple <= p
		    	-- E.g. if the minimal tuple is {1,2,3} then
		    	--      one of the inequalities is given by {1,2,3} <= {1,3,2}
		    	--      which cancels down further since 1 is in the same place
		    	for coord in (0,1) .. (MF.k - 1 + opts.ExtraZeroRows, MF.n) list (
		    	    sum {if coord_0 < MF.k and p_(coord_0) == coord_1 then 1 else 0, 
			    	if coord_0 < MF.k and minimalTuple_(coord_0) == coord_1 then -1 else 0}
		    	    )
		    	)
	    	    )
    	    	);
	    ) else (
	    inequalities = matrix {toList((MF.k + opts.ExtraZeroRows) * MF.n : 0)};
	    );
	C := coneFromHData(inequalities);   
	if opts.ExtraZeroRows == 0 then MF.cache.computedWeightMatrixCone = C;
        C
    	)
    )

weightMatrixCone(FlMatchingField) := opts -> MF -> (
    if opts.ExtraZeroRows == 0 and MF.cache.?computedWeightMatrixCone then (
	MF.cache.computedWeightMatrixCone
	) else (
    	kMax := max MF.kList;
    	weightMatrixConeList := for grMF in MF.grMatchingFieldList list (
	    weightMatrixCone(grMF, ExtraZeroRows => (kMax - grMF.k + opts.ExtraZeroRows))
	    );
	inequalityMatrix := facets (weightMatrixConeList_0);
	hyperplanesMatrix := hyperplanes (weightMatrixConeList_0);
	for i from 1 to #weightMatrixConeList - 1 do (
	    inequalityMatrix = inequalityMatrix || facets (weightMatrixConeList_i);
	    hyperplanesMatrix = hyperplanesMatrix || hyperplanes (weightMatrixConeList_i);
	    );
	C := coneFromHData(inequalityMatrix, hyperplanesMatrix);
	if opts.ExtraZeroRows == 0 then MF.cache. computedWeightMatrixCone = C;
	C
	)
    )

----------------------------------------------------------
-- isCoherent
-- check if a matching field is induced by a weight matrix
--
isCoherent = method()
isCoherent(GrMatchingField) := MF -> (
    if MF.cache.?weightMatrix then true else (
	C := weightMatrixCone MF;
	(dim C) == (MF.k * MF.n) -- coherent iff C is full-dimensional
	)
    )
isCoherent(FlMatchingField) := MF -> (
    if MF.cache.?weightMatrix then true else (
	C := weightMatrixCone MF;
	(dim C) == ((max MF.kList)* MF.n) -- coherent iff C is full-dimensional
	)
    )

------------------------
-- computeWeightMatrix
-- finds a weight matrix that induces the matching field
-- unexported (see getWeightMatrix)
computeWeightMatrix = method()
computeWeightMatrix(GrMatchingField) := MF -> (
    if not isCoherent MF then (
	error("expected a coherent matching field");
	);
    C := weightMatrixCone MF;
    CRays := rays C;
    -- construct an interior point of the cone
    weight := first entries transpose sum for c from 0 to numColumns CRays - 1 list CRays_{c};
    matrix for i from 0 to MF.k - 1 list weight_{i*MF.n .. (i+1)*MF.n - 1}
    )

computeWeightMatrix(FlMatchingField) := MF -> (
    if not isCoherent MF then (
	error("expected a coherent matching field");
	);
    C := weightMatrixCone MF;
    CRays := rays C;
    -- construct an interior point of the cone
    weight := first entries transpose sum for c from 0 to numColumns CRays - 1 list CRays_{c};
    matrix for i from 0 to (max MF.kList) - 1 list weight_{i*MF.n .. (i+1)*MF.n - 1}
    )

--------------------------------------------
-- compute linear span of the tropical cone
--
-- assume the initial ideal is toric
-- take the generators x^u - x^v of the initial ideal
-- form a matrix M with rows {.. 1_u .. -1_v ..}
-- kernel M is the linear span of the tropical cone 

removeZeroRows = method()
removeZeroRows(Matrix) := inputMatrix -> (
    nonZeroRowIndices := for i from 0 to numRows inputMatrix - 1 list if not zero inputMatrix^{i} then i else continue;
    inputMatrix^nonZeroRowIndices
    )

linearSpanTropCone = method(
    Options => {
	VerifyToricDegeneration => true
	}
    )

linearSpanTropCone(GrMatchingField) := opts -> MF -> (
    if not MF.cache.?computedLinearSpanTropCone then (
    	if opts.VerifyToricDegeneration and not isToricDegeneration MF then (
	    error("expected GrMatchingField that gives a toric degeneration.");
	    );
    	matchingFieldIdealExponents := exponents \ first entries gens matchingFieldIdeal MF; -- a list of pairs (since generators are binomials)
    	constraintMatrix := matrix apply(matchingFieldIdealExponents, exponentPair -> exponentPair_0 - exponentPair_1);
    	constraintMatrix = removeZeroRows reducedRowEchelonForm sub(constraintMatrix, QQ);
    	MF.cache.computedLinearSpanTropCone = ker constraintMatrix;
	);
    MF.cache.computedLinearSpanTropCone
    )


---------------------------
-- compute the algebraic matroid of the Matching field inside Grassmannian
-- this gives bases for the algebraic matroid of the grassmannian implied by the matching field
algebraicMatroid = method()
algebraicMatroid(GrMatchingField) := MF -> (
    if not MF.cache.?computedAlgebraicMatroid then (
	MF.cache.computedAlgebraicMatroid = matroid transpose gens linearSpanTropCone MF;
	);
    MF.cache.computedAlgebraicMatroid
    )

-- write down the bases of the algebraic matroid as subsets 
algebraicMatroidBases = method()
algebraicMatroidBases(GrMatchingField) := MF -> (
    SS := subsets(toList(1 .. MF.n), MF.k);
    for B in bases algebraicMatroid MF list (i -> SS_i) \ B
    )

-- write down the bases of the algebraic matroid as subsets 
algebraicMatroidCircuits = method()
algebraicMatroidCircuits(GrMatchingField) := MF -> (
    SS := subsets(toList(1 .. MF.n), MF.k);
    for B in circuits algebraicMatroid MF list (i -> SS_i) \ B
    )

-- tope fields
-- a tope field (for Gr(k,n)) is pair:
-- (i) GrMatchingField 
-- (ii) type T = {t_1 .. t_s}
--
-- The notes in the code follow notation of Smith-Loho
-- A matching field is a tope field of type {1 .. 1}
-- the type is the 'right degree vector'
-- for each tuple (i_1,1 .. i_1,t_1 .. i_s,t_s) of the GrMatchingField, 
-- we get one bipartite graph of the tope field where 1 in R is 
-- adjacent to i_1,1 .. i_1,t_1, etc.  
TopeField = new Type of HashTable

topeField = method()
topeField(GrMatchingField) := MF -> (
    new TopeField from {
	"type" => toList(MF.k : 1),
	"matchingField" => MF
	}
    )

topeField(GrMatchingField, List) := (MF, type) -> (
    assert(sum type == MF.k);
    new TopeField from {
	"type" => type,
	"matchingField" => MF
	}
    )

net TopeField := TF -> (
    ("Tope field: n = " | toString TF#"matchingField".n | " and type = " | toString TF#"type")
    )

getTuples(TopeField) := TF -> (
    getTuples TF#"matchingField"
    )

-- isLinkage
-- tests if a tope field is Linkage by checking that for each k+1 subset \tau, 
-- the union of graphs M_\sigma where \sigma \subset \tau is a forest 
--
isLinkage = method()
isLinkage(TopeField) := TF -> (
    result := true;
    MF := TF#"matchingField";
    subsetList := subsets(1 .. MF.n, MF.k);
    subsetIndex := new HashTable from for j from 0 to binomial(MF.n, MF.k)-1 list subsetList_j => j;
    tuples := getTuples MF;
    
    for s in subsets(1 .. MF.n, MF.k + 1) do (
	-- take the union of all edges over the k-subsets of s
        -- list the vertices in L adjacent to each j in R in the union
        -- L edges: 1 .. n
	-- R edges: n+1 .. n+t where t = #TF#"type"
	
	edges := flatten flatten for s' in subsets(s, MF.k) list (
	    tuple := tuples_(subsetIndex#s');  
	    tuplePosition := -1;
	    for typeIndex from 0 to #TF#"type"-1 list (
	        t := TF#"type"_typeIndex;
		 for j from 1 to t list (
		    tuplePosition = tuplePosition +1;
		    {tuple_tuplePosition, MF.n + typeIndex + 1}
		    )
		)
	    );
        
	G := graph edges;
	if not isForest G then (
	    result = false;
	    break
	    );
	);
    
    result
    )

isLinkage(GrMatchingField) := MF -> (
    isLinkage topeField MF
    )

-- tope field amalgamation
amalgamation = method()
amalgamation(ZZ, TopeField) := (i, TF) -> (
    assert(isLinkage TF);
    assert(1 <= i and i <= TF#"matchingField".n);
    MF := TF#"matchingField";
    subsetList := subsets(1 .. MF.n, MF.k);
    subsetIndex := new HashTable from for j from 0 to binomial(MF.n, MF.k)-1 list subsetList_j => j;
    tuples := getTuples MF;
    
    -- construct the new tuples of the matching field:
    newTuples := for s in subsets(1 .. MF.n, MF.k + 1) list (
	-- take the union of all edges over the k-subsets of s
        -- list the vertices in L adjacent to each j in R in the union
	edges := new MutableHashTable from for j from 0 to #TF#"type"-1 list (
	    j => set {}
	    );
	
	for s' in subsets(s, MF.k) do (
	    tuple := tuples_(subsetIndex#s');  
	    tuplePosition := -1;
	    for typeIndex from 0 to #TF#"type"-1 do (
	        t := TF#"type"_typeIndex;
		edges#typeIndex = edges#typeIndex + set for j from 1 to t list (
		    tuplePosition = tuplePosition +1;
		    tuple_tuplePosition
		    );
		);
	    );
        
	matchedL := edges#(i-1);
	matchedR := set {i-1};
	
	-- remove the edges of the union graph to get the amalgamation
	while #matchedL < MF.k+1 do (
	    for j from 0 to #TF#"type"-1 do (
		if not member(j, matchedR) then (
		    edges#j = edges#j - matchedL;
		    if #edges#j == TF#"type"_j then (
			matchedR = matchedR + set {j};
			matchedL = matchedL + edges#j;
			);
		    );
		);
	    );
        
	fold((a,b) -> a | b, for j from 0 to #TF#"type"-1 list sort toList edges#j)
	);
    
    
    newMF := grMatchingField(MF.k+1, MF.n, newTuples);
    newType := for j from 1 to #TF#"type" list if i == j then TF#"type"_(j-1)+1 else TF#"type"_(j-1); 
    
    topeField(newMF, newType)
    )

amalgamation(ZZ, GrMatchingField) := (i, MF) -> (
    TF := topeField MF;
    amalgamation(i, TF)
    )


-- #################
-- # Documentation #
-- #################

beginDocumentation()

doc ///
      Key
        MatchingFields
      Headline
        A package for working with matching fields for Grassmannians and partial flag varieties
      Description
        Text
	  A matching field $\Lambda$ for the Grassmannian Gr($k$, $n$), is a simple combinatorial object.
	  It may be thought of as a choice of initial term for each maximal minor of a generic $k \times n$ matrix 
	  of variables. For example, take $k = 2$ and $n = 4$. Let $X = (x_{i,j})$ be a generic $2 \times 4$ matrix of variables. 
	  Suppose that a matching field $\Lambda$ has tuples $\{12, 31, 14, 32, 24, 34\}$. This means that $\Lambda$
	  distinguishes the term $x_{1,1} x_{2,2}$ from the maximal minors on columns $1$ and $2$ of $X$: $x_{1,1} x_{2,2} - x_{1,2} x_{2,1}$.
	  Similarly for the terms $x_{1,3} x_{2,1}$, $x_{1,1} x_{2,4}$, and so on.
	  
	  If the terms of all maximal minors distinguished by a matching field are their initial terms with respect to a fixed weight matrix,
	  then we say that the matching field is coherent. Each such weight matrix induces a weight vector on the Pluecker coordinates of the 
	  Grassmannian. If the initial ideal of the Pluecker ideal of the Grassmannian with respect to this weight vector is a toric ideal,
	  i.e. a prime binomial ideal, then we say that the matching field gives rise to a toric degeneration of the Grassmannian.
	  By a result of Sturmfels (1996), a matching field gives rise to a toric degeneration if and only if the maximal minors of $X$ form
	  a subalgebra basis (or SAGBI basis) with respect to the order induced by the weight matrix.
	  
	  This concept naturally generalises to partial flag varieties under the Pluecker embedding.
	  
	  The MatchingFields package gives basic functions, to construct many of the well-studied examples of matching fields.
	  Given a matching field $L$, it is straight forward to check whether $L$ is coherent, what is a weight matrix that induces it,
	  and whether is gives rise to a toric degeneration. The package also produces polytopes associated to matching fields and Newton-Okounkov bodies.
        Example
	  L = grMatchingField(2, 4, {{1,2}, {3,1}, {1,4}, {3,2}, {2,4}, {3,4}})
	  isCoherent L
	  getWeightMatrix L
	  isToricDegeneration L
      SeeAlso
      Subnodes
        :Main objects
	  GrMatchingField
	  FlMatchingField
	  
	:Constructing matching fields
	  grMatchingField
	  flMatchingField
	  diagonalMatchingField
	  matchingFieldFromPermutation
	
	:Basic properties and functions
	  getTuples
	  getGrMatchingFields
	  isCoherent
	  getWeightMatrix
	  getWeightPluecker
	  isToricDegeneration
	  (net, FlMatchingField)
	  (symbol ==, GrMatchingField, GrMatchingField)
	  (symbol ==, FlMatchingField, FlMatchingField)
	  
	:Rings, ideals and maps
	  plueckerIdeal
	  matchingFieldIdeal
	  plueckerMap
	  matchingFieldRingMap
	  plueckerAlgebra
	  
	:Convex bodies and polyhedra
	  matchingFieldPolytope
	  NOBody
	  weightMatrixCone
	  
	:Dressians and matroids
	  algebraicMatroid
	  algebraicMatroidCircuits
	  algebraicMatroidBases
	  matroidSubdivision
	  linearSpanTropCone
	
	:Topes and tope fields
	  TopeField
	  topeField
	  (net, TopeField)
	  (getTuples, TopeField)
	  isLinkage
	  amalgamation
///

doc ///
      Key
        plueckerIdeal
	(plueckerIdeal, FlMatchingField)
	(plueckerIdeal, GrMatchingField)
	(Grassmannian, GrMatchingField)
	[plueckerIdeal, MonomialOrder]
      Headline
        The Pluecker ideal of a matching field
      Usage
      	I = plueckerIdeal Lgr
	I = plueckerIdeal Lfl
	I = Grassmannian Lgr
      Inputs
        Lgr: GrMatchingField
	Lfl: FlMatchingField
	MonomialOrder => String 
	  either "default" or "none" (supply "none" only if $L$ is not coherent) 
      Outputs
        I: Ideal
	  The Pluecker ideal associated to the corresponding Grassmannian or partial flag variety
	  with the correct term order given by a weight that induced the matching field
      Description
        Text
	  The Pluecker ideal is the defining ideal of a partial flag variety embedded in a product of Grassmannians, where
	  each Grassmannian is embedded, by the Pluecker embedding, into a suitable projective space.
	  In the case of the Grassmannian Gr($k$, $n$), it is concretely given by kernel of the ring map 
	  $K[P_I : I \subseteq [n],\  |I| = k] \rightarrow K[x_{i,j} : i \in [k], \ j \in [n]]$ where $P_I$ is mapped 
	  to the $k \times k$ maximal minor of the matrix $(x_{i,j})$ whose columns are indexed by the set $I$.
	  It is well-known that this ideal has a Groebner basis consisting of homogeneous quadrics.
	  
	  The function @TO "plueckerIdeal"@ takes a matching field, either for the Grassmannian or a partial flag variety
	  and outputs the Pluecker ideal for that Grassmannian or partial flag variety. The ambient polynomial ring that
	  contains this ideal is constructed to have the term order induced by the matching field.
	  
	Example
	  L = grMatchingField(2, 4, {{1,2}, {1,3}, {1,4}, {2,3}, {2,4}, {3,4}})
	  I = plueckerIdeal L
	  (monoid ring I).Options.MonomialOrder
	  getWeightPluecker L
	
	Text
	  In the above example, the weights for the ambient ring are not the same as the Pluecker weights of the matching field.
	  This is because of the minimum-maximum convention problem. For compatibility with packages such as @TO "Tropical"@, we use
	  the minimum convention in @TO "MatchingFields"@ so the smallest weight with respect to the weight matrix that 
	  induces the matching field is the initial term of a Pluecker form. 
	  However, the monomial ordering given by @TO "Weights"@ uses the 
	  maximum convention, so the ambient ring has weights that are based on the negative of the induced Pluecker Weight.    
	  
	  Note that the given matching field must be coherent. If the matching field is not defined in terms of a weight
	  matrix, then the function will attempt to compute a weight matrix for the matching field. If the matching field is
	  not coherent then the function will produce an error.
        Example
	  L = grMatchingField(2, 4, {{1,2}, {1,3}, {4,1}, {2,3}, {2,4}, {3,4}})
	  isCoherent L
	  -- I = plueckerIdeal L -- "error: expected a coherent matching field"
	Text
	  To construct the pluecker ideal for a non-coherent matching field, set the option MonomialOrder to "none".
	  The resulting ideal is constructed in a polynomial ring with the @TO "GRevLex"@ order.
	Example
	  I = plueckerIdeal(L, MonomialOrder => "none")       
      SeeAlso
        matchingFieldIdeal
      Subnodes
///


doc ///
      Key
        matroidSubdivision
	(matroidSubdivision, GrMatchingField)
        (matroidSubdivision, ZZ, ZZ, List)
      Headline
        The matroid subdivision induced by the Pluecker weight of a coherent matching field
      Usage
        listOfBases = matroidSubdivision L
	listOfBases = matroidSubdivision(k, n, plueckerWeight)
      Inputs
        L: GrMatchingField 
	k: ZZ
	n: ZZ
	plueckerWeight: List
	  the weight of the pluecker coordinates in revLex order using minimum convention
      Outputs
        listOfBases: List
	  Each element is a list of the vertices of a maximal cell of the matroid subdivision of the hypersimplex induced by 
	  the Pluecker weight of the matching field.
      Description
        Text
	  The hypersimplex $\Delta(k, n) \subseteq \RR^{n}$ is the convex hull of the characteristic vectors of all $k$-subsets
	  of $\{1, \dots, n\}$, and we label each vertex with with its corresponding subset. A regular subdivision of the vertices of $\Delta(k, n)$
	  is said to be matroidal if, for each maximal cell of the subdivision, the subsets labelling its vertices form the set of bases of a matroid.
	  The well-known result is: a point lies in the Dressian Dr($k$, $n$), the tropical prevariety of all $3$-term Pluecker relation in Gr($k$, $n$), if and only if
	  it induces a matroidal subdivision of the hypersimplex.
	Example
	  L = grMatchingField(2, 4, {{1,2}, {1,3}, {1,4}, {2,3}, {2,4}, {3,4}})
	  netList matroidSubdivision L -- an octahedron sliced into 2 pieces
	Text
	  Whenever the function @TO "matroidSubdivision"@ is supplied with a Grassmannian matching field, the cached weight that induces the matching field
	  is used for the computation of the matroid subdivision. Note that, if the function is supplied directly with the \textit{plueckerWeight}, then
	  the coordinates are ordered so that the corresponding sets are listed in reverse lexicographic order. 
      SeeAlso
      Subnodes
///

doc ///
      Key
        algebraicMatroid
	(algebraicMatroid, GrMatchingField)
      Headline
        The algebraic matroid of the tropical cone that induces the matroid
      Usage
        M = algebraicMatroid L
      Inputs
        L: GrMatchingField 
      Outputs
        M: "matroid"
	  The algebraic matroid of the cone in Trop Gr$(k,n)$ that induces the matching field.
      Description
        Text
	  Let $V \subseteq \CC^n$ be an affine variety. 
	  The algebraic matroid of $V$ is a matroid whose independent sets $S \subseteq [n]$
	  are the subsets such that the projection from $V$ to the coordinates indexed by $S$
	  is a dominant morphism. Similarly, if $C \subseteq \RR^n$ is a polyhedral cone, then the algebraic matroid
	  of $C$ is the matroid whose independent sets $S \subseteq [n]$ are the subsets such that image of the
	  projection of $C$ onto the coordinates indexed by $S$ is full-dimensional.
	  
	  In the case of the affine cone of Grassmannian under the Pluecker embedding, 
	  there are a few different ways to compute its algebraic matroid. One way is to use its tropicalization.
	  The algebraic matroid of the Grassmannian is equal to the matroid whose bases are the union of all bases of the
	  algebraic matroid for all maximal cones of Trop Gr($k$, $n$).
	  
	  For each coherent matching field, we compute its cone in the tropicalization of the Grassmannian.
	  We compute the algebraic matroid of this cone. To view the bases of this matroid in terms of the $k$-subsets of $[n]$,
	  use the function @TO "algebraicMatroidBases"@. Similarly, to view its circuits use @TO "algebraicMatroidCircuits"@ 
	  
	Example
	  L = grMatchingField(2, 4, {{1,2}, {1,3}, {1,4}, {2,3}, {2,4}, {3,4}})
	  M = algebraicMatroid L
	  netList algebraicMatroidBases L
      SeeAlso
        algebraicMatroidBases
	algebraicMatroidCircuits
      Subnodes
///

doc ///
      Key
        getGrMatchingFields
	(getGrMatchingFields, FlMatchingField)
      Headline
        The Grassmannian matching fields of a Flag matching field
      Usage
        matchingFieldList = getGrMatchingFields L
      Inputs
        L: FlMatchingField 
      Outputs
        matchingFieldList: List
	  The Grassmannian matching fields contained in L.
      Description
        Text
	  This function returns a list of the @TO "GrMatchingField"@s that are contained
	  within the given @TO "FlMatchingField"@.
	Example
	  D = diagonalMatchingField({1,2,3}, 6);
	  getWeightMatrix D
	  netList getGrMatchingFields D
	  D2 = (getGrMatchingFields D)_1;
	  getWeightMatrix D2
	Text
	  The above example constructs the diagonal matching field for the partial
	  flag variety Fl(123; 6), which contains the data for
	  three distinct Grassmannian matching fields. Each @TO "GrMatchingField"@
	  is a diagonal matching field, which are induced by a submatrix of the original
	  weight matrix that induces the flag matching field.
      SeeAlso
      Subnodes

///

doc ///
      Key
         flMatchingField
	(flMatchingField, List, Matrix)
	(flMatchingField, List, ZZ, List)
	(flMatchingField, Matrix)
      Headline
        Construct a matching field for a partial flag variety
      Usage
        L = flMatchingField(kList, weightMatrix)
	L = flMatchingField(kList, n, tuples)
	L = flMatchingField(weightMatrix)
      Inputs
        kList: List
	  positive integers; the sizes of the tuples of the flag matching field
	n: ZZ
	  positive integer; the tuples have entries in 1 .. n
	weightMatrix: Matrix
	  induces the flag matching field 
      Outputs
        L: FlMatchingField
      Description
        Text
	  This function is the basic constructor for
	  matching fields for partial flag varieties, which we simply call
	  flag matching fields. The function outputs an instance of type @TO "FlMatchingField"@,
	  which represents the flag matching field and stores all data related and 
	  computed about it.
	  
	  There are three basic ways to define a flag matching field. The first way is to
	  supply a weight matrix that induces the flag matching field. This produces a flag matching field
	  for the full flag variety.
	Example
	  M = matrix {{0,0,0,0}, {4,2,3,1}, {10, 40, 30, 20}}
	  L1 = flMatchingField M
	  netList getTuples L1
	  isToricDegeneration L1
	Text
	  In the above example, we construct the flag matching field for the full
	  flag variety induced by the given weight matrix. The tuples for the 
	  flag matching field are listed by their size. Similarly to Grassmannian
	  matching fields: @TO "GrMatchingField"@, the function @TO "isToricDegeneration"@
	  checks the equality of the @TO "matchingFieldIdeal"@ and the initial ideal
	  of the @TO "plueckerIdeal"@ with respect to the weight of the matching field.
	  
	  The second way to define a flag matching field
	  is to supply a weight matrix and specify the size of the sets
	  or, in other words, specify the dimensions of the vector spaces in the flags.
	Example
	  L2 = flMatchingField({1,2}, M)
	  netList getTuples L2
	Text
	  The third way to define a flag matching field is by listing out its tuples.
	Example
	  T = getTuples L1
	  L3 = flMatchingField({1,3}, 4, {T_0, T_2})
	  getTuples L3
	  isCoherent L3
	  getWeightMatrix L3
	Text
	  As shown in the example above, the first argument "kList" 
	  specifies the size of the sets.
	  The third argument is a list whose i-th entry is a list of tuples
	  of size "kList_i". In this example, the size of the sets are 1 and 3, 
	  which correspond to "T_0" and "T_2".
	  When a flag matching field is constructed in this way, it is not
	  guaranteed to be coherent, i.e., it may not be induced by a weight matrix.
	  Similarly to Grassmannian matching fields, the function @TO "isCoherent"@
	  checks whether the matching field is coherent and the function @TO "getWeightMatrix"@
	  returns a weight matrix that induces the matching field, if it exists.
	  If the matching field is not coherent, then these methods produce an error.
	  
	  A note of caution. Two different weight matrices may induce the same matching field
	  so the function @TO "getWeightMatrix"@ may return a weight matrix that is
	  different to what may be expected. However, if a matching field is defined 
	  by a weight matrix, then that weight matrix will be returned.
	  
      SeeAlso
        FlMatchingField
        GrMatchingField
	grMatchingField
	isToricDegeneration
	plueckerIdeal
	matchingFieldIdeal
	isCoherent
	getWeightMatrix
      Subnodes
      
///


doc ///
      Key
         matchingFieldIdeal
	(matchingFieldIdeal, FlMatchingField)
	(matchingFieldIdeal, GrMatchingField)
	[matchingFieldIdeal, Strategy]
	[matchingFieldIdeal, MonomialOrder]
      Headline
        The toric ideal of a matching field
      Usage
        I = matchingFieldIdeal L
      Inputs
        L: {GrMatchingField, FlMatchingField}
	Strategy => String
	  either "M2" or "4ti2" the strategy for computing the generators
	MonomialOrder => String 
	  either "default" or "none" (supply "none" only if $L$ is not coherent)
      Outputs
        I: Ideal
	  toric ideal of the matching field
      Description
        Text
	  A matching field $\Lambda$ for the Grassmannian Gr$(k,n)$ associates to each subset $J = \{j_1 < \dots < j_k\}$
	  an ordering of that subset $\Lambda(J) = (j_{\sigma(1)}, \dots, j_{\sigma(k)})$ for some permutation $\sigma \in S_k$.
	  The monomial map associated to a matching field $\Lambda$ is defined as the map that sends each Pluecker 
	  coordinate $p_J$ to the monomial sgn$(\sigma)x_{1, \Lambda(J)_1} x_{2, \Lambda(J)_2} \cdots x_{k, \Lambda(J)_k}$ 
	  where sgn$(\sigma) \in \{+1, -1\}$ is the sign of the permutation. The matching field
	  ideal is the kernel of this monomial map.
	Example
	  L = diagonalMatchingField(2, 4)
	  m = matchingFieldRingMap L
	  I = matchingFieldIdeal L
	  ker m === I
	Text
	  The analogous setup holds for flag matching fields. A flag matching field can be thought of as a union of Grassmannian matching fields.
	  The inclusion of the Grassmannian matching field naturally extends to an inclusion of the corresponding ideals.
	  The flag matching field ideals are also generated by 'incident relations' that involve Pluecker coordinates from distinct pairs of
	  Grassmannians within the flag variety.
	Example
	  L = diagonalMatchingField({1,2}, 4)
	  I = matchingFieldIdeal L
        Text
	  The functions @TO "matchingFieldIdeal"@ and @TO "plueckerIdeal"@ both construct ideals that belong to the same
	  polynomial ring. Similarly, the ring maps constructed by the function @TO "plueckerMap"@ and @TO "matchingFieldRingMap"@
	  have the same target ring. 
	Example
	  I' = plueckerIdeal L
	  ring I === ring I'
	  source plueckerMap L
	  source plueckerMap L === source matchingFieldRingMap L
	  target plueckerMap L
	  target plueckerMap L === target matchingFieldRingMap L
	Text
	  If the matching field is not coherent, then the matching field ideal can be constructed by setting the option MonomialOrder to "none".
	  Doing this sets the monomial order of the target polynomial ring above to @TO "GRevLex"@.
	Example
	  L = grMatchingField(2, 5, {{2,1}, {3,2}, {4,3}, {1,4}, {2,4}, {1,3}, {1,5}, {5,2}, {3,5}, {4,5}})
	  isCoherent L
	  I = matchingFieldIdeal(L, MonomialOrder => "none")
	  (options ring I).MonomialOrder
	Text 	  
	  The option @TO "Strategy"@ determines how the matching field ideal is computed. The default uses the package @TO "FourTiTwo"@. This strategy works
	  by passing in the matrix of the toric ideal to @TO "toricGroebner"@ with the correct weight vector. In the case of Grassmannian matching fields,
	  the columns of the matrix of the toric ideal are exactly the vertices of the matching field polytope. For a flag matching field $\Lambda$, 
	  the matrix of the toric ideal is the top-justified juxtaposition of such matrices for the Grassmannian matching fields contained in $\Lambda$.
	  On the other hand, the strategy "M2" simply uses the in-built function to compute the kernel of the map @TO "matchingFieldRingMap"@.	  
      Caveat
        For some versions of the package @TO "FourTiTwo"@, the strategy "4ti2" may not correctly take into account the weights. See the caveat in the
	documentation of the function @TO "toricGroebner"@. If there are any problems, it may be more reliable to use the option "M2".
      SeeAlso
      Subnodes
///

doc ///
      Key
         matchingFieldPolytope
	(matchingFieldPolytope, FlMatchingField)
	(matchingFieldPolytope, GrMatchingField)
	[matchingFieldPolytope, ExtraZeroRows]
      Headline
        The polytope of a matching field
      Usage
        P = matchingFieldPolytope L
      Inputs
        L: {GrMatchingField, FlMatchingField}
	ExtraZeroRows => ZZ 
	  produces a matching field polytope embedded in a larger space
	  typically used for producing polytopes of flag matching field
      Outputs
        P: Polyhedron
	  polytope of the matching field
      Description
        Text
	  Each matching field defines a projective toric variety whose defining
	  ideal is given by the kernel of the monomial map. See @TO "matchingFieldRingMap"@.
	  The coordinate ring of this toric variety is the Ehrhart ring of 
	  the matching field polytope. Note that for flag matching fields, the
	  toric variety is embedded into a high-dimensional
	  projective space via the Segre embedding whose domain is a product of
	  Grassmannians.
	  
	  Given a matching field $\Lambda$ for the Grassmannian Gr$(k,n)$, the matching field 
	  polytope $P(\Lambda)$ is simply the convex hull of the exponent
	  vectors of the image of Pluecker variables under the monomial map of 
	  $\Lambda$. The polytope naturally lives in the space
	  $\RR^{k \times n}$.
	Example
	  L2 = diagonalMatchingField(2, 4)
	  P2 = matchingFieldPolytope L2
	  fVector P2
	  vertices P2
	Text
	  The columns of the above matrix are the vertices of the matching field
	  polytope $P(\Lambda)$. Each column should be thought of as a $2 \times 4$ 
	  matrix whose entries are listed row by row.
	  
	  A matching field $\Lambda$ for a partial flag variety Fl$(k_1, \dots, k_s; n)$ is a union of matching 
	  fields $\Lambda = \bigcup \Lambda_i$ for some Grassmannians. The matching field polytope for a
	  partial flag variety is the
	  Minkowski sum $P(\Lambda) = \sum P(\Lambda_i)$ of Grassmannian matching field polytopes in $\Lambda$.
	  For this sum to make sense, each Grassmannian matching field polytope
	  must be put into the same space, which is taken to be $\RR^{k_{\max} \times n}$
	  where $k_{\max} = \max\{k_i\}$ is the largest $k$ such that there is a Grassmannian matching field
	  for Gr$(k,n)$ contained in $\Lambda$. If $v \in \RR^{k_i \times n}$ is a vertex for a
	  Grassmannian matching field polytope, then we embed $v$ into $\RR^{k_{\max} \times n}$
	  by joining a suitably sized matrix of zeros to $v$ from below.
	  
	  Embedding a Grassmannian matching field polytope into a higher dimensional space as
	  described is done by specifying the optional value @TO "ExtraZeroRows"@.
	Example
	  L1 = diagonalMatchingField(1, 4)
	  P1 = matchingFieldPolytope(L1, ExtraZeroRows => 1)
	  vertices P1
	  P12 = minkowskiSum(P1, P2)
	  vertices P12
	Text
	  The above example constructs the diagonal matching field polytope for
	  the partial flag variety Fl$(1, 2; 4)$ as a Minkowski sum. 
	  The quick way to do this is as follows.
    	Example
	  L = diagonalMatchingField({1,2}, 4)	
    	  Q = matchingFieldPolytope L 
	  Q == P12
      SeeAlso
        ExtraZeroRows
      Subnodes
        ExtraZeroRows
///

doc ///
      Key
         plueckerMap
	(plueckerMap, FlMatchingField)
	(plueckerMap, GrMatchingField)
	[plueckerMap, MonomialOrder]
      Headline
        The ring map of the Pluecker embedding
      Usage
        m = plueckerMap L
      Inputs
        L: {GrMatchingField, FlMatchingField}
	MonomialOrder => String 
	  either "default" or "none" (supply "none" only if $L$ is not coherent)
      Outputs
        m: RingMap
	  the ring map of the Pluecker embedding
      Description
        Text
          The ring map for the Pluecker embedding of the Grassmannian
	  sends each Pluecker variable $P_J$,
	  where $J$ is a $k$-subset of $[n]$, to its corresponding maximal minor in a
	  generic $k \times n$ matrix of variables $X = (x_{i,j})$.
	  	
	  The domain and codomain of this ring map are naturally equipped with term 
	  orders derived from a weight matrix, which induces the matching field.
	  So, for this function, we require that the matching fields be coherent.
          If a weight matrix is not supplied, then one is automatically computed.
	  If the matching field is not coherent, then an error is thrown.
      	Example
          L = grMatchingField(2, 4, {{1, 2}, {1, 3}, {3, 2}, {1, 4}, {4, 2}, {3, 4}})
	  isCoherent L
	  getWeightMatrix L
	  plueckerMap L
          describe target plueckerMap L
      	Text
          For the above polynomial ring, the monomial order is given by a weight ordering.
	  Note that the weights are based on $-1 \times W$ where $W$ is the weight matrix
	  that induces $L$, displayed using the function @TO "getWeightMatrix"@. 
	  The purpose of $-1$ is to transition between the minimum convention
	  of matching fields and the maximum convention of initial terms in @TO "Macaulay2"@.
	  	
	  The ring map for the Pluecker embedding of a partial flag variety is
	  completely analogous.
      	Example
          L = diagonalMatchingField({1,2}, 4)
	  getWeightMatrix L
	  m = plueckerMap L
	  describe source m
        Text
          The monomial order on the ring of Pluecker variables, 
	  shown above, is also based on $-1 \times W$. More concretely,
	  the weight vector of a Pluecker variable $P_J$ is the weight of 
	  the initial term of the image of the Pluecker variable $m(P_J) = \det(X_J)$ under the map.
	  
	  The monomial map associated to the matching field, see @TO "matchingFieldRingMap"@ 
	  is the map that sends each
	  Pluecker variable $P_J \mapsto \rm{in}(\det(X_J))$ to the lead term of the
	  maximal minor $\det(X_J)$. 
	
      SeeAlso
      Subnodes
///

doc ///
      Key
         weightMatrixCone
	(weightMatrixCone, FlMatchingField)
	(weightMatrixCone, GrMatchingField)
	[weightMatrixCone, ExtraZeroRows]
      Headline
        The cone of weight matrices that induce the matching field
      Usage
        C = weightMatrixCone L
      Inputs
        L: {GrMatchingField, FlMatchingField}
	ExtraZeroRows => ZZ
	  produces a cone embedded in a higher dimensional space
	  typically used for constructing weight matrix cones for flag matching fields
      Outputs
        C: Cone
	  the cone of weight matrices that induce the matching field
      Description
        Text
          Given a coherent matching field $\Lambda$, either for the Grassmannian or partial flag variety,
	  the set of weight matrices that induce $\Lambda$ naturally form a polyhedral cone.
	  The function @TO "weightMatrixCone"@ constructs this cone by writing down a collection of inequalities.
	  To illustrate this assume that $(1,2)$ is a tuple of $\Lambda$. A weight matrix $M = (m_{i,j})$ 
	  induces a matching field with the tuple $(1,2)$ if and only if $m_{1,1} + m_{2,2} < m_{1,2} + m_{2,1}$.
	  Continuing in this way for all other tuples of $\Lambda$ produces the cone of weight matrices.
	  Note, the inequalities, like the one above, are strict. So, in general, only the interior points of 
	  the cone give rise to generic weight matrices that induce the matching field.
	Example
	  L = diagonalMatchingField(2, 4)
	  C = weightMatrixCone L  
	  rays C
	  linealitySpace C
	  dim C
	Text
	  In the above example, we can see that adding a vector from the lineality space 
	  can be interpreted as adding a constant to each element in a specific row or column 
	  of the weight matrix.
	  
	  For matching fields that are not originally defined by a weight matrix, the cone of weight matrices
	  allows us to test if the matching field is coherent. The matching field is coherent if and only if
	  the cone is full dimensional. This is the strategy implemented by the function @TO "isCoherent"@.
        Example
	  L = grMatchingField(2, 3, {{1, 2}, {2, 3}, {3, 1}})
	  isCoherent L
	  dim weightMatrixCone L
	Text
	  In the example above, the cone naturally lives in $\RR^6$ so it is not full dimensional.
	  Therefore, the matching field is not coherent.  
      SeeAlso
      Subnodes
///

doc ///
      Key
         algebraicMatroidBases
	(algebraicMatroidBases, GrMatchingField)
      Headline
        The bases of the algebraic matroid
      Usage
        B = algebraicMatroidBases L
      Inputs
        L: GrMatchingField
      Outputs
        B: List
	  the bases of the algebraic matroid of the matching field as $k$-subsets
      Description
        Text
          Displays the bases of the algebraic matroid associated to the Grassmannian Gr$(k,n)$ matching field
	  in terms of the $k$-subsets of $[n]$. For more details about the matroid, see the function @TO "algebraicMatroid"@.
	Example
	  L = diagonalMatchingField(2, 4)
	  netList algebraicMatroidBases L
      SeeAlso
        algebraicMatroid
	algebraicMatroidCircuits
      Subnodes
///

doc ///
      Key
         algebraicMatroidCircuits
	(algebraicMatroidCircuits, GrMatchingField)
      Headline
        The bases of the algebraic matroid
      Usage
        C = algebraicMatroidCircuits L
      Inputs
        L: GrMatchingField
      Outputs
        C: List
	  the circuits of the algebraic matroid of the matching field as $k$-subsets
      Description
        Text
          Displays the circuits of the algebraic matroid associated to the Grassmannian Gr$(k,n)$ matching field
	  in terms of the $k$-subsets of $[n]$. For more details about the matroid, see the function @TO "algebraicMatroid"@.
	Example
	  L = diagonalMatchingField(2, 5)
	  netList algebraicMatroidCircuits L
      SeeAlso
        algebraicMatroid
	algebraicMatroidCircuits
      Subnodes
///

doc ///
      Key
         isToricDegeneration
	(isToricDegeneration, GrMatchingField)
	(isToricDegeneration, FlMatchingField)
      Headline
        Does the matching field give rise to a toric degeneration
      Usage
        result = isToricDegeneration L
      Inputs
        L: {GrMatchingField, FlMatchingField}
      Outputs
        result: Boolean
	  does the matching field give rise to a toric degeneration
      Description
        Text
          A matching field is said to give rise to a toric degeneration (of the corresponding variety: Grassmannian
	  or partial flag variety) if the matching field ideal is equal to the initial ideal of the Pluecker ideal
          with respect the weight order that induces the matching field. For further details on each of these ideals
	  see the functions @TO "matchingFieldIdeal"@ and @TO "plueckerIdeal"@.
	Example
	  L = diagonalMatchingField(2, 4)
	  I = plueckerIdeal L
	  J = matchingFieldIdeal L
	  J == ideal leadTerm(1, I)
	  isToricDegeneration L
	Text
	  In the above example, the last two tests are the same.
	  
	  If the matching field provided is not defined in terms of a 
	  weight matrix then one is automatically computed for it.
	  If the matching field is not coherent then this will produce an error.
      SeeAlso
        matchingFieldIdeal
	plueckerIdeal
      Subnodes
///

doc ///
      Key
         getTuples
	(getTuples, FlMatchingField)
	(getTuples, GrMatchingField)
      Headline
        The tuples of a matching field
      Usage
        tuples = getTuples L 
      Inputs
        L: {GrMatchingField, FlMatchingField}
      Outputs
        tuples: List
	  A list of subsets of $1, \dots, n$; the tuples of the matching field
      Description
        Text
          A matching field $\Lambda$ for the Grassmannian Gr$(k, n)$ is a collection tuples $\Lambda(J)$ for each 
	  $k$-subset $J \subseteq [n]$. The entries of the tuple form a permutation of $J$, so in some literature
	  $\Lambda(J)$ is taken to be the element of the symmetric group $\sigma \in S_k$ such that
	  $\Lambda(J) = (j_{\sigma(1)}, j_{\sigma(2), \dots, j_{\sigma(k)}})$ where $J = \{j_1 < j_2 < \dots < j_k\}$.
	Example
	  L = diagonalMatchingField(2, 4) 
	  getTuples L
	Text
	  The tuples are stored such that their underlying sets are in RevLex order, which is the order
	  produced by the method @TO "subsets"@.
	  
	  For flag matching fields, the tuples are stored as a list of list of tuples for each Grassmannian
	  matching field contained within.
	Example
	  L = diagonalMatchingField({1,2}, 4) 
	  netList getTuples L
      SeeAlso
        grMatchingField
	flMatchingField
	diagonalMatchingField
      Subnodes
///

doc ///
      Key
         isCoherent
	(isCoherent, FlMatchingField)
	(isCoherent, GrMatchingField)
      Headline
        Is the matching field coherent
      Usage
        result = isCoherent L
      Inputs
        L: {GrMatchingField, FlMatchingField}
      Outputs
        result: Boolean
	  is the matching field coherent, i.e., induced by a weight matrix
      Description
        Text
          We say that a matching field $\Lambda$ is coherent if it is induced by a weight matrix.
	  Note that we use the minimum convention for weight matrices however for polynomial rings, the
	  @TO "Weights"@ option for @TO "MonomialOrder"@ uses the maximum convention.
	Example
	  L1 = grMatchingField(2, 4, {{1,2}, {1,3}, {2,3}, {1,4}, {2,4}, {4,3}})
	  isCoherent L1
	  getWeightMatrix L1
	  L2 = grMatchingField(2, 3, {{1,2}, {2,3}, {3,1}})
	  isCoherent L2
	Text
	  In the examples above, the matching fields are defined in terms of their tuples. To check whether the
	  matching fields are coherent, the weight matrix cone is constructed, see the function @TO "weightMatrixCone"@.
	  The matching field is coherent if and only if the weight matrix cone is full dimensional.
	  If the matching field happens to be coherent, then an interior point is used for any further
	  computations that require a weight matrix.
	  
      SeeAlso
        weightMatrixCone
      Subnodes
///

doc ///
      Key
        RowNum
      Headline
        the row of the diagonal weight matrix to permute
      Usage
        Lgr = matchingFieldFromPermutation(k, n, S, RowNum => r)
	Lfl = matchingFieldFromPermutation(kList, n, S, RowNum => r) 
      Inputs
        k: ZZ
	kList: List
	n: ZZ
	S: List
	  a permutation of $1, \dots, n$
	r: ZZ
	  an integer at most $k$ or at most $\max(kList)$
      Outputs
        Lgr: GrMatchingField
	Lfl: FlMatchingField
      Description
        Text
          The option @TO "RowNum"@ chooses the row of the diagonal matching field weight matrix to permute.
	  By default the value is $2$.
	Example
	  getWeightMatrix matchingFieldFromPermutation(3, 6, {4,5,6,1,2,3}, RowNum => 1)
	  getWeightMatrix matchingFieldFromPermutation(3, 6, {4,5,6,1,2,3}, RowNum => 2)
	  getWeightMatrix matchingFieldFromPermutation(3, 6, {4,5,6,1,2,3}, RowNum => 3)  
      SeeAlso
        matchingFieldFromPermutation
      	diagonalMatchingField
      Subnodes
///


doc ///
      Key
        ExtraZeroRows
      Headline
        enlarging a matrix with zero rows 
      Description
        Text
          The option @TO "ExtraZeroRows"@ is used by the functions @TO "matchingFieldPolytope"@ and
	  @TO "weightMatrixCone"@. In each case, the option controls the ambient space of the polyhedron.
	  By default the value is zero. It is typically used internally for computing Minkowski sums of
	  polyhedra that would ordinarily belong to different ambient spaces.
	Example
	  L = diagonalMatchingField(2, 4)
	  P = matchingFieldPolytope(L, ExtraZeroRows => 1)
	  vertices P
	  C = weightMatrixCone(L, ExtraZeroRows => 1)
	  rays C
	Text
	  In the above examples, the polyhedral object typically live in the space $\RR^{2 \times 4}$. However,
	  by adding an additional row, the objects live in $\RR^{3 \times 4} \cong \RR^{12}$.
	  Reading the down the entries of columns corresponds to reading row-by-row the entries of the corresponding
	  matrix. 
      SeeAlso
        matchingFieldPolytope
	weightMatrixCone
      Subnodes
///


doc ///
      Key
        GrMatchingField
      Headline
        the class of Grassmannian matching fields 
      Description
        Text
	  Common ways to define Grassmannian matching fields:
	  
	  @UL {
	    {TO {"grMatchingField"}, "-- defined in terms of tuples or a weight matrix"},
	    {TO {"diagonalMatchingField"}, "-- the diagonal matching field"},
	    {TO {"matchingFieldFromPermutation"}, "-- family of matching fields indexed by permutations"}
      	  }@
	  
	  Two Grassmannian matching fields are said to be equal if and only if their tuples are the same.
	  
	  {\bf Technical details.}
	  A Grassmannian matching field is derived from the class @TO "HashTable"@. All
	  Grassmannian matching fields have the following fields:
	  
	  @ UL {
	    {"k of type ZZ"},
	    {"n of type ZZ"},
	    {"tuples of type List, accessible with", TO {"getTuples"}},
	    {"cache"}
	  }@
      
	  Everything else, including: weight matrices; polynomial rings, maps and ideals; polyhedra
	  such as the weight matrix cone and matching field polytope, are all stored inside the cache.
	  Note that the package does not export the keys, such as k or n. 
	  If you wish to directly address the contents of the
	  GrMatchingField, then use "debug MatchingFields".
      SeeAlso
        FlMatchingField
	grMatchingField
      Subnodes
///


doc ///
      Key
        FlMatchingField
      Headline
        the class of matching fields for partial flag varieties
      Description
        Text
	  Common ways to define flag matching fields:
	  
	  @ UL { 
	    {TO {"flMatchingField"}, "-- defined in terms of tuples or a weight matrix"},
	    {TO {"diagonalMatchingField"}, "-- the diagonal matching field"},
	    {TO {"matchingFieldFromPermutation"}, "-- family of matching fields indexed by permutations"}
	  }@
      	  
	  {\bf Technical details.}
	  A flag matching field is derived from the class @TO "HashTable"@. All
	  flag matching fields have the following fields:
	  
	  @ UL {
	    {"kList of type ZZ"},
	    {"n of type ZZ"}, 
	    {"grMatchingFieldList of type List, the list of Grassmannian matching fields contained within,
	    accessible with the function", TO "getGrMatchingFields"},
	    {"cache"}
	  }@
	  
	  Everything else, including: weight matrices; polynomial rings, maps and ideals; polyhedra
	  such as the weight matrix cone and matching field polytope, are all stored inside the cache.
	  Note that the package does not export the keys, such as kList or n. 
	  If you wish to directly address the contents of the
	  FlMatchingField, then use "debug MatchingFields".
      SeeAlso
        GrMatchingField
	flMatchingField
      Subnodes
///

doc ///
      Key
         matchingFieldFromPermutation
        (matchingFieldFromPermutation, List, ZZ, List)
	(matchingFieldFromPermutation, ZZ, ZZ, List)
	[matchingFieldFromPermutation, UsePrimePowers]
	[matchingFieldFromPermutation, PowerValue]
	[matchingFieldFromPermutation, RowNum]
	[matchingFieldFromPermutation, ScalingCoefficient]
      Headline
        matching field parametrised by permutations
      Usage
        Lgr = matchingFieldFromPermutation(k, n, S)
	Lfl = matchingFieldFromPermutation(kList, n, S)
      Inputs
        k: ZZ
	  positive integer; the size of the tuples of the Grassmannian matching field
	kList: List
          positive integers; the sizes of the tuples of the flag matching field
	n: ZZ
	  positive integer; the tuples have entries in 1 .. n
        RowNum => ZZ
	  which row of the digonal weight matrix to permute
	UsePrimePowers => Boolean
	  use multiples of prime power used for the entries of the diagonal weight matrix
	PowerValue => ZZ
	  use multiples of powers this value in the diagonal weight matrix 
	ScalingCoefficient => ZZ
	  the value by which to scale the permuted row of the diagonal weight matrix
      Outputs
	Lgr: GrMatchingField
	Lfl: FlMatchingField
      Description
        Text
	  Let $M_0 \in \RR^{k \times n}$ be a matrix that induced the diagonal matching field.
	  Usually, we take this matrix to be $(m_{i,j})$ with $m_{i,j} = (n-j)n^(i-2)$ if $i > 1$ and $m_{i,j} = 0$ if $i = 1$.
	  Note that this matrix is different to the weight matrix of the matching field produced by the function @TO "diagonalMatchingField"@,
	  however the matching fields are the same. 
	  Given a permutation $\sigma \in S_n$, the matching field associated to $\sigma$ has weight matrix
	  $M_\sigma$, which is the same as $M_0$ except in the second row, which is given by $\sigma(1), \sigma(2), \dots, \sigma(n)$.
	  If $\sigma = (n, n-1, \dots, 1) \in S_n$ is the permutation written in single-line notation, then the matching field induced by $M_\sigma$ is
	  the diagonal matching field.
	Example
	  L0 = diagonalMatchingField(3, 6) 
	  getWeightMatrix L0
	  L1 = matchingFieldFromPermutation(3, 6, {6,5,4,3,2,1})
	  getWeightMatrix L1
	  L0 == L1
	  L2 = matchingFieldFromPermutation(3, 6, {1,3,2,6,4,5})
	  getWeightMatrix L2
	  L3 = matchingFieldFromPermutation({1,2,3}, 6, {1,4,2,3,6,5})
	  getWeightMatrix L3
	Text
	  The optional argument @TO "RowNum"@ is used to change which row of $M_0$ is permuted.
	Example
	  L4 = matchingFieldFromPermutation(3, 6, {1,4,2,3,6,5}, RowNum => 3)
	  getWeightMatrix L4
	Text
	  The optional argument @TO "UsePrimePowers"@ is used to modify the original diagonal weight matrix $M_0$. If
	  the option is set to true then we use the matrix with entries $m_{i,j} = (n-j) p^(i-2)$ if $i>1$ and $m_{i,j} = 0$
	  if $i = 1$, where $p \ge n$ is the smallest prime number greater than or equal to $n$.
        Example
	  L5 = matchingFieldFromPermutation(3, 6, {1,3,2,4,6,5}, UsePrimePowers => true)
	  getWeightMatrix L5
	Text
	  The optional argument @TO "PowerValue"@ is used to give a specific value to $p$ in the diagonal weight matrix
	  as described above. If the value is not positive, then the argument is ignored. This option should be used carefully.
	  If the power value is set incorrectly (typically by setting a value that is too low) then the weight matrix may not be {\it generic},
	  i.e., it does not define a matching field. However, in such a case, the function produces a matching field without error, 
	  as shown in the example $L7$ below. Working with such matching fields may lead to unexpected behaviours.
	Example
	  L6 = matchingFieldFromPermutation(3, 6, {1,3,2,4,6,5}, PowerValue => 10)
	  getWeightMatrix L6
	  L7 = matchingFieldFromPermutation(3, 6, {5,4,3,2,1,0}, PowerValue => 1)
	  getWeightMatrix L7
	Text
	  Any positive integer supplied to the argument @TO "PowerValue"@ takes precedence over @TO "UsePrimePowers"@. 
	  The optional argument @TO "ScalingCoefficient"@ is used to scale the entries of the row that is permuted by the permutation.
	  If @TO "UsePrimePowers"@ is set to true, and $p \ge n$ is the smallest prime number less than $n$, then the scaling coefficient
	  can be set to any $c \in \{1, 2, \dots, p-1\}$ and the resulting weight matrix is guaranteed to be generic.
	Example
	  L8 = matchingFieldFromPermutation(3, 6, {6,1,5,2,3,4}, UsePrimePowers => true, ScalingCoefficient => 3)
	  getWeightMatrix L8
	  isToricDegeneration L8
	Text
	  The above is an example of a {\it hexagonal matching field}, which does not give rise to a toric degeneration of
	  the Grassmannian Gr$(3, 6)$.
      SeeAlso
        RowNum
	UsePrimePowers
	PowerValue
	ScalingCoefficient
      Subnodes
        RowNum
	UsePrimePowers
	ScalingCoefficient
///

doc ///
      Key
         NOBody
        (NOBody, GrMatchingField)
	(NOBody, FlMatchingField)
      Headline
        Newton-Okounkov body of the matching field
      Usage
        D = NOBody L
      Inputs
        L: {GrMatchingField, FlMatchingField}
      Outputs
        D: Polyhedron
	  Newton-Okounkov body of the matching field
      Description
        Text
	  The Pluecker algebra is generated by Pluecker forms given by top-justified
	  minors of a generic matrix. The Pluecker algebra can be constructed with
	  the function @TO "plueckerAlgebra"@, of the image of the Pluecker ring map that can be
	  accessed with the function @TO "plueckerMap"@. Note that the ambient ring
	  containing the Pluecker algebra has a weight-based term order that comes from
	  the matching field. We compute a subalgebra basis (SAGBI basis) using the
	  package @TO "SubalgebraBases"@ for the Pluecker algebra.
	  
	  The Newton-Okounkov body of the matching field is constructed from this subalgebra basis.
	  In the case of Grassmannian matching fields, the NO body is simply the convex
	  hull of the exponent vectors of the initial terms of the subalgebra basis.
	  If the matching field gives rise to a toric degeneration (see the function @TO "isToricDegeneration"@)
	  then the NO body coincides with the matching field polytope
	  (see @TO "matchingFieldPolytope"@) because the maximal minors form a subalgebra basis for the Pluecker algebra.
	Example
	  L = diagonalMatchingField(2, 4)
	  P = matchingFieldPolytope L
	  vertices P
	  noBody = NOBody L
	  vertices noBody
	  P == noBody
	Text
	  In the case of flag matching fields, the NO body is computed in a similar way.
	  First a subalgebra basis is computed for the Pluecker algebra.
	  However, to construct the NO body from the subalgebra basis, we need to take into account 
	  the grading on the Pluecker forms. From the geometric perspective, we are simply using the
	  Segre embedding to view the flag variety as a subvariety of a suitably large projective space.
	Example
	  L = diagonalMatchingField({1,2}, 4)
	  noBody = NOBody L
	  vertices noBody
	  noBody == matchingFieldPolytope L
	Text
	  Note that the matching field polytope is equal to the NO body if and only if the matching field
	  gives rise to a toric degeneration. So, for a {\it hexagonal matching field} for Gr$(3,6)$, the
	  NO body has an additional vertex. We construct a hexagonal matching field using the function
	  @TO "matchingFieldFromPermutation"@ as follows.
	Example
	  L = matchingFieldFromPermutation(3, 6, {6,1,5,2,3,4}, UsePrimePowers => true, ScalingCoefficient => 3)
	  isToricDegeneration L
	  vertices NOBody L
      SeeAlso
        matchingFieldFromPermutation
	isToricDegeneration
	SubalgebraBases
        plueckerAlgebra
	plueckerMap
      Subnodes
///

doc ///
      Key
        UsePrimePowers
	PowerValue
      Headline
        use a diagonal weight matrix with multiples of certain powers
      Usage
        Lgr = matchingFieldFromPermutation(k, n, S, UsePrimePowers => b, PowerValue => v)
	Lfl = matchingFieldFromPermutation(kList, n, S, UsePrimePowers => b, PowerValue => v) 
      Inputs
        k: ZZ
	kList: List
	n: ZZ
	S: List
	  a permutation of $1, \dots, n$
	r: Boolean
	  set whether prime powers are to be used
	v: ZZ
	  set the value of the powers to be used
      Outputs
        Lgr: GrMatchingField
	Lfl: FlMatchingField
      Description
        Text
	  The options @TO "UsePrimePowers"@ and @TO "PowerValue"@ are optional arguments for the function
	  @TO "matchingFieldFromPermutation"@, which constructs a matching field by permuting a row (usually the second row)
	  of a weight matrix that gives rise to the diagonal matching field.
	  
          If the option @TO "UsePrimePowers"@ is set to true, then the underlying {\it diagonal} weight matrix is given by
	  $M_0 = (m_{i,j})$ with $m_{i,j} = (n-j) p^(i-2)$ if $i > 1$ and $m_{i,j} = 0$ if $i = 1$, where
	  $p \ge n$ is the smallest prime number greater than or equal to $n$.
	Example
	  getWeightMatrix matchingFieldFromPermutation(3, 6, {1,2,3,4,5,6}, UsePrimePowers => false)
	  getWeightMatrix matchingFieldFromPermutation(3, 6, {1,2,3,4,5,6}, UsePrimePowers => true)
    	Text
	  To set the value of $p$ in the above matrix to a specific value, use the option @TO "PowerValue"@.
	Example
	  getWeightMatrix matchingFieldFromPermutation(3, 6, {1,2,3,4,5,6}, PowerValue => 10)
      	Text
	  The option @TO "PowerValue"@ overrides the option @TO "UsePrimePowers"@. 
	  
	  {\bf Warning.} Certain values for the option @TO "PowerValue"@ will produce weight matrices that are
	  not {\it generic}, i.e., the initial term of the corresponding Pluecker forms are not all monomials, 
	  hence the weight matrix does not define a matching field. The function @TO "matchingFieldFromPermutation"@ will
	  produce a matching field, which may lead to unexpected behaviours. 
	  
	  {\bf Precise details.} The value of $p$ is determined as follows. 
	  The option @TO "PowerValue"@ is used if it is a positive value. If @TO "PowerValue"@ is not positive
	  then the function checks to see if the option @TO "UsePrimePowers"@ is set to true. If it is, then
	  the value of $p$ is set to be the small prime number greater than or equal to $n$. Otherwise, if
	  @TO "UsePrimePowers"@ is set to false, then $p$ is set to be $n$.
      SeeAlso
        matchingFieldFromPermutation
        RowNum
      Subnodes
///

doc ///
      Key
        ScalingCoefficient
      Headline
        scale the permuted row of the weight matrix
      Usage
        Lgr = matchingFieldFromPermutation(k, n, S, ScalingCoefficient => c)
	Lfl = matchingFieldFromPermutation(kList, n, S, ScalingCoefficient => c) 
      Inputs
        k: ZZ
	kList: List
	n: ZZ
	S: List
	  a permutation of $1, \dots, n$
	c: ZZ
	  scale the value of permuted row by $c$
      Outputs
        Lgr: GrMatchingField
	Lfl: FlMatchingField
      Description
        Text
	  The function @TO "matchingFieldFromPermutation"@ constructs a weight matrix
	  by permuting the row of a weight matrix that induces the diagonal matching field.
	  The option @TO "ScalingCoefficient"@ sets the scaling coefficient of the row
	  being permuted, which by default is $1$.
	
	  Note that by setting the option @TO "UsePrimePowers"@ to true, it guarantees that the
	  weight matrix is {\it generic}, i.e., the matching field is well defined, as long as the scaling coefficient is less
	  than the prime power used.
	Example	  
	  getWeightMatrix matchingFieldFromPermutation(3, 6, {1, 3, 2, 4, 6, 5}, UsePrimePowers => true, ScalingCoefficient => 1)
	  getWeightMatrix matchingFieldFromPermutation(3, 6, {1, 3, 2, 4, 6, 5}, UsePrimePowers => true, ScalingCoefficient => 2)
	  getWeightMatrix matchingFieldFromPermutation(3, 6, {1, 3, 2, 4, 6, 5}, UsePrimePowers => true, ScalingCoefficient => 3)
      SeeAlso
        matchingFieldFromPermutation
        RowNum
	UsePrimePowers
      Subnodes
///

doc ///
      Key
         diagonalMatchingField
        (diagonalMatchingField, ZZ, ZZ)
	(diagonalMatchingField, List, ZZ)
	(diagonalMatchingField, ZZ)
      Headline
        the diagonal matching field
      Usage
        Lgr = diagonalMatchingField(k, n)
	Lfl = diagonalMatchingField(kList, n)
	Lfl = diagonalMatchingField(n)
      Inputs
        k: ZZ
	kList: List
	n: ZZ
      Outputs
        Lgr: GrMatchingField
	  diagonal Grassmannian matching field
	Lfl: FlMatchingField
	  diagonal flag matching field
      Description
        Text
	  The diagonal matching field is defined to be the matching field
	  whose tuples are all in ascending order. It is a coherent matching field
	  so it is induced by a weight matrix.
	  
	  The weight matrix used to construct the diagonal matching field us given by
	  $M = (m_{i,j})$ with $m_{i,j} = (i-1)(n-j+1)$.
	Example
	  L = diagonalMatchingField(3, 6)
	  getWeightMatrix L
	Text
	  The function @TO "diagonalMatchingField"@ can be used in three different ways.
	  If it is supplied two integers $(k,n)$ then it produces the diagonal matching field
	  for the Grassmannian, as shown in the above example.
	  If it is supplied a single integer $n$, then it produces the diagonal matching field
	  for the full flag variety. The matching fields of the full flag variety have tuples of
	  size $1, 2, \dots, n-1$.
	  The function can be made to produce diagonal matching fields for partial flag varieties
	  by supplying it a list $kList$ and integer $n$. The sizes of the tuples are the entries
	  of $kList$.
	Example
	  L = diagonalMatchingField 4;
	  netList getTuples L
	  L = diagonalMatchingField({1, 2}, 5);
    	  netList getTuples L
	Text
	  Diagonal matching fields always give rise to toric degenerations
	  of Grassmannians and flag varieties. In the literature, 
	  this toric degeneration is also known as Gelfand-Tsetlin
	  degeneration. The matching field polytopes for the diagonal matching field, 
	  which can be constructed with the function @TO "matchingFieldPolytope"@,
	  are unimodularly equivalent to Gelfand-Tsetlin polytopes.   
      SeeAlso
        GrMatchingField
	FlMatchingField
	isToricDegeneration
	matchingFieldPolytope
      Subnodes
///

doc ///
      Key
         matchingFieldRingMap
        (matchingFieldRingMap, FlMatchingField)
        (matchingFieldRingMap, GrMatchingField)
	[matchingFieldRingMap, MonomialOrder]
      Headline
        monomial map of the matching field
      Usage
        m = matchingFieldRingMap L
      Inputs
        L: {GrMatchingField, FlMatchingField}
	MonomialOrder => String
	  either "default" or "none" (supply "none" only when $L$ is not coherent)
      Outputs
        m: RingMap
	  monomial ring map whose kernel is the matching field ideal
      Description
        Text
	  Each tuple $J = (j_1, j_2, \dots, j_k)$ of a matching field defines a monomial given by
	  $m(J) = c x_{1, j_1} x_{2, j_2} \dots x_{k, j_k}$ where the coefficient $c \in \{+1, -1\}$ is
	  the sign of the permutation that permutes $J$ into ascending order. Equivalently,
	  $c = (-1)^d$ where $d = |\{(a, b) \in [k]^2 : a < b, j_a > j_b \}|$ is the number of descents of
	  $J$. The monomial $m(J)$ is the lead term of the corresponding Pluecker form with respect to the
	  weight order given by the matching field.
	Example
	  L = matchingFieldFromPermutation(2, 4, {2, 3, 4, 1})   
	  getTuples L
	  matchingFieldRingMap L
	  plueckerForms = matrix plueckerMap L
	  leadTerm plueckerForms
	  leadTerm plueckerForms == matrix matchingFieldRingMap L
	Text
	  Note that the polynomial rings have weight-based term orders that depend on a weight matrix that
	  induces the matching field. So if the matching field supplied is not coherent then function gives an
	  error. To check that a matching field is coherent use the function @TO "isCoherent"@.
      SeeAlso
        getTuples
	plueckerMap
	isCoherent
      Subnodes
///

doc ///
      Key
         getWeightPluecker
        (getWeightPluecker, FlMatchingField)
        (getWeightPluecker, GrMatchingField)
      Headline
        weight of the Pluecker variables induced by the weight matrix
      Usage
        W = getWeightPluecker L
      Inputs
        L: {GrMatchingField, FlMatchingField}
      Outputs
        W: List
	  weights of the Pluecker variables induced by the matching field
      Description
        Text
	  Suppose that a coherent matching field is induced by a $k \times n$ weight matrix $M$.
	  The Pluecker forms are minors of a generic matrix of variables. For example, for the Grassmannian
	  the Pluecker forms are the maximal minors. The weight matrix $M$ is {\it generic}, which is equivalent
	  to the property: the initial form of each Pluecker form with respect to $M$ is a monomial.
	  The weight of the initial term of each Pluecker form is the induced weight on the ring in the Pluecker
	  variables, which is given by the function @TO "getWeightPluecker"@. By convention, the Pluecker variables
	  are listed such that their subsets are in RevLex order, which is the order given by the function @TO "subsets"@.
	  
	  An equivalent formulation is: the Pluecker weight vector is the tuple of tropical determinants of $M$, also
	  known as the image of $M$ under the {\it tropical Stiefel map} (or its natural generalisation to partial
	  flag varieties).
        Example
	  L = diagonalMatchingField(2, 4)
	  getWeightMatrix L
	  getWeightPluecker L
	Text
	  Note that the polynomial rings associated to a matching field have weight vectors based on the weight matrix
	  given by @TO "getWeightMatrix"@ and weight vector given by @TO "getWeightPluecker"@. The package @TO "MatchingFields"@
	  uses a minimum convention but the initial terms of polynomials uses the maximum convention so the weight vectors may look
	  a little different.
	Example
	  m = matchingFieldRingMap L
	  describe source m
	  describe target m
      SeeAlso
        getWeightMatrix
	matchingFieldRingMap
      Subnodes
///

doc ///
      Key
         getWeightMatrix
        (getWeightMatrix, FlMatchingField)
        (getWeightMatrix, GrMatchingField)
      Headline
        weight matrix that induces the matching field
      Usage
        M = getWeightMatrix L
      Inputs
        L: {GrMatchingField, FlMatchingField}
      Outputs
        M: Matrix
	  weight matrix that induces the matching field
      Description
        Text
	  If the supplied matching field is coherent, then this function returns a weight matrix that
	  induces the matching field. If the matching field was originally defined by a weight matrix then
	  that weight matrix is returned. Otherwise, a weight matrix is computed.
	  The weight matrix is computed by computing the weight matrix cone, which can be returned with the
	  function @TO "weightMatrixCone"@. If the supplied matching field is not coherent, then the function
	  gives an error.
	Example
	  L = diagonalMatchingField(2, 4)
	  getWeightMatrix L
	  L = grMatchingField(2, 4, {{1,2}, {1,3}, {3,2}, {1,4}, {4,2}, {3,4}})
	  isCoherent L
	  getWeightMatrix L
	Text
	  The weight on the ring containing the Pluecker forms, i.e., minors of a generic matrix, is based on
	  the weight matrix returned by @TO "getWeightMatrix"@. Note that the package @TO "MatchingFields"@ uses
	  the minimum convention but polynomial ring weight vectors use the maximum convention so some
	  conversion is required.
	Example
	  plueckerMap L
	  R = target plueckerMap L
	  describe R
      SeeAlso
        getWeightPluecker
	plueckerMap
	weightMatrixCone
      Subnodes
///

doc ///
      Key
         linearSpanTropCone
        (linearSpanTropCone, GrMatchingField)
	[linearSpanTropCone, VerifyToricDegeneration]
	VerifyToricDegeneration
      Headline
        linear span of the tropical cone associated to the matching field
      Usage
        linSpace = linearSpanTropCone L
      Inputs
        L: GrMatchingField
	VerifyToricDegeneration => Boolean
	  controls if @TO "isToricDegeneration"@ is run
      Outputs
        linSpace: Module
	  a free QQ-module, the linear span of the cone in the tropicalisation
	  corresponding to the matching field
      Description
        Text
	  Suppose that $I$ is an ideal and $in_w(I)$ is a binomial initial ideal of $I$ with
	  resepct to a weight $w$.
	  Let $C_w$ be the cone in the Groebner fan of $I$ that contains $w$ in its relative interior.
	  The linear span of $C_w$ can be constructed from a generating set of $in_w(I)$. Each generator
	  $x^u - x^v$ gives a hyperplane defined by kernel of $(0 .. 0, 1_u, 0 .. 0, -1_v, 0 .. 0)$.
	  The intersection of these hyperplanes gives the linear span of the Groebner cone.
	  
	  The function @TO "linearSpanTropCone"@ checks if the supplied matching field gives rise to 
	  a toric degeneration, which happens if and only if the initial ideal of
	  the Pluecker ideal is toric, i.e., the ideal is generated by binomials and is prime.
	  If it is already known that the matching field gives rise to a toric degeneration then
	  set the option @TO "VerifyToricDegeneration"@ to false to avoid repeating this check.
	  
	  The linear span is a realisation of the algebraic matroid associated to the matching field.
	  See the function @TO "algebraicMatroid"@.
	Example
	  L = diagonalMatchingField(2, 4)
	  linearSpanTropCone L
	  algebraicMatroid L == matroid transpose gens linearSpanTropCone L
      SeeAlso
        algebraicMatroid
	isToricDegeneration
      Subnodes
///

doc ///
      Key
         grMatchingField
        (grMatchingField, Matrix)
	(grMatchingField, ZZ, ZZ, List)
      Headline
        Construct a matching field for the Grassmannian variety
      Usage
	L = grMatchingField(weightMatrix)
	L = grMatchingField(k, n, tuples)
      Inputs
        k: ZZ
	  positive integer; the size of the tuples of the matching field
	n: ZZ
	  positive integer; the tuples have entries in 1 .. n
	weightMatrix: Matrix
	  induces the matching field 
      Outputs
        L: GrMatchingField
      Description
        Text
	  This function is the basic constructor for Grassmannian
	  matching fields. The function outputs an instance of type @TO "GrMatchingField"@,
	  which represents the matching field and stores all data related and 
	  computed about it.
	  
	  There are two basic ways to define a Grassmannian matching field. The first way is to
	  supply a weight matrix that induces the matching field. This produces a coherent matching field
	  and is well-defined if the matrix is {\it generic}.
	Example
	  M = matrix {{0,0,0,0,0,0}, {1,6,2,5,3,4}, {60,50,10,20,40,30}}
	  L1 = grMatchingField M
	  getTuples L1
	  isToricDegeneration L1
	Text
	  In the above example, we construct the Grassmannian matching field 
	  induced by the given weight matrix. The tuples for the 
	  matching field are listed in RevLex order. The function @TO "isToricDegeneration"@
	  checks the equality of the @TO "matchingFieldIdeal"@ and the initial ideal
	  of the @TO "plueckerIdeal"@ with respect to the weight inducing the matching field.
	  
	  The second way to define a Grassmannian matching field
	  is to list out its tuples.
	Example
	  T = {{1,4}, {2,4}, {3,4}, {3,1}, {3,2}, {1,2}}
	  L2 = grMatchingField(2, 4, T)
	  getTuples L2
	  isCoherent L2
	  getWeightMatrix L2
	Text
	  As shown in the example above, the first argument "k" 
	  specifies the size of the tuples.
	  The third argument is a list of the tuples.
	  Note that the tuples can be supplied in any order. 
	  If the list of tuples is not correct, i.e. if some are missing or duplicated then 
	  the function raises an error.
	  When a Grassmannian matching field is constructed in this way, it is not
	  guaranteed to be coherent, i.e., it may not be induced by a weight matrix.
	  The function @TO "isCoherent"@
	  checks whether the matching field is coherent and the function @TO "getWeightMatrix"@
	  returns a weight matrix that induces the matching field, if it exists.
	  If the matching field is not coherent, then these methods produce an error.
	  
	  A note of caution. Two different weight matrices may induce the same matching field
	  so the function @TO "getWeightMatrix"@ may return a weight matrix that is
	  different to what may be expected. However, if a matching field is defined 
	  by a weight matrix, then that weight matrix will be returned.
      SeeAlso
        GrMatchingField
        FlMatchingField
	flMatchingField
	isToricDegeneration
	plueckerIdeal
	matchingFieldIdeal
	isCoherent
	getWeightMatrix
      Subnodes
      
///

doc ///
      Key
        (symbol ==, FlMatchingField, FlMatchingField)
      Headline
        equality of flag matching fields
      Usage
        result = L1 == L2
      Inputs
        L1: FlMatchingField
	L2: FlMatchingField
      Outputs
        result: Boolean
	  are the flag matching fields equal
      Description
        Text
	  Two matching fields are said to be equal if their tuples are equal.
	  In the case of flag matching fields, the $kList$s must be equal.
	Example
	  L1 = diagonalMatchingField({1,2}, 4)
	  getWeightMatrix L1
	  getTuples L1
	  L2 = flMatchingField({1,2}, matrix {{0,0,0,0}, {8,4,2,1}})
	  getWeightMatrix L2
	  getTuples L2
	  L1 == L2
	  L3 = flMatchingField({1,2}, 4, {{{1}, {4}, {3}, {2}}, {{3,4},{2,4},{1,4},{2,3},{1,3},{1,2}}})
	  L3 == L1
      SeeAlso
        GrMatchingField
        FlMatchingField
	getTuples
	getWeightMatrix
      Subnodes
      
///

doc ///
      Key
        (symbol ==, GrMatchingField, GrMatchingField)
      Headline
        equality of Grassmannian matching fields
      Usage
        result = L1 == L2
      Inputs
        L1: GrMatchingField
	L2: GrMatchingField
      Outputs
        result: Boolean
	  are the matching fields equal
      Description
        Text
	  Two matching fields are said to be equal if their tuples are equal.
	Example
	  L1 = diagonalMatchingField(2, 4)
	  getWeightMatrix L1
	  getTuples L1
	  L2 = grMatchingField matrix {{0,0,0,0}, {8,4,2,1}}
	  getWeightMatrix L2
	  getTuples L2
	  L1 == L2
	  L3 = grMatchingField(2, 4, {{3,4},{2,4},{1,4},{2,3},{1,3},{1,2}})
	  L3 == L1
      SeeAlso
        GrMatchingField
        FlMatchingField
	getTuples
	getWeightMatrix
      Subnodes      
///

doc ///
      Key
        (net, GrMatchingField)
	(net, FlMatchingField)
      Headline
        display a matching field
      Usage
        net L
      Inputs
        L: {GrMatchingField, FlMatchingField}
      Description
        Text
	  The @TO "net"@ of a matching field displays $k$ or $kList$ and $n$ for that matching field.
	  See @TO "GrMatchingField"@ and @TO "FlMatchingField"@.
      SeeAlso
        GrMatchingField
        FlMatchingField
      Subnodes      
///

doc ///
      Key
        plueckerAlgebra
        (plueckerAlgebra, GrMatchingField)
	(plueckerAlgebra, FlMatchingField)
	[plueckerAlgebra, MonomialOrder]
      Headline
        Pluecker algebra of a (partial) flag variety
      Usage
        S = plueckerAlgebra L
      Inputs
        L: {GrMatchingField, FlMatchingField}
	MonomialOrder => String 
	  either "default" or "none" (supply "none" only if $L$ is not coherent)
      Outputs
        S: Subring
	  generated by minors of a generic matrix of variables
      Description
        Text
	  The Pluecker algebra for the Grassmannian is generated by the maximal minors of a
	  generic $k \times n$ matrix of variables. Similarly, for a partial flag variety, the
	  Pluecker algebra is generated by a collection of top-justified minors of a generic matrix
	  of variables.
	  
	  A matching field specifies a weight order on the ambient ring containing the Pluecker algebra.
	Example
	  L = diagonalMatchingField(2, 4)
	  S = plueckerAlgebra L
	  transpose gens S
      SeeAlso
        GrMatchingField
        FlMatchingField
	plueckerMap
	Subring
      Subnodes      
///

doc ///
      Key
        TopeField
      Headline
        A tope field structure on a matching field
      Description
        Text
	  Tope fields were introduced in the study of tropical oriented matroids 
	  and have been used generalise and study matching fields. In this package we follow the conventions
	  of tope fields given by Smith and Loho, i.e., the type of a tope field contains positive entries. 
	  
	  The combinatorial data of a tope field is given by a matching field for $Gr(k,n)$ together with a type: $(t_1, \dots, t_s)$
	  where $t_1 + \dots + t_s = k$ and each $t_i$ is a positive integer. The bipartite graphs of the tope field are encoded in the
	  tuples of the matching field as follows. Let $(i_{1,1}, i_{1,2}, \dots i_{1,t_1}, i_{2,1}, \dots, i_{s, t_s})$ be a tuple of the 
	  matching field, the bipartite graph on vertices $L := [n]$ and $R := [s]$ has edges $\{i_{j, t}, j\}$ where $j \in [s]$ and $t \in [t_j]$.
	  
	  For example, if $(1,3,2)$ is a tuple of a matching field for $Gr(3,4)$ of a tope field of type $(2,1)$, then corresponding bipartite graph on
	  vertices $L = [4]$ and $R = [2]$ has edges: $E = \{11, 31, 22 \}$.
	  
	  The TopeField type in this package is a HashTable that stores the matching field and type. A tope field can be defined from a matching field
	  using the constructor @TO "topeField"@. New tope fields can be constructed from old using the function @TO "amalgamation"@. Note that
	  amalgamation is only defined for linkage tope field, see @TO "isLinkage"@.
      SeeAlso
	GrMatchingField
	grMatchingField
	topeField
	amalgamation
	isLinkage
      Subnodes
///


doc ///
      Key
         topeField
	(topeField, GrMatchingField)
	(topeField, GrMatchingField, List)
      Headline
        Constructor of a tope field
      Usage
        TF = topeField MF
	TF = topeField(MF, T)
      Inputs
        MF: GrMatchingField
	  matching field containing the tuples of the tope field
	T: List
	  the type of the tope field
      Outputs
        TF: TopeField
      Description
        Text
	  The standard constructor of a tope field. If the constructor is supplied with a matching field and no type, then 
	  the type is automatically set to $1,1, \dots, 1$.
	Example
	  MF = diagonalMatchingField(3,6);
	  TF = topeField MF
	  TF' = topeField(MF, {2,1})  
      SeeAlso
	GrMatchingField
	grMatchingField
	TopeField
	amalgamation
      Subnodes
///



doc ///
      Key
         isLinkage
	(isLinkage, GrMatchingField)
	(isLinkage, TopeField)
      Headline
        Test if a tope field is linkage
      Usage
        result = isLinkage MF
	result = isLinkage TF
      Inputs
        MF: GrMatchingField
	TF: TopeField
      Outputs
        result: Boolean
      Description
        Text
	  Consider a tope field given by a collection of bipartite graphs.
	  The tope field is said to be linkage if for each $k+1$-subset $S$ of $[n]$, the union of the edges of the bipartite graphs
	  $G$ where the non-isolated left-vertices of $G$ are contained in $S$, is a forest.
	  
	  Note that all coherent matching fields are linkage.
	Example
	  L = diagonalMatchingField(2,4);
	  isLinkage L    
      SeeAlso
	GrMatchingField
	grMatchingField
	TopeField
	amalgamation
      Subnodes
///



doc ///
      Key
         amalgamation
	(amalgamation, ZZ, GrMatchingField)
	(amalgamation, ZZ, TopeField)
      Headline
        The $i$th amalgamation of a tope field
      Usage
        result = amalgamation(i, MF)
	result = amalgamation(i, TF)
      Inputs
        i: ZZ
	  the $i$th amalgamation
        MF: GrMatchingField
	TF: TopeField
      Outputs
        result: TopeField
      Description
        Text
	  Computes the $i$th amalgamation of a tope field. Note that the tope field must be linkage for amalgamation to be
	  well-defined.
	Example
	  L = matchingFieldFromPermutation(3,6,{4,5,6,1,2,3});
	  getTuples L
	  T = topeField L
	  T1 = amalgamation(1, T)
	  getTuples T1
      SeeAlso
	GrMatchingField
	grMatchingField
	TopeField
	topeField
	isLinkage
      Subnodes
///


doc ///
      Key
        (net, TopeField)
      Headline
        display a tope field
      Usage
        net TF
      Inputs
        TF: TopeField
      Description
        Text
	  The @TO "net"@ of a tope field displays $n$ and the type of the tope field.
	  See @TO "TopeField"@.
      SeeAlso
        TopeField
      Subnodes      
///

doc ///
      Key
        (getTuples, TopeField)
      Headline
        tuples of a tope field
      Usage
        tuples = getTuples TF
      Inputs
        TF: TopeField
      Outputs
        tuples: List
	  list of tuples of the matching field of the tope field
      Description
        Text
	  Lists the tuples of the matching field of the tope field.
	Example
	  L = diagonalMatchingField(3, 6);
	  T = topeField L
	  getTuples T
      SeeAlso
        TopeField
      Subnodes      
///

-- #########
-- # Tests #
-- #########

-- MF from weight matrix: tuples, pluecker weight, toric degen
TEST ///
L = grMatchingField matrix {
    {0,0,0,0}, 
    {1,3,2,4}};
tupleList = {{2,1},{3,1},{2,3},{4,1},{4,2},{4,3}};
assert(getTuples L == tupleList);
assert(getWeightPluecker L == {1, 1, 2, 1, 3, 2});
assert(isToricDegeneration L);
///

-- non coherent from tuples
TEST ///
L = grMatchingField(2, 3, {{1,2}, {2,3}, {3,1}});
assert(isCoherent L == false);
///

-- diagonal MF: weight matrix cone
TEST ///
L = diagonalMatchingField(2, 6);
assert(dim weightMatrixCone L == 12);
assert(numColumns rays weightMatrixCone L == 5);
assert(numColumns linealitySpace weightMatrixCone L == 7);
///

-- MF from permutations: equality
TEST ///
L = matchingFieldFromPermutation(2, 4, {2,1,4,3});
L' = matchingFieldFromPermutation(2, 4, {3,2,10,5});
assert(getTuples L == getTuples L');
assert(L == L')
///

-- pluecker algebra: isSAGBI
TEST ///
L = diagonalMatchingField(2, 4);
S = plueckerAlgebra L;
assert(isSAGBI S);
///

-- non diag pluekcer algebra 
TEST ///
L = matchingFieldFromPermutation(2, 4, {2,3,1,4});
S = plueckerAlgebra L;
assert(isSAGBI S);
///

-- algebraic matroid
TEST ///
L = diagonalMatchingField(2, 4);
S = set {{1, 3}, {1, 4}, {2, 3}, {2, 4}};
assert(isSubset((algebraicMatroidCircuits L)_0, S))
assert(isSubset(S, (algebraicMatroidCircuits L)_0))
///

-- tope field amalgamations
TEST ///
L = grMatchingField(3, 5, {{1,3,2}, {1,4,2}, {1,5,2}, {3,4,1}, {1,3,5}, {1,4,5}, {3,4,2}, {2,3,5}, {2,4,5}, {3,4,5}});
T = topeField L;
assert(isLinkage T);
T2 = amalgamation(2, T);
assert(T2#"type" == {1,2,1});
assert(getTuples T2 == {{1, 3, 4, 2}, {1, 3, 5, 2}, {1, 4, 5, 2}, {1, 3, 4, 5}, {2, 3, 4, 5}});
T23 = amalgamation(3, T2);
assert(T23#"type" == {1,2,2});
assert(getTuples T23 == {{1,3,4,2,5}});
///

-- tope field linkage
TEST ///
L = grMatchingField(2, 3, {{1,2}, {3,1}, {2,3}});
assert(not isCoherent L);
assert(not isLinkage L);
///

-- matching field ideal for non-coherent
TEST ///
L = grMatchingField(2, 4, {{1,2}, {3,1}, {2,3}, {1,4}, {2,4}, {4,3}});
assert(not isCoherent L);
I = matchingFieldIdeal(L, MonomialOrder => "none");
assert(zero I);
///

-- non-coherent matching field ideal equals diagonal matching field ideal
TEST ///
S = subsets(1 .. 5, 3);
S = {{2,1,3}} | delete({1,2,3}, S);
L = grMatchingField(3, 5, S);
assert(not isCoherent L);
I = matchingFieldIdeal(L, MonomialOrder => "none");
D = diagonalMatchingField(3, 5);
I' = matchingFieldIdeal D;
m = map(ring I, ring I', vars ring I);
assert(m I' == I);
///

end --
