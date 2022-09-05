-- initializeCompTable: produces a compTable - a mutable version of a SAGBIBasis object, to be used for sagbi computation 
-- expect this function to be called at the beginning of sagbi computation
-- the options for the compTable are taken from SAGBIBasis except 
--   if RenewOptions is true then only the specified options are used
initializeCompTable = method();
initializeCompTable (SAGBIBasis, HashTable):= (S,opts) -> (
    local options;
    
    rings := new MutableHashTable from S#"rings";
    maps := new MutableHashTable from S#"maps";
    ideals := new MutableHashTable from S#"ideals";
    data := new MutableHashTable from S#"data";
    pending := new MutableHashTable from S#"pending";
    
    if opts.RenewOptions then (
	options = new MutableHashTable from opts;
	) else (
	options = new MutableHashTable from S#"options";
	);
    
    data#"limit" = opts.Limit;
    options#PrintLevel = opts.PrintLevel; -- allow the user to specify the print level
    options#RenewOptions = false; -- reset RenewOptions since they are now set correctly
    options#Recompute = false; -- at this point the correct SAGBIBasis must have been supplied so the option is reset
    options#"variableName" = "p"; -- S#"options"#"variableName";
    apply(keys pending, i -> pending#i = new MutableList from pending#i);

    new HashTable from {
        "rings" => rings,
        "maps" => maps,
        "ideals" => ideals,
        "data" => data,
        "pending" => pending,
        "options" => options
    }
)

-- limitedCompTable: construct a small compTable with enough data for use in autoSubduction
limitedCompTable = method();
limitedCompTable (HashTable, Matrix) := (H,M) -> (
    -- Construct the monoid of a ring with variables corresponding to generators of the ambient ring and the subalgebra.
    -- Has an elimination order that eliminates the generators of the ambient ring.
    -- The degrees of generators are set so that the SyzygyIdeal is homogeneous.
    numberVariables := numgens H#"rings"#"liftedRing";
    numberGenerators := numColumns M;
    newMonomialOrder := append((monoid H#"rings"#"liftedRing").Options.MonomialOrder, Eliminate numberVariables);
    tensorVariables := monoid[
        VariableBaseName => H#"options"#"variableName",
        Variables => numberVariables + numberGenerators,
        Degrees => first entries (matrix{flatten degrees source vars H#"rings"#"liftedRing"}|matrix{flatten degrees source M}),
        MonomialOrder => newMonomialOrder];
    tensorRing := (coefficientRing H#"rings"#"liftedRing") tensorVariables;
    rings := new MutableHashTable from {
        "quotientRing" => H#"rings"#"quotientRing",
        "liftedRing" => H#"rings"#"liftedRing",
        "tensorRing" => tensorRing
    };

    -- Maps:
    inclusionLifted := map(rings#"tensorRing",rings#"liftedRing",(vars rings#"tensorRing")_{0..numberVariables-1});
    substitution := map(rings#"tensorRing",rings#"tensorRing",(vars rings#"tensorRing")_{0..numberVariables-1} | inclusionLifted(M));
    projectionLifted := map(rings#"liftedRing",rings#"tensorRing",vars rings#"liftedRing" | matrix {toList(numberGenerators:0_(rings#"liftedRing"))});
    sagbiInclusion := map(rings#"tensorRing",rings#"tensorRing",matrix {toList (numberVariables:0_(rings#"tensorRing"))} | (vars rings#"tensorRing")_{numberVariables .. numberVariables + numberGenerators - 1});

    maps := new MutableHashTable from {
        "inclusionLifted" => inclusionLifted,
        "projectionLifted" => projectionLifted,
        "sagbiInclusion" => sagbiInclusion,
        "substitution" => substitution,
        "fullSubstitution" => projectionLifted * substitution,
        "quotient" => H#"maps"#"quotient"
    };
    
    -- Ideals:
    generatingVariables := (vars rings#"tensorRing")_{numberVariables..numberVariables + numberGenerators - 1};
    SIdeal := ideal(generatingVariables - maps#"inclusionLifted"(leadTerm M));
    ideals := new MutableHashTable from {
        "I" => H#"ideals"#"I",
        "SIdeal" => SIdeal,
        "leadTermsI" => H#"ideals"#"leadTermsI",
        "reductionIdeal" => maps#"inclusionLifted" H#"ideals"#"leadTermsI" + SIdeal
    };
    options := new MutableHashTable from H#"options";
    data := null;
    pending := null;
    new HashTable from {
        "rings" => rings,
        "maps" => maps,
        "ideals" => ideals,
        "data" => data,
        "pending" => pending,
        "options" => options
    }
)


-- compSubduction is an internal (unexported) version of subduction for use in sagbi
-- the user-friendly version is subduction, defined below, which is exported
-- compSubduction takes a compTable and 1-row matrix M with entries in the quotientRing and subducts the elements of M against compTable

compSubduction = method( 
    TypicalValue => HashTable,
    Options => {
	AutoSubduce => true,
        ReduceNewGenerators => true, -- applys gaussian elimination to sagbiGens before adding them
	StorePending => true,
        Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Top", -- top or engine
    	Limit => 100,
	AutoSubduceOnPartialCompletion => false, -- applies autosubduction to the sagbiGens the first time no new terms are added
    	PrintLevel => 0,
	Recompute => false,
	RenewOptions => false
    	}
);

subduction = method( 
    TypicalValue => Matrix,
    Options => { -- These options are only used when the user wants to use subduction for their own purposes
	AutoSubduce => true,
        ReduceNewGenerators => true, -- applys gaussian elimination to sagbiGens before adding them
	StorePending => true,
        Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Top", -- top or engine
    	Limit => 100,
	AutoSubduceOnPartialCompletion => false, -- applies autosubduction to the sagbiGens the first time no new terms are added
    	PrintLevel => 0,
	Recompute => false,
	RenewOptions => false
    	}
);


compSubduction(HashTable, MutableMatrix) := opts -> (compTable, M) -> (
    new MutableMatrix from compSubduction(compTable, matrix M)
    )

compSubduction(HashTable, Matrix) := opts -> (compTable, M) -> (
    local result;
    local g;
    if compTable#"options"#PrintLevel > 3 then (    
	print("-- subduction input:");
	print(M);
	);
    
    if zero(M) then return M;
    result = matrix {toList(numcols(M):0_(compTable#"rings"#"liftedRing"))};
    g = matrix {
	for m in first entries M list sub(m, compTable#"rings"#"liftedRing")
	};
    local subductedPart;
    local leadTermSubductedPart;    
    if compTable#"options"#PrintLevel > 5 then(
	print("-- [compSubduction] polys to subduct:");
	print(transpose g);
	);
    
    while not (zero(g)) do (
	if compTable#"options"#SubductionMethod == "Top" then (
            subductedPart = subductionTopLevelLeadTerm(compTable, g);
	    ) else if compTable#"options"#SubductionMethod == "Engine" then (
    	    subductedPart = subductionEngineLevelLeadTerm(compTable, g);
	    ) else (
	    error ("Unknown subduction type " | toString compTable#"options"#SubductionMethod); 
	    );
	leadTermSubductedPart = leadTerm subductedPart;
	result = result + leadTermSubductedPart;
	g = (subductedPart - leadTermSubductedPart) % compTable#"ideals"#"I";
	
	if compTable#"options"#PrintLevel > 5 then(
	    print("-- [compSubduction] result so far:");
	    print(transpose result);
	    print("--[compSubduction] remaining to subduct:");
	    print(transpose g);
	    );
	);
    result = result % compTable#"ideals"#"I";
    
    if compTable#"options"#PrintLevel > 3 then (    
	print("-- subduction result using "| compTable#"options"#SubductionMethod |" strategy:");
	print(result);
	);
    result
    )

-- the user-friendly subduction methods:
subduction(SAGBIBasis, Matrix) := opts -> (SB, M) -> (
    compTable := initializeCompTable(SB, opts);
    compSubduction(opts, compTable, M)
    )

subduction(SAGBIBasis, RingElement) := opts -> (SB, m) -> (
    compTable := initializeCompTable(SB, opts);
    first first entries compSubduction(opts, compTable, matrix {{m}})
    )

subduction(Matrix, Matrix) := opts -> (F, M) -> (
    SB := initializeCompTable(sagbiBasis subring F, opts);
    SB#"data"#"sagbiGenerators" = F;
    updateComputation(SB);
    compSubduction(opts, SB, M)
    )

subduction(Matrix, RingElement) := opts -> (F, m) -> (
    first first entries subduction(opts, F, matrix {{m}})
    )

subduction(Subring, Matrix) := opts -> (S, M) -> (
    F := gens S;
    subduction(opts, F, M)
    )

subduction(Subring, RingElement) := opts -> (S, m) -> (
    F := gens S;
    first first entries subduction(opts, F, matrix {{m}})
    )

subduction(List, List) := opts -> (FList, MList) -> (
    first entries subduction(opts, matrix {FList}, matrix {MList})
    )

subduction(List, RingElement) := opts -> (FList, m) -> (
    first first entries subduction(opts, matrix {FList}, matrix {{m}})
    )

-- subductionTopLevelLeadTerm: expects the matrix M to have entries in liftedRing
-- performs subduction only until the lead term cannot be subducted further
-- NB this method assumes M is reduced mod I
subductionTopLevelLeadTerm = method();
subductionTopLevelLeadTerm (HashTable, Matrix) := (compTable, M) -> (
    liftg := M;
    while not (zero liftg) do ( 
        tensorRingLiftg := compTable#"maps"#"inclusionLifted" liftg;
	tensorRingLeadTermg := leadTerm tensorRingLiftg;
	h := tensorRingLeadTermg % (compTable#"ideals"#"reductionIdeal"); -- do partial % based on compTable option
	projectionh := compTable#"maps"#"fullSubstitution" compTable#"maps"#"sagbiInclusion" h;

    	if compTable#"options"#PrintLevel > 6 then (
	    print("-- [subductionTopLevelLeadTerm] --");
	    print("-- lift g:");
	    print(transpose liftg);
	    print("-- tensorRingLiftg:");
	    print(transpose tensorRingLiftg);
	    print("-- tensorRingLeadTermg:");
	    print(transpose tensorRingLeadTermg);
	    print("-- h:");
	    print(transpose h);
	    print("-- projectionh:");
	    print(transpose projectionh);
	    );
		
	-- exit the loop if h does not lie in K[p_1 .. p_r] <- the variables tagging the generators of S
	-- note that h is a monomial, if it is divisible by some x_i (from the lifted ring) then projectionh is zero
	--
	if zero(projectionh) then break;

	-- if I is nonzero then reduce projectionh modulo I:
	if not zero compTable#"ideals"#"I" then (projectionh = projectionh % compTable#"ideals"#"I"); 
 	
	--update g
	liftg = liftg - projectionh;
	);
    
    -- check if g is zero or has degree zero 
    matrix {apply(
	    first entries liftg,
	    i -> 
	    if (not (i == 0_(compTable#"rings"#"liftedRing"))) and (degree(i))_0 == 0 then (
		0_(compTable#"rings"#"liftedRing")
		) else (
		i
		)
	    )}
);


-- Engine Subduction of the lead term
-- perform the same function as subductionTopLevelLeadTerm except
-- with engine-level code.
subductionEngineLevelLeadTerm = method();
subductionEngineLevelLeadTerm(HashTable, RingElement) := (compTable, g) -> (
    (subductionEngineLevelLeadTerm(compTable, matrix {{g}}))_(0,0) 
    );

subductionEngineLevelLeadTerm(HashTable,Matrix) := (compTable, M) -> (
    local result;
    tense := compTable#"rings"#"tensorRing";
    	    
    ambR := source compTable#"maps"#"inclusionLifted";
    if ring M === tense then (
	M = (compTable#"maps"#"fullSubstitution")(M);
	)else if ring M =!= ambR then (
	error "M must be from ambR or tensorRing.";
	);
    -- It is possible for ring f === ambient to be true but f is still from a different ring
    -- than pres#"tensorRing". In this case, it shouldn't try to prevent an error by using "sub"
    -- or something. Instead, the following line will deliberately throw an error:
    -- (This is done because otherwise there is potential for a segfault.)
    throwError := M_(0,0) - 1_(ambR);
    -- Use the same pres ring as much as possible.
    -- M2 will automatically cache the gb calculation
    -- as long as the pres ring is not reconstructed.
    gbI := gb compTable#"ideals"#"I";
    gbReductionIdeal := gb compTable#"ideals"#"reductionIdeal";
    F := compTable#"maps"#"substitution";
    N := monoid ambR;
    numblocks := rawMonoidNumberOfBlocks raw N;
    result = rawSubduction1(numblocks, raw tense, raw ambR, raw M, raw compTable#"maps"#"inclusionLifted", raw compTable#"maps"#"fullSubstitution", raw (compTable#"maps"#"substitution" * compTable#"maps"#"sagbiInclusion"), raw gbI, raw gbReductionIdeal);
    result = matrix{apply(first entries result,i->promote(i,ambR))}
    );

---------------
-- AutoSubduce: (compTable)
---------------
-- for each subalgebraGenerator subduct it against the rest of the subalgebraGenerators
-- for each such generator we produce a temporary compTable object that contains
-- enough data to perform subduction with
--

autosubduce = method();
autosubduce (HashTable) := (compTable) -> (
    
    local tempCompTable;
    local M;
    generatorMatrix := new MutableMatrix from compTable#"data"#"subalgebraGenerators";
    for i from 0 to (numColumns generatorMatrix) - 1 do (
            M = new Matrix from generatorMatrix_(toList join(0..(i-1),(i+1)..((numColumns generatorMatrix)-1)));
            tempCompTable = limitedCompTable(compTable,lift(M,compTable#"rings"#"liftedRing"));
	    generatorMatrix_(0,i) = (compSubduction(tempCompTable,generatorMatrix_{i}))_(0,0);
            if not generatorMatrix_(0,i) == 0 then
                generatorMatrix_(0,i) = generatorMatrix_(0,i)*(1/leadCoefficient(generatorMatrix_(0,i)));
    );
    compress new Matrix from generatorMatrix
)

---------------------------------
-- AutoSubduceSagbi: (compTable)
---------------------------------
-- for each sagbiGenerator subduct it against the rest of the sagbiGenerators
-- for each such generator we produce a temporary compTable object that contains
-- enough data to perform subduction with

autosubduceSagbi = method();
autosubduceSagbi (HashTable) := (compTable) -> (
    
    if compTable#"options"#PrintLevel > 0 then (
	print("-- AutoSubducting Sagbi Generators ...");
	);	
    
    local tempCompTable;
    local M;
    generatorMatrix := new MutableMatrix from compTable#"data"#"sagbiGenerators";
    for i from 0 to (numColumns generatorMatrix) - 1 do (
            M = new Matrix from generatorMatrix_(toList join(0..(i-1),(i+1)..((numColumns generatorMatrix)-1)));
            tempCompTable = limitedCompTable(compTable,lift(M,compTable#"rings"#"liftedRing"));
	    generatorMatrix_(0,i) = (compSubduction(tempCompTable,generatorMatrix_{i}))_(0,0);
            if not generatorMatrix_(0,i) == 0 then
                generatorMatrix_(0,i) = generatorMatrix_(0,i)*(1/leadCoefficient(generatorMatrix_(0,i)));
    );
    compress new Matrix from generatorMatrix
)


-- updateComputation: checks the Strategy: DegreeByDegree or Incremental.
--   passes to a function that recomputed the rings, maps and ideals wrt new sagbiGenerators
-- NB if there are no new sagbiGenerators, then this function is not called

updateComputation = method();
updateComputation(HashTable) := (compTable) -> (
    
    if compTable#"options"#Strategy == "Master" then (
	updateComputationMaster(compTable);
	);
    
    if compTable#"options"#Strategy == "DegreeByDegree" then (
	updateComputationDegreeByDegree(compTable);
	);
    
    if compTable#"options"#Strategy == "Incremental" then (
	updateComputationIncremental(compTable);
	);
    )


-- updateComputationMaster: Master Strategy for computation updates
-- We use the heuristic that sagbiGenerators are generally clumped together
-- at low degree and become sparser at higher degrees
--
-- So, we check the number of sagbiGenerators that were added last time
-- during the last loop of sagbi algorithm
-- if 0 or 1 sagbiGenerators were added and the degree of the last added generator > 5, then use Incremental
-- otherwise use DegreeByDegree

updateComputationMaster = method();
updateComputationMaster(HashTable) := (compTable) -> (
    
    if (compTable#"data"#?"numberOfNewGenerators") and (numColumns compTable#"data"#"sagbiGenerators" > 0) then (
	numNewGens := compTable#"data"#"numberOfNewGenerators";
	lastSagbiGenDegree := last first entries compTable#"data"#"sagbiDegrees";
	-- compare the number of new generators with the total number of generators
	-- e.g. if you're adding less than 2-3% of the total number of generators
	if (numNewGens == 0 or numNewGens == 1) and (lastSagbiGenDegree > 8) then (
	    if compTable#"options"#PrintLevel > 4 then (
		print("-- [updateComputationMaster] Detected few new generators; using Incremental Strategy");
		);
	    updateComputationIncremental(compTable);
	    ) else (
	    if compTable#"options"#PrintLevel > 4 then (
		print("-- [updateComputationMaster] Detected many or low-degree new generators; using DegreeByDegree Strategy");
		);
	    updateComputationDegreeByDegree(compTable);
	    );
	 
	) else (
	if compTable#"options"#PrintLevel > 4 then (
	    print("-- [updateComputationMaster] Defaulting to DegreeByDegree Strategy");
	    );
	updateComputationDegreeByDegree(compTable);
	);
    );


-- updateComputation using DegreeByDegree strategy 
-- NB this does not compute a gb for the reductionIdeal 
-- The gb computation next occurs in collectSPairs or during subduction
-- The gb computation is only necessary up to a certain degree

updateComputationDegreeByDegree = method();
updateComputationDegreeByDegree(HashTable) := (compTable) -> (
        
    sagbiGens := compTable#"data"#"sagbiGenerators";
    
    -- Changes to the rings:
    -- quotientRing (unchanged)
    -- liftedRing   (unchanged)
    -- tensorRing   (modified)
    liftedRing := compTable#"rings"#"liftedRing";
    
    numberVariables := numgens compTable#"rings"#"liftedRing";
    numberGenerators := numColumns sagbiGens;
    newMonomialOrder := append((monoid liftedRing).Options.MonomialOrder, Eliminate numberVariables);    
    tensorVariables := monoid[
        VariableBaseName => compTable#"options"#"variableName",
        Variables => numberVariables + numberGenerators,
        Degrees => degrees source vars liftedRing | degrees source sagbiGens,
        MonomialOrder => newMonomialOrder];
    tensorRing := (coefficientRing liftedRing) tensorVariables;
    compTable#"rings"#"tensorRing" = tensorRing;
    
    -- Changes to the maps:
    -- inclusionLifted  (modified)
    -- substitution     (modified)
    -- projectionLifted (modified)
    -- sagbiInclusion   (modified)
    -- fullSubstitution (modified)
    -- quotient         (unchanged)
    
    inclusionLifted := map(tensorRing, liftedRing, (vars tensorRing)_{0..numberVariables-1});
    substitution := map(tensorRing, tensorRing , (vars tensorRing)_{0..numberVariables-1} | inclusionLifted(sub(sagbiGens, liftedRing)));
    projectionLifted := map(liftedRing, tensorRing, vars liftedRing | matrix {toList(numberGenerators:0_(liftedRing))});
    sagbiInclusion := map(tensorRing, tensorRing, matrix {toList (numberVariables:0_(tensorRing))} | (vars tensorRing)_{numberVariables .. numberVariables + numberGenerators - 1});
    
    compTable#"maps"#"inclusionLifted"  = inclusionLifted;
    compTable#"maps"#"substitution"     = substitution;
    compTable#"maps"#"projectionLifted" = projectionLifted;
    compTable#"maps"#"sagbiInclusion"   = sagbiInclusion;
    compTable#"maps"#"fullSubstitution" = projectionLifted * substitution,
    
    -- Changes to the ideals:
    -- I              (unchanged)
    -- SIdeal         (modified)
    -- leadTermsI     (unchanged)
    -- reductionIdeal (modified)
    
    generatingVariables := (vars tensorRing)_{numberVariables..numberVariables + numberGenerators - 1};
    SIdeal := ideal(generatingVariables - inclusionLifted(sub(leadTerm sagbiGens, liftedRing)));
    compTable#"ideals"#"SIdeal" = SIdeal;
    compTable#"ideals"#"reductionIdeal" = inclusionLifted compTable#"ideals"#"leadTermsI" + SIdeal;    
    );

----------------------------------------------------------
-- updateComputation by using the incremental strategy
--
-- This method computes a full GB for the reductionIdeal by
-- using a (potentially previously computed) GB. This is done
-- by computing a GB in for the following ideal:
--   J = ideal (newTagVariables - newSagbiGenerators) 
-- which lives in the quotient ring:
--   oldTensorRing / oldReductionIdeal
-- Note this GB computation is faster than computing a GB for the new reductionIdeal
-- because we already have a GB for the oldReductionIdeal. 

updateComputationIncremental = method();
updateComputationIncremental(HashTable) := (compTable) -> (
    sagbiGens := compTable#"data"#"sagbiGenerators";
    
    -- Changes to the rings:
    -- quotientRing (unchanged)
    -- liftedRing   (unchanged)
    -- tensorRing   (modified)
    liftedRing := compTable#"rings"#"liftedRing";
    
    numberVariables := numgens compTable#"rings"#"liftedRing";
    numberGenerators := numColumns sagbiGens;
    newMonomialOrder := append((monoid liftedRing).Options.MonomialOrder, Eliminate numberVariables);    
    tensorVariables := monoid[
        VariableBaseName => compTable#"options"#"variableName",
        Variables => numberVariables + numberGenerators,
        Degrees => degrees source vars liftedRing | degrees source sagbiGens,
        MonomialOrder => newMonomialOrder];
    tensorRing := (coefficientRing liftedRing) tensorVariables;
    oldTensorRing := compTable#"rings"#"tensorRing";
    
    -- Changes to the maps:
    -- inclusionLifted  (modified)
    -- substitution     (modified)
    -- projectionLifted (modified)
    -- sagbiInclusion   (modified)
    -- fullSubstitution (modified)
    -- quotient         (unchanged)
    
    inclusionLifted := map(tensorRing, liftedRing, (vars tensorRing)_{0..numberVariables-1});
    substitution := map(tensorRing, tensorRing , (vars tensorRing)_{0..numberVariables-1} | inclusionLifted(sub(sagbiGens, liftedRing)));
    projectionLifted := map(liftedRing, tensorRing, vars liftedRing | matrix {toList(numberGenerators:0_(liftedRing))});
    sagbiInclusion := map(tensorRing, tensorRing, matrix {toList (numberVariables:0_(tensorRing))} | (vars tensorRing)_{numberVariables .. numberVariables + numberGenerators - 1});
       
    -- Changes to the ideals:
    -- I              (unchanged)
    -- SIdeal         (modified)
    -- leadTermsI     (unchanged)
    -- reductionIdeal (modified)
    
    -- Incremental steps to form reductionIdeal and compute a GB:
    -- 1) Write down new sagbi gens, construct new tensor ring [done above] and inclusion map.
    -- 2) Map the old gb into new tensor ring and promise it’s a gb (forceGB)
    -- 3) Construct the ideal, quotient ring, and the quotient map
    -- 4) Apply the quotient map to the new generators, compute a gb, and lift
    -- 5) Concatenate the lifted elements with the old gb generators and promise that’s a gb (forceGB)
    
    -- (1)    
    numberOfNewGenerators := (numgens tensorRing) - (numgens oldTensorRing);
    generatingVariables := (vars tensorRing)_{numberVariables + numberGenerators - numberOfNewGenerators ..numberVariables + numberGenerators - 1};
    newSagbiGens := sagbiGens_{numberGenerators - numberOfNewGenerators .. numberGenerators - 1};
    newSIdealGens := generatingVariables - inclusionLifted(sub(leadTerm newSagbiGens, liftedRing));    
    tensorRingInclusion := map(tensorRing, oldTensorRing, (vars tensorRing)_{0 .. numberVariables + numberGenerators - numberOfNewGenerators - 1});
    
    -- (2)
    oldReductionIdealGB := tensorRingInclusion gens gb compTable#"ideals"#"reductionIdeal";
    forceGB oldReductionIdealGB;
    
    -- (3) 
    tensorRingQuotientOldReduction := tensorRing / (ideal oldReductionIdealGB);
    quotientMapOldReduction := map(tensorRingQuotientOldReduction, tensorRing, vars tensorRingQuotientOldReduction);
    
    -- (4)
    quotientReductionGB := lift(gens gb quotientMapOldReduction newSIdealGens, tensorRing);
    
    -- (5)
    newReductionGB := oldReductionIdealGB | quotientReductionGB;    
    forceGB newReductionGB;
    
    
    -- Update the compTable:
    compTable#"rings"#"tensorRing" = tensorRing;            
    compTable#"maps"#"inclusionLifted"  = inclusionLifted;
    compTable#"maps"#"substitution"     = substitution;
    compTable#"maps"#"projectionLifted" = projectionLifted;
    compTable#"maps"#"sagbiInclusion"   = sagbiInclusion;
    compTable#"maps"#"fullSubstitution" = projectionLifted * substitution,
    compTable#"ideals"#"SIdeal" = ideal newSIdealGens;
    compTable#"ideals"#"reductionIdeal" = ideal newReductionGB;    
    );


-- Finds the lowest list in Pending
lowestDegree = method()
lowestDegree HashTable := compTable -> (
    min keys compTable#"pending"
)

-- Adds newGens to compTable#"data"#"sagbiGenerators" 
--   newGens is a 1-row matrix of generators to be added
-- updates the sagbiDegrees with the degrees of the generators added
-- saves the number of sagbiGenerators that were just added
--
appendToBasis = method()
appendToBasis (HashTable, Matrix) := (compTable, newGenerators) -> (
    compTable#"data"#"sagbiDegrees" = compTable#"data"#"sagbiDegrees" | matrix{flatten degrees source newGenerators};
    compTable#"data"#"sagbiGenerators" = compTable#"data"#"sagbiGenerators" | newGenerators;
    compTable#"data"#"numberOfNewGenerators" = numColumns newGenerators;
)

--------------------------------------------------
-- Process Pending: (compTable) -> currentDegree
--------------------------------------------------
-- 1) Reduces the elements of lowest degree in the pending list (triangularBasis)
-- 2) Adds these elements to the SAGBI basis (insertPending)
-- 3) Updates all the maps, rings, and ideals (updateComputation)
-- NB Assumes that the pending list has been subducted 
-- 
-- returns the lowest degree of a new sagbiGenerator added
--
          
processPending = method();
processPending (HashTable) := compTable -> (
    local reducedGenerators; 
    currentLowest := lowestDegree(compTable);
    if currentLowest < infinity then (
	if compTable#"options"#PrintLevel > 4 then (
	    print("-- [processPending] generators before reduction:");
	    print(transpose matrix{toList compTable#"pending"#currentLowest});
	    );
	if compTable#"options"#ReduceNewGenerators then ( --perform guassian elimination on the new generators
	    reducedGenerators = triangularBasis matrix{toList compTable#"pending"#currentLowest};
	    ) else (
	    reducedGenerators = matrix{toList compTable#"pending"#currentLowest};
	    );
	
	if compTable#"options"#PrintLevel > 4 then (
	    print("-- [process pending]: reduced generators:");
	    print(transpose reducedGenerators);
	    );
	
	remove(compTable#"pending", currentLowest);
        insertPending(compTable, reducedGenerators);

        currentLowest = lowestDegree(compTable);
        if currentLowest < infinity then (
	    
	    if compTable#"options"#PrintLevel > 4 then (
		print("-- [processPending]: new sagbi generators being added:");
		print(transpose matrix{toList compTable#"pending"#currentLowest});
		);    
	    
            appendToBasis(compTable, matrix{toList compTable#"pending"#currentLowest});
            updateComputation(compTable);
            remove(compTable#"pending", currentLowest);
            );
    	);
    currentLowest
    )


-- Gaussian Elimination:
--
-- Takes a 1xn matrix M
-- Finds all the monomials contained in polynomials of M
-- Substitutes each monomial with a variable
-- Computes a gb for the ideal generated by these linear polynomials
-- Substitutes back and forms a matrix from the gb generators

triangularBasis = method();
triangularBasis(Matrix) := M -> (
    R := ring M;
    mons := monomials M;
    A := last coefficients(M,Monomials=>mons);
    S := (coefficientRing R) (monoid[Variables => (numcols mons)]);
    I := ideal(vars S * sub(A, S));
    reduced := last coefficients(gens gb I);
    compress (mons * sub(reduced,R))
    );


insertPending = method();
insertPending (HashTable,Matrix) := (compTable, candidates) -> (
    for candidate in first entries candidates do(
        -- get the entry of the column and its degree
        level := (degree candidate)_0;
        if compTable#"pending"#?level then(
            compTable#"pending"#level = append(compTable#"pending"#level, candidate)
        ) else (
	        compTable#"pending"#level = new MutableList from {candidate}
	    );
    );
)

processFirstStep = method();
processFirstStep HashTable := (compTable) -> (
    if compTable#"options"#AutoSubduce then (
        if compTable#"options"#PrintLevel > 0 then
            print("-- Performing initial autosubduction...");
	compTable#"data"#"subalgebraGenerators" = autosubduce compTable;
    	compTable#"options"#AutoSubduce = false; -- autosubduction may now be skipped if the computation is resumed
    );
    
    if (numcols compTable#"data"#"sagbiGenerators" == 0) then (
        liftedGenerators := lift(compTable#"data"#"subalgebraGenerators",compTable#"rings"#"liftedRing");
        insertPending(compTable, liftedGenerators);
        -- Remove elements of the underlying field
        remove(compTable#"pending", 0);
        compTable#"data"#"degree" = processPending(compTable) + 1;
    );
)

--Accepts a 1-row matrix inputMatrix and returns a matrix of columns 
-- of inputMatrix where the highest degree entry has total degree equal to selectedDegree
submatrixByDegree = method()
submatrixByDegree (Matrix,ZZ) := (inputMatrix, selectedDegree) -> (
    matrixDegrees := degrees source inputMatrix;
    selectedEntries := positions(0..numcols inputMatrix - 1, i-> (matrixDegrees)_i === {selectedDegree});
    inputMatrix_selectedEntries
)

collectSPairs = method();
collectSPairs (HashTable) := (compTable) -> (
    
    if compTable#"options"#PrintLevel > 0 then (
	    print("---------------------------------------");
	    print("-- Current degree:"|toString(compTable#"data"#"degree"));
	    print("---------------------------------------");
	    print("-- Computing the tete-a-tete's...");
    );
    sagbiGB := gb(compTable#"ideals"#"reductionIdeal", DegreeLimit => compTable#"data"#"degree");
    k := rawMonoidNumberOfBlocks(raw monoid (compTable#"rings"#"tensorRing")) - 2;
    zeroGens := submatrixByDegree(selectInSubring(k, gens sagbiGB), compTable#"data"#"degree");
    SPairs := compTable#"maps"#"fullSubstitution"(zeroGens) % compTable#"ideals"#"I";
    
    if compTable#"pending"#?(compTable#"data"#"degree") then (
        SPairs = SPairs | matrix{toList compTable#"pending"#(compTable#"data"#"degree")};
	remove(compTable#"pending", compTable#"data"#"degree");
    );
    
    if compTable#"options"#PrintLevel > 2 then ( -- extra information
	print("-- GB for reductionIdeal:");
	print(sagbiGB);
	print(gens sagbiGB);
	print("-- zeroGens:");
	print(zeroGens);
	);
    if compTable#"options"#PrintLevel > 0 then (
	print("-- Num. S-polys before subduction: "| toString(numcols SPairs));
	);
    if compTable#"options"#PrintLevel > 1 then (
	print("-- S-polys:");
	print(SPairs);
	);
        
    SPairs
)

-- sagbi -> updatePending -> insertPending/processPending
-- updatePending(compTable, Spairs)
-- Takes the already subducted SPairs and removes any 0's from the matrix
-- Returns true if new generators are added
--
updatePending = method();
updatePending(HashTable, Matrix) := (compTable, SPairs) -> (
    
    local newGens;
    local currentLowestDegree;
    addedGenerators := false;
    
    if numcols SPairs != 0 then (
	newGens = compress (SPairs);
	) else (
	newGens = SPairs;
	);
    
    if compTable#"options"#PrintLevel > 0 then(
	print("-- Num. S-polys after subduction: " | toString(numcols newGens));
	);
    
    if compTable#"options"#PrintLevel > 1 then(
	print("-- New generators:");
	if (numcols newGens == 0) then(
	    -- It has to treat this as a special case because zero matrices are special.
	    print("| 0 |");
	    )else(
	    print(newGens);
	    );
    	);
    
    -- add the newGens to the pending list
    if numcols newGens > 0 then (
	insertPending(compTable, newGens);
	currentLowestDegree = processPending(compTable);
	if not currentLowestDegree == infinity then ( 
	    compTable#"data"#"degree" = currentLowestDegree;
	    );
	addedGenerators = true;
	compTable#"data"#"autoSubductedSagbiGenerators" = false; -- need to autoSubduct new generators (see compTable option: AutoSubductOnPartialCompletion)
	);
    
    addedGenerators
    )


--------------------------------
-- Check Termination
--------------------------------
-- check whether the sagbi computation should terminate
-- criteria for termination:
-- 0) the pending list is empty
--   - if not: an element of the pending list may be a new sagbiGenerator
-- 1) fully computed the GB for the reductionIdeal (always true for the Incremental strategy)
--   - if not: new elements of GB of the reductionIdeal may give new SPairs and new sagbiGenerators
-- 2) the degree of the computation > the maximum degree of sagbiGenerators
--   - if not: there was a drop in computation degree, so the computation degree needs to catch up
-- 3) the degree of the computation > the maximum degree of gb for reductionIdeal
--   - if not: it may be possible to get lower degree generators

-- if the computation should terminate then set compTable#"data"#"sagbiDone" to true
-- if the computation should not terminate then:
--   if AutoSubductOnPartialCompletion and (not autoSubductedSagbiGenerators) then:
--     autosubduceSagbi(compTable)


checkTermination = method();
checkTermination(HashTable) := (compTable) -> (
    
    local terminationCondition0;
    local terminationCondition1;
    local terminationCondition2;
    local terminationCondition3;
    
    -- NB sagbGB's gb should be computed in processPending -> updateComputation (for incremental) or collectSpairs (for DegreeByDegree)
    -- that computation depends on the option: "DegreeByDegree" or "Incremental" 
    sagbiGB := gb(compTable#"ideals"#"reductionIdeal", DegreeLimit => compTable#"data"#"degree");
    terminationCondition0 = #(compTable#"pending") == 0;
    terminationCondition1 = rawStatus1 raw sagbiGB == 6; -- is the GB computation completed?
    
    -- check if there are still generators of higher degree to add to the sagbiGenerators
    terminationCondition2 = compTable#"data"#"degree" > max flatten (degrees compTable#"data"#"sagbiGenerators")_1;
    
    -- check to make sure it is not possible to get lower degree sagbiGenerators
    -- by taking them modulo the reductionIdeal
    terminationCondition3 = compTable#"data"#"degree" > max flatten (degrees gens sagbiGB)_1; 
    if compTable#"options"#PrintLevel > 0 then(
	print("-- Stopping conditions:");
	print("--    No higher degree candidates: "|toString(terminationCondition0));
	print("--    S-poly ideal GB completed:   "|toString(terminationCondition1));
	print("--    Degree lower bound:          "|toString(terminationCondition2));
	print("--    Degree bound by sagbiGB:     "|toString(terminationCondition3));
	);
    
    if terminationCondition0 and terminationCondition1 and terminationCondition2 and terminationCondition3 then (
	compTable#"data"#"sagbiDone" = true;
	if compTable#"options"#PrintLevel > 0 then (
	    print("-- Computation complete. Finite sagbi basis found!")
	    );
	) else (
	if (compTable#"options"#AutoSubduceOnPartialCompletion) and (not compTable#"data"#"autoSubductedSagbiGenerators") then (
	    -- apply autosubduction to the sagbiGenerators
	    compTable#"data"#"sagbiGenerators" = autosubduceSagbi(compTable);
	    compTable#"data"#"autoSubductedSagbiGenerators" = true;
	    ); 
	);        
    )


end --
