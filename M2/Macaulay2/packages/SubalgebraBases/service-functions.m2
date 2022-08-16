-- importFrom (Core,{"raw","rawStatus1","rawMonoidNumberOfBlocks"});
importFrom (Core,{"raw","rawStatus1","rawMonoidNumberOfBlocks","rawSubduction1"});

initializeCompTable = method();
initializeCompTable (SAGBIBasis, HashTable):= (S,opts) -> (
    rings := new MutableHashTable from S#"rings";
    maps := new MutableHashTable from S#"maps";
    ideals := new MutableHashTable from S#"ideals";
    data := new MutableHashTable from S#"data";
    pending := new MutableHashTable from S#"pending";

    -- Need to check that there aren't clashes between old options and new options
    options := new MutableHashTable from opts;

    data#"limit" = opts.Limit;
    options#"variableName" = S#"options"#"variableName";
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

limitedCompTable = method();
limitedCompTable (HashTable, Matrix) := (H,M) -> (
    -- Construct the monoid of a ring with variables corresponding to generators of the ambient ring and the subalgebra.
    -- Has an elimination order that eliminates the generators of the ambient ring.
    -- The degrees of generators are set so that the SyzygyIdeal is homogeneous.
    numberVariables := numgens H#"rings"#"liftedRing";
    numberGenerators := numColumns M;
    -- newMonomialOrder := prepend(Eliminate numberVariables, (monoid H#"rings"#"liftedRing").Options.MonomialOrder);
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

    -- The maps are ...
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
    
    
    -- See initializeCompTable (used to be a MutableHashTable) --
    new HashTable from {
        "rings" => rings,
        "maps" => maps,
        "ideals" => ideals,
        "data" => data,
        "pending" => pending,
        "options" => options
    }
)

-- Subduction takes a compTable and 1-row matrix M with entries in the quotientRing and subducts the elements of M against compTable
-- there may be some significant overhead for creating a MutableMatrix

subduction = method();
subduction(HashTable, MutableMatrix) := (compTable, M) -> (
    new MutableMatrix from subduction(compTable, matrix M)
    )

subduction(HashTable, Matrix) := (compTable, M) -> (
    local result;
    
    if compTable#"options"#PrintLevel > 3 then (    
	print("-- subduction input:");
	print(M);
	);
    
    result = subductionTopLevel(compTable, M);
     --if compTable#"options"#SubductionMethod == "Internal" then (
--	result = subductionInternal(compTable, M);
--	);
    
    if compTable#"options"#PrintLevel > 3 then (    
	print("-- subduction result using "| compTable#"options"#SubductionMethod |" strategy:");
	print(result);
	);
    
    result
    )


-- subduction TopLevel (compTable, M) -> Matrix
-- M a 1 row matrix of ring elements in the liftedRing
-- takes each element of M and subducts it against the sagbiGenerators of the compTable
--
-- TODO: combine subduction and subductionTopLevel
-- NB: subductionTopLevel passes off to either the top-level lead-term subduction method
--     or the internal subduction 
--

subductionTopLevel = method();
subductionTopLevel(HashTable, Matrix) := (compTable, M) -> (
    if zero(M) then return M;
    result := matrix {toList(numcols(M):0_(compTable#"rings"#"liftedRing"))};
    g := matrix {
	for m in first entries M list sub(m, compTable#"rings"#"liftedRing")
	};
    local subductedPart;
    local leadTermSubductedPart;
    -- can the following if statement be absorbed into the while loop?
    -*
    if not (zero (g)) then (
	subductedPart := subductionLeadTerm(compTable, g);
	leadTermSubductedPart := leadTerm subductedPart;
	result = result + leadTermSubductedPart;
	g = (subductedPart - leadTermSubductedPart) % compTable#"ideals"#"I";
	);
    *-
    -- exit the loop if g is zero
    
    if compTable#"options"#PrintLevel > 5 then(
	print("-- [subductionTopLevel] polys to subduct:");
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
	    print("-- [subductionTopLevel] result so far:");
	    print(transpose result);
	    print("--[subductionTopLevel] remaining to subduct:");
	    print(transpose g);
	    );
	
	);
    result % compTable#"ideals"#"I"
);


-- this subduction expects the matrix M to have entries in liftedRing
-- performs subduction only until the lead term is correct

-- todo: check which compTable options have been set when doing % reductionIdeal. Check other options. 
-- 
-- 
-- Add more documentation on why this method works ...
--   > explain what are h, projection h 
--   > why is projectionh == 0 the correct condition for termination?
--   > 
-- 

-- NB this method assumes M is reduced mod I
subductionTopLevelLeadTerm = method();
subductionTopLevelLeadTerm (HashTable, Matrix) := (compTable, M) -> (
    liftg := M;
    while not (zero liftg) do ( 
        tensorRingLiftg := compTable#"maps"#"inclusionLifted" liftg;
	tensorRingLeadTermg := leadTerm tensorRingLiftg;
	h := tensorRingLeadTermg % (compTable#"ideals"#"reductionIdeal"); -- do partial % based on compTable option
	
	-- sagbiInclusion = map(tensorRing, tensorRing, {0 .. 0, p_1 .. p_r})
	-- substitution = map(tensorRing, tensorRing, {x_1 .. x_n, f_1 .. f_r}) where f_i are the sagbiGens
	-- fullSubstitution = map(liftedRing tensorRing, {x_1 .. x_n, f_1 .. f_r}) same as substitution except goes to the lifted ring
	 
	projectionh := compTable#"maps"#"fullSubstitution" compTable#"maps"#"sagbiInclusion" h;
	-- previous version of projectionh lived in the tensor ring
	-- projectionh := compTable#"maps"#"substitution" compTable#"maps"#"sagbiInclusion" h;

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
	-- note that h is a monomial, if it is divisible by some x_i then projectionh is zero
	--
	if zero(projectionh) then break;
	
	-- previously projectionh lived in tensorRing and we checked that projectionh was the zero matrix directly
	-- Ollie assumes that the zero function is inbuilt and somehow faster (?)
	--
	-- if (projectionh == 0_(compTable#"rings"#"tensorRing")) then break;
	-*
	h = matrix {apply(
		numcols projectionh,
		i -> 
		if projectionh_(0,i) == 0_(compTable#"rings"#"tensorRing") then (
		    0_(compTable#"rings"#"tensorRing") 
		    ) else (
		    h_(0,i))
		)};
	-- update g
	-- fullSubstitution = map(liftedRing, TR, {x_1 .. x_n, f_1 .. f_r})
	hSub := (compTable#"maps"#"fullSubstitution" h) % (compTable#"ideals"#"I");
        liftg = liftg - hSub;
	*-
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
	    generatorMatrix_(0,i) = (subduction(tempCompTable,generatorMatrix_{i}))_(0,0);
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
	    generatorMatrix_(0,i) = (subduction(tempCompTable,generatorMatrix_{i}))_(0,0);
            if not generatorMatrix_(0,i) == 0 then
                generatorMatrix_(0,i) = generatorMatrix_(0,i)*(1/leadCoefficient(generatorMatrix_(0,i)));
    );
    compress new Matrix from generatorMatrix
)


-- Checks the Strategy: DegreeByDegree or Incremental.
--   passes to a function that recomputed the rings, maps and ideals wrt new sagbiGenerators
--
-- NB if there are no new sagbiGenerators, then this function is not called

updateComputation = method();
updateComputation(HashTable) := (compTable) -> (
    
    -- TODO: we can add a "Master" (default) strategy which can dynamically switch between these strategy 
    -- Current ideas for "Master" strategy:
    -- 1) If "Incremental" was used last time (i.e. we have a full GB for reductionIdeal computed) and only 1 new
    --    sagbiGenerator is added, then use "Incremental"
    -- [Add more specifications based on heuristics about computations]
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


-- updateComputation Master Strategy
-- We use the heuristic that sagbiGenerators are generally clumped together
-- at low degree and become sparser at higher degrees
--
-- So, we check the number of sagbiGenerators that were added last time
-- during the last loop of sagbi algorithm
-- if 0 or 1 sagbiGenerators were added and the degree of the last added generator > 5 then use Incremental
-- otherwise use DegreeByDegree
--
-- TODO: the number 5 is COMPLETELY ARBITRARY, find a better heuristic
--       Ollie's idea is that while there are generators of low degree it's okay to use DegreeByDegree
--       

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
-- This does not compute a gb for the reductionIdeal 
-- The gb computation next occurs in collectSPairs or during subduction

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
    -- newMonomialOrder := prepend(Eliminate numberVariables, (monoid liftedRing).Options.MonomialOrder);
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


-- TODO:
-- SIdeal can be safely removed from the sagbiBasis/compTable objects

----------------------------------------------------------
-- update computation by using the incremental strategy
--
-- This method computes a full GB for the reductionIdeal by
-- using a (potentially previously computed) GB. This is done
-- by computing a GB in for the following ideal:
--   J = ideal (newTagVariables - newSagbiGenerators) 
-- which lives in the quotient ring:
--   oldTensorRing / oldReductionIdeal
-- Note this GB computation is faster than computing a GB for the new reductionIdeal
-- because we already have a GB for the oldReductionIdeal. Ask Mike for the details for why 
-- it works, but Ollie's guess is that: 
--   all new S-Pairs for J are formed and reduced modulo the oldReductionIdeal
--   which is equivalent to performing Buchbergers algorithm on the new reductionIdeal
--   except that we skip all the generators of oldReductionIdeal. 
-- After forming the new objects, the compTable is updated.

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
    -- newMonomialOrder := prepend(Eliminate numberVariables, (monoid liftedRing).Options.MonomialOrder);
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
    
    -- TODO: can use data#NumberOfNewGenerators to get the number of generators added
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
-- -> returns the lowest degree of a new sagbiGenerator added
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
	
        --   Previously, reduction used gb:
        -- reducedGenerators := gens gb( matrix{toList compTable#"pending"#currentLowest}, DegreeLimit=> currentLowest);
	--   What happens to all the generators that jump up in degree in the gb?
        --   >> the jump in degree does not occur is the monomial order is graded
        
	-- Todo:
	--   check to see if the leading terms have the same degree. If so, we can use the degree-limited gb computation above instead
	--   clean up the following (perhaps removing ReduceNewGenerators options)	
	if compTable#"options"#ReduceNewGenerators then (
	    -- reducedGenerators = subductNewGenerators(compTable, matrix{toList compTable#"pending"#currentLowest});
	    reducedGenerators = triangularBasis matrix{toList compTable#"pending"#currentLowest};
	    ) else (
	    -- TODO: This shouldn't be an option, we should always reduce the generators ... unless ...
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

-*
-- Original code:

triangularBasis = L -> (
    R:=ring first L;
    --if (options R).Global then error("use a local order!");
    mons:=rsort unique join flatten apply(L,f-> flatten entries first coefficients f);
    A:=matrix apply(L,f->flatten entries last coefficients(f,Monomials=>mons));
    WW:=symbol WW;
    S:=QQ[WW_1..WW_(numcols A)];
    compress (matrix{mons}*sub(last coefficients(gens gb ideal(sub(A,S)*transpose vars S)),R))
    );
*-				



-----------------------------------
-- THE FOLLOWING IS NO LONGER USED
-----------------------------------
-- subductNewGenerators performs an "autosubduction" on a list of new Generators
-- Pseudocode: 
-- 1)  list of new generators G = {g_1 .. g_k}
-- 2)  for each i from 1 to k:
-- 3)    g_i = subduction(g_i against a compTable with SagbiGenerators g_1 .. g_(i-1))
-- 4)  return G

subductNewGenerators = method();
subductNewGenerators (HashTable, Matrix) := (compTable, newGenerators) -> (
    local tempCompTable;
    local M;
    generatorMatrix := new MutableMatrix from newGenerators;
    
    for i from 0 to (numColumns generatorMatrix) - 1 do (
	M = new Matrix from generatorMatrix_(toList (0..(i-1)));
	tempCompTable = limitedCompTable(compTable,lift(M,compTable#"rings"#"liftedRing"));
	generatorMatrix_(0,i) = (subduction(tempCompTable,generatorMatrix_{i}))_(0,0);
	if not generatorMatrix_(0,i) == 0 then (
	    generatorMatrix_(0,i) = generatorMatrix_(0,i)*(1/leadCoefficient(generatorMatrix_(0,i)));
	    );
	);
    compress new Matrix from generatorMatrix
    );
----------------------------------

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
    -- Question: what should this k be? why does the value -2 work? 
    --   TODO: confirm that 2 is the correct value
    zeroGens := submatrixByDegree(selectInSubring(k, gens sagbiGB), compTable#"data"#"degree");
    SPairs := compTable#"maps"#"fullSubstitution"(zeroGens) % compTable#"ideals"#"I";
    
    -- Have we previously found any syzygies of degree currDegree?        
    if compTable#"pending"#?(compTable#"data"#"degree") then (
        --SPairs = SPairs | compTable#"maps"#"inclusionLifted"(matrix{toList compTable#"pending"#(compTable#"data"#"degree")});
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
--   - if not: it may be possible to get lower degree generators (cf 3-screws example)

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
    -- by taking them modulo the reductionIdeal (check this)
    -- [recall this was necessary for correctness from the 3-screws example]
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


-- The presring part of this needs to be worked on.
-- Perhaps just create a SAGBIBasis object from generators
-- sagbiBasisFromGenerators?
autosubduce = method(TypicalValue => Matrix)
autosubduce(Matrix) := G -> (
    noDupes := new MutableList from first entries G;
    reducedGens := for i from 0 to (numcols G)-1 list(
    	s := G_(0,i);
    	notS := compress submatrix'(matrix({toList noDupes}),,{i});
	if zero notS then return matrix{{s}};
    presNotS := makePresRing(ring notS,notS);
	answer := presNotS#"fullSubstitution"(internalSubduction(presNotS, s));
    	if(answer != 0) then (
	    answer = answer*(1/leadCoef(answer));
	    );
    	noDupes#i = answer;
    	answer
    	);
        compress matrix{reducedGens}
    );



-- return the monomial order stashed inside of a ring
getMonomialOrder = S -> (options S).MonomialOrder

-- Sorts and adds the elements of the matrix "candidates" to the pending list of R
    -- R is a subalgebra
    -- candidates is a matrix of elements of the subalgebra.
    -- Algorithm makes a pass through the elements in the first row of "candidates" and places them in the correct sublist of subalgComp#"Pending".

--Accepts a 1-row matrix inputMatrix and returns a matrix of columns of inputMatrix whose entries all have total degree less than maxDegree
submatBelowDegree = (inputMatrix,maxDegree) -> (
    selectedCols := positions(0..numcols inputMatrix - 1,
        i -> (degrees source inputMatrix)_i < {maxDegree});
    inputMatrix_selectedCols
    )

topLevelSubduction(PresRing, Ideal, Ideal, Matrix) := o -> (S, I, inAIdeal, M) -> (
    R := ring M;	
    tensorRing := S#"tensorRing";
    LTI := ideal leadTerm I;
    -- note the gb computation is already cached
    tensorRingLTI := S#"inclusionAmbient" LTI;
    liftg := M;
    while not (liftg == 0_R) do (
	tensorRingg := S#"inclusionAmbient" liftg;
	--rename tensorRingLiftg?
	tensorRingLTg := leadTerm tensorRingg;
	h := tensorRingLTg % (inAIdeal);
	projectionh := S#"substitution" S#"sagbiInclusion" h;
	-- exit the loop if h does not lie in K[p_1 .. p_r] <- the variables tagging the generators of S
	if (projectionh == 0_tensorRing) then break;
	--h = map(target h, source h, (i,j) ->
	--    if projectionh_(i,j) == 0_R
	--    then 0_(S#"tensorRing")
	--    else h_(i,j));
	h = matrix {apply(
		numcols projectionh,
		i -> 
		if projectionh_(0,i) == 0_tensorRing then (
		    0_tensorRing 
		    ) else (
		    h_(0,i))
		)};
	-- update g
	hSub := (S#"fullSubstitution" h) % I;
	-- Is this % I necessary?
	liftg = liftg - hSub;
	-- Do we need % I here? 
	);
    matrix {apply(
	    first entries liftg,
	    i -> 
	    if (not (i == 0_R)) and (degree(i))_0 == 0 then (
		0_R
		) else (
		i
		)
	    )}
    );



topLevelFullSubduction = method(TypicalValue => RingElement, Options => {PrintLevel => 0})

topLevelFullSubduction(PresRing, Ideal, Ideal, Matrix) := o -> (S, I, inAIdeal, M) -> (
    R := ring M;
    LTI := ideal leadTerm I;	
    tensorRingLTI := S#"inclusionAmbient" LTI;
    result := matrix map(R^1,R^(numcols M),0_R);
    g := M;
    if not (g == 0_R) then (
	subductedPart := topLevelSubduction(S, I, inAIdeal, M, o);
	LTSubductedPart := leadTerm subductedPart;
	-- DANGER!! This may need "% I"!!
	result = result + LTSubductedPart;
	g = (subductedPart - LTSubductedPart) % I;
	);
    -- exit the loop if g is zero or a constant
    while not (g == 0_R) do (
	subductedPart = topLevelSubduction(S, I, inAIdeal, g, o);
	LTSubductedPart = leadTerm subductedPart;
	-- DANGER!! This may need "% I"!!
	result = result + LTSubductedPart;
	g = (subductedPart - LTSubductedPart) % I;
	);
    result % I
    );

topLevelFullSubduction(PresRing, Ideal, Matrix) := o -> (S, I, M) -> (
    R := ring M;
    LTI := ideal leadTerm I;
    tensorRingLTI := S#"inclusionAmbient" LTI;
    inAIdeal := S#"syzygyIdeal" + tensorRingLTI;
    topLevelFullSubduction(S, I, inAIdeal, M)
    );
