-- subalgebraBasis returns a matrix of sagbi generators
-- of a given subring, matrix, or list.
-- The returned matrix has a cached copy of the subring
-- which can be recovered with subring(Matrix)

subalgebraBasis = method(
    TypicalValue => Matrix,
    Options => {
	AutoSubduce => true,
        ReduceNewGenerators => true, -- applies gaussian elimination to sagbiGens before adding them
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

subalgebraBasis(Matrix) := opts -> M -> (
    S := subring M;
    SB := sagbi(opts, S);
    gens SB
    );

subalgebraBasis(List) := opts -> L -> (
    SB := sagbi(opts, L);
    gens SB
    );

subalgebraBasis(Subring) := opts -> S -> (
    SB := sagbi(opts, S);
    gens SB
    );

-- sagbi returns a computation object of a given subring, matrix, or list
-- Since the return type is a computation object, the computation can pick up where it left off.
--
-- PrintLevels:
-- PrintLevel > 0: Print some information each loop (don't print any polynomials):
--                 computation degree, number of SPairs, number of new generators, termination conditions.
-- PrintLevel > 1: Print basic polynomials: SPairs and new sagbiGens.
-- PrintLevel > 2: Print extra polynomials: reductionIdeal gens, zeroGens, current sagbiGens
-- PrintLevel > 3: Print the input and output of each subduction.
-- PrintLevel > 4: Print processPending data and Master Strategy choices (for debugging)
-- PrintLevel > 5: Print subductionTopLevel intermediate steps (for debugging)
--
--
-- Behaviour of RenewOptions and Recompute:
--   if RenewOptions is false and Recompute is true
--     the computation will be renewed using the options of the previous computation
--   if RenewOptions is true and Recompute is false
--     the computation will resume all current options except:
--     if the previous options has Recompute set to be true
--        then the computation will be started from new **
--   if both are true or both are false then it does what is expected
--
-- ** the only way this happens is if the SAGBIBasis object was
--    created from a isSAGBI call. In this case we MUST recompute
--

sagbi = method(
    TypicalValue => Subring,
    Options => {
	AutoSubduce => true,
        ReduceNewGenerators => true, -- applies gaussian elimination to sagbiGens before adding them
	StorePending => true,
        Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Top", -- top or engine
    	Limit => 100,
	AutoSubduceOnPartialCompletion => false, -- applies autosubduction to the sagbiGens the first time no new terms are added
    	PrintLevel => 0,
	Recompute => false, -- restart the computation (ignored by RenewOptions)
	RenewOptions => false -- use only the supplied options
    	}
);

sagbi(Matrix) := opts -> M -> (
    B := sagbi(opts, subring M);
    M.cache#Subring = B#SAGBIdata#subring;
    B
);

sagbi(List) := opts -> L -> (
    sagbi(opts, subring L)
);--

sagbi(Subring) := opts -> S -> (
    local SB;
    if opts.Recompute then ( -- forget about the previous computation object
	remove(S.cache, SAGBIBasis);
	);
    SB = sagbiBasis(S, opts);
    newSB := sagbi(opts, SB);
    S.cache#SAGBIBasis = newSB;
    newSB
);

sagbi(SAGBIBasis) := opts -> SB -> (
    local S;
    SBSubring := subring SB;
    -- if Recomputing then create a new SAGBIBasis object
    if opts.Recompute or SB#SAGBIoptions#Recompute then (
	remove(SBSubring.cache, SAGBIBasis);
	S = sagbiBasis(SBSubring, opts); 
	) else (
	S = SB;
	);
    
    if (S#SAGBIdata#"limit" > opts.Limit) or S#SAGBIdata#"sagbiDone" then return S;
    compTable := initializeCompTable(S,opts);
    processFirstStep(compTable);
    
    local SPairs;

    while (compTable#SAGBIdata#degree <= opts.Limit) and
          (not compTable#SAGBIdata#"sagbiDone") do (
    		
        SPairs = collectSPairs(compTable);
	SPairs = compSubduction(compTable, SPairs);
	
	-- update and process the new sagbi generators
	-- update pending returns true if new sagbiGenerators were added and false otherwise
	-- if new sagbiGenerators were added then updatePending
	--    sets the compTable#SAGBIdata#degree to the lowest degree of a new generator
	-- if no new sagbiGenerators were added then check for termination conditions
	if not updatePending(compTable, SPairs) then (
	    checkTermination(compTable);
	    );

	-- move on to the next degree
	compTable#SAGBIdata#degree = compTable#SAGBIdata#degree + 1;
	
	if compTable#SAGBIoptions#PrintLevel > 2 then(
	    print("-- [main] sagbiGenerators are currently: ");
	    print(transpose compTable#SAGBIdata#"sagbiGenerators");
	    );	
    );
    sagbiBasis compTable
)

-- the user subduction methods:
subduction = method( 
    TypicalValue => Matrix,
    Options => { -- These options are only used when the user wants to use subduction for their own purposes
	AutoSubduce => true,
        ReduceNewGenerators => true,
	StorePending => true,
        Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Top", -- Top or Engine
    	Limit => 100,
	AutoSubduceOnPartialCompletion => false,
    	PrintLevel => 0,
	Recompute => false,
	RenewOptions => false
    	}
);


subduction(SAGBIBasis, Matrix) := opts -> (S, M) -> (
    compTable := initializeCompTable(S, opts);
    compSubduction(opts, compTable, M)
    )

subduction(SAGBIBasis, RingElement) := opts -> (S, m) -> (
    compTable := initializeCompTable(S, opts);
    first first entries compSubduction(opts, compTable, matrix {{m}})
    )

subduction(Matrix, Matrix) := opts -> (F, M) -> (
    S := initializeCompTable(sagbiBasis(subring F, opts), opts);
    S#SAGBIdata#"sagbiGenerators" = F;
    updateComputation(S);
    compSubduction(opts, S, M)
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


end --
