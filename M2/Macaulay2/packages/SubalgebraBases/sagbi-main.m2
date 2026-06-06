-- subalgebraBasis returns a matrix of sagbi generators
-- of a given subring, matrix, or list.
-- The returned matrix has a cached copy of the subring
-- which can be recovered with subring(Matrix)
subalgebraBasis = method(
    TypicalValue => Matrix,
    Options => {
        AutoSubduce => true,
        -- applies gaussian elimination to sagbiGens before adding them
        ReduceNewGenerators => true,
        StorePending => true,
        Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Top", -- top or engine
        Limit => 20,
        -- applies autosubduction to the sagbiGens the first time no new terms are added
        AutoSubduceOnPartialCompletion => false,
        PrintLevel => 0,
        Recompute => false,
        RenewOptions => false
    }
)

subalgebraBasis Matrix := opts -> M -> (
    S := subring M;
    SB := sagbi(opts, S);
    gens SB
)

subalgebraBasis List := opts -> L -> (
    SB := sagbi(opts, L);
    gens SB
)

subalgebraBasis Subring := opts -> S -> (
    SB := sagbi(opts, S);
    gens SB
)

-- sagbi returns a computation object of a given subring, matrix, or list
-- Since the return type is a computation object, the computation can pick up where it left off.
-- PrintLevels:
-- PrintLevel > 0: Print some information each loop (don't print any polynomials):
-- computation degree, number of SPairs, number of new generators, termination conditions.
-- PrintLevel > 1: Print basic polynomials: SPairs and new sagbiGens.
-- PrintLevel > 2: Print extra polynomials: reductionIdeal gens, zeroGens, current sagbiGens
-- PrintLevel > 3: Print the input and output of each subduction.
-- PrintLevel > 4: Print processPending data and Master Strategy choices (for debugging)
-- PrintLevel > 5: Print subductionTopLevel intermediate steps (for debugging)
-- Behaviour of RenewOptions and Recompute:
-- if RenewOptions is false and Recompute is true
-- the computation will be renewed using the options of the previous computation
-- if RenewOptions is true and Recompute is false
-- the computation will resume all current options except:
-- if the previous options has Recompute set to be true
-- then the computation will be started from new **
-- if both are true or both are false then it does what is expected
-- ** this should never happen

infiniteLimitWarning := false;

sagbi = method(
    TypicalValue => SAGBIBasis,
    Options => {
        AutoSubduce => true,
        ReduceNewGenerators => true, -- applies gaussian elimination to sagbiGens before adding them
        StorePending => true,
        Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Top", -- top or engine
        Limit => 20,
        -- applies autosubduction to the sagbiGens the first time no new terms are added
        AutoSubduceOnPartialCompletion => false,
        PrintLevel => 0,
        Recompute => false, -- restart the computation (ignored by RenewOptions)
        RenewOptions => false -- use only the supplied options
    }
)

sagbi Matrix := opts -> M -> (
    B := sagbi(opts, subring M);
    M.cache#Subring = B#SAGBIdata#subring;
    B
)

sagbi List := opts -> L -> (
    sagbi(opts, subring L)
)

sagbi Subring := opts -> S -> (
    if opts.Recompute then (
        -- forget about the previous computation object
        remove(S.cache, SAGBIBasis);
    );
    SB := sagbiBasis(S, opts);
    newSB := sagbi(opts, SB);
    S.cache#SAGBIBasis = newSB;
    newSB
)

sagbi SAGBIBasis := opts -> SB -> (
    local S;
    SBSubring := subring SB;
    if #{heft ring SB} == 0 then error "expected ring with heft vector";
    if (not infiniteLimitWarning) and (opts.Limit == infinity) then (
        printerr "Warning: Option Limit is set to infinity.\nThis may produce an infinite loop.";
        infiniteLimitWarning = true;
    );
    -- if Recomputing then create a new SAGBIBasis object
    if opts.Recompute or SB#SAGBIoptions#Recompute then (
        remove(SBSubring.cache, SAGBIBasis);
        S = sagbiBasis(SBSubring, opts);
    )
    else S = SB;
    if (S#SAGBIdata#"limit" > opts.Limit) or S#SAGBIdata#"sagbiStatus" == 1 then return S;
    sagbiComputation := initializeSagbiComputation(S,opts);
    processFirstStep sagbiComputation;
    local SPairs;
    while sagbiComputation#SAGBIdata#degree <= opts.Limit and
        not sagbiComputation#SAGBIdata#"sagbiStatus" == 1 do (
        SPairs = collectSPairs sagbiComputation;
        SPairs = compSubduction(sagbiComputation, SPairs);
        -- update and process the new sagbi generators
        -- update pending returns true if new sagbiGenerators were added and false otherwise
        -- if new sagbiGenerators were added then updatePending
        --    sets the sagbiComputation#SAGBIdata#degree to the lowest degree of a new generator
        -- if no new sagbiGenerators were added then check for termination conditions
        if not updatePending(sagbiComputation, SPairs) then (
            checkTermination sagbiComputation;
        );
        -- move on to the next degree
        sagbiComputation#SAGBIdata#degree = sagbiComputation#SAGBIdata#degree + 1;
        if sagbiComputation#SAGBIoptions#PrintLevel > 2 then(
            print "-- [main] sagbiGenerators are currently:";
            print transpose sagbiComputation#SAGBIdata#"sagbiGenerators";
        );
    );
    sagbiBasis sagbiComputation
)

-- the user subduction methods:
subduction = method(
    Options => {
        -- These options are only used when the user wants to use subduction for their own purposes
        AutoSubduce => true,
        ReduceNewGenerators => true,
        StorePending => true,
        Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Top", -- Top or Engine
        Limit => 20,
        AutoSubduceOnPartialCompletion => false,
        PrintLevel => 0,
        Recompute => false,
        RenewOptions => false
    }
)

subduction(SAGBIBasis, Matrix) := Matrix => opts -> (S, M) -> (
    sagbiComputation := initializeSagbiComputation(S, opts);
    S#SAGBImaps#"inverseFlatteningMap" compSubduction(opts,
        sagbiComputation, S#SAGBImaps#"flatteningMap" M)
)

subduction(SAGBIBasis, RingElement) := RingElement => opts -> (S, m) -> (
    sagbiComputation := initializeSagbiComputation(S, opts);
    S#SAGBImaps#"inverseFlatteningMap" first first entries compSubduction(opts,
        sagbiComputation, S#SAGBImaps#"flatteningMap" matrix {{m}})
)

subduction(Matrix, Matrix) := Matrix => opts -> (F, M) -> (
    S := initializeSagbiComputation(sagbiBasis(subring F, opts), opts);
    S#SAGBIdata#"sagbiGenerators" = F;
    updateComputation S;
    S#SAGBImaps#"inverseFlatteningMap" compSubduction(opts, S, S#SAGBImaps#"flatteningMap" M)
)

subduction(Matrix, RingElement) := RingElement => opts -> (F, m) -> (
    first first entries subduction(opts, F, matrix {{m}})
)

subduction(Subring, Matrix) := Matrix => opts -> (S, M) -> (
    F := gens S;
    subduction(opts, F, M)
)

subduction(Subring, RingElement) := RingElement => opts -> (S, m) -> (
    F := gens S;
    first first entries subduction(opts, F, matrix {{m}})
)

subduction(List, List) := List => opts -> (FList, MList) -> (
    first entries subduction(opts, matrix {FList}, matrix {MList})
)

subduction(List, RingElement) := RingElement => opts -> (FList, m) -> (
    first first entries subduction(opts, matrix {FList}, matrix {{m}})
)

end --
