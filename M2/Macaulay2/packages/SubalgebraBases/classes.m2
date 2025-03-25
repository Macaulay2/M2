-- Subring data-type inherits from HashTable
-- Subrings are very light-weight and contain:
-- ambient ring,
-- generators,
-- presentation data: map and ring,
-- flattening data: map, inverse map, ring;
-- for working with rings like (QQ[x])[y]
-- It also contains in its cache a SAGBIBasis
-- object which is the 'furthest-advanced'
-- computation data
Subring = new Type of HashTable

-- Subring constructor
subring = method(Options => {GeneratorSymbol => null})
subring Matrix := opts -> M -> (
    if M.cache#?Subring then (
        M.cache#Subring
    )
    else (
        R := ring M;
        (F,RtoF,FtoR) := flattenRing(R,Result=>3);
        coeffRing := coefficientRing F;
        local subductionRing;
        if instance(opts.GeneratorSymbol,Nothing) then
            subductionRing = coeffRing(monoid[Variables => numcols M])
        else if instance(opts.GeneratorSymbol,Symbol) then
            subductionRing = coeffRing[opts.GeneratorSymbol_1..opts.GeneratorSymbol_(numcols M)]
        else error("Invalid GeneratorSymbol option");
        presentationMap := map(R,subductionRing,M);
        S := new Subring from {
            "ambientRing" => R,
            "flattenedRing" => F,
            "generators" => RtoF M,
            "presentationRing" => subductionRing,
            "presentationMap" => presentationMap,
            "flatteningMap" => RtoF,
            "inverseFlatteningMap" => FtoR,
            cache => new CacheTable from {if M.cache#?Subring then M.cache#Subring}
        };
        M.cache#Subring = S
    )
)
subring List := opts -> L -> subring(matrix{L}, opts)

-- Subring access functions
-- gens must take an option since gens is defined to be a method function with options
ambient Subring := S -> S#"ambientRing"
flattenedRing = method()
flattenedRing Subring := S -> S#"flattenedRing"
gens Subring := opts -> S -> S#"inverseFlatteningMap" S#"generators"
numgens Subring := S -> numcols gens S
presentationRing = method()
presentationRing Subring := S -> S#"presentationRing"
net Subring := S -> (
    R := ambient S;
    A := presentationRing S;
    toString(A) | ", subring of " | toString(R)
)

-- SAGBIBasis computation object data-type that inherits from a HashTable
-- A SAGBIBasis represents a partial or complete SAGBI computation.
-- It contains all the data necessary to continue a computation
-- including: sagbi generators, rings, ideals, maps, and options.
-- When constructing a SAGBIBasis, it is therefore suggested to specify
-- all options of the computation, so the default options table is 
-- identical to the sagbi options table.

SAGBIBasis = new Type of HashTable

-- Options:
-- AutoSubduce is a boolean that determines whether the generators should be subducted against each other
-- at the beginning of the sagbi computation.
-- ReduceNewGenerators is a boolean that determines whether gaussian elimination is applied to newly found
-- sagbi generators.
-- StorePending is a boolean that determines whether the pending list is stored in the computation
-- object. The pending list may be quite large in some cases.
-- Strategy is the strategy used in the algorithm: Master, DegreeByDegree or Incremental
-- SubductionMethod is either "Top" or "Engine", determines which code is used for performing subductions
-- Limit is a positive integer that determines when the computation should terminate if no finite sagbi
-- basis is computed
-- AutoSubduceOnPartialCompletion is a boolean that determines whether autoSubduction is applied
-- to the sagbi Generators the first time no new sagbi generators are added at a particular degree
-- PrintLevel a non-negative integer that controls the verbosity of the computation
-- Recompute if true then the sagbi computation restarts
-- RenewOptions if true then the options of the SAGBIBasis are ignored and the newly supplied
-- options are used instead
-- Note: the sagbiBasis object stores all the options used for the sagbi basis computation
sagbiBasis = method(
    TypicalValue => SAGBIBasis,
    Options => {
        AutoSubduce => true,
        ReduceNewGenerators => true,
        StorePending => true,
        Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Top", -- Top (default) or Engine
        Limit => 20,
        AutoSubduceOnPartialCompletion => false,
        PrintLevel => 0,
        Recompute => false,
        RenewOptions => false
    }
)

-- SAGBIBasis constructor
sagbiBasis Subring := opts -> S -> (
    if S.cache#?SAGBIBasis then return S.cache#SAGBIBasis;
    -- Keys:
    -- rings stores the various rings we need in our constructions
    -- maps stores the maps between our rings
    -- ideals stores the ideals for the sagbi computations
    -- data stores any SAGBI-related data from the computation
    -- pending stores the pending list from the computation
    -- (if StorePending is true)
    -- options stores the options used for the computation
    -- Rings:
    -- The rings are the quotientRing, liftedRing, tensorRing.
    -- quotientRing is the ring of the generators of S
    -- liftedRing is the ring of the quotientRing
    -- tensorRing is the ring consisting of the variables
    -- of ambientRing and a polynomial for each SAGBI generator.
    -- Initially, the tensorRing is the same as the ambientRing,
    -- but using a different variable name.
    ambientRing := ambient S;
    liftedRing := ambient flattenedRing S;
    if heft liftedRing === null then error "expected ring with heft vector";
    heftVector := transpose matrix {heft liftedRing};
    numberVariables := numgens liftedRing;
    numberGenerators := numColumns gens S;
    newMonomialOrder := (monoid liftedRing).Options.MonomialOrder;
    tensorVariables := monoid[
        Variables => numberVariables,
        Degrees => flatten entries ((matrix degrees source vars liftedRing)*heftVector),
        MonomialOrder => newMonomialOrder];
    rings := new HashTable from {
        "ambientRing" => ambientRing,
        quotientRing => flattenedRing S,
        "liftedRing" => liftedRing,
        tensorRing => (coefficientRing liftedRing) tensorVariables,
        "heftVector" => heftVector
    };
    -- With the notation:
    -- sagbi generators = {g_1 .. g_t}
    -- quotientRing = K[z_1 .. z_s]/I
    -- liftedRing = K[z_1 .. z_s]
    -- tensorRing = K[x_1 .. x_s, y_1 .. y_t]
    -- Note: initially t = 0 i.e. TensorRing is initially a copy of LiftedRing
    -- Maps:
    -- inclusionLifted:  LiftedRing -> TensorRing:    z_i -> x_i
    -- substitution:     TensorRing -> TensorRing:    x_i -> x_i, y_j -> g_j(x_1 .. x_s)
    -- projectionLifted: TensorRing -> LiftedRing:    x_i -> z_i, y_j -> 0
    -- sagbiInclusion:   TensorRing -> TensorRing:    x_i -> 0,   y_j -> y_j
    -- fullSubstitution: TensorRing -> LiftedRing:    x_i -> z_i, y_j -> g_j(z_1 .. z_s)
    -- quotientRing:     LiftedRing -> QuotientRing:  z_i -> z_i
    inclusionLifted := map(rings.tensorRing,rings#"liftedRing",
        (vars rings.tensorRing)_{0..numberVariables-1});
    substitution := map(rings.tensorRing,rings.tensorRing,
        (vars rings.tensorRing)_{0..numberVariables-1});
    projectionLifted := map(rings#"liftedRing",rings.tensorRing,
        vars rings#"liftedRing");
    sagbiInclusion := map(rings.tensorRing,rings.tensorRing,
        matrix {toList (numberVariables:0_(rings.tensorRing))});
    maps := new HashTable from {
        "inclusionLifted" => inclusionLifted,
        "projectionLifted" => projectionLifted,
        "sagbiInclusion" => sagbiInclusion,
        "substitution" => substitution,
        "fullSubstitution" => projectionLifted * substitution,
        "quotient" => map(rings.quotientRing,liftedRing, vars rings.quotientRing),
        "presentationMap" => S#"presentationMap",
        "flatteningMap" => S#"flatteningMap",
        "inverseFlatteningMap" => S#"inverseFlatteningMap"
    };
    -- Ideals:
    -- I = the ideal that induces quotientRing
    -- SIdeal = the ideal of S-polynomials (in the tensorRing)
    -- leadTermsI = initial ideal of I
    -- reductionIdeal = SIdeal + lift(leadTermsI)
    SIdeal := ideal(0_(rings.tensorRing));
    leadTermsI := ideal leadTerm ideal flattenedRing S;
    ideals := new HashTable from {
        "I" => ideal flattenedRing S,     -- I --> quotient / presentation ideal
        "SIdeal" => SIdeal,
        "leadTermsI" => leadTermsI,
        "reductionIdeal" => maps#"inclusionLifted" leadTermsI + SIdeal,
    };
    -- Data: subalgebraGenerators, sagbiGenerators, sagbiDegrees, sagbiStatus,
    -- degree, limit, autoSubductedSagbiGenerators, subring
    -- subalgebraGenerators is a matrix with the original generators of the subring
    -- sagbiGenerators is a matrix of all the sagbi elements
    -- that we have found so far in the liftedRing. It is initially empty.
    -- sagbiDegrees are the degrees of the sagbiGenerators,
    -- it is initially empty.
    -- sagbiStatus is an integer that stores the current status of the computation:
    -- 0 no result - the computation has terminated but cannot guarantee a sagbi basis or non sagbi basis
    -- 1 result - the sagbi generators form a sagbi basis
    -- 2 result - the sagbi generators do not form a sagbi basis
    -- degree is the current degree under consideration by
    -- the algorithm.  It is initially -1 to indicate that no
    -- computation has occurred.
    -- limit is the maximum value of the degree.
    -- It is initially set to -1 to indicate that no limit is set.
    -- autoSubductedSagbiGenerators indicates if the sagbiGenerators
    -- have been autoSubducted (see sagbi option: AutoSubductOnPartialCompletion).
    -- subring a pointer back to S, used for updating the pointer from S to a
    -- SAGBIBasis object once a more advanced object has been computed.
    data := new HashTable from {
        "subalgebraGenerators" => S#"generators",
        "sagbiGenerators" => matrix(rings#"liftedRing",{{}}),
        "sagbiDegrees" => matrix(ZZ,{{}}),
        "sagbiStatus" => 0,
        degree => -1,
        "limit" => -1,
        "autoSubductedSagbiGenerators" => false,
        subring => S
    };
    -- Pending: initially an empty hashtable. It is filled with
    -- keys of degrees pointing to lists of potential sagbi generators.
    pending := new HashTable from {};
    -- Options: see above description of the options for sagbi computations
    optionTable := new HashTable from opts;
    newSAGBIBasis := new SAGBIBasis from {
        SAGBIrings => rings,
        SAGBImaps => maps,
        SAGBIideals => ideals,
        SAGBIdata => data,
        SAGBIpending => pending,
        SAGBIoptions => optionTable
    };
    S.cache#SAGBIBasis = newSAGBIBasis
)

net SAGBIBasis := S -> (
    local description;
    if S#SAGBIdata#"sagbiStatus" == 1 then (
        description = "SAGBIBasis Computation Object with "
    )
    else (
        description = "Partial SAGBIBasis Computation Object with "
    );
    description | toString(numcols S#SAGBIdata#"sagbiGenerators") |
    " generators, Limit = " | toString(S#SAGBIdata#"limit") | "."
)

-- gens(SAGBIBasis) returns the current list of sagbi generators.
-- This might not generate the entire algebra if the computation is
-- not complete.  This matches the behavior of gb.
-- Note: the sagbiGenerators lie in the liftedRing, so we
-- apply the quotient map for the user.
gens SAGBIBasis := opts -> S -> (
    if numColumns S#SAGBIdata#"sagbiGenerators" == 0 then (
        S#SAGBImaps#"inverseFlatteningMap" S#SAGBImaps#"quotient" matrix(S#SAGBIrings#"liftedRing",{{}})
    )
    else (
        S#SAGBImaps#"inverseFlatteningMap" S#SAGBImaps#"quotient" S#SAGBIdata#"sagbiGenerators"
    )
)

-- Returns the lifted ring
ring SAGBIBasis := S -> (
    S#SAGBIrings#"liftedRing"
)

-- Returns the quotient ring
ambient SAGBIBasis := S -> (
    S#SAGBIrings#"ambientRing"
)

-- Returns the flattened ring
flattenedRing SAGBIBasis := S -> (
    S#SAGBIrings.quotientRing
)

-- Returns the subring that a SAGBIBasis points to
subring SAGBIBasis := opts -> S -> (
    S#SAGBIdata#subring
)

-- current degree of the computation
sagbiDegree = method()
sagbiDegree SAGBIBasis := SB -> (
    SB#SAGBIdata#degree
)

-- limit of the computation
sagbiLimit = method()
sagbiLimit SAGBIBasis := SB -> (
    SB#SAGBIdata#"limit"
)

-- is the computation finished
sagbiStatus = method()
sagbiStatus SAGBIBasis := SB -> (
    SB#SAGBIdata#"sagbiStatus" == 1
)

-- status of the computation
status SAGBIBasis := opts -> SB -> (
    "status: " | if sagbiStatus SB then (
        "done; sagbiGenerators encountered up to degree " |
        toString max first entries SB#SAGBIdata#"sagbiDegrees"
    )
    else (
        "DegreeLimit; current degree " | toString sagbiDegree SB
    )
)

numgens SAGBIBasis := S -> numcols gens S

-- SAGBIComputation is an internal type used for SAGBI computation
-- it is a SAGBIBasis where the hash tables, such as rings, maps, ideals, are mutable
SAGBIComputation = new Type of HashTable

-- once a SAGBI computation has finished then the resulting SAGBIComputation object
-- is de-converted into a SAGBIBasis
sagbiBasis SAGBIComputation := opts -> H -> (
    rings := new HashTable from H#SAGBIrings;
    maps := new HashTable from H#SAGBImaps;
    ideals := new HashTable from H#SAGBIideals;
    data := new HashTable from H#SAGBIdata;
    pending := new HashTable from apply(keys H#SAGBIpending,i-> i => new List from H#SAGBIpending#i);
    optionTable := new HashTable from H#SAGBIoptions;
    newSAGBIBasis := new SAGBIBasis from {
        SAGBIrings => rings,
        SAGBImaps => maps,
        SAGBIideals => ideals,
        SAGBIdata => data,
        SAGBIpending => pending,
        SAGBIoptions => optionTable
    };
    newSAGBIBasis#SAGBIdata#subring.cache#SAGBIBasis = newSAGBIBasis
)

end--
