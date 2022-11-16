-- Subring data-type inherits from HashTable
-- Subrings are very light-weight and contain
-- only the defining ring and the generators.
-- It also contains in its cache a SAGBIBasis
-- object which is the 'furthest-advanced'
-- computation data

Subring = new Type of HashTable

-- Subring constructor
subring = method(Options => {GeneratorSymbol => null})
subring Matrix := opts -> M -> (
    if M.cache#?Subring then (
	M.cache#Subring
	) else (
	R := ring M;
	coeffRing := coefficientRing R;
	local subductionRing;
	if instance(opts.GeneratorSymbol,Nothing) then
	     subductionRing = coeffRing[Variables => numcols M]
	else if instance(opts.GeneratorSymbol,Symbol) then
	     subductionRing = coeffRing[opts.GeneratorSymbol_1..opts.GeneratorSymbol_(numcols M)]
	else error("Invalid GeneratorSymbol option");

    	S := new Subring from{
            "ambientRing" => R,
            "generators" => M,
	    "subductionQuotientRing" => subductionRing,
            cache => new CacheTable from M.cache
    	    };
    	M.cache#Subring = S
	)
    )
subring List := opts -> L -> subring(matrix{L}, opts)

-- Subring access functions
-- gens must take an option since gens is defined to be a method function with options
ambient Subring := S -> S#"ambientRing"
gens Subring := opts -> S -> S#"generators"
numgens Subring := S -> numcols gens S
subductionQuotientRing = method()
subductionQuotientRing Subring := S -> S#"subductionQuotientRing"
net Subring := S -> (
    R := ambient S;
    A := subductionQuotientRing S;    
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
-- > AutoSubduce is a boolean that determines whether the generators should be subducted against each other
--   at the beginning of the sagbi computation.
-- > ReduceNewGenerators is a boolean that determines whether gaussian elimination is applied to newly found
--   sagbi generators.
-- > StorePending is a boolean that determines whether the pending list is stored in the computation
--   object. The pending list may be quite large in some cases.
-- > Strategy is the strategy used in the algorithm: Master, DegreeByDegree or Incremental
-- > SubductionMethod is either "Top" or "Engine", determines which code is used for performing subductions
-- > Limit is a positive integer that determines when the computation should terminate if no finite sagbi
--   basis is computed
-- > AutoSubduceOnPartialCompletion is a boolean that determines whether autoSubduction is applied
--   to the sagbi Generators the first time no new sagbi generators are added at a particular degree
-- > PrintLevel a non-negative integer that controls the verbosity of the computation
-- > Recompute if true then the sagbi computation restarts
-- > RenewOptions if true then the options of the SAGBIBasis are ignored and the newly supplied
---  options are used instead
--- 
--- Note, the sagbiBasis object stores all the options used for the sagbi basis computation

sagbiBasis = method(
    TypicalValue => Subring,
    Options => {
    	AutoSubduce => true,
        ReduceNewGenerators => true,
	StorePending => true,
        Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Top", -- Top (default) or Engine
    	Limit => 100,
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
    -- > rings stores the various rings we need in our constructions
    -- > maps stores the maps between our rings
    -- > ideals stores the ideals for the sagbi computations
    -- > data stores any SAGBI-related data from the computation
    -- > pending stores the pending list from the computation
    -- (if StorePending is true)
    -- > options stores the options used for the computation
    
    -- Rings:
    -- The rings are the quotientRing, liftedRing, tensorRing.
    -- > quotientRing is the ring of the generators of S
    -- > liftedRing is the ring of the quotientRing
    -- > tensorRing is the ring consisting of the variables
    --   of ambientRing and a polynomial for each SAGBI generator.
    -- Initially, the tensorRing is the same as the ambientRing,
    -- but using a different variable name.
    liftedRing := ambient ambient S;   
    numberVariables := numgens liftedRing;
    numberGenerators := numColumns gens S;
    newMonomialOrder := (monoid liftedRing).Options.MonomialOrder;
    tensorVariables := monoid[
        Variables => numberVariables,
        Degrees => degrees source vars liftedRing,
        MonomialOrder => newMonomialOrder];
    rings := new HashTable from {
        quotientRing => ambient S,
        "liftedRing" => liftedRing,
        tensorRing => (coefficientRing liftedRing) tensorVariables
        };
    
    -- With the notation:
    -- > sagbi generators = {g_1 .. g_t}
    -- > quotientRing = K[z_1 .. z_s]/I
    -- > liftedRing = K[z_1 .. z_s]
    -- > tensorRing = K[x_1 .. x_s, y_1 .. y_t]
    -- NB initially t = 0 i.e. TensorRing is initially a copy of LiftedRing
    --
    -- Maps:
    -- > inclusionLifted:  LiftedRing -> TensorRing :   z_i -> x_i 
    -- > substitution:     TensorRing -> TensorRing:    x_i -> x_i, y_j -> g_j(x_1 .. x_s)
    -- > projectionLifted: TensorRing -> LiftedRing:    x_i -> z_i, y_j -> 0
    -- > sagbiInclusion:   TensorRing -> TensorRing:    x_i -> 0,   y_j -> y_j
    -- > fullSubstitution: TensorRing -> LiftedRing:    x_i -> z_i, y_j -> g_j(z_1 .. z_s)
    -- > quotientRing:     LiftedRing -> QuotientRing : z_i -> z_i    
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
        "quotient" => map(rings.quotientRing,liftedRing,vars rings.quotientRing)
    };
    
    -- Ideals:
    -- > I = the ideal that induces quotientRing
    -- > SIdeal = the ideal of S-polynomials (in the tensorRing)
    -- > leadTermsI = initial ideal of I
    -- > reductionIdeal = SIdeal + lift(leadTermsI)    
    SIdeal := ideal(0_(rings.tensorRing));
    leadTermsI := ideal leadTerm ideal ambient S;
    ideals := new HashTable from {
        "I" => ideal ambient S,     -- I --> quotient / presentation ideal 
        "SIdeal" => SIdeal,         --  
        "leadTermsI" => leadTermsI, -- 
        "reductionIdeal" => maps#"inclusionLifted" leadTermsI + SIdeal,
    };
    
    -- Data: subalgebraGenerators, sagbiGenerators, sagbiDegrees, sagbiDone, 
    --       degree, limit, autoSubductedSagbiGenerators, subring
    -- > subalgebraGenerators is a matrix with the original generators of the subring
    -- > sagbiGenerators is a matrix of all the sagbi elements
    --   that we have found so far in the liftedRing. It is initially empty.
    -- > sagbiDegrees are the degrees of the sagbiGenerators,
    --   it is initially empty.
    -- > sagbiDone is set to be true if the sagbiGenerators form
    --   a finite sagbi basis.  It is initially false.
    -- > degree is the current degree under consideration by
    --   the algorithm.  It is initially -1 to indicate that no
    --   computation has occurred.
    -- > limit is the maximum value of the degree.
    --   It is initially set to -1 to indicate that no limit
    --   is set.
    -- > autoSubductedSagbiGenerators indicates if the sagbiGenerators 
    --   have been autoSubducted (see sagbi option: AutoSubductOnPartialCompletion).
    -- > subring a pointer back to S, used for updating the pointer from S to a
    --   SAGBIBasis object once a more advanced object has been computed.
    data := new HashTable from {
        "subalgebraGenerators" => S#"generators",
        "sagbiGenerators" => matrix(rings#"liftedRing",{{}}),
        "sagbiDegrees" => matrix(ZZ,{{}}),
        "sagbiDone" => false,
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

sagbiBasis HashTable := opts -> H -> (
    rings := new HashTable from H#SAGBIrings;
    maps := new HashTable from H#SAGBImaps;
    ideals := new HashTable from H#SAGBIideals;
    data := new HashTable from H#SAGBIdata;
    pending := new HashTable from apply(keys H#SAGBIpending,i-> i => new List from (H#SAGBIpending#i));
    optionTable := new HashTable from H#SAGBIoptions;
    newSAGBIBasis := new SAGBIBasis from {
        SAGBIrings => rings, -- rings -> SAGBIRings + protect
        SAGBImaps => maps,
        SAGBIideals => ideals,
        SAGBIdata => data,
        SAGBIpending => pending,
        SAGBIoptions => optionTable
    };
    
    newSAGBIBasis#SAGBIdata#subring.cache#SAGBIBasis = newSAGBIBasis
)

net SAGBIBasis := S -> (
    local description;
    if S#SAGBIdata#"sagbiDone" then (
    	description = "SAGBIBasis Computation Object with "
    ) else (
    	description = "Partial SAGBIBasis Computation Object with "
    );
    description | toString(numcols S#SAGBIdata#"sagbiGenerators") |
    		" generators, Limit = " | toString(S#SAGBIdata#"limit") | "."
)

-- gens(SAGBIBasis) returns the current list of sagbi generators.
-- This might not generate the entire algebra if the computation is
-- not complete.  This matches the behavior of gb.
-- Note that the sagbiGenerators lie in the liftedRing, so we
-- apply the quotient map for the user.

gens SAGBIBasis := opts -> S -> (
    if numColumns S#SAGBIdata#"sagbiGenerators" == 0 then (
        S#SAGBImaps#"quotient" matrix(S#SAGBIrings#"liftedRing",{{}})
    	) else (
    	S#SAGBImaps#"quotient" S#SAGBIdata#"sagbiGenerators"
	)
)

-- Returns the lifted ring
ring SAGBIBasis := S -> (
    S#SAGBIrings#"liftedRing"
)

-- Returns the quotient ring
ambient SAGBIBasis := S -> (
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

-- is the computation finished (sagbiDone)
sagbiStatus = method()
sagbiStatus SAGBIBasis := SB -> (
    SB#SAGBIdata#"sagbiDone"
    )

-- status of the computation
status SAGBIBasis := opts -> SB -> (
    "status: " | if sagbiStatus SB then (
	"done; sagbiGenerators encountered up to degree " | toString max first entries SB#SAGBIdata#"sagbiDegrees"
	) else (
	"DegreeLimit; current degree " | toString sagbiDegree SB
	)
    )


numgens SAGBIBasis := S -> numcols gens S

end--
