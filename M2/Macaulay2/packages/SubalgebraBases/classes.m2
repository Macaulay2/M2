-- Subring data-type inherits from HashTable
-- Subrings are very light-weight and contain
-- only the defining ring and the generators.
-- It also contains in its cache a SAGBIBasis
-- object which is the 'furthest-advanced'
-- computation data

Subring = new Type of HashTable

-- Subring constructor
subring = method()
subring Matrix := M -> (
    S := new Subring from{
        "ambientRing" => ring M,
        "generators" => M,
        cache => new CacheTable from M.cache
    	};
    S#"generators".cache#"Subring" = S;
    S
    )
subring List := L -> subring(matrix{L})

-- Subring access functions
-- gens must take an option since gens is defined to be a method function with options
ambient Subring := A -> A#"ambientRing"
gens Subring := opts -> A -> A#"generators"
numgens Subring := A -> (numcols gens A)
net Subring := A -> "subring of " | toString(ambient A) | " with " | numgens A | " generators"


-- SAGBIBasis computation object data-type that inherits from a HashTable
-- A SAGBIBasis represents a partial or complete SAGBI computation.
-- It contains all the data necessary to continue a computation
-- including: sagbi generators, rings, ideals, maps, and options.
-- When constructing a SAGBIBasis, it is therefore necessary to specify
-- all options of the computation, so the default options table is 
-- identical to the sagbi options table.

SAGBIBasis = new Type of HashTable

-- Options:
-- > AutoSubduce is a boolean that determined whether the generators should be subducted against each other
--   at the beginning of the sagbi computation.
-- > ReduceNewGenerators boolean that determined whether guassian elimination is applied to newly found
--   sagbi generators.
-- > StorePending is a boolean that determines whether the pending list is stored in the computation
--   object. The pending list may be quite large in some cases.
-- > Strategy is the strategy used in the algorithm: Master, DegreeByDegree or Incremental
-- > SubductionMethod is either "Top" or "Engine", determines which code is used for performing subductions
-- > Limit is a positive integer that determines which the computation should terminate if no finite sagbi
--   basis is computed
-- > AutoSubduceOnPartialCompletion if true then apllies autoSubduction to the sagbi Generators the first time no new
--   sagbi generators are added at a particular degree
-- > PrintLevel a non-negative integer that controls the verbosity of the computation
-- > Recompute if true then the sagbi computation restarts
-- > RenewOptions if true then the options of the SAGBIBasis are ignored and the newly supplied options are used instead
-- 
-- Note, the sagbiBasis object stores all the options used for the computation

sagbiBasis = method(
    TypicalValue => Subring,
    Options => {
    	AutoSubduce => true,
        ReduceNewGenerators => true,
	StorePending => true,
        Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Top", -- Top or Engine
    	Limit => 100,
	AutoSubduceOnPartialCompletion => false, -- applies autosubduction to the sagbiGens the first time no new terms are added
    	PrintLevel => 0,
	Recompute => false,
	RenewOptions => false
    }
)

-- SAGBIBasis constructor
sagbiBasis Subring := opts -> S -> (
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
        VariableBaseName => "p",
        Variables => numberVariables,
        Degrees => degrees source vars liftedRing,
        MonomialOrder => newMonomialOrder];
    rings := new HashTable from {
        "quotientRing" => ambient S,
        "liftedRing" => liftedRing,
        "tensorRing" => (coefficientRing liftedRing) tensorVariables
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
    inclusionLifted := map(rings#"tensorRing",rings#"liftedRing",(vars rings#"tensorRing")_{0..numberVariables-1});
    substitution := map(rings#"tensorRing",rings#"tensorRing",(vars rings#"tensorRing")_{0..numberVariables-1});
    projectionLifted := map(rings#"liftedRing",rings#"tensorRing",vars rings#"liftedRing");
    sagbiInclusion := map(rings#"tensorRing",rings#"tensorRing",matrix {toList (numberVariables:0_(rings#"tensorRing"))});
    maps := new HashTable from {
        "inclusionLifted" => inclusionLifted,
        "projectionLifted" => projectionLifted,
        "sagbiInclusion" => sagbiInclusion,
        "substitution" => substitution,
        "fullSubstitution" => projectionLifted * substitution, 
        "quotient" => map(rings#"quotientRing",liftedRing,vars rings#"quotientRing")
    };
    
    -- Ideals:
    -- > I = the ideal that induces quotientRing
    -- > SIdeal = the ideal of S-polynomials (in the tensorRing)
    -- > leadTermsI = initial ideal of I
    -- > reductionIdeal = SIdeal + lift(leadTermsI)    
    SIdeal := ideal(0_(rings#"tensorRing"));
    leadTermsI := ideal leadTerm ideal ambient S;
    ideals := new HashTable from {
        "I" => ideal ambient S,
        "SIdeal" => SIdeal,
        "leadTermsI" => leadTermsI,
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
        "degree" => -1,
        "limit" => -1,
	"autoSubductedSagbiGenerators" => false, 
	"subring" => S
    };

    -- Pending: initially an empty hashtable. It is filled with
    -- keys of degrees pointing to lists of potential sagbi generators.
    pending := new HashTable from {};
    
    -- Options: see above description of the options for sagbi computations
    options := new HashTable from opts;
    new SAGBIBasis from {
        "rings" => rings,
        "maps" => maps,
        "ideals" => ideals,
        "data" => data,
        "pending" => pending,
        "options" => options
    }
)

net SAGBIBasis := S -> (
    local description;
    if S#"data"#"sagbiDone" then (
    	description = "SAGBIBasis Computation Object with "
    ) else (
    	description = "Partial SAGBIBasis Computation Object with "
    );
    description | toString(numcols S#"data"#"sagbiGenerators") | " generators, Limit = " | toString(S#"data"#"limit") | "."
)

sagbiBasis HashTable := opts -> H -> (
    rings := new HashTable from H#"rings";
    maps := new HashTable from H#"maps";
    ideals := new HashTable from H#"ideals";
    data := new HashTable from H#"data";
    pending := new HashTable from apply(keys H#"pending",i-> i => new List from (H#"pending"#i));
    options := new HashTable from H#"options";
    new SAGBIBasis from {
        "rings" => rings,
        "maps" => maps,
        "ideals" => ideals,
        "data" => data,
        "pending" => pending,
        "options" => options
    }
)

-- gens(SAGBIBasis) returns the current list of sagbi generators.
-- This might not generate the entire algebra if the computation is
-- not complete.  This matches the behavior of gb.
-- Note that the sagbiGenerators lie in the liftedRing, so we
-- apply the quotient map for the user.

gens SAGBIBasis := opts -> S -> (
    local M;
    if numColumns S#"data"#"sagbiGenerators" == 0 then (
    	M = S#"maps"#"quotient" matrix(S#"rings"#"liftedRing",{{}});
    	) else (
    	M = S#"maps"#"quotient" S#"data"#"sagbiGenerators";
	);
    M.cache#"Subring" = S#"data"#"subring";
    M 
)

-- Returns the lifted ring
ring SAGBIBasis := A -> (
    A#"rings"#"liftedRing"
)

-- Returns the quotient ring
ambient SAGBIBasis := A -> (
    A#"rings"#"quotientRing"
)

-- Returns the subring that a SAGBIBasis points to
subring SAGBIBasis := S -> (
    S#"data"#"subring"
)


-- internalIsSAGBI is a version of isSAGBI for a SAGBIBasis a object SB
-- it returns a modified SAGBIBasis object with the SB#"data"#"sagbiDone" flag correctly set
-- it is used as an intermediate step for isSAGBI when it is passed something
--   that does not have a cached SAGBIBasis object

internalIsSAGBI = method(
    TypicalValue => SAGBIBasis,
    Options => {
	Compute => true,
	Strategy => "Master",
        SubductionMethod => "Top",
	Limit => 100,
	PrintLevel => 0,
    	Recompute => false,
	RenewOptions => false
	}
    );

internalIsSAGBI(SAGBIBasis) := opts -> SB -> (
    compTable := initializeCompTable(SB, opts);    
    -- Get the SPairs
    sagbiGB := gb(compTable#"ideals"#"reductionIdeal");
    k := rawMonoidNumberOfBlocks(raw monoid (compTable#"rings"#"tensorRing")) - 2;
    zeroGens := selectInSubring(k, gens sagbiGB);
    SPairs := compTable#"maps"#"fullSubstitution"(zeroGens) % compTable#"ideals"#"I";
    -- Reduce the SPairs
    reducedSPairs := compSubduction(compTable, SPairs);
    -- check the sagbi gens are high enough degree (i.e. higher than the subring generators)
    highEnoughDegree := max flatten (degrees compTable#"data"#"subalgebraGenerators")_1 <= max flatten (degrees compTable#"data"#"sagbiGenerators")_1;
    -- if all the reduced SPairs are zero then we have a sagbiBasis
    compTable#"data"#"sagbiDone" = zero(reducedSPairs) and highEnoughDegree;
    -- if the computation gives a sagbi basis then the don't recompute in the future
    if compTable#"data"#"sagbiDone" then (
	compTable#"options"#Recompute = false;
	) else (
	compTable#"options"#Recompute = true; 
	);
    sagbiBasis compTable
    );


-- isSAGBI determines / checks if the generators of an objects form a sagbi basis
-- isSAGBI SAGBIBasis checks the flag of the SAGBIBasis
-- isSAGBI Subring checks the generators of the subring
-- if an object has a cached SAGBIBasis, then it checks the flag of that cached object: 
--   e.g. Matrix that comes from gens SAGBIBasis
-- if an object that does not have a cached SAGBIBasis object is passed then 
--   if Compute == true then we perform 1-step of the sagbi algorithm to check
--      whether all the S-polys subduct to 0
--   otherwise we output null

isSAGBI = method(
    TypicalValue => SAGBIBasis,
    Options => {
	Compute => true,
	Strategy => "Master",
	SubductionMethod => "Top",
	Limit => 0, 
	PrintLevel => 0,
	Recompute => false,
	RenewOptions => false
	}
    );

isSAGBI SAGBIBasis := opts -> SB -> (
    if SB#"data"#"sagbiDone" or (not opts.Compute) then (
        SB#"data"#"sagbiDone"
	) else (
	SB' := internalVerifySagbi(SB, opts);
	if SB'#"data"#"sagbiDone" then ( -- only update the Subring of the SAGBIBasis if we have a sagbi basis
	    S := SB#"data"#"subring";
	    S.cache#"SAGBIBasis" = SB';
	    );
	SB'#"data"#"sagbiDone"
	)
    )

isSAGBI Subring := opts -> S -> (
    local SB;
    local compTable;
    if S.cache#?"SAGBIBasis" then (
	SB = S.cache#"SAGBIBasis";
	if isSubset(first entries gens SB , first entries gens S) then (
	    isSAGBI(opts, SB)
	    ) else (
	    false
	    ) 
	) else (
	if opts.Compute then (
	    -- construct a SAGBIBasis for S and verify whether it is a sagbi basis
	    compTable = initializeCompTable(sagbiBasis S, opts);
	    -- add the generators to the sagbiGenerators
	    compTable#"data"#"sagbiGenerators" = gens S;
	    updateComputation(compTable);
	    SB = sagbiBasis compTable;
	    SB = internalIsSAGBI(opts, SB);
    	    S.cache#"SAGBIBasis" = SB;
    	    SB#"data"#"sagbiDone"
	    ) else ( 
	    null -- S has no SAGBIBasis cached and Compute is set to false
	    )
	)
    )    

isSAGBI Matrix := opts -> M -> (
    local S;
    local SB;
    if (M.cache#?"Subring") and (M.cache#"Subring".cache#?"SAGBIBasis") then (
	S = M.cache#"Subring";
	SB = S.cache#"SAGBIBasis";
	if isSubset(first entries gens SB, first entries M) then (
	    isSAGBI(opts, SB)
	    ) else (
	    false
	    ) 
	) else (
	S = subring M;
	M.cache#"Subring" = S;
	isSAGBI(opts, S)
	)
    )

isSAGBI List := opts -> L -> (
    local S;
    S = subring L;
    isSAGBI(opts, S)
    )


-- groebnerMembershipTest(f, S) = (f lies in S)
-- f an element of the ambient ring of S
-- S a subring of a polynomial ring (or quotient ring)
--
groebnerMembershipTest = method() 
groebnerMembershipTest(RingElement, Subring) := (f, S) -> (
    G := gens S;
    Q := ring G;
    R := ambient Q;
    J := ideal Q;
    fR := lift(f, R);
    GR := lift(G, R);
    tensorRingNumVars := (numgens R) + (numcols G);
    tensorRing := QQ[Variables => tensorRingNumVars, MonomialOrder => {Eliminate(numgens R)}];    
    liftToTensorRing := map(tensorRing, R, (vars tensorRing)_{0 .. numgens R - 1});
    fInTensorRing := liftToTensorRing fR;
    GInTensorRing := liftToTensorRing GR;
    JInTensorRing := liftToTensorRing J;
    I := ideal((vars tensorRing)_{numgens R .. tensorRingNumVars - 1} - GInTensorRing);
    fNormalForm := fInTensorRing % (I + JInTensorRing);
    numcols selectInSubring(1, matrix {{fNormalForm}}) == 1
    )

-- groebnerSubductionQuotient(f, S) = h
-- Computes the subduction quotient of f with respect to the generators of S
-- setup: gens S = {g_1 .. g_s} in Q = K[x_1 .. x_n]/I and f in Q 
-- output: h in K[y1 .. ys] such that (f - f%S) = h(g_1 .. g_s)
groebnerSubductionQuotient = method() 
groebnerSubductionQuotient(RingElement, Subring) := (f, S) -> (
    local outputRing;
    G := gens S;
    Q := ring G;
    R := ambient Q;
    J := ideal Q;
    FF := coefficientRing R;
    fR := lift(f, R);
    GR := lift(G, R);
    tensorRingNumVars := (numgens R) + (numcols G);
    oldOrder := (options R).MonomialOrder;
    newOrder := prepend(Eliminate(numgens R), oldOrder);
    tensorRing := FF[Variables => tensorRingNumVars, MonomialOrder => oldOrder];
    liftToTensorRing := map(tensorRing, R, (vars tensorRing)_{0 .. numgens R - 1});
    fInTensorRing := liftToTensorRing fR;
    GInTensorRing := liftToTensorRing GR;
    JInTensorRing := liftToTensorRing J;
    I := ideal((vars tensorRing)_{numgens R .. tensorRingNumVars - 1} - GInTensorRing);
    fNormalForm := fInTensorRing % (I + JInTensorRing);
    -- output fNormalForm in the subductionQuotientRing
    --  construct the ring if not present in the cache of S 
    --  also store the natural map from the subductionQuotientRing to Q 
    if not S.cache#?"subductionQuotientRing" then (
	outputRing = FF[Variables => numcols G];
	S.cache#"subductionQuotientRing" = outputRing;
	S.cache#"subductionQuotientRingMap" = map(Q, outputRing, G);
	);
    outputRing = S.cache#"subductionQuotientRing";
    outputMap := map(outputRing, tensorRing, matrix {toList((numgens R):0)} | vars outputRing);
    outputMap fNormalForm
    )


-- RingElement // Subring 
-- returns the subduction quotient
RingElement // Subring := (f, S) -> (
    groebnerSubductionQuotient(f, S)    
    )


-- Matrix or RingElement % Subring
-- M % S, f % S
-- Returns the smallest r in the ambient ring of S
-- such that M or f = r + s for some s in S
-- Two modes of operation: 
-- 1) If Subring has a sagbi basis (stored in its cache) then use subduction
-- 2) If there is not a complete sagbi basis then use the 'extrinsic method' - see groebnerMembershipTest above
-- Note: we construct the tensor ring with a monomial order lifted from the ambient ring

Matrix % SAGBIBasis := (M, SB) -> (
    assert(ambient SB === ring M);
    subduction(SB, M)
    );

RingElement % SAGBIBasis := (f, SB) -> (
    assert(ambient SB === ring f);
    first first entries subduction(SB, matrix{{f}})
    );

Matrix % Subring := (M, S) -> (
    local result;
    assert(ring M === ambient S); 
    if (S#cache#?"SAGBIBasis") and (S#cache#"SAGBIBasis"#"data"#"sagbiDone") then (
	-- S has a complete sagbi basis so use internal subduction
	SB := S#cache#"SAGBIBasis";
	result = subduction(SB, M);	
	) else (
	-- extrinsic subduction
	G := gens S;
	Q := ring G;
    	R := ambient Q;
    	J := ideal Q;
    	FF := coefficientRing R;
    	MR := lift(M, R);
    	GR := lift(G, R);
    	tensorRingNumVars := (numgens R) + (numcols G);
    	oldOrder := (options R).MonomialOrder;
    	newOrder := prepend(Eliminate(numgens R), oldOrder);
    	tensorRing := FF[Variables => tensorRingNumVars, MonomialOrder => oldOrder];
    	liftToTensorRing := map(tensorRing, R, (vars tensorRing)_{0 .. numgens R - 1});
    	MInTensorRing := liftToTensorRing MR;
    	GInTensorRing := liftToTensorRing GR;
    	JInTensorRing := liftToTensorRing J;
    	I := ideal((vars tensorRing)_{numgens R .. tensorRingNumVars - 1} - GInTensorRing);
    	MNormalForm := MInTensorRing % (I + JInTensorRing);	
	projectToQ := map(Q, tensorRing, matrix {toList(numgens Q : 0_Q)} | G);
        result = M - (projectToQ MNormalForm);
	);    
    result
    );

RingElement % Subring := (f, S) -> (
    first first entries (matrix{{f}} % S)
    );

-- subringIntersection(Subring, Subring)
-- intersects the subrings using a method analogous to the GB method

subringIntersection = method(
    Options => {
	Strategy => "Master",
	SubductionMethod => "Top",
	"SAGBILimitType" => "Fixed", -- "Fixed" or "Function"
	Limit => 20,
	PrintLevel => 0
	}
    )

subringIntersection(Subring, Subring) := opts -> (S1, S2) -> (
    local limit;
    local t;
    Q1 := ambient S1;
    Q2 := ambient S2;
    assert(Q1 === Q2);
    Q := Q1;
    I := ideal(Q);
    R := ambient(Q);
    
    -- construct the large ring
    -- monomial order:
    -- 1) monomial order from R (on p_1 ... p_n+1)
    -- 2) eliminate p_0
    
    newMonomialOrder := prepend(GRevLex => 1, (monoid Q).Options.MonomialOrder); -- product order with p_0 in a different subring
    M := monoid [
	Variables => (numgens Q + 1), 
	MonomialOrder => newMonomialOrder
	];
    TAmb := (coefficientRing Q) M;
    t = (vars TAmb)_(0,0);
    RtoTAmb := map(TAmb, R, (vars TAmb)_{1 .. numgens Q});
    J := (RtoTAmb I) + ideal(t^2 - t); -- I + (t^2 - t)
    T := TAmb / J; -- tensor product of Q and K[t]/(t^2-t)
    QtoT := map(T, Q, (vars T)_{1 .. numgens Q});
    TtoQ := map(Q, T, matrix{{0_Q}} | vars Q);
    t = (vars T)_(0,0);
    ---------
    G1 := QtoT gens S1;
    G2 := QtoT gens S2;
    use T;
    G := t*G1 | (1-t)*G2;
    S := subring G;
    if opts#"SAGBILimitType" == "Fixed" then (
	limit = opts.Limit;
	) else if opts#"SAGBILimitType" == "Function" then (
	limit = (max (degrees gens S1)_1)*(max (degrees gens S2)_1);
	);
    SB := sagbi(S, 
	Strategy => opts.Strategy,
	SubductionMethod => opts.SubductionMethod,
	Limit => limit,
	PrintLevel => opts.PrintLevel
	);
    -- if SB is a sagbi basis the intersection computation is correct!
    --  in this case, the intersectionGens form a sagbi basis for the intersection 
    intersectionGens := selectInSubring(1, gens SB);
    subring TtoQ intersectionGens
    );

end--
