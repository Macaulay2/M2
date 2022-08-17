-- Perhaps use options => true instead of specifying the options?

-- Defining a new subring data type
Subring = new Type of HashTable

-- Constructors for the subring data type
-- Subrings are very light-weight and contain
-- only the defining ring, the generators, and
-- a flag for if the subring is known to be a
-- SAGBI basis.
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
-- gens must take an option since gens is defined
-- to be a method function with options
ambient Subring := A -> A#"ambientRing"
gens Subring := opts -> A -> A#"generators"
numgens Subring := A -> (numcols gens A)
net Subring := A -> "subring of " | toString(ambient A) | " with " | numgens A | " generators"


-- Defining a new computation object for SAGBI bases
SAGBIBasis = new Type of HashTable

-- Constructor for a sagbiBasis
-- Options:
-- -- VariableBaseName [no longer used] is a choice for which variable is used
-- to name variables in the constructed rings
-- -- Strategy is the strategy used in the algorithm.
-- It can be either DegreeByDegree or Incremental
-- -- StorePending is a boolean that determines
-- whether the pending list is stored in the computation
-- object.  The pending list may be quite large in some
-- cases.
-- the sagbiBasis object stores all the options used for the computation

sagbiBasis = method(
    TypicalValue => Subring,
    Options => {
    	-- VariableBaseName => "p"
    	AutoSubduce => true,
        ReduceNewGenerators => true, -- applys gaussian elimination to sagbiGens before adding them
	StorePending => true,
        -- FullSubduct => true,
        -- DegreeLimitedSubduction => false,
        Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Top", -- top or engine
    	Limit => 10, -- change back to 100
	AutoSubduceOnPartialCompletion => false, -- applies autosubduction to the sagbiGens the first time no new terms are added
    	PrintLevel => 0,
	Recompute => false,
	RenewOptions => false
    }
)

-- Basic constructor for a sagbiBasis object.
-- TODO: Documentation
sagbiBasis Subring := opts -> S -> (
    -- rings stores the various rings we need in our constructions
    -- maps stores the maps between our rings
    -- ideals stores the ideals for the sagbi computations
    -- data stores any SAGBI-related data from the computaiton
    -- pending stores the pending list from the computation
    -- (if StorePending is true)
    -- strategy stores the desired strategy.  DegreeByDegree is 0
    -- and Incremental is 1.

    -- The rings are the quotientRing, liftedRing, tensorRing.
    -- The quotientRing is the ring of the generators of S
    -- The liftedRing is the ring of the quotientRing
    -- The tensorRing is the ring consisting of the variables
    -- of ambientRing and a polynomial for each SAGBI generator.
    -- Initially, the tensorRing is the same as the ambientRing,
    -- but using a different variable name.
    
    
    -- todo: revert the changes so that the sagbiGenerators do not include the generators by default
    --       the generators will be included in the pending list for the compTable and will be processed 
    --       at their degree (keeps them non-redundant and reduced)
    --       

    liftedRing := ambient ambient S;
    
    numberVariables := numgens liftedRing;
    numberGenerators := numColumns gens S;
    newMonomialOrder := (monoid liftedRing).Options.MonomialOrder;
    
    tensorVariables := monoid[
        VariableBaseName => "p", -- opts.VariableBaseName,
        Variables => numberVariables,
        Degrees => degrees source vars liftedRing,
        MonomialOrder => newMonomialOrder];

    rings := new HashTable from {
        "quotientRing" => ambient S,
        "liftedRing" => liftedRing,
        "tensorRing" => (coefficientRing liftedRing) tensorVariables
        };

    -- The maps are ...
    
    -- inclusionLifted: LiftedRing := K[z_1 .. z_s] -> TensorRing := K[x_1 .. x_s, y_1 .. y_t] : z_i -> x_i 
    --
    -- substitution: TensorRing -> TensorRing: x_i -> x_i, y_j -> g_j(x_1 .. x_s)
    -- where g_1 .. g_t are the sagbiGenerators of this sagbiBasis
    -- NB initially t = 0 i.e. TensorRing is initially a copy of LiftedRing
    --
    -- projectionLifted: TensorRing -> LiftedRing: x_i -> z_i, y_j -> 0
    --
    -- sagbiInclusion: TensorRing -> TensorRing: x_i -> 0, y_j -> y_j
    -- NB initially this is just the zero map
    --
    -- fullSubstitution: TensorRing -> LiftedRing: x_i -> z_i, y_j -> g_j(z_1 .. z_s)
    --
    -- quotientRing: LiftedRing -> QuotientRing
    

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
    
    -- The ideals are the ideal that induces quotientRing
    -- The ideal of S-polynomials, which is initially empty
    -- The ideal SIdeal (formally InAIdeal) ...
    -- The ideal tensorRingI (using forceGB to give a GroebnerBasis)
    -- Any other ideals?
    
    
    SIdeal := ideal(0_(rings#"tensorRing"));
    leadTermsI := ideal leadTerm ideal ambient S;

    ideals := new HashTable from {
        "I" => ideal ambient S,
        "SIdeal" => SIdeal,
        "leadTermsI" => leadTermsI,
        "reductionIdeal" => maps#"inclusionLifted" leadTermsI + SIdeal,
    };

    -- The data is sagbiGenerators, sagbiDegrees,
    -- sagbiDone, degree, limit
    --
    -- sagbiGenerators is a matrix of all the sagbi elements
    -- that we have found so far in the liftedRing.
    -- It is initially empty.
    --
    -- sagbiDegrees are the degrees of the sagbiGenerators,
    -- it is initially empty.
    --
    -- sagbiDone is set to be true if the sagbiGenerators form
    -- a finite sagbi basis.  It is initially false.
    -- The degree is the current degree under consideration by
    -- the algorithm.  It is initially -1 to indicate that no
    -- computation has occured.
    -- The limit is the maximum value of the degree.
    -- It is initally set to infinity to indicate that no limit
    -- is set.

    data := new HashTable from {
        "subalgebraGenerators" => S#"generators",
        "sagbiGenerators" => matrix(rings#"liftedRing",{{}}),
        "sagbiDegrees" => matrix(ZZ,{{}}),
        "sagbiDone" => false,
        "degree" => -1,
        "limit" => -1,
	"autoSubductedSagbiGenerators" => false, -- have the sagbiGenerators been autoSubducted (see sagbi option: AutoSubductOnPartialCompletion)
	"subring" => S -- points back to the original subring
    };

    -- Pending is an empty hashtable, later it is filled with
    -- keys of degrees pointing to lists of potential sagbi generators.

    pending := new HashTable from {};

    options := new HashTable from opts; -- {"variableName" => opts.VariableBaseName};

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

-- gens returns the current list of generators.
-- This might not generate the entire algebra if the computation is
-- not complete.  This matches the behavior of gb
-- I'm not sure in which ring these entries should be.
-- Perhaps we need the quotient map and this should be applied here.

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

-- Create a subring from a computation object
subring SAGBIBasis := S -> (
    new Subring from{
        "ambientRing" => ambient S,
        "generators" => gens S,
        cache => new CacheTable from {"SAGBIBasis" => S}
    }
)


-- internalIsSAGBI is a version of isSAGBI for SAGBIBasis a object SB
-- it returns a SAGBIBasis object with the SB#"data"#"sagbiDone" flag correctly set
-- it is used as an intermediate step for isSAGBI when it is passed something
--   that does not have a cached SAGBIBasis object
-- 

internalIsSAGBI = method(
    TypicalValue => SAGBIBasis,
    Options => {
	Compute => true,
	Strategy => "Master", -- Master (default), DegreeByDegree, Incremental
        SubductionMethod => "Top", -- top or engine
	Limit => 100,
	PrintLevel => 0, -- see print level for sagbi
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
    
    -- if all the reduced SPairs are zero then we have a sagbiBasis
    compTable#"data"#"sagbiDone" = zero(reducedSPairs);
    
    -- if the computation gives a sagbi basis then the don't recompute in the future
    if compTable#"data"#"sagbiDone" then (
	compTable#"options"#Recompute = false;
	) else (
	compTable#"options"#Recompute = true; -- opts.Recompute; -- set the recompute option for resuming computation purposes
	);
    
    sagbiBasis compTable
    );



-- isSAGBI checks whether the SAGBI generators of an objects associated SAGBIBasis object form a sagbi basis
-- isSAGBI Subring checks the generators of the subring
-- if an object that does not have a cached SAGBIBasis object is passed then 
--   if Compute then we perform 1-step of the sagbi algorithm to check
--      whether all the S-polys subduct to 0
--
-- Default options:
--  Recompute => true 
--  Limit => 0
--    If a subring or SAGBIBasis object is created then its options will be set 
--    so that it will recompute because the generators are not autosubducted in 
--    isSAGBI 

isSAGBI = method(
    TypicalValue => SAGBIBasis,
    Options => {
	Compute => true,
	Strategy => "Master",
	SubductionMethod => "Top",
	Limit => 0, 
	PrintLevel => 0,
	Recompute => false, -- when running sagbi on a subring / SAGBIBasis object constructed
	RenewOptions => false
	}
    );

isSAGBI SAGBIBasis := opts -> SB -> (
    if SB#"data"#"sagbiDone" or (not opts.Compute) then (
        SB#"data"#"sagbiDone"
	) else (
	SB' := internalVerifySagbi(SB, opts);
	S := SB#"data"#"subring";
	S.cache#"SAGBIBasis" = SB';
	SB'#"data"#"sagbiDone"
	)
    )

isSAGBI Subring := opts -> S -> (
    local SB;
    local compTable;
    if S.cache#?"SAGBIBasis" then (
	SB = S.cache#"SAGBIBasis";
	if isSubset(first entries gens SB , first entries gens S) then (
	    isSAGBI S.cache#"SAGBIBasis"
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
    	    isSAGBI SB
	    ) else ( 
	    null -- S has no SAGBIBasis cached and Compute is set to false
	    )
	)
    )    

isSAGBI Matrix := opts -> M -> (
    local S;
    local SB;
    -*
    if M.cache#?"Subring" then (
	isSAGBI(M.cache#"Subring", opts) 
	) else (
	S = subring M;
	M.cache#"Subring" = S;
	isSAGBI S
	)
    *-
    if (M.cache#?"Subring") and (M.cache#"Subring".cache#?"SAGBIBasis") then (
	S = M.cache#"Subring";
	SB = S.cache#"SAGBIBasis";
	if isSubset(first entries gens SB, first entries M) then (
	    isSAGBI SB
	    ) else (
	    false
	    ) 
	) else (
	S = subring M;
	M.cache#"Subring" = S;
	isSAGBI S
	)
    )

isSAGBI List := opts -> L -> (
    local S;
    S = subring L;
    isSAGBI S
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
    -- assert instance(Amb, PolynomialRing);
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



-- Matrix or RingElement % Subring
-- M % S
-- f % S
-- Returns the smallest r in the ambient ring of S
-- such that M or f = r + s for some s in S
--
-- There are two options: 
-- 1) If Subring has a sagbi basis (stored in its cache) then use subduction
-- 2) If no sagbi basis then use the 'extrinsic method' - see groebnerMembershipTest above
--
-- Note: we construct the tensor ring with a monomial order lifted from the ambient ring
-- Note: the extrinsic method is only available for subrings of polynomial rings

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
	-- NB this is only for subrings of polynomial rings (not quotient rings)
    	G := gens S;
    	R := ring G;
    	assert instance(R, PolynomialRing);
    	tensorRingNumVars := (numgens R) + (numcols G);
	newMonomialOrder := append((monoid R).Options.MonomialOrder, Eliminate(numgens R));
    	tensorRing := QQ[Variables => tensorRingNumVars, MonomialOrder => newMonomialOrder];
    	liftToTensorRing := map(tensorRing, R, (vars tensorRing)_{0 .. numgens R - 1});
    	projectToR := map(R, tensorRing, matrix {toList(numgens R : 0_R)} | G);
	MInTensorRing := liftToTensorRing M;
    	GInTensorRing := liftToTensorRing G;
    	I := ideal((vars tensorRing)_{numgens R .. tensorRingNumVars - 1} - GInTensorRing);
    	MNormalForm := MInTensorRing % I;
    	result = M - projectToR(MNormalForm);
	);
    
    result
    );

RingElement % Subring := (f, S) -> (
    first first entries (matrix{{f}} % S)
    );

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
    --  in this case, we can use forceSAGBI on the intersectionGens or
    --  on their image under TtoQ or on the subring
    --
    intersectionGens := selectInSubring(1, gens SB);
    subring TtoQ intersectionGens
    );


end--

-- f % Subring is never going to be an element of the subalgebra, hence the ouput
-- is in the lower variables of tensorRing.
-- input: f in ambient A or tensorRing of A.
-- output: r in tensorRing of A such that f = a + r w/ a in A, r "minimal"
RingElement % Subring := (f, A) -> (
    pres := A#"presentation";
    if ring f === ambient A then(
	f = (pres#"inclusionAmbient")(f);
	) else if ring f =!= pres#"tensorRing" then(
	error "The RingElement f must be in either tensorRing or ambient A.";
	);
    ans := (internalSubduction(pres, f));
    ans
    );

-- f // Subring is always going to be inside of the subalgebra, hence the output
-- should be in the upper variables of tensorRing.
-- NOTE: If you want to compute fullSubstitution(f//A), it is a lot faster to compute f-(f%A).
-- input: f in ambient A or tensorRing of A.
-- output: a in tensorRing of A such that f = a + r w/ a in A, r "minimal."
RingElement // Subring := (f, A) -> (
    pres := A#"presentation";
    tense := pres#"tensorRing";
    if ring f === ambient A then(
	f = (pres#"inclusionAmbient")(f);
	) else if ring f =!= tense then(
	error "The RingElement f must be in either the tensorRing or ambient ring of A.";
	);
    result := f - (f % A);
    I := pres#"liftedPres";
    result % I
    );

-- Sends each entry e to e%A
Matrix % Subring := (M, A) -> (
    pres := A#"presentation";
    ents := for i from 0 to numrows M - 1 list(
	for j from 0 to numcols M - 1 list(M_(i,j) % A)
	);
    matrix(pres#"tensorRing", ents)
    );

-- Sends each entry e to e//A
Matrix // Subring := (M, A) -> (
    pres := A#"presentation";
    ents := for i from 0 to numrows M - 1 list(
	for j from 0 to numcols M - 1 list(M_(i,j) // A)
	);
    matrix(pres#"tensorRing", ents)
    );

end-- 

