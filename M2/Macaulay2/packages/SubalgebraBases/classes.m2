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
    new Subring from{
        "ambientRing" => ring M,
        "generators" => M,
        cache => new CacheTable from {}
    }
)
subring List := L -> subring(matrix{L})

-- Subring access functions
-- gens must take an option since gens is defined
-- to be a method function with options
ambient Subring := A -> A#"ambientRing"
gens Subring := opts -> A -> A#"generators"
numgens Subring := A -> (numcols gens A)
net Subring := A -> "subring of " | toString(ambient A) | " with " | numgens A | " generators"

isSAGBI = method()
isSAGBI Subring := S -> (
    if S.cache#?"SAGBIBasis" then isSAGBI S.cache#"SAGBIBasis" else null
)

-- Defining a new computation object for SAGBI bases
SAGBIBasis = new Type of HashTable

-- Constructor for a sagbiBasis
-- Options:
-- -- VariableBaseName is a choice for which variable is used
-- to name variables in the constructed rings
-- -- Strategy is the strategy used in the algorithm.
-- It can be either DegreeByDegree or Incremental
-- -- StorePending is a boolean that determines
-- whether the pending list is stored in the computation
-- object.  The pending list may be quite large in some
-- cases.
sagbiBasis = method(Options => {
    VariableBaseName => "p"
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
        VariableBaseName => opts.VariableBaseName,
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
	"autoSubductedSagbiGenerators" => false -- have the sagbiGenerators been autoSubducted (see sagbi option: AutoSubductOnPartialCompletion)
    };

    -- Pending is an empty hashtable, later it is filled with
    -- keys of degrees pointing to lists of potential sagbi generators.

    pending := new HashTable from {};

    options := new HashTable from {"variableName" => opts.VariableBaseName};

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
    if numColumns S#"data"#"sagbiGenerators" == 0 then matrix(S#"rings"#"liftedRing",{{}})
    else S#"maps"#"quotient" S#"data"#"sagbiGenerators" 
)

isSAGBI SAGBIBasis := S -> S#"data"#"sagbiDone"

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

-- groebnerMembershipTest(f, S) = (f lies in S)
-- f an element of the ambient ring of S
-- S a subring of a polynomial ring

groebnerMembershipTest = method() 
groebnerMembershipTest(RingElement, Subring) := (f, S) -> (
    G := gens S;
    Amb := ring G;
    assert instance(Amb, PolynomialRing);
    tensorRingNumVars := (numgens Amb) + (numcols G);
    tensorRing := QQ[Variables => tensorRingNumVars, MonomialOrder => {Eliminate(numgens Amb)}];
    
    liftToTensorRing := map(tensorRing, Amb, (vars tensorRing)_{0 .. numgens Amb - 1});
    fInTensorRing := liftToTensorRing f;
    GInTensorRing := liftToTensorRing G;
    I := ideal((vars tensorRing)_{numgens Amb .. tensorRingNumVars - 1} - GInTensorRing);
    fNormalForm := fInTensorRing % I;
    
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

