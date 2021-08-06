-- Subring data type
-- A subring is meant to be fairly light-weight.
-- Subrings include the ambient ring, their generators
-- a boolean indicating whether the generators are SAGBI and
-- a PresRing (see later)

Subring = new Type of HashTable

-- Make options => true
subring = method(Options => {VarBaseName => "p"})
subring Matrix := opts -> M -> (
    new Subring from{
        "ambientRing" => ring M,
        "generators" => M,
        "presentation" => makePresRing(opts, ring M, M),
        "isSAGBI" => false,
        cache => new CacheTable from {}
    }
)
subring List := opts -> L -> subring(opts, matrix{L})

-- Subring access functions

ambient Subring := A -> A#"ambientRing"
gens Subring := o -> A -> A#"generators"
numgens Subring := A -> (numcols gens A)
net Subring := A -> "subring of " | toString(ambient A)

-- SAGBIBasis data type
-- This is a computation object which can hold the intermediate
-- results of a sagbi basis computation.
-- This is similar to the output of a gb calculation

SAGBIBasis = new Type of HashTable

net SAGBIBasis := S -> (
    numGens := numcols S#"sagbiGenerators";
    sagbiLimit := S#"stoppingData"#"limit";
    intro := null;
    if S#"sagbiDone" then (
    	intro = "SAGBIBasis Computation Object with ";
    ) else (
    	intro = "Partial SAGBIBasis Computation Object with ";
    );
    intro | toString(numGens) | " generators, Limit = " | toString(sagbiLimit) | "."
)

sagbiBasis = method(Options => {storePending => true, VarBaseName => "p"})
sagbiBasis Subring := opts -> S -> (
    stopping := new HashTable from {"limit" => -1, "degree" => -1};
    pending := new HashTable;
    new SAGBIBasis from {
        "ambientRing" => ambient S,
        "subringGenerators" => gens S,
        "sagbiGenerators" => matrix(ambient S,{{}}),
        "sagbiDegrees" => matrix(ZZ,{{}}),
        "sagbiDone" => false,
        "stoppingData" => stopping,
        "pending" => pending,
        "presentation" => null
    }
)

sagbiBasis MutableHashTable :=  opts -> H -> (
    stopping := new HashTable from {"limit" => H#"stoppingData"#"limit", "degree" => H#"stoppingData"#"degree"};
    pending := if opts.storePending then new HashTable from H#"pending" else new HashTable;
    new SAGBIBasis from {
        "ambientRing" => H#"ambientRing",
        "subringGenerators" => H#"subringGenerators",
        "sagbiGenerators" => H#"sagbiGenerators",
        "sagbiDegrees" => H#"sagbiDegrees",
        "sagbiDone" => H#"sagbiDone",
        "stoppingData" => stopping,
        "pending" => pending,
        "presentation" => makePresRing(VarBaseName => opts.VarBaseName, H#"ambientRing", H#"sagbiGenerators")
    }
)

gens SAGBIBasis := o -> S -> (
    if #flatten entries S#"sagbiGenerators" == 0 then S#"subringGenerators"
    else if S#"sagbiDone" then (S#"sagbiGenerators")
    else (
    	  reducedGenerators := compress subduction(S#"sagbiGenerators",S#"subringGenerators");
	  S#"sagbiGenerators" | reducedGenerators
    )
)

subring SAGBIBasis := opts -> S -> (
    G := gens S;
    if S#"sagbiDone" then ( 
    	new Subring from{
            "ambientRing" => ring S#"sagbiGenerators",
            "generators" => G,
            "presentation" => makePresRing(opts, ring S#"sagbiGenerators", S#"sagbiGenerators"),
            "isSAGBI" => true,
            cache => new CacheTable from {}}
    	) else (
    	subring(opts, G)
    	)
    )

sagbiDone = method(Options => {})
sagbiDone SAGBIBasis := opts -> S -> S#"sagbiDone"

-- This type is compatible with internal maps that are generated in the Sagbi algorithm.
-- Originally, this was stored directly in the cache of an instance of Subring.
-- The problem with that solution is there is a need to use these maps outside of the Sagbi algorithm computations.
-- Also, the cache should not be used in a way that causes side effects.
PresRing = new Type of HashTable

net PresRing := pres -> (
    tense := pres#"tensorRing";
    A := numcols vars tense;
    B := numcols selectInSubring(1, vars tense);
    "PresRing instance ("|toString(B)|" generators in "|toString(A-B)|" variables)"
)

-- gensR are elements of R generating some subalgebra.
-- R is a polynomial ring.
makePresRing = method(TypicalValue => PresRing, Options => {VarBaseName => "p"})
makePresRing(Ring, Matrix) := opts -> (R, gensR) -> (
    if(R =!= ring(gensR)) then(
    error "The generators of the subalgebra must be in the ring R.";
    );
    makePresRing(opts, R, first entries gensR)
)

makePresRing(Ring, List) := opts -> (R, gensR) ->(
    gensR = sort gensR;

    if #gensR == 0 then(
        error "List must not be empty.";
    );

    if(ring(matrix({gensR})) =!= R) then(
        error "The generators of the subalgebra must be in the ring R.";
    );

    ambR := R;
    nBaseGens := numgens ambR;
    nSubalgGens := length gensR;

    -- Create a ring with combined generators of base and subalgebra.
    monoidAmbient := monoid ambR;
    coeffField := coefficientRing ambR;

    -- Construct the monoid of a ring with variables corresponding to generators of the ambient ring and the subalgebra.
    -- Has an elimination order that eliminates the generators of the ambient ring.
    -- The degrees of generators are set so that the SyzygyIdeal is homogeneous.
    newOrder := prepend(Eliminate nBaseGens, monoidAmbient.Options.MonomialOrder);

    newVariables := monoid[
    VariableBaseName=> opts.VarBaseName,
    Variables=>nBaseGens+nSubalgGens,
    Degrees=>join(degrees source vars ambR, degrees source matrix({gensR})),
    MonomialOrder => newOrder];

    tensorRing := coeffField newVariables;

    sagbiInclusion := map(tensorRing, tensorRing,
    (matrix {toList(nBaseGens:0_(tensorRing))}) |
    (vars tensorRing)_{nBaseGens .. nBaseGens+nSubalgGens-1});

    projectionAmbient := map(ambR, tensorRing,
    (vars ambR) | matrix {toList(nSubalgGens:0_(ambR))});

    inclusionAmbient := map(tensorRing, ambR,
    (vars tensorRing)_{0..nBaseGens-1});

    substitution := map(tensorRing, tensorRing,
    (vars tensorRing)_{0..nBaseGens-1} | inclusionAmbient(matrix({gensR})));

    genVars := (vars tensorRing)_{numgens ambient R..numgens tensorRing-1};

    syzygyIdeal := ideal(genVars - inclusionAmbient(leadTerm matrix({gensR})));
    

    liftedPres := ideal(substitution(genVars) - genVars);
    fullSubstitution := projectionAmbient*substitution;

    ht := new HashTable from {
    "tensorRing" => tensorRing,
    "sagbiInclusion" => sagbiInclusion,
    "projectionAmbient" => projectionAmbient,
    "inclusionAmbient" => inclusionAmbient,
    "substitution" => substitution,
    "fullSubstitution" => fullSubstitution,
    "syzygyIdeal" => syzygyIdeal,
    "liftedPres" => liftedPres
    };

    new PresRing from ht
);

-- The reason why this is implemented is to prevent incorrect usage of the makePresRing constructor.
-- A subring is already associated with an immutable PresRing instance which should be used instead of
-- constructing a new instance. Don't use makePresRing when you can use the function subring.
makePresRing(Subring) := opts -> subR -> (
    subR#"presentation"
);

-- f % Subring is never going to be an element of the subalgebra, hence the output
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

-- Returns the tensor ring because the function ambient returns the ambient ring.
ring Subring := A -> (
    A#"presentation"#"tensorRing"
);

end-- 

