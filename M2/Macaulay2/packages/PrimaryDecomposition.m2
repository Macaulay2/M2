---------------------------------------------------------------------------
-- PURPOSE : Computation of primary decomposition, associated primes, isPrimary
--
-- UPDATE HISTORY : created pre-2000 EHV algorithm for associated primes &
--                                    SY algorithm for primary decomposition
--                  updated Feb 2005
--                  updated Nov 2006
--                  updated Mar 2007 GTZ algorithm expanded
--                  updated Jan 2009 support for towers of rings and newGTZ
--                  updated May 2015 more testing added
--                  updated Nov 2020
--
-- TODO :
--  1. combine the computation objects for associated primes and primary decomposition
---------------------------------------------------------------------------
newPackage(
    "PrimaryDecomposition",
    Version => "2.0",
    Date => "July 4, 2020",
    Headline => "primary decomposition and associated primes routines",
    Authors => {
	{Name => "Mike Stillman",  Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike"},
	{Name => "Carolyn Yackel", Email => "cyackel@math.indiana.edu"},
	{Name => "Justin Chen",    Email => "justin.chen@math.gatech.edu"},
	{Name => "Mahrud Sayrafi", Email => "mahrud@umn.edu",        HomePage => "https://math.umn.edu/~mahrud"}},
    Keywords => {"Commutative Algebra"},
    PackageExports => { "Colon" },
    AuxiliaryFiles => true,
    DebuggingMode => true
    )

export {
    -- methods
    "primaryDecomposition",
    "irreducibleDecomposition",
    "isPrimary",
    -- keys for strategies
    "EisenbudHunekeVasconcelos",					    -- cryptic
    "Hybrid",
    "Increment",
    "GTZ",
    "ShimoyamaYokoyama",
--     "binomialCD",
--     "extract",
--     "findNonMember",
--     "flattener",
    "localize",
--     "minSat",
    "primaryComponent",
--     "quotMin",
    "kernelOfLocalization",
    "regSeqInIdeal",
    "radicalContainment"
    }

importFrom_Core { "printerr", "raw", "rawIndices", "rawGBContains", "rawRemoveScalarMultiples" }

-- private symbols used as keys:

protect H, protect U, protect W

--     EHVprimaryDecomposition,			    -- cryptic
--     HprimaryDecomposition,
--     Hybrid,
--     primdecComputation,
--     minSatPPD,
--     sortByDegree

algorithms = new MutableHashTable from {}

--------------------------------------------------------------------
-- Support routines
--------------------------------------------------------------------

cacheHit := type -> if debugLevel > 0 then printerr("Cache hit on a ", synonym type, "! 🎉");

-- TODO: is there a better name for these two?
isSupportedRing = method()
isSupportedRing Module := M -> isSupportedRing ring presentation ring M
isSupportedRing Ideal  := I -> isSupportedRing ring first flattenRing I
isSupportedRing Ring   := A -> (
    -- ring should be a commutative polynomial ring or a quotient of one
    isPolynomialRing A and isCommutative A
    -- base field should be QQ or ZZ/p
    and (QQ === (kk := coefficientRing A) or instance(kk, QuotientRing) -*or instance(kk, GaloisField)*-))

flattenRingMap = method()
flattenRingMap Ideal := I -> (
    -- R is the ring of I, and A is a polynomial ring over a prime field
    R := ring I;
    (J, F) := flattenRing I; -- the ring map is not needed
    A := ring J;
    -- the map back to R, TODO: why not use F?
    fback := if A === R then identity else map(R, A, generators(R, CoefficientRing => coefficientRing A));
    (J, fback))
flattenRingMap Module := M -> (
    -- R is the ring of M, and S is the ambient polynomial ring which R is a quotient of
    R := ring M;
    S := ring (rel := presentation R);
    (M', fback) := if R === S then (M, identity) else (
	liftRel := id_(lift(target relations M, S)) ** rel;
	M' = trim subquotient(lift(gens M, S), lift(relations M, S) | liftRel);
	fback = map(R, S, vars R); -- TODO: not generators(R, CoefficientRing => coefficientRing S)?
	(M', fback)))

-- TODO: make this unnecessary
storeAssociatedPrimesComputation = (M, L, c) -> (
    M.cache#(AssociatedPrimesOptions{}) = new AssociatedPrimesComputation from {
	CodimensionLimit => c, Result => new MutableList from L});

-- TODO: move exported methods out of the files below
load "./PrimaryDecomposition/radical.m2"
load "./PrimaryDecomposition/GTZ.m2"
load "./PrimaryDecomposition/Shimoyama-Yokoyama.m2"
load "./PrimaryDecomposition/Eisenbud-Huneke-Vasconcelos.m2"

--------------------------------------------------------------------
-- isPrimary
--------------------------------------------------------------------

isPrimary = method()
isPrimary Ideal           :=  Q     -> isPrimary(Q, radical Q)
-- TODO: the documentation says primality of P isn't checked
isPrimary(Ideal,  Ideal)  := (Q, P) -> ( if isPrime P then Q == top Q else false )
isPrimary(Module, Module) := (M, Q) -> #associatedPrimes(M / Q) == 1

--------------------------------------------------------------------
-- localize
--------------------------------------------------------------------

localizefcn := strat -> (
    if debugLevel > 0 then printerr("using localization Strategy " | toString strat);
    if strat === 0 then EHVlocalize     else
    if strat === 1 then SYlocalize ass1 else
    if strat === 2 then SYlocalize ass2
    else error("unrecognized Strategy for localization " | toString strat));

localize = method(Options => { Strategy => 1 })
localize(Ideal, Ideal) := Ideal => opts -> (I, P) -> (localizefcn opts.Strategy) (I, P)

--------------------------------------------------------------------
-- Primary Component
--------------------------------------------------------------------

primaryComponent = method( Options => { Strategy => 2, Increment => 1 })
primaryComponent(Ideal, Ideal) := Ideal => opts -> (I, P) -> (primarycomponent (localizefcn opts.Strategy)) (I, P, opts.Increment)

--------------------------------------------------------------------
-- Associated Primes
--------------------------------------------------------------------

-- keys: none so far
AssociatedPrimesOptions = new SelfInitializingType of BasicList
AssociatedPrimesOptions.synonym = "associated primes options"

-- keys: CodimensionLimit and Result
AssociatedPrimesComputation = new Type of MutableHashTable
AssociatedPrimesComputation.synonym = "associated primes computation"

isComputationDone = method(TypicalValue => Boolean, Options => true)
isComputationDone AssociatedPrimesComputation := Boolean => options associatedPrimes >> opts -> container -> (
    -- this function determines whether we can use the cached result, or further computation is necessary
    try instance(container.Result, BasicList) and opts.CodimensionLimit <= container.CodimensionLimit else false)

cacheComputation = method(TypicalValue => CacheFunction, Options => true)
cacheComputation AssociatedPrimesComputation := CacheFunction => options associatedPrimes >> opts -> container -> new CacheFunction from (
    -- this function takes advantage of FunctionClosures by modifying the container
    computation -> (
	if isComputationDone(opts, container) then ( cacheHit class container; container.Result ) else
	if (result := computation(opts, container)) =!= null then (
	    container.CodimensionLimit = opts.CodimensionLimit;
	    container.Result = result)))

associatedPrimes Ring   := List => opts -> R -> associatedPrimes(comodule ideal R, opts)
associatedPrimes Ideal  := List => opts -> I -> assassinsHelper(I, (associatedPrimes, Ideal), opts)
associatedPrimes Module := List => opts -> M -> assassinsHelper(M, (associatedPrimes, Module), opts)

-- Helper for associatedPrimes
assassinsHelper = (A, key, opts) -> (
    strategy := opts.Strategy;
    doTrim := if opts.MinimalGenerators then trim else identity;

    -- TODO: is this correct?
    S := ring presentation ring A; -- S is the ambient polynomial ring which ring A is a quotient of

    codimLimit := min(opts.CodimensionLimit, dim S);
    doLimit := L -> select(L, P -> codim(P, Generic => true) <= codimLimit);
    opts = opts ++ { CodimensionLimit => codimLimit };

    -- this logic determines what strategies will be used
    computation := (opts, container) -> runHooks(key, (opts ++ { Strategy => null }, A), Strategy => opts.Strategy);

    -- this is the logic for caching partial associated primes computations. A.cache contains an option:
    --   AssociatedPrimesOptions{} => AssociatedPrimesComputation{ CodimensionLimit, Result }
    cacheKey := AssociatedPrimesOptions{};
    container := try A.cache#cacheKey else A.cache#cacheKey = (
	new AssociatedPrimesComputation from { CodimensionLimit => 0, Result => new MutableList from {} });

    -- TODO: is this a good idea for letting the algorithms accumulate information?
    opts = opts ++ { cache => new CacheTable from { cacheKey => container } };

    -- the actual computation of associated primes occurs here
    -- TODO: is it a good idea for computation to return a MutableList?
    L := toList (cacheComputation(opts, container)) computation;

    if L =!= null then doLimit \\ doTrim \ L else if strategy === null
    then error("no applicable method for ", toString key)
    else error("assumptions for associatedPrimes strategy ", toString strategy, " are not met"))

--------------------------------------------------------------------

algorithms#(associatedPrimes, Ideal) = new MutableHashTable from {
    1 => (opts, I) -> ass1(I, opts), -- see Eisenbud-Huneke-Vasconcelos.m2

    2 => (opts, I) -> associatedPrimes(comodule I, opts),

    3 => (opts, I) -> (
	-- take the radical of a cached primary decomposition
	-- TODO: is there a way to do this for modules too?
	cacheKey := PrimaryDecompositionOptions{};
	M := comodule I;
	C := if M.cache#?cacheKey then M.cache#cacheKey
	-- TODO: should it always be cached under comodule?
	else if I.cache#?cacheKey then I.cache#cacheKey
	else return null;
	if not isComputationDone C
	then return null;
	cacheHit class C;
	radical \ C.Result),

    Monomial => (opts, I) -> (
	R := ring I;
	if not isMonomialIdeal I
	or not isPolynomialRing R
	or not isCommutative R
	then return null;
	cast := if instance(I, MonomialIdeal) then monomialIdeal else ideal;
	I = monomialIdeal I;
	cast \ apply(inds := ass0 I,
	    ind -> ( g := (gens ring I)_ind; if g === {} then g = 0_(ring I) else g ))),
    }

-- Installing hooks for (associatedPrimes, Ideal)
scan({1, 2, Monomial, 3}, strategy ->
    addHook(key := (associatedPrimes, Ideal), algorithms#key#strategy, Strategy => strategy))

ass0 = I -> (
    R := ring I;
    J := dual I;
    M := first entries generators J;
    H := new MutableHashTable;
    scan(M, m -> (
	    s := rawIndices raw m;
	    if not H#?s then H#s = true));
    inds := sort apply(keys H, ind -> (#ind, ind));
    apply(inds, s -> s#1))

--------------------------------------------------------------------

algorithms#(associatedPrimes, Module) = new MutableHashTable from {
    1 => (opts, M0) -> (
	-- modified code in ass1 for modules
	-- returns a MutableList
	(M, fback) := flattenRingMap M0;
	S := ring M;
	A := trim ann M;
	c := codim M;
	d := dim S;
	C := null; -- will be a resolution of M
	k := opts.CodimensionLimit;
	container := opts.cache#(AssociatedPrimesOptions{});
	p := container.CodimensionLimit;
	L := container.Result;
	for i from max(p + 1, c) to min(d, k) do (
	    if debugLevel > 0 then printerr("Extracting associated primes of codim " | toString i);
	    newPrimes :=
	    if i == d and (
		-- TODO: make these conditions into named functions?
		(isHomogeneous M and (c == d or pdim M == d))
		or (c == d and all(gens S, v -> radicalContainment(v, A))))
	    then {ideal gens S}
	    else (
		if c < i then (
		    -- computes ann Ext^i(M, S)
		    if C === null then C = res(M, LengthLimit => k + 1);
		    if length C < i then break;
		    A = trim ann minPres(ker transpose C.dd_(i+1) / image transpose C.dd_i));
		if codim A <= i then minimalPrimes(A, CodimensionLimit => i) else {});
	    -- cache the results
	    scan(newPrimes, P -> L#(#L) = fback P);
	    container.CodimensionLimit = i);
	container.Result),
    }

-- Installing hooks for (associatedPrimes, Module)
scan({1}, strategy ->
    addHook(key := (associatedPrimes, Module), algorithms#key#strategy, Strategy => strategy))

--------------------------------------------------------------------
-- Primary Decomposition
--------------------------------------------------------------------

Hybrid = new SelfInitializingType of BasicList

primaryDecomposition = method(
    TypicalValue => List,
    Options => {
	Strategy          => null,
	MinimalGenerators => true, -- whether to trim the output
	cache             => null
	}
    )
primaryDecomposition Ring   := List => opts -> R -> primaryDecomposition(comodule ideal R, opts)
primaryDecomposition Ideal  := List => opts -> I -> primedecompHelper(I, (primaryDecomposition, Ideal), opts)
primaryDecomposition Module := List => opts -> M -> primedecompHelper(M, (primaryDecomposition, Module), opts)

-- keys: none so far
PrimaryDecompositionOptions = new SelfInitializingType of BasicList
PrimaryDecompositionOptions.synonym = "primary decomposition options"

-- keys: CodimensionLimit and Result
PrimaryDecompositionComputation = new Type of MutableHashTable
PrimaryDecompositionComputation.synonym = "primary decomposition computation"

isComputationDone PrimaryDecompositionComputation := Boolean => options primaryDecomposition >> opts -> container -> (
    -- this function determines whether we can use the cached result, or further computation is necessary
    -- TODO: don't accept BasicList once (primaryDecomposition, Ideal) strategies also return mutable hash tables
    try instance(container.Result, MutableHashTable) or instance(container.Result, BasicList) else false)

cacheComputation PrimaryDecompositionComputation := CacheFunction => options primaryDecomposition >> opts -> container -> new CacheFunction from (
    -- this function takes advantage of FunctionClosures by modifying the container
    computation -> (
	if isComputationDone(opts, container) then ( cacheHit class container; container.Result ) else
	if (result := computation(opts, container)) =!= null then ( container.Result = result )))

-- Helper for primaryDecomposition
primedecompHelper = (A, key, opts) -> (
    strategy := opts.Strategy;
    doTrim := if opts.MinimalGenerators then trim else identity;

    -- this logic determines what strategies will be used
    computation := (opts, container) -> (
	if not instance(opts.Strategy, Hybrid)
	then runHooks(key, (opts, A), Strategy => opts.Strategy)
	-- hybrid strategies can still be used:
	-- if #opts.Strategy =!= 2 then error "primaryDeccomposition: the Hybrid strategy requires 2 arguments";
	else HprimaryDecomposition(A, -- defined in EHV
	    opts.Strategy#0, -- associated primes strategy
	    opts.Strategy#1  -- localize strategy
	    ));

    -- this is the logic for caching partial primary decomposition computations. A.cache contains an option:
    --   PrimaryDecompositionOptions{} => PrimaryDecompositionComputation{ CodimensionLimit, Result }
    cacheKey := PrimaryDecompositionOptions{};
    -- TODO: is caching primary decomposition of an ideal in its comodule correct?
    M := if instance(A, Ideal) then comodule A else A;
    container := try M.cache#cacheKey else M.cache#cacheKey = (
	new PrimaryDecompositionComputation from { Result => null });

    -- TODO: is this a good idea for letting the algorithms accumulate information?
    opts = opts ++ { cache => new CacheTable from { cacheKey => container } };

    -- if associated primes are cached, pass the result as an option
    cacheKeyAP  := AssociatedPrimesOptions{};
    containerAP := if M.cache#?cacheKeyAP then opts.cache#cacheKeyAP = M.cache#cacheKeyAP;

    -- the actual computation of primary decomposition occurs here
    L := (cacheComputation(opts, container)) computation;
    -- TODO: make the (primaryDecomposition, Ideal) code to also return a hash table
    -- TODO: does the order matter? should it be sorted like apply(AP, p -> container.Result#p)?
    if instance(L, MutableHashTable) then L = values L;

    if L =!= null then doTrim \ L else if strategy === null
    then error("no applicable method for ", toString key)
    else error("assumptions for primaryDecomposition strategy ", toString strategy, " are not met"))

--------------------------------------------------------------------
--- primedecomp strategies
--------------------------------------------------------------------

algorithms#(primaryDecomposition, Module) = new MutableHashTable from {
    -- TODO: add assumptions
    1 => (opts, M) -> (
	-- Returns a primary decomposition of 0 in M.
	-- Assumes all embedded primes appear after all primes they contain, i.e. isSubset(AP#i, AP#j) => i \le j
	-- (equivalently, the ordering of associated primes is a linear extension of the partial order by inclusion).
	-- This is the case for associatedPrimes(Module), which returns associated primes ordered by codimension
	S := ring M;
	AP := try (
	    -- the associated primes computation object
	    -- the Result key is a mutable list of associated primes
	    containerAP := opts.cache#(AssociatedPrimesOptions{});
	    -- TODO: confirm that dim S is correct bound
	    if isComputationDone(containerAP, CodimensionLimit => dim S)
	    then ( cacheHit class containerAP; toList containerAP.Result ) else null) else null;
	if AP === null then AP = associatedPrimes(M, CodimensionLimit => dim S);
	-- the primary decomposition computation object
	-- the Result key is a mutable hash table of primary components
	-- each corresponding to an associated prime
	container := opts.cache#(PrimaryDecompositionOptions{});
	if container.Result === null then container.Result = new MutableHashTable from {};
	-- check whether all components are found
	if #values(container.Result) != #AP then (
	    H := hashTable apply(AP, p -> p => select(#AP, i -> isSubset(AP#i, p)));
	    for i to #AP - 1 do (
		if debugLevel > 0 then printerr("Prime: " | toString(i+1) | "/" | toString(#AP));
		p := AP#i;
		if container.Result#?p then continue;
		f := product(AP - set AP_(H#p), q -> q_(position(q_*, g -> g % p != 0)));
		isolComp := if f == 1 then 0*M else saturate(0*M, f);
		container.Result#p = if #(H#p) > 1 then (
		    B := intersect apply(H#p - set{i}, k -> container.Result#(AP#k));
		    getEmbeddedComponent(M, p, C -> isSubset(intersect(B, C), isolComp), Strategy => opts.Strategy)
		    ) else isolComp);
	    );
	container.Result),
    }

-- Installing hooks for (primaryDecomposition, Module)
scan({1}, strategy ->
    addHook(key := (primaryDecomposition, Module), algorithms#key#strategy, Strategy => strategy))

--------------------------------------------------------------------

-- TODO: make these algorithms also return a MutableHashTable { prime => primary component }
algorithms#(primaryDecomposition, Ideal) = new MutableHashTable from {
    1 => (opts, I) -> ideal \ relations \ primaryDecomposition(comodule I, opts),

    -- TODO: what order should these go in?
    -- TODO: add heuristics to rejecting them for speed
    -- TODO: move the functions here instead?
    EisenbudHunekeVasconcelos => (opts, I) -> (
	if not isSupportedRing I
	then return null;
	(I', fback) := flattenRingMap I;
	fback \ EHVprimaryDecomposition I'),

    ShimoyamaYokoyama => (opts, I) -> (
	if not isSupportedRing I
	then return null;
	(I', fback) := flattenRingMap I;
	fback \ SYprimaryDecomposition I'),

    Monomial => (opts, I) -> (
	R := ring I;
	if not isMonomialIdeal I
	or not isPolynomialRing R
	or not isCommutative R
	then return null;
	cast := if instance(I, MonomialIdeal) then monomialIdeal else ideal;
	I = monomialIdeal I;
	if (J := dual I) == 1 then return {monomialIdeal 0_R};
	M := first entries generators J;
	aI := first exponents lcm I;
	--
	H := new MutableHashTable;
	scan(M, m -> (
		s := first keys standardForm leadMonomial m;
		Q := monomialIdeal apply(keys s, v -> R_v^(aI#v + 1 - s#v));
		ind := sort keys s;
		H#ind = if H#?ind then intersect(H#ind, Q) else Q));
	cast \ apply(ass0 I, ind -> H#ind)),

    Binomial => (opts, I) -> error "primaryDecomposition: Binomial strategy not implemented yet",
    }

-- Installing hooks for (primaryDecomposition, Ideal)
scan({1, EisenbudHunekeVasconcelos, ShimoyamaYokoyama, Monomial}, strategy ->
    addHook(key := (primaryDecomposition, Ideal), algorithms#key#strategy, Strategy => strategy))

--------------------------------------------------------------------
----- Irreducible Decomposition
--------------------------------------------------------------------

irreducibleDecomposition = method();
irreducibleDecomposition MonomialIdeal := List => I -> (
    -- probably written by Greg Smith
    R := ring I;
    aI := first exponents lcm I;
    M := first entries generators dual I;
    apply(M, m -> (
	    s := first keys standardForm leadMonomial m;
	    if #s === 0 then return monomialIdeal 0_R;
	    monomialIdeal apply(keys s, v -> R_v^(aI#v + 1 - s#v))))
    )

--------------------------------------------------------------------
----- Tests section
--------------------------------------------------------------------

load "./PrimaryDecomposition/tests.m2"

--------------------------------------------------------------------
----- Documentation section
--------------------------------------------------------------------

beginDocumentation()

-- TODO: review
load "./PrimaryDecomposition/doc.m2"

--------------------------------------------------------------------
----- Development section
--------------------------------------------------------------------

end--

restart
debugLevel = 1
debug PrimaryDecomposition
