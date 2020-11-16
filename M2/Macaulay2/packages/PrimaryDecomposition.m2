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
-- TODO : 1.
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

cacheHit := () -> if debugLevel > 0 then printerr "MinimalPrimes: cache hit! 🎉";

-- TODO: is there a better name for these two?
isSupportedRing := I -> (
    A := ring first flattenRing I;
    -- ring should be a commutative polynomial ring or a quotient of one
    isPolynomialRing A and isCommutative A
    -- base field should be QQ or ZZ/p
    and (QQ === (kk := coefficientRing A) or instance(kk, QuotientRing) -*or instance(kk, GaloisField)*-))

flattenRingMap := I -> (
    -- R is the ring of I, and A is a polynomial ring over a prime field
    R := ring I;
    (J, F) := flattenRing I; -- the ring map is not needed
    A := ring J;
    -- the map back to R, TODO: why not use F?
    fback := if A === R then identity else map(R, A, generators(R, CoefficientRing => coefficientRing A));
    (J, fback))

-- TODO: move exported methods out of the files below
load "./PrimaryDecomposition/radical.m2"
load "./PrimaryDecomposition/GTZ.m2"
load "./PrimaryDecomposition/Shimoyama-Yokoyama.m2"
load "./PrimaryDecomposition/Eisenbud-Huneke-Vasconcelos.m2"

--------------------------------------------------------------------
-- isPrimary
--------------------------------------------------------------------

isPrimary = method()
isPrimary(Ideal)          :=  Q     -> isPrimary(Q, radical Q)
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

primaryComponent = method( Options => { Strategy => 2, Increment =>1 })
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
    try instance(container.Result, List) and opts.CodimensionLimit <= container.CodimensionLimit else false)

cacheComputation = method(TypicalValue => CacheFunction, Options => true)
cacheComputation AssociatedPrimesComputation := CacheFunction => options associatedPrimes >> opts -> container -> new CacheFunction from (
    -- this function takes advantage of FunctionClosures by modifying the container
    computation -> (
	if isComputationDone(opts, container) then ( cacheHit(); container.Result ) else
	if (result := computation(opts, container)) =!= null then (
	    container.CodimensionLimit = opts.CodimensionLimit;
	    container.Result = result)))

associatedPrimes Ring   := List => opts -> R -> associatedPrimes(comodule ideal R, opts)
associatedPrimes Ideal  := List => opts -> I -> assassinsHelper(I, (associatedPrimes, Ideal), opts)
-- TODO:
--associatedPrimes Module := List => opts -> M -> assassinsHelper(M, (associatedPrimes, Module), opts)
associatedPrimes Module := List => opts -> M -> algorithms#(associatedPrimes, Module)#1(opts, M) -- assassinsHelper(M, (associatedPrimes, Module), opts)

-- Helper for associatedPrimes
assassinsHelper = (A, key, opts) -> (
    strategy := opts.Strategy;
    doTrim := if opts.MinimalGenerators then trim else identity;

    S := ring presentation ring A;  -- S is the ambient polynomial ring which ring A is a quotient of

    codimLimit := min(opts.CodimensionLimit, dim S);
    doLimit := L -> select(L, P -> codim(P, Generic => true) <= codimLimit);
    opts = opts ++ { CodimensionLimit => codimLimit };

    -- this logic determines what strategies will be used
    computation := (opts, container) -> runHooks(key, (opts, A), Strategy => opts.Strategy);

    -- this is the logic for caching partial associated primes computations. A.cache contains an option:
    --   AssociatedPrimesOptions{} => AssociatedPrimesComputation{ CodimensionLimit, Result }
    -- currently there are no options that could go in AssociatedPrimesOptions, but this pattern is useful for saturate, etc.
    cacheKey := AssociatedPrimesOptions{};
    container := try A.cache#cacheKey else A.cache#cacheKey = (
	new AssociatedPrimesComputation from { CodimensionLimit => 0, Result => null });

    -- the actual computation of associated primes occurs here
    L := (cacheComputation(opts, container)) computation;

    if L =!= null then doLimit \\ doTrim \ L else if strategy === null
    then error("no applicable method for ", toString key)
    else error("assumptions for associatedPrimes strategy ", toString strategy, " are not met"))

--------------------------------------------------------------------

algorithms#(associatedPrimes, Ideal) = new MutableHashTable from {
    1 => (opts, I) -> ass1(I, opts), -- see Eisenbud-Huneke-Vasconcelos.m2

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
scan({1, Monomial}, strategy ->
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
    1 => (opts, M) -> (
	-- modified code in ass1 for modules
	if not M.cache#?"AssociatedPrimes" then M.cache#"AssociatedPrimes" = {};
	k := if opts.CodimensionLimit < 0 then infinity else opts.CodimensionLimit;
	p := if M.cache#?"associatedPrimesCodimLimit" then M.cache#"associatedPrimesCodimLimit" else -2;
	if p >= opts.CodimensionLimit then return select(M.cache#"AssociatedPrimes", P -> codim P <= k);
	rel := presentation ring M;
	S := ring rel; -- S is the ambient polynomial ring which ring M is a quotient of
	M1 := if ring M === S then M else (
	    liftRel := id_(lift(target relations M, S)) ** rel;
	    trim subquotient(lift(gens M, S), lift(relations M, S) | liftRel)
	    );
	mapback := I -> trim((map(ring M, S, vars ring M)) I);
	(A, c, d, C) := (trim ann M1, codim M1, dim S, null);
	for i from max(1+p, c) to min(d, k) do (
	    if debugLevel > 0 then print("Extracting associated primes of codim " | toString i);
	    newPrimes := if i == d and ((isHomogeneous M and (c == d or pdim M1 == d)) or (c == d and all(gens S, v -> radicalContainment(v, A)))) then {ideal gens S} else (
		if i > c then (
		    if C === null then C = res(M1, LengthLimit => k+1);
		    if length C < i then ( k = infinity; break; );
		    A = trim ann minPres(ker transpose C.dd_(i+1) / image transpose C.dd_i);
		    ); -- computes ann Ext^i(M1, S)
		if codim A > i then {} else minimalPrimes(A, CodimensionLimit => i)
		);
	    M.cache#"AssociatedPrimes" = M.cache#"AssociatedPrimes" | newPrimes/mapback;
	    M.cache#"associatedPrimesCodimLimit" = i;
	    );
	if k >= dim S then M.cache#"associatedPrimesCodimLimit" = infinity;
	M.cache#"AssociatedPrimes"),
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
	MinimalGenerators => true -- whether to trim the output
	}
    )
primaryDecomposition Ideal  := List => opts -> I -> primedecompHelper(I, (primaryDecomposition, Ideal), opts)
primaryDecomposition Ring   := List => opts -> R -> primaryDecomposition(comodule ideal R, opts)
-- Returns a primary decomposition of 0 in M.
-- Assumes all embedded primes appear after all primes they contain, i.e. isSubset(AP#i, AP#j) => i \le j
-- (equivalently, the ordering of associated primes is a linear extension of the partial order by inclusion).
-- This is the case for associatedPrimes(Module), which returns associated primes ordered by codimension
primaryDecomposition Module := List => opts -> M -> (
     if not M.cache#?"primaryComponents" then M.cache#"primaryComponents" = new MutableHashTable;
     AP := associatedPrimes(M, CodimensionLimit => infinity);
     if #values(M.cache#"primaryComponents") != #AP then (
	  H := hashTable apply(AP, p -> p => select(#AP, i -> isSubset(AP#i, p)));
	  for i to #AP - 1 do (
	       if debugLevel > 0 then printerr("Prime: " | toString(i+1) | "/" | toString(#AP));
	       p := AP#i;
	       if M.cache#"primaryComponents"#?p then continue;
	       f := product(AP - set AP_(H#p), q -> q_(position(q_*, g -> g % p != 0)));
	       isolComp := if f == 1 then 0*M else saturate(0*M, f);
	       M.cache#"primaryComponents"#p = if #(H#p) > 1 then (
		    B := intersect apply(H#p - set{i}, k -> M.cache#"primaryComponents"#(AP#k));
		    getEmbeddedComponent(M, p, C -> isSubset(intersect(B, C), isolComp), Strategy => opts.Strategy)
	       ) else isolComp;
	  );
     );
     apply(AP, p -> M.cache#"primaryComponents"#p)
)

-- keys: none so far
PrimaryDecompositionOptions = new SelfInitializingType of BasicList
PrimaryDecompositionOptions.synonym = "primary decomposition options"

-- keys: CodimensionLimit and Result
PrimaryDecompositionComputation = new Type of MutableHashTable
PrimaryDecompositionComputation.synonym = "primary decomposition computation"

isComputationDone PrimaryDecompositionComputation := Boolean => options primaryDecomposition >> opts -> container -> (
    -- this function determines whether we can use the cached result, or further computation is necessary
    try instance(container.Result, List) else false)

cacheComputation PrimaryDecompositionComputation := CacheFunction => options primaryDecomposition >> opts -> container -> new CacheFunction from (
    -- this function takes advantage of FunctionClosures by modifying the container
    computation -> (
	if isComputationDone(opts, container) then ( cacheHit(); container.Result ) else
	if (result := computation(opts, container)) =!= null then ( container.Result = result )))

-- Helper for primaryDecomposition
primedecompHelper = (I, key, opts) -> (
    -- TODO: move this to individual strategies, so strategies for e.g. Weyl algebras can be added later
    if not isSupportedRing I
    then error "primaryDecomposition: expected a commutative polynomial ring or a quotient of one";

    strategy := opts.Strategy;
    doTrim := if opts.MinimalGenerators then trim else identity;

    -- this logic determines what strategies will be used
    computation := (opts, container) -> (
	(I', fback) := flattenRingMap I;
	C := if not instance(opts.Strategy, Hybrid)
	then runHooks(key, (opts, I'), Strategy => opts.Strategy)
	-- hybrid strategies can still be used:
	-- if #opts.Strategy =!= 2 then error "primaryDeccomposition: the Hybrid strategy requires 2 arguments";
	else HprimaryDecomposition(I', -- defined in EHV
	    opts.Strategy#0, -- associated primes strategy
	    opts.Strategy#1  -- localize strategy
	    );
	I.cache#"AssociatedPrimes" = doTrim \ fback \ associatedPrimes I'; -- FIXME: check-PrimaryDecomposition-9 fails
	if C =!= null then fback \ C);

    -- this is the logic for caching partial primary decomposition computations. I.cache contains an option:
    --   PrimaryDecompositionOptions{} => PrimaryDecompositionComputation{ CodimensionLimit, Result }
    cacheKey := PrimaryDecompositionOptions{};
    container := try I.cache#cacheKey else I.cache#cacheKey = (
	new PrimaryDecompositionComputation from { Result => null });

    -- the actual computation of primary decomposition occurs here
    L := (cacheComputation(opts, container)) computation;

    if L =!= null then doTrim \ L else if strategy === null
    then error("no applicable method for ", toString key)
    else error("assumptions for primaryDecomposition strategy ", toString strategy, " are not met"))

--------------------------------------------------------------------
--- primedecomp strategies
--------------------------------------------------------------------

algorithms#(primaryDecomposition, Ideal) = new MutableHashTable from {

    -- TODO: what order should these go in?
    -- TODO: add heuristics to rejecting them for speed
    EisenbudHunekeVasconcelos => (opts, I) -> EHVprimaryDecomposition I,
    ShimoyamaYokoyama => (opts, I) -> SYprimaryDecomposition I,

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
	C := apply(ass0 I, ind -> H#ind);
	I.cache#"AssociatedPrimes" = apply(C, I -> ideal radical I);
	cast \ C),

    Binomial => (opts, I) -> error "primaryDecomposition: Binomial strategy not implemented yet",
    }

-- Installing hooks for (primaryDecomposition, Ideal)
scan({EisenbudHunekeVasconcelos, ShimoyamaYokoyama, Monomial}, strategy ->
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
