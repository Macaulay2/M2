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
    PackageExports => { "Saturation", "MinimalPrimes" },
    PackageImports => { "Elimination" },
    AuxiliaryFiles => true,
    DebuggingMode => false
    )

export {
    -- methods
    "isPrimary", "localize", "primaryComponent",
    "primaryDecomposition", "associatedPrimes",
    "irreducibleDecomposition",
    "topComponents",
    -- strategies
    "ShimoyamaYokoyama", "EisenbudHunekeVasconcelos",
    -- option symbols
    "Increment", "GTZ",
    -- defined in Eisenbud-Huneke-Vasconcelos.m2
    --  EHVprimaryDecomposition, HprimaryDecomposition
    "kernelOfLocalization", "regSeqInIdeal",
    -- defined in Shimoyama-Yokoyama.m2
    --  minSat, minSatPPD, quotMin
    --  primdecComputation, flattener
    --  sortByDegree, extract, findNonMember
    -- synonyms:
    "ass" => "associatedPrimes",
    "top" => "topComponents"
    }

-- TODO: this is only declared there for technical reasons
exportFrom_MinimalPrimes { "removeLowestDimension" }

importFrom_Core { "raw", "rawIndices", "rawGBContains", "rawRemoveScalarMultiples" }
importFrom_Core { "isComputationDone", "cacheComputation", "fetchComputation", "updateComputation", "cacheHit", "Context", "Computation" }

algorithms = new MutableHashTable from {}

-- this needs to be loaded before Eisenbud-Huneke-Vasconcelos.m2
associatedPrimes = method(
    TypicalValue => List,
    Options => {
	Strategy          => null,     -- try hooks(associatedPrimes, Ideal)
	CodimensionLimit  => infinity, -- maximum codimension to look for
	MinimalGenerators => true      -- whether to trim the output
	}
    )

--------------------------------------------------------------------
-- Support routines
--------------------------------------------------------------------

-- TODO: is there a better name for these two?
-- TODO: also support GF(q)
-- also see a similar function in MinimalPrimes
isSupportedRing = method()
isSupportedRing Module := M -> isSupportedRing ring presentation first flattenRing ring M
isSupportedRing Ideal  := I -> isSupportedRing ring first flattenRing I
isSupportedRing Ring   := A -> (
    -- ring should be a commutative polynomial ring or a quotient of one
    isPolynomialRing A and isCommutative A
    -- base field should be QQ or ZZ/p
    and (QQ === (kk := coefficientRing A) or instance(kk, QuotientRing) -*or instance(kk, GaloisField)*-))

-- TODO: can this functionality be simplified and put into flattenRing?
-- also see a similar function in MinimalPrimes
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
--    S  = first flattenRing S; -- TODO
    (M', fback) := if R === S then (M, identity) else (
	liftRel := id_(lift(target relations M, S)) ** rel;
	M' = trim subquotient(lift(generators M, S), lift(relations M, S) | liftRel);
	fback = map(R, S, vars R); -- TODO: not generators(R, CoefficientRing => coefficientRing S)?
	(M', fback)))

-- TODO: move exported methods out of the files below
load "./PrimaryDecomposition/GTZ.m2"
load "./PrimaryDecomposition/Shimoyama-Yokoyama.m2"
load "./PrimaryDecomposition/Eisenbud-Huneke-Vasconcelos.m2"

--------------------------------------------------------------------
-- isPrimary
--------------------------------------------------------------------

isPrimary = method(Options => { Strategy => null })
isPrimary Ideal           := opts ->  Q     -> isPrimary(Q, radical Q, opts)
isPrimary(Ideal,  Ideal)  := opts -> (Q, P) -> isPrime(P, Strategy => opts.Strategy) and Q == topComponents Q
isPrimary(Module, Module) := opts -> (M, Q) -> #associatedPrimes(M / Q, Strategy => opts.Strategy) == 1

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
AssociatedPrimesContext = new SelfInitializingType of Context
AssociatedPrimesContext.synonym = "associated primes context"

-- keys: CodimensionLimit and Result
AssociatedPrimesComputation = new Type of Computation
AssociatedPrimesComputation.synonym = "associated primes computation"

new AssociatedPrimesContext from Ideal  :=
new AssociatedPrimesContext from Module := (C, M) -> AssociatedPrimesContext{}

new AssociatedPrimesComputation from Ideal  := (C, I) -> new C from comodule I
new AssociatedPrimesComputation from Module := (C, M) -> new AssociatedPrimesComputation from {
    Result => new MutableList from {},
    CodimensionLimit => -1 }

-- TODO: make this unnecessary
storeAssociatedPrimesComputation = (M, L, c) -> (
    container := fetchComputation(AssociatedPrimesComputation, M, new AssociatedPrimesContext from M);
    updateComputation(container, new MutableList from L, CodimensionLimit => c))

isComputationDone AssociatedPrimesComputation := Boolean => options associatedPrimes >> opts -> container -> (
    -- this function determines whether we can use the cached result, or further computation is necessary
    instance(container.Result, BasicList)
    and opts.CodimensionLimit <= container.CodimensionLimit)

updateComputation(AssociatedPrimesComputation, List) := MutableList => options associatedPrimes >> opts -> (container, result) -> (
    container.CodimensionLimit = opts.CodimensionLimit;
    container.Result = result)

associatedPrimes Ring   := List => opts -> R -> associatedPrimes(comodule ideal R, opts)
associatedPrimes Ideal  := List => opts -> I -> assassinsHelper(I, (associatedPrimes, Ideal), opts)
associatedPrimes Module := List => opts -> M -> assassinsHelper(M, (associatedPrimes, Module), opts)

-- Helper for associatedPrimes
-- A:    ideal or module
-- key:  the key for runHooks
-- opts: options for associatedPrimes
assassinsHelper = (A, key, opts) -> (
    -- TODO: are there any instant checks to do?
    -- TODO: any better way to do this?
    -- S := first flattenRing ring presentation ring A;

    strategy := opts.Strategy;
    doTrim := if opts.MinimalGenerators then trim else identity;

    codimLimit := opts.CodimensionLimit; -- min(opts.CodimensionLimit, dim S);
    doLimit := if codimLimit == infinity then identity else L -> select(L, P -> codim(P, Generic => true) <= codimLimit);
    -- opts = opts ++ { CodimensionLimit => codimLimit };

    -- this logic determines what strategies will be used
    computation := (opts, container) -> runHooks(key,
	(opts ++ { cache => container }, A), Strategy => opts.Strategy);

    -- this is the logic for caching partial associated primes computations. A.cache contains an option:
    --   AssociatedPrimesContext{} => AssociatedPrimesComputation{ CodimensionLimit, Result }
    container := fetchComputation(AssociatedPrimesComputation, A, new AssociatedPrimesContext from A);

    -- the actual computation of associated primes occurs here
    -- TODO: is it a good idea for computation to return a MutableList?
    L := toList (cacheComputation(opts, container)) computation;

    if L =!= null then doLimit \\ doTrim \ L else if strategy === null
    then error("no applicable method for ", toString key)
    else error("assumptions for associatedPrimes strategy ", toString strategy, " are not met"))

--------------------------------------------------------------------

algorithms#(associatedPrimes, Ideal) = new MutableHashTable from {
    -- TODO: can this be simplified?
    1 => (opts, I) -> ass1(I, -- see Eisenbud-Huneke-Vasconcelos.m2
	CodimensionLimit  => opts.CodimensionLimit,
	MinimalGenerators => opts.MinimalGenerators),

    -- TODO: can this be simplified?
    2 => (opts, I) -> associatedPrimes(comodule I,
	CodimensionLimit  => opts.CodimensionLimit,
	MinimalGenerators => opts.MinimalGenerators),

    "cached" => (opts, I) -> (
	-- take the radical of a cached primary decomposition
	-- TODO: is there a way to do this for modules too?
	cacheKey := PrimaryDecompositionContext{};
	M := comodule I;
	C := if M.cache#?cacheKey then M.cache#cacheKey
	-- TODO: should it always be cached under comodule?
	else if I.cache#?cacheKey then I.cache#cacheKey
	else return null;
	if not isComputationDone C
	then return null;
	cacheHit C;
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
scan({1, 2, Monomial, "cached"}, strategy ->
    addHook(key := (associatedPrimes, Ideal), algorithms#key#strategy, Strategy => strategy))

-- used in Monomial strategy of associatedPrimes and primaryDecomposition
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
    Default => (opts, M0) -> (
	-- modified code in ass1 for modules, based on EHV
	-- returns a MutableList
	(M, fback) := flattenRingMap M0;
	S := ring M;
	m := ideal gens S;
	A := trim ann M;
	c := codim M;
	d := dim S;
	C := null; -- will be a resolution of M
	k := opts.CodimensionLimit + d - dim ring M0; -- !!
        -- if debugLevel > 0 then print(opts.CodimensionLimit, k);
	comp := opts.cache;
	p := comp.CodimensionLimit + d - dim ring M0; -- !!
	L := comp.Result;
	for i in max(p + 1, c) .. min(d, k) do (
	    if debugLevel > 0 then printerr("Extracting associated primes of codim " | toString i);
	    newPrimes :=
	    if i == d and (isHomogeneous M or (c == d and all(gens S, v -> radicalContainment(v, A))))
		-- TODO: make these conditions into named functions?
	    then ( if c == d or pdim M == d then {m} else {} )
	    else (
		if c < i then (
		    -- computes ann Ext^i(M, S)
		    if C === null then C = res(M, LengthLimit => k + 1);
		    if length C < i then ( comp.CodimensionLimit = d; break );
		    A = trim ann minPres(ker transpose C.dd_(i+1) / image transpose C.dd_i));
		if codim A <= i then minimalPrimes(A, CodimensionLimit => i) else {});
	    -- cache the results
	    scan(newPrimes, P -> L#(#L) = fback P);
	    comp.CodimensionLimit = i);
	comp.Result),
    }

-- Installing hooks for (associatedPrimes, Module)
scan({Default}, strategy ->
    addHook(key := (associatedPrimes, Module), algorithms#key#strategy, Strategy => strategy))

--------------------------------------------------------------------
-- Primary Decomposition
--------------------------------------------------------------------

-- for (primaryDecomposition, Ideal), strategy can also be a list
--   Hybrid{associated prime strategy, localize strategy}
-- for (primaryDecomposition, Module), strategy can also be a list
--   Hybrid{ one of "Hom", "Res", or "Sat" (default) }
primaryDecomposition = method(
    TypicalValue => List,
    Options => {
	Strategy          => null, -- try hooks(primaryDecomposition, Ideal)
	MinimalGenerators => true  -- whether to trim the output
	}
    )
primaryDecomposition Ring   := List => opts -> R -> primaryDecomposition(comodule ideal R, opts)
primaryDecomposition Ideal  := List => opts -> I -> primarydecompHelper(I, (primaryDecomposition, Ideal), opts)
primaryDecomposition Module := List => opts -> M -> primarydecompHelper(M, (primaryDecomposition, Module), opts)

-- keys: none so far
PrimaryDecompositionContext = new SelfInitializingType of Context
PrimaryDecompositionContext.synonym = "primary decomposition context"

-- keys: CodimensionLimit and Result
PrimaryDecompositionComputation = new Type of Computation
PrimaryDecompositionComputation.synonym = "primary decomposition computation"

new PrimaryDecompositionContext from Ideal  :=
new PrimaryDecompositionContext from Module := (C, M) -> PrimaryDecompositionContext{}

new PrimaryDecompositionComputation from Ideal  := (C, I) -> new C from comodule I
new PrimaryDecompositionComputation from Module := (C, M) -> (
    container := new PrimaryDecompositionComputation from { Result => new MutableHashTable from {} };
    -- if associated primes are cached, store a pointer in the container
    cacheKeyAP := new AssociatedPrimesContext from M;
    if M.cache#?cacheKeyAP then container#cacheKeyAP = M.cache#cacheKeyAP;
    container)

isComputationDone PrimaryDecompositionComputation := Boolean => options primaryDecomposition >> opts -> container -> (
    -- this function determines whether we can use the cached result, or further computation is necessary
    cacheKeyAP := AssociatedPrimesContext{}; -- may depend on opts
    -- TODO: (primaryDecomposition, Ideal) strategies should also return a mutable hash table
    instance(container.Result, BasicList)
    or instance(container.Result, MutableHashTable)
    and container#?cacheKeyAP and #keys(container.Result) == #container#cacheKeyAP.Result)

-- Helper for primaryDecomposition
-- A:    ideal or module
-- key:  the key for runHooks
-- opts: options for primaryDecomposition
primarydecompHelper = (A, key, opts) -> (
    strategy := opts.Strategy;
    doTrim := if opts.MinimalGenerators then trim else identity;

    -- this logic determines what strategies will be used
    computation := (opts, container) -> runHooks(key,
	(opts ++ { cache => container }, A), Strategy => opts.Strategy);

    -- this is the logic for caching partial primary decomposition computations. A.cache contains an option:
    --   PrimaryDecompositionContext{} => PrimaryDecompositionComputation{ CodimensionLimit, Result }
    container := fetchComputation(PrimaryDecompositionComputation, A, new PrimaryDecompositionContext from A);

    -- the actual computation of primary decomposition occurs here
    L := (cacheComputation(opts, container)) computation;
    -- TODO: make the (primaryDecomposition, Ideal) code to also return a hash table
    if instance(L, MutableHashTable) then L = apply(associatedPrimes A, P -> L#P);

    if L =!= null then doTrim \ L else if strategy === null
    then error("no applicable strategy for ", toString key)
    else error("assumptions for primaryDecomposition strategy ", toString strategy, " are not met"))

--------------------------------------------------------------------
--- primarydecomp strategies
--------------------------------------------------------------------

algorithms#(primaryDecomposition, Module) = new MutableHashTable from {
    -- TODO: add assumptions
    -- Note: when the strategy key for a hook is a type, strategy
    -- values matching that type will be dispatched to that hook
    Hybrid => (opts, M) -> (
	-- Returns a primary decomposition of 0 in M.
	-- the Hybrid strategy for modules requires 1 argument passed as a list
	--   Hybrid{getEmbeddedComponent strategy}
	strategy := if opts.Strategy === null then "Sat"
	else if instance(opts.Strategy, Hybrid) and #opts.Strategy == 1 then opts.Strategy#0
	else return null;
	-- Assumes all embedded primes appear after all primes they contain, i.e. isSubset(AP#i, AP#j) => i \le j
	-- (equivalently, the ordering of associated primes is a linear extension of the partial order by inclusion).
	-- This is the case for associatedPrimes(Module), which returns associated primes ordered by codimension
	S := ring M;
	-- the primary decomposition computation object
	-- the Result key is a mutable hash table with entries
	--   associated prime => respective primary component
	comp := opts.cache;
	-- the list of associated primes, either from cache or computed
	AP := associatedPrimes M; --, CodimensionLimit => dim S);
	-- check whether all components are found
	if #values(comp.Result) != #AP then (
	    -- hash table of embeddings among associated primes
	    H := hashTable apply(AP, p -> p => select(#AP, i -> isSubset(AP#i, p)));
	    for i to #AP - 1 do (
		if debugLevel > 0 then printerr("Prime: " | toString(i+1) | "/" | toString(#AP));
		p := AP#i;
		if comp.Result#?p then continue;
		f := product(AP - set AP_(H#p), q -> q_(position(q_*, g -> g % p != 0)));
		isolComp := if f == 1 then 0*M else saturate(0*M, f);
		comp.Result#p = if #(H#p) > 1 then (
		    B := intersect apply(H#p - set{i}, k -> comp.Result#(AP#k));
		    getEmbeddedComponent(M, p, C -> isSubset(intersect(B, C), isolComp), Strategy => strategy)
		    ) else isolComp);
	    );
	comp.Result),
    }

-- Installing hooks for (primaryDecomposition, Module)
scan({Hybrid}, strategy ->
    addHook(key := (primaryDecomposition, Module), algorithms#key#strategy, Strategy => strategy))

--------------------------------------------------------------------

-- TODO: make these algorithms also return a MutableHashTable { prime => primary component }
algorithms#(primaryDecomposition, Ideal) = new MutableHashTable from {
    "Comodule" => (opts, I) -> (
	-- TODO: can this be simplified?
	L := primaryDecomposition(comodule I, MinimalGenerators => opts.MinimalGenerators);
	apply(L, Q -> I + ideal generators Q)),

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

    -- Note: when the strategy key for a hook is a type, strategy
    -- values matching that type will be dispatched to that hook
    Hybrid => (opts, I) -> (
	-- the Hybrid strategy for ideals requires 2 arguments passed as a list
	--   Hybrid{associated prime strategy, localize strategy}
	if not instance(opts.Strategy, Hybrid)
	or not #opts.Strategy === 2
	then return null;
	HprimaryDecomposition(I, -- defined in EHV
	    opts.Strategy#0, -- associated primes strategy
	    opts.Strategy#1  -- localize strategy
	    )),

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
scan({"Comodule", EisenbudHunekeVasconcelos, ShimoyamaYokoyama, Hybrid, Monomial}, strategy ->
    addHook(key := (primaryDecomposition, Ideal), algorithms#key#strategy, Strategy => strategy))

-------------------------------
-- Irreducible Decomposition --
-------------------------------

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

--------------------------------
-- top dimensional component ---
--------------------------------
-- Based on the Macaulay (classic) scripts written by
-- D. Eisenbud.  Translated to Macaulay2 by M. Stillman

-- translated: remove_low_dim --> topComponents
--             remove_low_dim_id --> topComponents
--             remove_lowest_dim --> removeLowestDimension

topComponents = method()
topComponents Ideal       := Ideal  =>  I     -> (
    R := ring I;
    c := codim I;
    annihilator Ext^c(cokernel generators I, R))
topComponents Module      := Module =>  M     -> (
    R := ring M;
    if not isPolynomialRing R or not isAffineRing R then error "expected a polynomial ring";
    c := codim M;
    p := pdim M;  -- will compute a resolution if needed...
    while p > c do (
	E := minimalPresentation Ext^p(M,R);
	if E != 0 and codim E === p then (
	    -- improve M
	    J := annihilator E;
	    I := saturate(M, J);
	    -- alternate strategy: modify M as well:
	    -- this next line could be commented out
	    M = (ambient I)/I);
	p = if pdim M < p then pdim M else p-1);
    M)
topComponents(Module, ZZ) := Module => (M, e) -> (
    S := ring M;
    N := 0 * M;
    f := pdim M;  -- calls minimalPresentation, will compute a resolution if needed...
    while f > e do (
	E := Ext^f(M, S);
	if codim E == f then (
	    if debugLevel > 0 then printerr("Getting annihilator of Ext...");
	    I := annihilator E;
	    if debugLevel > 0 then printerr("Removing components of codim " | toString(f));
	    N = N : I);
	f = f-1);
    N)

-- This used to be commented out in modules2.m2
-- if it isn't useful anymore, delete it
--topComponents Module := M -> (
--     R := ring M;
--     c := codim M;
--     annihilator minimalPresentation Ext^c(M, R))
--document { topComponents,
--     TT "topComponents M", "produce the annihilator of Ext^c(M, R), where c
--     is the codimension of the support of the module M."
--     }

---------------------------
-- removeLowestDimension --
---------------------------
-- TODO: can these two be combined?

-- TODO: defined in MinimalPrimes because it is used there, but exported here
--removeLowestDimension = method()
removeLowestDimension Module := Module => M -> (
    -- only works for polynomial rings...
    local E;
    R := ring M;
    c := codim M;
    p := pdim M;
    -- now loop (starting at p) trying to find the largest
    -- d such that codim Ext^d(M,R) == d
    while p > c and codim (E = Ext^p(M, R)) > p do p = p-1;
    if p == c then ambient M -- M is C.M. and unmixed, so return (1)
    else ( -- use the annihilator of Ext to improve M
        J := annihilator E;
        cokernel generators saturate(image presentation M, J))
    )
removeLowestDimension Ideal  := Ideal  => I -> (
    -- only works for polynomial rings...
    local E;
    M := cokernel generators I;
    R := ring M;
    c := codim M;
    p := pdim M;
    -- now loop (starting at p) trying to find the largest
    -- d such that codim Ext^d(M,R) == d
    while p > c and codim (E = Ext^p(M, R)) > p do p = p-1;
    if p == c then ideal 1_R -- M is C.M. and unmixed, so return (1)
    else ( -- use the annihilator of Ext to improve M
        J := annihilator E;
        saturate(I, J))
    )

--------------------------------------------------------------------

-- Moved here from m2/monideals.m2 because it depends on associatedPrimes
-- probably written by Greg Smith
Delta := I -> (
    X := generators ring I;
    d := #X - pdim cokernel generators I;
    toList \ select( apply(associatedPrimes I, J -> set X - set first entries generators J), Y -> #Y >= d ))

standardPairs MonomialIdeal := I -> standardPairs(I, Delta I)

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

-- something like this might work for primary decomposition over any PID
-- TODO: how to extend it to modules?
A = ideal(60, 40)
if ring A === ZZ then ideal \ power \ toSequence \ toList factor (trim A)_0
