---------------------------------------------------------------------------
-- PURPOSE : Computation of minimal primes, and related functions
--
-- UPDATE HISTORY : created July 25, 2011 at the IMA M2 workshop as PD.m2
--                  renamed  Oct  7, 2014 to MinimalPrimes.m2
--                  updated July 26, 2019 at the IMA M2/Sage workshop
--                  updated  Nov 12, 2020 to use hooks
--                  updated  Nov 18, 2020 moved radical and radicalContainment here
--
-- TODO : 1. move documentation to this package and complete
--        2. move tests to this package
--           test trivial ideals, cases that shouldn't work
--           funny gradings, quotient rings, etc.
--        3. turn strategies into hooks
--        4. which symbols need to be exported
--        5. Binomials package should add binomialMinimalPrimes as a strategy to minimalPrimes
--        6. add installMinprimes that gives a warning
---------------------------------------------------------------------------
newPackage(
    "MinimalPrimes",
    Version => "0.10",
    Date => "November 12, 2020",
    Headline => "minimal primes and radical routines for ideals",
    Authors => {
	{Name => "Frank Moore",    Email => "moorewf@wfu.edu",       HomePage => "https://users.wfu.edu/moorewf"},
	{Name => "Mike Stillman",  Email => "mike@math.cornell.edu", HomePage => "https://www.math.cornell.edu/~mike"},
	{Name => "Franziska Hinkelmann"},
	{Name => "Justin Chen",    Email => "justin.chen@math.gatech.edu"},
	{Name => "Mahrud Sayrafi", Email => "mahrud@umn.edu",        HomePage => "https://math.umn.edu/~mahrud"}},
    Keywords => {"Commutative Algebra"},
    PackageImports => { "Elimination" },
    AuxiliaryFiles => true,
    DebuggingMode => false
    )

-- TODO: The following functions are used in tests.m2
-- They should be removed from the export list upon release
--  factors, findNonMemberIndex,

-- MES notes 26 July 2019, flight back from IMA 2019 M2/Sage workshop
-- Overall structure of the algorithm.

-- idea is that we want to use some heuristics to split the ideal I
-- into a list of ideals such that: WRITE DOWN THE INVARIANT HERE
-- (The plan is that radical(I) = intersection of all ideals collected so far.).
-- We first make a simplification:
-- I is an ideal in a (flattened) polynomial ring.
--   Perhaps the invariant data should contain the list of computed annotated ideals, AND a
--   way to get back to the ring of I.
-- DESIRE: if the computation is interrupted, partial results can be viewed, and the computation
--   can be restarted.

-- TODO to get this into the system:
-- minprimes: installs itself into decompose. DONE
--   minprimes should stash its answer DONE
--             should work for quotients DONE
--             should give errors for general situations DONE?
--             what about over ZZ?
--             absolute case?
--             factorization over towers?
--  a better inductive system?

-- Mike and Frank talking 10/9/2014
-- to do:
-- .  The function 'factors' needs documentation and tests.
--    In fact, it is failing in some cases (e.g. when 'factor' returns a factor not in the polynomial ring)
-- .  Document minprimes, something about the strategies
-- .  Export only the symbols we want

export { "Hybrid", "minimalPrimes", "minprimes" => "minimalPrimes", "radical", "radicalContainment", "installMinprimes" }

importFrom_Core { "printerr", "raw", "rawCharSeries", "rawGBContains", "rawRadical", "newMonomialIdeal" }

-*------------------------------------------------------------------
-- Can we use these as keys to a ring's HashTable without exporting them?
-- It seems awkward to have to export these:
  toAmbientField, fromAmbientField
--- annotated ideal keys
  AnnotatedIdeal, Linears, NonzeroDivisors, Inverted, FiberInfo, LexGBOverBase, nzds
--- Splitting options. Should we be exporting these?
-- Are we going to be allowing the user to split (annotated) ideals using these options?
  Birational, -- Strategy option for splitIdeal.  Exported now for simplicity
  IndependentSet, SplitTower, LexGBSplit, Factorization,
  DecomposeMonomials, Trim, CharacteristicSets, Minprimes, Squarefree
-*------------------------------------------------------------------

protect symbol IndependentSet
protect symbol Trim
protect symbol Birational
protect symbol Linears
protect symbol DecomposeMonomials
protect symbol Factorization
protect symbol SplitTower
protect symbol CharacteristicSets

protect symbol Inverted
protect symbol LexGBOverBase
protect symbol NonzeroDivisors
protect symbol LexGBSplit
protect symbol Squarefree  -- MES todo: this is never being set but is being tested.

protect symbol toAmbientField
protect symbol fromAmbientField

algorithms = new MutableHashTable from {}

-- used for dynamic strategies here and in PrimaryDecomposition
Hybrid = new SelfInitializingType of List

-- deprecate this soon
installMinprimes = () -> printerr "warning: the installMinprimes routine is now deprecated and should be removed"

--------------------------------------------------------------------
-- Support routines
--------------------------------------------------------------------

-- TODO: this declaration should be moved to PrimaryDecomposition,
-- but a radical routine uses it. Methods are installed there.
removeLowestDimension = method()

load "./MinimalPrimes/AnnotatedIdeal.m2"
load "./MinimalPrimes/PDState.m2"
load "./MinimalPrimes/splitIdeals.m2"
load "./MinimalPrimes/factorTower.m2"
load "./MinimalPrimes/radical.m2"

cacheHit := type -> if debugLevel > 0 then printerr("Cache hit on a ", synonym type, "! 🎉");

-- Redundancy control:
-- find, if any, an element of I which is NOT in the ideal J.
-- returns the index x of that element, if any, else returns -1.
findNonMemberIndex = method()
findNonMemberIndex(Ideal, Ideal) := (I, J) -> rawGBContains(raw gb J, raw generators I)

-- The following function removes any elements which are larger than another one.
-- Each should be tagged with its codimension.  For each pair (L_i, L_j), check containment of GB's
selectMinimalIdeals = L -> (
    L = L / (i -> (codim i, flatten entries gens gb i)) // sort / last / ideal;
    ML := new MutableList from L;
    for i from 0 to #ML - 1 list (
        if ML#i === null then continue;
        for j from i + 1 to #ML - 1 do (
            if ML#j === null then continue;
            if findNonMemberIndex(ML#i, ML#j) === -1 then ML#j = null);
        ML#i))

-- also see a similar method in PrimaryDecomposition
isSupportedRing := I -> (
    A := ring first flattenRing I;
    -- ring should be a commutative polynomial ring or a quotient of one
    isPolynomialRing A and isCommutative A
    -- base field should be QQ or ZZ/p or GF(q)
    and (QQ === (kk := coefficientRing A) or instance(kk, QuotientRing) or instance(kk, GaloisField)))

-- TODO: can this functionality be simplified and put into flattenRing?
-- also see a similar method in PrimaryDecomposition
flattenRingMap := I -> (
    -- R is the ring of I, and A is a polynomial ring over a prime field
    R := ring I;
    (J, F) := flattenRing I; -- the ring map is not needed
    A := ring J;
    -- the map back to R, TODO: why not use F?
    fback := if A === R then identity else map(R, A, generators(R, CoefficientRing => coefficientRing A));
    (J, fback))

------------------------------
--- isPrime ------------------
------------------------------

isPrimeOptions := {
    Verbosity              => 0,
    Strategy               => null,
    "SquarefreeFactorSize" => 1
    }
isPrime Ideal := Boolean => isPrimeOptions >> opts -> I -> (
    C := minimalPrimes(I, opts ++ {"CheckPrimeOnly" => true}); #C === 1 and C#0 == I)

--------------------------------------------------------------------
-- decompose, minimalPrimes, and minprimes
--------------------------------------------------------------------

minimalPrimes = method(
    Options => {
	Verbosity              => 0,
	Strategy               => null,
	CodimensionLimit       => infinity, -- only find minimal primes of codim <= this bound
	MinimalGenerators      => true,     -- whether to trim the output
	"CheckPrimeOnly"       => false,
	"SquarefreeFactorSize" => 1
	}
    )

-- returns a list of ideals, the minimal primes of I
minimalPrimes Ideal := List => opts -> I -> minprimesHelper(I, (minimalPrimes, Ideal), opts)
-- decompose is declared in m2/shared.m2 as method(Options => true)
decompose     Ideal := List => options minimalPrimes >> opts -> I -> minprimesHelper(I, (minimalPrimes, Ideal), opts)

-- keys: none so far
MinimalPrimesOptions = new SelfInitializingType of BasicList
MinimalPrimesOptions.synonym = "minimal primes options"

-- keys: CodimensionLimit and Result
MinimalPrimesComputation = new Type of MutableHashTable
MinimalPrimesComputation.synonym = "minimal primes computation"

-- if there is a compatible computation stored in I.cache,
-- returns the computation object, otherwise creates the entry:
--   MinimalPrimesOptions{} => MinimalPrimesComputation{ CodimensionLimit, Result }
new MinimalPrimesComputation from Ideal  := (C, I) -> (
    -- TODO: currently there are no options that could go in MinimalPrimesOptions,
    -- but can we give options to this function? This is needed for saturate, etc.
    cacheKey := MinimalPrimesOptions{};
    try I.cache#cacheKey else I.cache#cacheKey = new MinimalPrimesComputation from {
	Result => null, CodimensionLimit => -1 })

isComputationDone = method(TypicalValue => Boolean, Options => true)
isComputationDone MinimalPrimesComputation := Boolean => options minimalPrimes >> opts -> container -> (
    -- this function determines whether we can use the cached result, or further computation is necessary
    try instance(container.Result, List) and opts.CodimensionLimit <= container.CodimensionLimit else false)

cacheComputation = method(TypicalValue => CacheFunction, Options => true)
cacheComputation MinimalPrimesComputation := CacheFunction => options minimalPrimes >> opts -> container -> new CacheFunction from (
    -- this function takes advantage of FunctionClosures by modifying the container
    computation -> (
	if isComputationDone(opts, container) then ( cacheHit class container; container.Result ) else
	if (result := computation(opts, container)) =!= null then (
	    container.CodimensionLimit = opts.CodimensionLimit;
	    container.Result = result)))

-- Helper for minimalPrimes and decompose
minprimesHelper = (I, key, opts) -> (
    if I == 1 then return {};
    J := first flattenRing I;
    if J == 0 then return {I};
    -- TODO: make presentation work for ZZ, then move this line
    if ring I === ZZ then return ideal \ first \ toList factor (trim I)_0;
    S := ring presentation ring J;

    strategy := opts.Strategy;
    doTrim := if opts.MinimalGenerators then trim else identity;

    codimLimit := min(opts.CodimensionLimit, dim S, numgens J);
    doLimit := L -> select(L, P -> codim(P, Generic => true) <= codimLimit);
    opts = opts ++ { CodimensionLimit => codimLimit };

    -- this logic determines what strategies will be used
    computation := (opts, container) -> runHooks(key, (opts, I), Strategy => opts.Strategy);

    -- this is the logic for caching partial minimal primes computations. I.cache contains an option:
    --   MinimalPrimesOptions{} => MinimalPrimesComputation{ CodimensionLimit, Result }
    container := new MinimalPrimesComputation from I;

    -- the actual computation of minimal primes occurs here
    L := (cacheComputation(opts, container)) computation;

    if L =!= null then doLimit \\ doTrim \ L else if strategy === null
    then error("no applicable strategy for ", toString key)
    else error("assumptions for minimalPrimes strategy ", toString strategy, " are not met"))

--------------------------------------------------------------------
--- minprimes strategies
--------------------------------------------------------------------

strat0 = ({Linear, DecomposeMonomials}, infinity)
strat1 = ({Linear, DecomposeMonomials, (Factorization, 3)}, infinity)
BirationalStrat = ({strat1, (Birational, infinity)}, infinity)
NoBirationalStrat = strat1
stratEnd = {(IndependentSet, infinity), SplitTower, CharacteristicSets}

algorithms#(minimalPrimes, Ideal) = new MutableHashTable from {
    "Legacy" => (opts, I) -> (
    	-- TODO: is this based on this paper?
    	-- https://www-sop.inria.fr/members/Evelyne.Hubert/publications/PDF/Hubert00.pdf
    	A := ring first flattenRing I;
    	-- ring should be a commutative polynomial ring or a quotient of one
    	if not isPolynomialRing A
	or not isCommutative A
    	-- base field should be QQ or ZZ/p
    	or not (QQ === (kk := coefficientRing A)
	    or instance(kk, QuotientRing))
	then return null;
	legacyMinimalPrimes I),

    "NoBirational" => (opts, I) -> (
	-- TODO: add heuristics for when Legacy is better
	if not isSupportedRing I
	then return null;
	minprimesWithStrategy(I,
    	    Verbosity              => opts.Verbosity,
    	    Strategy               => NoBirationalStrat,
    	    CodimensionLimit       => opts.CodimensionLimit,
    	    "SquarefreeFactorSize" => opts#"SquarefreeFactorSize")),

    "Birational" => (opts, I) -> (
	-- TODO: add heuristics for when NoBirational is better
	if not isSupportedRing I
	then return null;
	minprimesWithStrategy(I,
    	    Verbosity              => opts.Verbosity,
    	    Strategy               => BirationalStrat,
    	    CodimensionLimit       => opts.CodimensionLimit,
    	    "SquarefreeFactorSize" => opts#"SquarefreeFactorSize")),

    Hybrid => (opts, I) -> (
	-- advanced strategies can still be used by passing
	--  Strategy => Hybrid{...}
	if not instance(opts.Strategy, Hybrid)
	then return null;
	minprimesWithStrategy(I,
	    Verbosity              => opts.Verbosity,
	    Strategy               => toList opts.Strategy,
	    CodimensionLimit       => opts.CodimensionLimit,
	    "SquarefreeFactorSize" => opts#"SquarefreeFactorSize")),

    Monomial => (opts, I) -> (
	R := ring I;
	if not isMonomialIdeal I
	or not isPolynomialRing R
	or not isCommutative R
	then return null;
	cast := if instance(I, MonomialIdeal) then monomialIdeal else ideal;
	minI := dual radical monomialIdeal I;
	-- TODO: make sure (monomialIdeal, MonomialIdeal) isn't forgetful
	cast \ if minI == 1 then { 0_R } else support \ minI_*),
    }

-- Installing hooks for (minimalPrimes, Ideal)
scan({"Legacy", "NoBirational", "Birational", Hybrid, Monomial}, strategy ->
    addHook(key := (minimalPrimes, Ideal), algorithms#key#strategy, Strategy => strategy))

--------------------------------------------------------------------
-- minprimes algorithms
--------------------------------------------------------------------

-- This function is called under Birational, NoBirational, and advanced options
minprimesWithStrategy = method(Options => options splitIdeals)
minprimesWithStrategy Ideal := opts -> J -> (
    (I, fback) := flattenRingMap J;
    --
    newstrat := {opts.Strategy, stratEnd};
    --
    pdState := createPDState(I);
    opts = opts ++ {"PDState" => pdState};
    M := splitIdeals({annotatedIdeal(I, {}, {}, {})}, newstrat, opts);
    -- if just a primality/primary check, then return result.
    -- should we cache what we have done somewhere?
    if opts#"CheckPrimeOnly" then return pdState#"isPrime";
    numRawPrimes := numPrimesInPDState pdState;
    --M = select(M, i -> codimLowerBound i <= opts.CodimensionLimit);
    --(M1,M2) := separatePrime(M);
    if #M > 0 then (
	printerr("warning: ideal did not split completely: ", toString(#M), " did not split!");
	error "answer not complete");
    if opts.Verbosity >= 2 then printerr "Converting annotated ideals to ideals and selecting minimal primes...";
    answer := timing(selectMinimalIdeals \\ getPrimesInPDState pdState);
    --
    if opts.Verbosity >= 2 then (
	printerr(" Time taken : ", toString answer#0);
	if #answer#1 < numRawPrimes then printerr(
	    "#minprimes=", toString(#answer#1),
	    " #computed=", toString numPrimesInPDState pdState));
    fback \ answer#1)

-- This function is called under the option Strategy => "Legacy"
legacyMinimalPrimes = J -> (
    (I, fback) := flattenRingMap J;
    --
    if debugLevel > 0 then homog := isHomogeneous I;
    ics := irreducibleCharacteristicSeries I;
    if debugLevel > 0 and homog then (
	if not all(ics#0, isHomogeneous) then error "minimalPrimes: irreducibleCharacteristicSeries destroyed homogeneity");
    -- remove any elements which have numgens > numgens I (Krull's Hauptidealsatz)
    ngens := numgens I;
    ics0 := select(ics#0, CS -> numgens source CS <= ngens);
    phi := apply(ics0, CS -> (
	    chk := topCoefficients CS;
	    chk = chk#1; -- just keep the coefficients
	    chk = first entries chk;
	    iniCS := select(chk, i -> # support i > 0); -- this is bad if degrees are 0: degree i =!= {0});
	    if gbTrace >= 1 then << "saturating with " << iniCS << endl;
	    CS = ideal CS;
	    --<< "saturating " << CS << " with respect to " << iniCS << endl;
	    -- warning: over ZZ saturate does unexpected things.
	    scan(iniCS, a -> CS = saturate(CS, a, Strategy=>Eliminate));
     	    -- scan(iniCS, a -> CS = saturate(CS, a));
	    --<< "result is " << CS << endl;
	    CS));
    --
    phi = select(phi, I -> I != 1);
    phi = new MutableList from phi;
    p := #phi;
    scan(0 .. p-1, i -> if phi#i =!= null then
	scan(i+1 .. p-1, j ->
	    if phi#i =!= null and phi#j =!= null then
	    if isSubset(phi#i, phi#j)            then phi#j = null else
	    if isSubset(phi#j, phi#i)            then phi#i = null));
    phi = toList select(phi,i -> i =!= null);
    fback \ apply(phi, p -> ics#1 p))

--------------------------------------------------------------------
----- Development section
--------------------------------------------------------------------
-- TODO: where should these go? Reduce redundancy

----------------------------------------------
-- Factorization and fraction field helper routines
----------------------------------------------

-- setAmbientField:
--   input: KR, a ring of the form kk(t)[u] (t and u sets of variables)
--          RU, kk[u,t] (with some monomial ordering)
--   consequence: sets information in KR so that
--     'factors' and 'numerator', 'denominator' work for elemnts of KR
--     sets KR.toAmbientField, KR.fromAmbientField
setAmbientField = method()
setAmbientField(Ring, Ring) := (KR, RU) -> (
    -- KR should be of the form kk(t)[u]
    -- RU should be kk[u, t], with some monomial ordering
    KR.toAmbientField = map(frac RU,KR);
    KR.fromAmbientField = (f) -> (if ring f === frac RU then f = numerator f; (map(KR,RU)) f);
    numerator KR := (f) -> numerator KR.toAmbientField f;
    denominator KR := (f) -> denominator KR.toAmbientField f;
    )

-- TODO: what does factors f do? is this always correct, or only for the purposes of MinimalPrimes
mySat = (I, f) -> saturate(I, f, Strategy => Factorization)
-- TODO: if this is always correct, move it to Saturation
-- Note: if it is the default strategy, it can lead to infinite loops
addHook((saturate, Ideal, RingElement), (opts, I, f) -> saturate(I, last \ factors f, opts), Strategy => Factorization)

-- needs documentation
factors = method()
factors RingElement := (F) -> (
    R := ring F;
    if F == 0 then return {(1,F)};
    facs := if R.?toAmbientField then (
        F = R.toAmbientField F;
        RU := ring numerator F;
        numerator factor F
        )
    else if isPolynomialRing R and instance(coefficientRing R, FractionField) then (
        KK := coefficientRing R;
        A := last KK.baseRings;
        RU = (coefficientRing A) (monoid[generators R, generators KK, MonomialOrder=>Lex]);
        setAmbientField(R, RU);
        F = R.toAmbientField F;
        numerator factor F
        )
    else if instance(R, FractionField) then (
        -- What to return in this case?
        -- WORKING ON THIS MES
        error "still need to handle FractionField case";
        )
    else (
        RU = ring F;
        factor F
        );
    facs = facs//toList/toList; -- elements of facs: {factor, multiplicity}
    facs = select(facs, z -> ring first z === RU);
    facs = apply(#facs, i -> (facs#i#1, (1/leadCoefficient facs#i#0) * facs#i#0 ));
    facs = select(facs, (n,f) -> # support f =!= 0);
    if R.?toAmbientField then apply(facs, (r,g) -> (r, R.fromAmbientField g)) else facs
    )

makeFiberRings = method()
makeFiberRings List       :=  basevars    -> (
    if #basevars =!= 0 then makeFiberRings(basevars, ring (basevars#0))
    else error "Expected at least one variable in the base")
makeFiberRings(List,Ring) := (basevars,R) -> (
    -- basevars: a list, possibly empty, of variables in the ring R.
    -- R: a flattened polynomial ring.
    -- result: (S, SF):
    --   S = R, but with a new monomial order.  S = kk[fibervars, basevars, Lex in fiber vars]
    --   SF = frac(kk[basevars])[fibervars, MonomialOrder=>Lex]
    -- warning: if R is already in Lex order, a new ring is currently created.  This behavior may change.
    -- consequences:
    --   In the cache of S:
    --     StoSF: S --> SF.
    --     SFtoS: SF --> S
    --     StoR: S --> R
    --     RtoS: R --> S
    --   Additionally:
    --     numerator(f in SF) gives element in S.
    --     denominator(f in SF) gives an element in S, although it will be in kk[basevars].
    --     factors(f in a ring)
    --   In the (mutable hash table of) SF:
    --     toAmbientField: SF -> frac S
    --     fromAmbientField(f in frac S) = (numerator f, which is in S), but mapped into SF.
    --
    -- Really, these are the rings we want?
    -- S = kk[fibervars, basevars]
    -- SF = kk(basevars)[fibervars]
    -- frac S
    -- A = kk[basevars] -- obtained via 'ambient coefficientRing SF'
    -- KA = frac(kk(basevars)) -- obtained via 'coefficientRing SF'
   local S;
   if #basevars == 0 then (
        -- in this case, we are not inverting any variables.  So, S = SF, and S just has the lex
        -- order.
        S = newRing(R, MonomialOrder=>Lex);
        S#cache = new CacheTable;
        S.cache#"RtoS" = map(S,R,sub(vars R,S));
        S.cache#"StoR" = map(R,S,sub(vars S,R));
        S.cache#"StoSF" = identity;
        S.cache#"SFtoS" = identity;
        numerator S := identity;
        (S,S)
   )
   else
   (
      if any(basevars, x -> ring x =!= R) then error "expected all base variables to have the same ring";
      allVars := set gens R;
      fiberVars := rsort toList(allVars - set basevars);
      basevars = rsort basevars;
      S = (coefficientRing R) monoid([fiberVars,basevars,MonomialOrder=>Lex]);
          --MonomialOrder=>{#fiberVars,#basevars}]);
      KK := frac((coefficientRing R)(monoid [basevars]));
      SF := KK (monoid[fiberVars, MonomialOrder=>Lex]);
      S#cache = new CacheTable;
      S.cache#"StoSF" = map(SF,S,sub(vars S,SF));
      S.cache#"SFtoS" = map(S,SF,sub(vars SF,S));
      S.cache#"StoR" = map(R,S,sub(vars S,R));
      S.cache#"RtoS" = map(S,R,sub(vars R,S));
      setAmbientField(SF, S);
      (S, SF)
   )
)

--------------------------------------------------------------------
----- Tests section
--------------------------------------------------------------------

load "./MinimalPrimes/tests.m2"

--------------------------------------------------------------------
----- Documentation section
--------------------------------------------------------------------

beginDocumentation()
load "./MinimalPrimes/doc.m2"

end--

restart
debugLevel = 1
debug MinimalPrimes

R1 = QQ[d, f, j, k, m, r, t, A, D, G, I, K];
I1 = ideal ( I*K-K^2, r*G-G^2, A*D-D^2, j^2-j*t, d*f-f^2, d*f*j*k - m*r, A*D - G*I*K);
-- TODO: how to get this to work?
C = doSplitIdeal(I1, Verbosity=>2)
time minprimes(I1, CodimensionLimit=>6, Verbosity=>2)
C = time minprimes I1
C = time minprimes(I1, Strategy=>"NoBirational", Verbosity=>2)

R1 = QQ[a,b,c]
I1 = ideal(a^2-3, b^2-3)
C = doSplitIdeal(I1, Verbosity=>2)
minprimes(I1, Verbosity=>2)

kk = ZZ/7
R = kk[x,y,t]
I = ideal {x^7-t^2,y^7-t^2}
minprimes I
C = doSplitIdeal(I, Verbosity=>2)
