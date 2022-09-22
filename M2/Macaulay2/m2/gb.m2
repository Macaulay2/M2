--		Copyright 1995-2002,2012 by Daniel R. Grayson
-- Notes:
--   set debugLevel = 77 for debug information
--   see processAlgorithm for a set of TODO items

needs "computations.m2"
needs "matrix1.m2"
needs "modules.m2"
needs "printing.m2" -- for unbag
needs "quotient.m2"
needs "rings.m2"

-----------------------------------------------------------------------------
-- Local variables
-----------------------------------------------------------------------------

-- Keep this in sync with the ComputationStatusCode in Macaulay2/e/interface/computation.h
RawStatusCodes := new HashTable from {
    1 => "need resize",              -- COMP_NEED_RESIZE
    2 => "error",                    -- COMP_ERROR
    3 => "interrupted",              -- COMP_INTERRUPTED
    4 => "not started",              -- COMP_NOT_STARTED
    5 => StopBeforeComputation,      -- COMP_INITIAL_STOP
    6 => "done",                     -- COMP_DONE
    7 => DegreeLimit,                -- COMP_DONE_DEGREE_LIMIT
    8 => LengthLimit,                -- COMP_DONE_LENGTH_LIMIT
    9 => SyzygyLimit,                -- COMP_DONE_SYZYGY_LIMIT
    10 => PairLimit,                 -- COMP_DONE_PAIR_LIMIT
    11 => BasisElementLimit,         -- COMP_DONE_GB_LIMIT
    12 => SyzygyLimit,               -- COMP_DONE_SYZ_LIMIT
    13 => CodimensionLimit,          -- COMP_DONE_CODIM
    14 => StopWithMinimalGenerators, -- COMP_DONE_MIN_GENS
    15 => "StepLimit",               -- COMP_DONE_STEPS
    16 => SubringLimit,              -- COMP_DONE_SUBRING_LIMIT
    17 => "computing",               -- COMP_COMPUTING
    18 => "overflowed",              -- COMP_OVERFLOWED
    }

-- Keep this in sync with StrategyValues in e/interface/computation.h
RawStrategyCodes := new HashTable from {
    LongPolynomial => 1,
    Sort           => 2,
    -- UseHilbertFunction => 4, -- this is defined in e/engine.h, but is it still usable?
    UseSyzygies    => 8
    }

computationOptionDefaults := new OptionTable from {
    SyzygyRows        => infinity,	-- n_rows_to_keep (-1 if infinity)
    Syzygies          => false,         -- collect_syz parameter
    HardDegreeLimit   => null,          -- use_max_degree and degree_limit
    ChangeMatrix      => false,         -- calculate change of basis matrix, too, for '//' operation
    Algorithm         => Inhomogeneous, -- Homogeneous (1) or Inhomogeneous (2)
    Strategy          => {},            -- strategy
    GBDegrees         => null,          -- positive integers
    Hilbert           => null,          -- also obtainable from m.cache.cokernel.poincare
    MaxReductionCount => 10             -- max_reduction_count in gbA.cpp (for a stubborn S-polynomial)
    }

stoppingOptionDefaults := new OptionTable from {
    StopBeforeComputation     => false,    -- stopping condition (always_stop)
    DegreeLimit               => {},       -- stopping condition (degree_limit) (not max_degree)
    BasisElementLimit         => infinity, -- stopping condition (basis_element_limit)
    SyzygyLimit               => infinity, -- stopping condition (syzygy_limit) (not for res computations)
    PairLimit                 => infinity, -- stopping condition (pair_limit)
    CodimensionLimit          => infinity, -- stopping condition (codim_limit) (not for res computations)
    SubringLimit              => infinity, -- stopping condition (subring_limit) (not for res computations)
    StopWithMinimalGenerators => false     -- stopping condition (just_min_gens) (not for res computations)
    -- LengthLimit               => null      -- is only for res computations
    -- StepLimit, maybe
    }

-- used here by gb and in pushforward.m2
gbDefaults = merge(computationOptionDefaults, stoppingOptionDefaults, x -> error "overlap")

notForSyz   := set { Syzygies, ChangeMatrix, CodimensionLimit, Hilbert, StopWithMinimalGenerators, SubringLimit }
syzDefaults := select(pairs gbDefaults, (k, v) -> not notForSyz#?k)

-- for internal use only, for general clean up
--Nothing#BeforeEval = x -> ( runHooks(symbol BeforeEvalHooks, ()); )
flagInhomogeneity = false -- for debugging homogeneous problems, to make sure homogeneity is not lost
--addHook(symbol BeforeEvalHooks, () -> flagInhomogeneity = false)

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

warnexp := () -> printerr("warning: gb algorithm requested is experimental")

-- TODO: implement general type checking
checkListOfIntegers := method()
checkListOfIntegers ZZ   := t -> {t}
checkListOfIntegers List := t -> if not all(t, i -> class i === ZZ) then error "expected list of integers" else t

-- get a subset of opts
getSomeOptions := (opts, which) -> applyPairs( which, (key, val) -> (key, opts#key) )

-- TODO: is this correct?
computationIsComplete := (m, type) -> m.cache#?type and m.cache#type.?returnCode and m.cache#type.returnCode === 0
getComputation        := (m, type) -> m.cache#?type

toEngineNat  := n -> if n === infinity then -1 else n

---------------------------

-- also used in res.m2 and tests/engine/raw-gb.m2
isComputationDone ZZ := {} >> o -> c -> if RawStatusCodes#?c then "done" === RawStatusCodes#c else error "unknown computation status code"

processStrategy := strategies -> sum(flatten {strategies}, strategy ->
    if RawStrategyCodes#?strategy then RawStrategyCodes#strategy else error "gb: unknown strategy encountered")

-- TODO:
--   move the codes below into a hashtable above
--   implement condition checking mechanism similar to Saturation.m2
--   F4 is still used by groebnerBasis, is it the same as LinearAlgebra?
--   integrate FGLM, GroebnerWalk, ThreadedGB, Hilbert driven Buchberger, etc.
processAlgorithm := (alg, m) -> (
    R := ring m;
    k := ultimate(coefficientRing, R);
    -- compatibility checking
    if (alg === Homogeneous or alg === Homogeneous2) and not isHomogeneous m
    then error "gb: homogeneous algorithm specified with inhomogeneous matrix";
    if k === ZZ and alg =!= Inhomogeneous
    then error "gb: only the algorithm 'Inhomogeneous' may be used with base ring ZZ";
    if R.?FlatMonoid and not R.FlatMonoid.Options.Global and alg =!= Inhomogeneous
    then error "gb: only the algorithm 'Inhomogeneous' may be used with a non-global monomial ordering";
    -- return algorithm code
    -- Keep these in sync with the values in e/comp-gb.cpp (TODO: where?)
    if      alg === Homogeneous   then 1
    else if alg === Inhomogeneous then 2
    -- else if alg === F4            then error "the F4 algorithm option has been replaced by LinearAlgebra"
    -- else if alg === Faugere       then error "the Faugere algorithm option has been replaced by LinearAlgebra"
    else if alg === Sugarless     then 4
    else if alg === Homogeneous2  then 5
    else if alg === LinearAlgebra then (warnexp(); 6)
    else if alg === Toric         then (warnexp(); 7)
    else if alg === Test          then 8
    else error "unknown algorithm encountered")

-----------------------------------------------------------------------------
-- GroebnerBasis type declarations and basic constructors and functions
-----------------------------------------------------------------------------

GroebnerBasis = new Type of MutableHashTable
GroebnerBasis.synonym = "Gröbner basis"

GroebnerBasisOptions = new Type of OptionTable
GroebnerBasisOptions.synonym = "Gröbner basis options"

-- m:    a Matrix
-- type: an GroebnerBasisOptions
-- opts: an OptionTable
-- TODO: document this
new GroebnerBasis from Sequence := (GB, S) -> (
    (m, type, opts) := if #S === 3 then S else error "GroebnerBasis: expected three initial values";
    -- TODO: implement recursive type checking
    if not instance(m, Matrix) or not instance(type, GroebnerBasisOptions) or not instance(opts, OptionTable)
    then error "GroebnerBasis: expected a sequence of type (Matrix, GroebnerBasisOptions, OptionTable)";
    if flagInhomogeneity and not isHomogeneous m then error "internal error: gb: inhomogeneous matrix flagged";
    -- initialize the toplevel container
    G := new GroebnerBasis;
    if debugLevel > 5 then registerFinalizer(G, "gb (new GroebnerBasis from Sequence)");
    G.ring = ring m;
    G.matrix = Bag {m};
    G.target = target m;
    -- initialize the engine computation, without starting it
    G.RawComputation = rawGB(
	raw m,
	type.Syzygies,
	toEngineNat type.SyzygyRows,
	checkListOfIntegers(
	    if      opts.GBDegrees =!= null          then opts.GBDegrees
	    else if opts.Algorithm =!= LinearAlgebra then {}
	    else apply(degrees G.ring, d -> dotprod(d, heft G.ring))),
	opts.HardDegreeLimit =!= computationOptionDefaults.HardDegreeLimit,
	if opts.HardDegreeLimit =!= computationOptionDefaults.HardDegreeLimit then first degreeToHeft(G.ring, opts.HardDegreeLimit) else 0,
	processAlgorithm(opts.Algorithm, m),
	processStrategy opts.Strategy,
	opts.MaxReductionCount);
    if debugLevel == 77 then printerr("initialized new gb computation of type ", net type, " and hash ", toString hash G, " with options ", net opts);
    m.cache#type = G) -- do this last, in case of an interrupt


raw    GroebnerBasis := g -> g.RawComputation
ring   GroebnerBasis := g -> g.ring
target GroebnerBasis := g -> g.target
status GroebnerBasis := o -> g -> (
    s := toString RawStatusCodes#(rawStatus1 raw g);
    "status: " | s | "; "|
    (if s === "done" then "S-pairs encountered up to degree " else "all S-pairs handled up to degree ") | toString rawStatus2 raw g
    )
net      GroebnerBasis :=
toString GroebnerBasis := g -> "GroebnerBasis[" | status g | "]"
texMath  GroebnerBasis := texMath @@ toString
html     GroebnerBasis := html    @@ toString


rawsort := m -> rawSubmatrix(m, rawSortColumns(m,1,1))

-- TODO: where should this be defined?
syz = method(TypicalValue => Matrix, Options => syzDefaults)
-- rawGBSyzygies doesn't sort its columns, so we do that here
syz              GroebnerBasis  := Matrix => o -> G -> map(ring G, rawsort rawGBSyzygies G.RawComputation)
leadTerm         GroebnerBasis  := Matrix =>      G -> map(ring G, rawGBGetLeadTerms(raw G, -1))
getChangeMatrix  GroebnerBasis  := Matrix =>      G -> map(ring G, rawGBChangeOfBasis G.RawComputation)
generators       GroebnerBasis  := Matrix => o -> G -> map(target unbag G.matrix, , rawGBGetMatrix G.RawComputation)
-- rawGBMinimalGenerators doesn't sort its columns, so we do that here

-- more methods are installed in matrix2.m2
mingens = method(Options => {Strategy => null})  -- Complement or Inhomogeneous -- TODO: add DegreeLimit => {}
mingens          GroebnerBasis  := Matrix => o -> G -> map(target unbag G.matrix, , rawsort rawGBMinimalGenerators G.RawComputation)

RingElement   // GroebnerBasis  := Matrix =>      (r, G) -> quotient(r * id_(target G), G)
Matrix        // GroebnerBasis  := Matrix =>      (n, G) -> quotient(n, G)
quotient(Matrix, GroebnerBasis) := Matrix => o -> (n, G) -> (
    -- this gb might not be one with change of basis matrix attached...
    -- so it is best for the user not to use it
    R := ring G;
    (rem, quot, cplt) := rawGBMatrixLift(raw G, raw n);
    map(R, quot))

quotientRemainder(Matrix, GroebnerBasis) := Matrix => (n, G) -> (
    -- this gb might not be one with change of basis matrix attached...
    -- so it is best for the user not to use it
    R := ring G;
    (rem, quot, cplt) := rawGBMatrixLift(raw G, raw n);
    (map(R, quot), map(R, rem)))

Matrix          % GroebnerBasis  :=
remainder(Matrix, GroebnerBasis) := Matrix      => (n, G) -> map(target n, , rawGBMatrixRemainder(raw G, raw n))
Number          % GroebnerBasis  :=
RingElement     % GroebnerBasis  := RingElement => (r, G) -> (remainder(promote(r, ring G) * id_(target G), G))_(0,0)

-----------------------------------------------------------------------------
-- helpers for gb
-----------------------------------------------------------------------------

gbTypeCode := opts -> new GroebnerBasisOptions from {
    SyzygyRows      => if opts.Syzygies or opts.ChangeMatrix then opts.SyzygyRows else 0,
    Syzygies        => opts.Syzygies,
    HardDegreeLimit => opts.HardDegreeLimit }
gbOnly       := gbTypeCode new OptionTable from { SyzygyRows => 0,        Syzygies => false, ChangeMatrix => false, HardDegreeLimit => null }
gbWithChg    := gbTypeCode new OptionTable from { SyzygyRows => infinity, Syzygies => false, ChangeMatrix => true,  HardDegreeLimit => null }
gbWithSyzygy := gbTypeCode new OptionTable from { SyzygyRows => infinity, Syzygies => true,  ChangeMatrix => false, HardDegreeLimit => null }

gbGetPartialComputation := (m, type) -> (
    verboseLog := if debugLevel == 77 then printerr else identity;
    verboseLog("gb type requested: ", net type);
    if m.cache#?type then (
	verboseLog("gb type found, with hash ", toString hash m.cache#type);
	m.cache#type)
    else if type === gbOnly and computationIsComplete(m, gbWithChg) then (
	verboseLog("gb type found, but fetching ", net gbWithChg);
	getComputation(m,gbWithChg))
    else if ( type === gbOnly or type === gbWithChg ) and computationIsComplete(m, gbWithSyzygy) then (
	verboseLog("gb type found, but fetching ", net gbWithSyzygy);
	getComputation(m, gbWithSyzygy))
    else (
	verboseLog("gb type not found");
	null))

-- handle the Hilbert numerator later, which might be here:
checkHilbertHint = m -> (
    R := ring m;
    -- Needed for using Hilbert functions to aid in Groebner basis computation:
    --    Ring is poly ring over a field (or skew commutative, or quotient ring of such, or both)
    --    Ring is singly graded, every variable is positive
    --    Ring is homogeneous in this grading
    --    Matrix is homogeneous in this grading
    isHomogeneous m
    and degreeLength R === 1
    and (instance(R, PolynomialRing) or isQuotientOf(PolynomialRing, R))
    and isField coefficientRing R
    and (isCommutative R or isSkewCommutative R)
    and all(degree \ generators(R, CoefficientRing => ZZ), deg -> deg#0 > 0)
    )

gbGetHilbertHint := (m, opts) -> (
    if opts.Hilbert =!= null then (
	if ring opts.Hilbert === degreesRing ring m then opts.Hilbert
	else error "expected Hilbert option to be in the degrees ring of the ring of the matrix")
    else if m.cache.?cokernel and m.cache.cokernel.cache.?poincare
    and   checkHilbertHint m then m.cache.cokernel.cache.poincare
    else if m.?generators then (
	g := m.generators;
	if g.cache.?image then (
	    M := g.cache.image;
	    if M.cache.?poincare and checkHilbertHint m
	    then poincare target g - poincare M)))

checkArgGB := m -> (
    R := ring target m;
    if ring source m =!= R then error "expected module map with source and target over the same ring";
    if not isFreeModule target m then error "Groebner bases of subquotient modules not yet implemented";
    if not isFreeModule source m then m = ambient m * generators source m;   -- sigh
    )

degreeToHeft = (R, d) -> (
    if d === null -- null is a default value for HardDegreeLimit
    or d === {}	  -- see stoppingOptionDefaults.DegreeLimit, which is {}
    then {} else {sum apply(heft R, checkListOfIntegers d, times)})

-----------------------------------------------------------------------------
-- gb
-----------------------------------------------------------------------------

gb = method(TypicalValue => GroebnerBasis, Options => gbDefaults)
gb Ideal  := GroebnerBasis => opts -> I -> gb (module I, opts)
gb Module := GroebnerBasis => opts -> M -> (
    if M.?relations then (
	if not M.cache#?"full gens" then M.cache#"full gens" = generators M | relations M;
	gb(M.cache#"full gens", opts, SyzygyRows => numgens source generators M))
    else gb(generators M, opts))
gb Matrix := GroebnerBasis => opts -> m -> (
    checkArgGB m;
    type := gbTypeCode opts;
    G := gbGetPartialComputation(m, type);
    G  = if G =!= null then G else new GroebnerBasis from (m, type, opts);
    if isComputationDone rawStatus1 raw G then return G;
    hf := gbGetHilbertHint(m, opts);
    if hf =!= null then
    value (
	G#"rawGBSetHilbertFunction log" = FunctionApplication {
	    rawGBSetHilbertFunction, (
		G.RawComputation,
		raw hf
		)});
    value (
	G#"rawGBSetStop log" = FunctionApplication {
	    rawGBSetStop, (
		G.RawComputation,
		opts.StopBeforeComputation,
		degreeToHeft(G.ring, opts.DegreeLimit),
		toEngineNat opts.BasisElementLimit,
		toEngineNat opts.SyzygyLimit,
		toEngineNat opts.PairLimit,
		toEngineNat opts.CodimensionLimit,
		toEngineNat opts.SubringLimit,
		toEngineNat opts.StopWithMinimalGenerators,
		{} -- not used, just for resolutions
		)});
    G#"stopping options"    = getSomeOptions(opts, stoppingOptionDefaults);
    G#"computation options" = getSomeOptions(opts, computationOptionDefaults);
    rawStartComputation G.RawComputation;
    m.cache#type = G)

-- introspective functions

gbSnapshot = method()
gbSnapshot Ideal  :=
gbSnapshot Module :=
gbSnapshot Matrix := M -> generators gb(M, StopBeforeComputation => true)

gbRemove = method()
gbRemove Ideal  :=
gbRemove Module := M -> gbRemove generators M
gbRemove Matrix := m -> scan(keys m.cache, o -> if instance(o, GroebnerBasisOptions) then remove(m.cache, o))

-- TODO: what is this?
gbBoolean = method()
gbBoolean Ideal := Ideal => I -> ideal map(ring I, rawGbBoolean(raw compress generators I))

-----------------------------------------------------------------------------
-- helpers for groebnerBasis
-----------------------------------------------------------------------------

-- possible values for Reducer: "Classic", "F4",  (0,1)
-- see 'mgb help logs' for format of the Logs argument.
engineMGB = method(
    Options => {
	"Reducer"        => null,
	"Threads"        => null,
	"SPairGroupSize" => 0,
	"Log"            => ""
	})
engineMGB Matrix := opts -> M -> (
     reducer := (
	 if      opts#"Reducer" === null      then 0
	 else if opts#"Reducer" === "F4"      then 1
	 else if opts#"Reducer" === "Classic" then 0
	 else if instance(opts#"Reducer", ZZ) then opts#"Reducer"
	 else error "Expected \"F4\" or \"Classic\" as reducer type");
     groupsize := if instance(opts#"SPairGroupSize", ZZ) then opts#"SPairGroupSize" else error "expected an integer for SPairGroupSize";
     nthreads  := if opts#"Threads" === null then numTBBThreads 
         else if instance(opts#"Threads",        ZZ) then opts#"Threads"
         else error "expected an integer for number of threads to use";
     logarg    := if instance(opts#"Log",        String) then opts#"Log"            else error "Log expects a string argument, e.g. \"all\" or \"F4\"";
     map(ring M, rawMGB(raw M, reducer, groupsize, nthreads, logarg)))

engineMGBF4 = method(Options => options engineMGB)
engineMGBF4 Ideal := opts -> I -> engineMGB(generators I, opts, "Reducer" => "F4")

-----------------------------------------------------------------------------
-- groebnerBasis
-----------------------------------------------------------------------------
-- TODO: hookify this

groebnerBasis = method(
    TypicalValue => Matrix,
    Options => new OptionTable from {
	Strategy     => null, -- possible values: "MGB", "F4"
	"MGBOptions" => options engineMGB
	})
-- TODO: this doesn't use MGB yet...
groebnerBasis Module := opts -> x -> generators gb x
groebnerBasis Ideal  := opts -> x -> groebnerBasis(generators x, opts)
groebnerBasis Matrix := opts -> x -> (
    R := ring x;
    if opts.Strategy =!= null
    and char R > 0 -- MGB only works over prime finite fields
    and 2^16 > char R -- FIXME: MGB fails for primes larger than 2^16
    and isPolynomialRing R
    and isCommutative R
    -- TODO: this fails for instance in check_4 PushForward
    and instance(coefficientRing R, QuotientRing) -- really: want to say it is a prime field
    then (
	mgbopts := new MutableHashTable from opts#"MGBOptions";
	if opts.Strategy === "F4" then mgbopts#"Reducer" = "F4"
	else if opts.Strategy =!= "MGB" then error "expected Strategy to be \"F4\" or \"MGB\"";
	mgbopts = new OptionTable from mgbopts;
	if gbTrace > 0 then << "-- computing mgb " << opts.Strategy << " " << mgbopts << endl;
	-- use engineMGB
	generators forceGB engineMGB(x, mgbopts))
    else generators gb x)

-----------------------------------------------------------------------------
-- forceGB
-----------------------------------------------------------------------------

forceGB = method(
    TypicalValue => GroebnerBasis,
    Options => {
	MinimalMatrix => null,
	SyzygyMatrix  => null,
	ChangeMatrix  => null
	})

forceGB Matrix := GroebnerBasis => opts -> m -> (
    if not isFreeModule source m then error "expected a free module";
    minmat := if opts.MinimalMatrix =!= null then opts.MinimalMatrix else m;
    chmat  := if opts.ChangeMatrix  =!= null then opts.ChangeMatrix  else id_(source m);
    syzmat := if opts.SyzygyMatrix  =!= null then opts.SyzygyMatrix  else map(target chmat, target chmat, 0);
    nsyz := numgens target chmat;
    if nsyz >= numgens source minmat then nsyz = -1;
    type := gbTypeCode new OptionTable from {
	SyzygyRows      => numgens target syzmat,
	Syzygies        => opts.SyzygyMatrix =!= null,
	ChangeMatrix    => opts.ChangeMatrix =!= null,
	HardDegreeLimit => null
	};
    g := new GroebnerBasis;
    if debugLevel > 5 then registerFinalizer(g, "gb (forceGB)");
    g.ring = ring m;
    g.matrix = Bag {m};
    g.target = target m;
    g.returnCode = 0;
    g.RawComputation = rawGBForce(raw minmat, raw m, raw chmat, raw syzmat);
    m.cache#type = g)

-----------------------------------------------------------------------------
-- markedGB
-----------------------------------------------------------------------------

markedGB = method(
    TypicalValue => GroebnerBasis,
    Options => {
	MinimalMatrix => null,
	SyzygyMatrix  => null,
	ChangeMatrix  => null
	})

markedGB(Matrix, Matrix) := GroebnerBasis => opts -> (leadterms, m) -> (
    if not isFreeModule source m then error "expected a free module";
    if target leadterms =!= target m then error "expected leadterms and elements in the same free module";
    if numgens source m =!= numgens source leadterms
    then error "expected the same number of generators as lead terms";
    minmat := if opts.MinimalMatrix =!= null then opts.MinimalMatrix else m;
    chmat  := if opts.ChangeMatrix  =!= null then opts.ChangeMatrix  else id_(source m);
    syzmat := if opts.SyzygyMatrix  =!= null then opts.SyzygyMatrix  else map(target chmat, target chmat, 0);
    nsyz := numgens target chmat;
    if nsyz >= numgens source minmat then nsyz = -1;
    type := gbTypeCode new OptionTable from {
	SyzygyRows      => numgens target syzmat,
	Syzygies        => opts.SyzygyMatrix =!= null,
	ChangeMatrix    => opts.ChangeMatrix =!= null,
	HardDegreeLimit => null
	};
    g := new GroebnerBasis;
    if debugLevel > 5 then registerFinalizer(g, "gb (markedGB)");
    g.ring = ring m;
    g.matrix = Bag {m};
    g.target = target m;
    g.returnCode = 0;
    g.RawComputation = rawMarkedGB(raw leadterms, raw minmat, raw m, raw chmat, raw syzmat);
    m.cache#type = g)

-----------------------------------------------------------------------------
-- miscellaneous
-----------------------------------------------------------------------------

-- TODO: what is this? it is never used. Should it be removed?
installGroebner = method()

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
