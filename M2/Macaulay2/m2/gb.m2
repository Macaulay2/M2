--		Copyright 1995-2002,2012 by Daniel R. Grayson

-- Keep this in sync with the ComputationStatusCode in Macaulay2/e/engine.h
RawStatusCodes = new HashTable from {
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

Nothing#BeforeEval = x -> (
     -- for internal use only, for general clean up
     runHooks(symbol BeforeEvalHooks,());
     )

flagInhomogeneity = false -- for debugging homogeneous problems, to make sure homogeneity is not lost
addHook(symbol BeforeEvalHooks,() -> flagInhomogeneity = false)

GroebnerBasis = new Type of MutableHashTable
GroebnerBasis.synonym = "Groebner basis"
target GroebnerBasis := g -> g.target
raw GroebnerBasis := G -> G.RawComputation
ring GroebnerBasis := G -> G.ring
status GroebnerBasis := opts -> G -> (
     s := toString RawStatusCodes#(rawStatus1 raw G);
     "status: " | s | "; "|
     (if s === "done" then "S-pairs encountered up to degree " else "all S-pairs handled up to degree ") | toString rawStatus2 raw G
     )
toString GroebnerBasis := net GroebnerBasis := g -> "GroebnerBasis[" | status g | "]"
texMath GroebnerBasis := x -> texMath toString x

checkListOfIntegers := method()
checkListOfIntegers ZZ := t -> {t}
checkListOfIntegers List := t -> (
     if not all(t, i -> class i === ZZ) then error "expected list of integers";
     t)

computationOptionDefaults := new OptionTable from {
     SyzygyRows => infinity,			    -- n_rows_to_keep (-1 if infinity)
     Syzygies => false,				    -- collect_syz parameter
     HardDegreeLimit => null,			    -- use_max_degree and degree_limit
     ChangeMatrix => false,			    -- calculate change of basis matrix, too, for '//' operation
     Algorithm => Inhomogeneous,		    -- Homogeneous (1) or Inhomogeneous (2)
     Strategy => {},				    -- strategy
     GBDegrees => null,				    -- positive integers
     Hilbert => null,				    -- also obtainable from f.cache.cokernel.poincare
     MaxReductionCount => 10			    -- max_reduction_count in gbA.cpp (for a stubborn S-polynomial)
     };

stoppingOptionDefaults = new OptionTable from {
     StopBeforeComputation => false,		    -- stopping condition (always_stop)
     DegreeLimit => {},				    -- stopping condition (degree_limit) (not max_degree)
     BasisElementLimit => infinity,		    -- stopping condition (basis_element_limit)
     SyzygyLimit => infinity,			    -- stopping condition (syzygy_limit) (not for res computations)
     PairLimit => infinity,			    -- stopping condition (pair_limit)
     CodimensionLimit => infinity,		    -- stopping condition (codim_limit) (not for res computations)
     SubringLimit => infinity,			    -- stopping condition (subring_limit) (not for res computations)
     StopWithMinimalGenerators => false		    -- stopping condition (just_min_gens) (not for res computations)
     -- LengthLimit => null -- is only for res computations
     -- StepLimit, maybe
     }


getSomeOptions := (opts,which) -> applyPairs( which, (key,val) -> (key,opts#key) )

gbDefaults = merge(computationOptionDefaults,stoppingOptionDefaults, x -> error "overlap")

computationIsComplete := (f,type) -> f.cache#?type and f.cache#type.?returnCode and f.cache#type.returnCode === 0
getComputation := (f,type) -> f.cache#?type

toEngineNat  := n -> if n === infinity then -1 else n

GroebnerBasisOptions = new Type of OptionTable

gbTypeCode   := opts    -> new GroebnerBasisOptions from { 
     SyzygyRows => if opts.Syzygies or opts.ChangeMatrix then opts.SyzygyRows else 0,
     Syzygies => opts.Syzygies,
     HardDegreeLimit => opts.HardDegreeLimit }
gbOnly       := gbTypeCode new OptionTable from { SyzygyRows => 0       , Syzygies => false, ChangeMatrix => false, HardDegreeLimit => null }
gbWithChg    := gbTypeCode new OptionTable from { SyzygyRows => infinity, Syzygies => false, ChangeMatrix => true , HardDegreeLimit => null }
gbWithSyzygy := gbTypeCode new OptionTable from { SyzygyRows => infinity, Syzygies => true , ChangeMatrix => false, HardDegreeLimit => null }

gbGetSuitable := (f,type) -> (
     if debugLevel == 77 then stderr << "-- gb type requested: " << type << endl;
     if f.cache#?type then (
	  if debugLevel == 77 then stderr << "-- gb type found, gb number " << hash f.cache#type << endl;
	  f.cache#type)
     else if type === gbOnly and computationIsComplete(f,gbWithChg) then (
	  if debugLevel == 77 then stderr << "-- gb type found, but fetching " << gbWithChg << endl;
	  getComputation(f,gbWithChg))
     else if ( type===gbOnly or type===gbWithChg ) and computationIsComplete(f,gbWithSyzygy) then (
	  if debugLevel == 77 then stderr << "-- gb type found, but fetching " << gbWithSyzygy << endl;
	  getComputation(f,gbWithSyzygy))
     else (
	  if debugLevel == 77 then stderr << "-- gb type not found, returning" << endl;
	  null))

engineMGB = method(Options => {"Reducer"=>null, "Threads"=>0, "SPairGroupSize"=>0,"Log"=>""})
  -- possible values for Reducer: "Classic", "F4",  (0,1)
  -- see 'mgb help logs' for format of the Logs argument.
engineMGB Matrix := opts -> (M) -> (
     reducer := if opts#"Reducer" === null then 0
                else if instance(opts#"Reducer", ZZ) then opts#"Reducer"
                else if opts#"Reducer" === "F4" then 1
                else if opts#"Reducer" === "Classic" then 0
                else error ///Expected "F4" or "Classic" as reducer type///;
     spairGroupSize := if instance(opts#"SPairGroupSize", ZZ) then opts#"SPairGroupSize"
                else error "expected an integer for SPairGroupSize";
     nthreads := if instance(opts#"Threads", ZZ) then opts#"Threads"
                else error "expected an integer for number of threads to use";
     log := if instance(opts#"Log", String) then opts#"Log"
                else error "Log expects a string argument, e.g. \"all\" or \"F4\"";
     rawgb := rawMGB(raw M, reducer, spairGroupSize, nthreads, log);
     map(ring M, rawgb)
     )
     
engineMGBF4 = method(Options => options engineMGB)
engineMGBF4 Ideal := opts -> (I) -> engineMGB(I, opts, "Reducer"=>"F4")

gb = method( TypicalValue => GroebnerBasis, Options => gbDefaults )

groebnerBasis = method( TypicalValue => Matrix, Options => new OptionTable from {
        Strategy => null, -- possible values: "MGB", "F4"
        "MGBOptions" => {"Reducer"=>null, "Threads"=>0, "SPairGroupSize"=>0,"Log"=>""}
        })

groebnerBasis Ideal := opts -> x -> groebnerBasis(generators x, opts)
groebnerBasis Matrix := opts -> x -> (
    R := ring x;
    if opts.Strategy =!= null 
      and char R > 0 -- MGB only works over prime finite fields
      and isPolynomialRing R
      and isCommutative R
      and instance(coefficientRing R, QuotientRing) -- really: want to say it is a prime field
      then (
        mgbopts := opts#"MGBOptions";
        if opts.Strategy === "F4" then mgbopts = append(mgbopts, "Reducer"=>"F4")
        else if opts.Strategy =!= "MGB" then error ///expected Strategy to be "F4" or "MGB"///;
        if gbTrace > 0 then << "-- computing mgb " << opts.Strategy << " " << mgbopts << endl;
        -- use rawMGB
        mgbopts = new OptionTable from mgbopts;
        g := engineMGB(x, new OptionTable from mgbopts);
        generators forceGB g
        )
    else
        generators gb(x)
    )
-- This doesn't use MGB yet...
groebnerBasis Module := opts -> x -> generators gb(x)

strategyCodes := new HashTable from { -- must match values in e/engine.h in enum StrategyValues
     LongPolynomial => 1,
     Sort => 2,
     UseSyzygies => 8
     }

processStrategy := (v) -> (
     if class v =!= List then v = {v};
     sum(v, s->(
	       if not strategyCodes#?s then error("unknown strategy encountered");
	       strategyCodes#s)))     

-- undocumented' Faugere
-- undocumented' F4

-- These must match the values in e/comp_gb.cpp

warnexp := () -> stderr << "--warning: gb algorithm requested is experimental" << endl

processAlgorithm := (a,f) -> (
     R := ring f;
     k := ultimate(coefficientRing, R);
     if (a === Homogeneous or a === Homogeneous2) and not isHomogeneous f then error "gb: homogeneous algorithm specified with inhomogeneous matrrix";
     if k === ZZ and a =!= Inhomogeneous then error "gb: only the algorithm 'Inhomogeneous' may be used with base ring ZZ";
     if R.?FlatMonoid and not R.FlatMonoid.Options.Global and a =!= Inhomogeneous then error "gb: only the algorithm 'Inhomogeneous' may be used with a non-global monomial ordering";
     if a === Homogeneous then 1
     else if a === Inhomogeneous then 2
--     else if a === F4 then error "the F4 algorithm option has been replaced by LinearAlgebra"
--     else if a === Faugere then error "the Faugere algorithm option has been replaced by LinearAlgebra"
     else if a === Sugarless then 4
     else if a === Homogeneous2 then 5
     else if a === LinearAlgebra then (warnexp(); 6)
     else if a === Toric then (warnexp(); 7)
     else if a === Test then 8
     else error ("unknown algorithm encountered"))

gb Ideal := GroebnerBasis => opts -> (I) -> gb ( module I, opts )

gb Module := GroebnerBasis => opts -> (M) -> (
     if M.?relations 
     then (
	  f := (
	       if M.cache#?"full gens" 
	       then M.cache#"full gens"
	       else M.cache#"full gens" = generators M|relations M);
	  gb(f, opts, SyzygyRows => numgens source generators M))
     else gb(generators M, opts))

	  -- handle the Hilbert numerator later, which might be here:
	  -- 

checkHilbertHint = f -> (
     R := ring f;
     -- Needed for using Hilbert functions to aid in Groebner basis computation:
     --    Ring is poly ring over a field (or skew commutative, or quotient ring of such, or both)
     --    Ring is singly graded, every variable is positive
     --    Ring is homogeneous in this grading
     --    Matrix is homogeneous in this grading
     isHomogeneous f
     and degreeLength R === 1
     and (instance(R,PolynomialRing) or isQuotientOf(PolynomialRing, R))
     and isField coefficientRing R
     and (isCommutative R or isSkewCommutative R)
     and all(degree \ generators(R, CoefficientRing => ZZ), deg -> deg#0 > 0)
     )

gbGetHilbertHint := (f,opts) -> (
     if opts.Hilbert =!= null then (
	  if ring opts.Hilbert =!= degreesRing ring f then error "expected Hilbert option to be in the degrees ring of the ring of the matrix";
	  opts.Hilbert)
     else if f.cache.?cokernel and f.cache.cokernel.cache.?poincare and checkHilbertHint f then f.cache.cokernel.cache.poincare
     else (
	  if f.?generators then (
	       g := f.generators.cache;
	       if g.?image then (
		    g = g.image.cache;
	            if g.?poincare and checkHilbertHint f then  poincare target f.generators - g.poincare))))

ifSomething := method()
ifSomething(Thing  ,Function) := (x,f) -> f x
ifSomething(Nothing,Function) := (x,f) -> null

elseSomething := method()
elseSomething(Thing  ,Function) := (x,f) -> x
elseSomething(Nothing,Function) := (x,f) -> f()

newGB := (f,type,opts) -> (
     if flagInhomogeneity then (
	  if not isHomogeneous f then error "internal error: gb: inhomogeneous matrix flagged";
	  );
     G := new GroebnerBasis;
     if debugLevel > 5 then (
	  registerFinalizer(G,"gb (newGB)");
	  );
     G.matrix = Bag{f};
     R := G.ring = ring f;
     G.target = target f;
     G.RawComputation = rawGB(
	  raw f,
	  type.Syzygies,
	  toEngineNat type.SyzygyRows,
	  checkListOfIntegers(
	       if opts.GBDegrees === null 
	       then if opts.Algorithm === LinearAlgebra
	       then (
		    heft := (options R).Heft;
		    apply(degrees R, d -> dotprod(d, heft)))
	       else {} 
	       else opts.GBDegrees
	       ),
	  opts.HardDegreeLimit =!= computationOptionDefaults.HardDegreeLimit,
	  if opts.HardDegreeLimit =!= computationOptionDefaults.HardDegreeLimit then first degreeToHeft(R,opts.HardDegreeLimit) else 0,
	  processAlgorithm(opts.Algorithm,f),
	  processStrategy opts.Strategy,
	  opts.MaxReductionCount
	  );
     f.cache#type = G;			  -- do this last, in case of an interrupt
     if debugLevel == 77 then stderr << "-- new gb computed of type " << type << " and number " << hash G << " with options " << opts << endl;
     G)

checkArgGB := f -> (
     R := ring target f;
     if ring source f =!= R then error "expected module map with source and target over the same ring";
     if not isFreeModule target f then error "Groebner bases of subquotient modules not yet implemented";
     if not isFreeModule source f then f = ambient f * generators source f;   -- sigh
     )

recordOptions := (G,opts) -> (
     G#"stopping options" = getSomeOptions(opts,stoppingOptionDefaults);
     G#"computation options" = getSomeOptions(opts,computationOptionDefaults);
     )

degreeToHeft = (R,d) -> (
     if d === null	 -- null is a default value for HardDegreeLimit
     or d === {}	 -- see stoppingOptionDefaults.DegreeLimit, which is {}
     then {}
     else {sum apply(heft R,checkListOfIntegers d,times)})

gb Matrix := GroebnerBasis => opts -> (f) -> (
     checkArgGB f;
     type := gbTypeCode opts;
     G := elseSomething( gbGetSuitable(f,type), () -> newGB(f,type,opts) );
     if "done" === RawStatusCodes#(rawStatus1 raw G) then return G;
     ifSomething( gbGetHilbertHint(f,opts), 
	  hil -> (
	       log := FunctionApplication { rawGBSetHilbertFunction, (G.RawComputation,raw hil) };
	       G#"rawGBSetHilbertFunction log" = log;
	       value log;
	       ));
     log := FunctionApplication { rawGBSetStop, (
	       G.RawComputation,
	       opts.StopBeforeComputation,
	       degreeToHeft(ring f, opts.DegreeLimit),
	       toEngineNat opts.BasisElementLimit,
	       toEngineNat opts.SyzygyLimit,
	       toEngineNat opts.PairLimit,
	       toEngineNat opts.CodimensionLimit,
	       toEngineNat opts.SubringLimit,
	       toEngineNat opts.StopWithMinimalGenerators,
	       {}						    -- not used, just for resolutions
	       )};
     G#"rawGBSetStop log" = log;
     value log;
     recordOptions(G,opts);
     rawStartComputation G.RawComputation;
     f.cache#type = G;
     G)

notForSyz := set { Syzygies, ChangeMatrix, CodimensionLimit, Hilbert, StopWithMinimalGenerators, SubringLimit }
syz = method(
     Options => select(pairs options gb, (k,v) -> not notForSyz#?k)
     )

rawsort := m -> rawSubmatrix(m,rawSortColumns(m,1,1))

generators      GroebnerBasis := Matrix => opts -> (G) -> map(target unbag G.matrix,,rawGBGetMatrix G.RawComputation)
mingens         GroebnerBasis := Matrix => opts -> (G) -> map(target unbag G.matrix,,rawsort rawGBMinimalGenerators G.RawComputation)
                -- rawGBMinimalGenerators doesn't sort its columns, so we do that here
syz             GroebnerBasis := Matrix => opts -> (G) -> map(ring G, rawsort rawGBSyzygies G.RawComputation)
                -- rawGBSyzygies doesn't sort its columns, so we do that here
getChangeMatrix GroebnerBasis := Matrix =>            (G) -> map(ring G, rawGBChangeOfBasis G.RawComputation)

forceGB = method(
     TypicalValue => GroebnerBasis,
     Options => {
          MinimalMatrix => null,
	  SyzygyMatrix => null,
	  ChangeMatrix => null
	  }
     )

forceGB Matrix := GroebnerBasis => options -> (f) -> (
     if not isFreeModule source f then error "expected a free module";
     minmat := if options.MinimalMatrix === null
               then f
               else options.MinimalMatrix;
     changemat := if options.ChangeMatrix === null
               then id_(source f)
               else options.ChangeMatrix;
     syzmat := if options.SyzygyMatrix === null
               then map(target changemat, target changemat, 0)
               else options.SyzygyMatrix;
     nsyz := numgens target changemat;
     if nsyz >= numgens source minmat then nsyz = -1;
     type := gbTypeCode new OptionTable from { SyzygyRows => numgens target syzmat,
	                                       Syzygies => options.SyzygyMatrix =!= null, 
					       ChangeMatrix => options.ChangeMatrix =!= null, 
					       HardDegreeLimit => null };
     g := new GroebnerBasis;
     if debugLevel > 5 then (
	  registerFinalizer(g,"gb (forceGB)");
	  );
     g.matrix = Bag {f};
     g.ring = ring f;
     g.target = target f;
     g.returnCode = 0;
     g.RawComputation = rawGBForce(raw minmat, raw f, raw changemat, raw syzmat);
     f.cache#type = g;
     g)

-- Marked GB --
markedGB = method(
     TypicalValue => GroebnerBasis,
     Options => {
          MinimalMatrix => null,
	  SyzygyMatrix => null,
	  ChangeMatrix => null
	  }
     )

markedGB(Matrix,Matrix) := GroebnerBasis => options -> (leadterms,f) -> (
     if not isFreeModule source f then error "expected a free module";
     if target leadterms =!= target f then error "expected leadterms and elements in the same free module";
     if numgens source f =!= numgens source leadterms
     then error "expected the same number of generators as lead terms";
     minmat := if options.MinimalMatrix === null
               then f
               else options.MinimalMatrix;
     changemat := if options.ChangeMatrix === null
               then id_(source f)
               else options.ChangeMatrix;
     syzmat := if options.SyzygyMatrix === null
               then map(target changemat, target changemat, 0)
               else options.SyzygyMatrix;
     nsyz := numgens target changemat;
     if nsyz >= numgens source minmat then nsyz = -1;
     type := gbTypeCode new OptionTable from { SyzygyRows => numgens target syzmat,
	                                       Syzygies => options.SyzygyMatrix =!= null, 
					       ChangeMatrix => options.ChangeMatrix =!= null, 
					       HardDegreeLimit => null };
     g := new GroebnerBasis;
     if debugLevel > 5 then (
	  registerFinalizer(g,"gb (forceGB)");
	  );
     g.matrix = Bag {f};
     g.ring = ring f;
     g.target = target f;
     g.returnCode = 0;
     g.RawComputation = rawMarkedGB(raw leadterms, raw minmat, raw f, raw changemat, raw syzmat);
     f.cache#type = g;
     g)
---------------

Matrix // GroebnerBasis := Matrix => (n,g) -> quotient(n,g)
quotient(Matrix,GroebnerBasis) := Matrix => opts -> (n,g) -> (
     -- this gb might not be one with change of basis matrix attached...
     -- so it is best for the user not to use it
     R := ring g;
     (rem,quot,cplt) := rawGBMatrixLift(raw g, raw n);
     map(R, quot))

quotientRemainder(Matrix,GroebnerBasis) := Matrix => (n,g) -> (
     -- this gb might not be one with change of basis matrix attached...
     -- so it is best for the user not to use it
     R := ring g;
     (rem,quo,cplt) := rawGBMatrixLift(raw g, raw n);
     (map(R, quo),map(R, rem)))

RingElement // GroebnerBasis := Matrix => (r,g) -> (r * id_(target g)) // g

remainder(Matrix,GroebnerBasis) :=
Matrix % GroebnerBasis := Matrix => (n,g) -> (
     R := ring n;
     map(target n,, rawGBMatrixRemainder(raw g, raw n)));

RingElement % GroebnerBasis := RingElement =>
Number % GroebnerBasis := (r,g) -> ((promote(r,ring g) * id_(target g)) % g)_(0,0)

leadTerm GroebnerBasis := (g) -> map(ring g, rawGBGetLeadTerms(raw g,-1))

-- new functions from Mike, needing a bit of development

installHilbertFunction = method()
installHilbertFunction(Module,RingElement) := (M,hf) -> (
     -- we need to place hf into the degree ring of M.
     hf = substitute(hf,degreesRing M);
     M.cache.poincare = hf;
     )
installHilbertFunction(Ideal,RingElement) := (I,hf) -> installHilbertFunction(comodule I,hf)
installHilbertFunction(Matrix,RingElement) := (f,hf) -> installHilbertFunction(cokernel f,hf)

installGroebner = method()

gbSnapshot = method()
gbSnapshot Module := gbSnapshot Ideal := gbSnapshot Matrix := M -> generators gb(M,StopBeforeComputation => true)

gbRemove = method()
gbRemove Module := gbRemove Ideal := (M) -> (
     c := (generators M).cache;
     scan(keys c, o -> if instance(o,GroebnerBasisOptions) then remove(c,o));
     )
gbRemove Matrix := (M) -> gbRemove generators M

-- 
gbBoolean = method()
gbBoolean Ideal := Ideal => I -> ideal map(ring I, rawGbBoolean(raw compress generators I))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
