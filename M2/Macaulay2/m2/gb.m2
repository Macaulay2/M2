--		Copyright 1995-2002 by Daniel R. Grayson

load "statuscodes.m2"

GroebnerBasis = new Type of MutableHashTable
GroebnerBasis.synonym = "Groebner basis"
target GroebnerBasis := g -> g.target
raw GroebnerBasis := G -> G.RawComputation
status GroebnerBasis := opts -> G -> (
     s := toString RawStatusCodes#(rawStatus1 raw G);
     "status: " | s | "; "|
     (if s === "done" then "S-pairs encountered up to degree " else "all S-pairs handled up to degree ") | toString rawStatus2 raw G
     )
toString GroebnerBasis := net GroebnerBasis := g -> "GroebnerBasis[" | status g | "]"

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
     Hilbert => null				    -- also obtainable from f.cache.cokernel.poincare
     };

stoppingOptionDefaults := new OptionTable from {
     StopBeforeComputation => false,		    -- stopping condition (always_stop)
     DegreeLimit => {},				    -- stopping condition (stop_after_degree and degree_limit) (not max_degree)
     BasisElementLimit => infinity,		    -- stopping condition (basis_element_limit)
     SyzygyLimit => infinity,			    -- stopping condition (syzygy_limit) (not for res computations)
     PairLimit => infinity,			    -- stopping condition (pair_limit)
     CodimensionLimit => infinity,			    -- stopping condition (codim_limit) (not for res computations)
     SubringLimit => infinity,			    -- stopping condition (subring_limit) (not for res computations)
     StopWithMinimalGenerators => false		    -- stopping condition (just_min_gens) (not for res computations)
     -- LengthLimit => null -- is only for res computations
     -- StepLimit, maybe
     }


getSomeOptions := (opts,which) -> applyPairs( which, (key,val) -> (key,opts#key) )

gbDefaults := merge(computationOptionDefaults,stoppingOptionDefaults, x -> error "overlap")

computationIsComplete := (f,type) -> f.cache#?type and f.cache#type.?returnCode and f.cache#type.returnCode === 0
getComputation := (f,type) -> f.cache#?type

toEngineNat  := n -> if n === infinity then -1 else n

gbTypeCode   := opts    -> new OptionTable from { 
     SyzygyRows => if opts.Syzygies or opts.ChangeMatrix then opts.SyzygyRows else 0,
     Syzygies => opts.Syzygies,
     HardDegreeLimit => opts.HardDegreeLimit }
gbOnly       := gbTypeCode new OptionTable from { SyzygyRows => 0       , Syzygies => false, ChangeMatrix => false, HardDegreeLimit => null }
gbWithChg    := gbTypeCode new OptionTable from { SyzygyRows => infinity, Syzygies => false, ChangeMatrix => true , HardDegreeLimit => null }
gbWithSyzygy := gbTypeCode new OptionTable from { SyzygyRows => infinity, Syzygies => true , ChangeMatrix => false, HardDegreeLimit => null }

gbGetSuitable := (f,type) -> (
     if f.cache#?type then f.cache#type
     else if type === gbOnly and computationIsComplete(f,gbWithChg) then getComputation(f,gbWithChg)
     else if ( type===gbOnly or type===gbWithChg ) and computationIsComplete(f,gbWithSyzygy) then getComputation(f,gbWithSyzygy)
     )

gb = method( TypicalValue => GroebnerBasis, Options => gbDefaults )

strategyCodes := new HashTable from { -- must match values in e/engine.h
     LongPolynomial => 1,
     Sort => 2,
     UseHilbertFunction => 4
     }

processStrategy := (v) -> (
     if class v =!= List then v = {v};
     sum(v, s->(
	       if not strategyCodes#?s then error("unknown strategy encountered");
	       strategyCodes#s)))     

processAlgorithm := (a,f) -> (
     if (a === Homogeneous or a === Homogeneous2) and not isHomogeneous f then error "gb: homogeneous algorithm specified with inhomogeneous matrrix";
     if a === Homogeneous then 1
     else if a === Inhomogeneous then 2
     else if a === LinearAlgebra or a === Faugere then 3
     else if a === Sugarless then 4
     else if a === Homogeneous2 then 5
     else if a === F4 then 6
     else error ("unknown algorithm encountered"))

gb Ideal := GroebnerBasis => options -> (I) -> gb ( module I, options )

gb Module := GroebnerBasis => options -> (M) -> (
     if M.?relations 
     then (
	  notImplemented();
	  -- provisional
	  m := generators M;
	  n := relations M;
	  gb (m|n, 
	       options,
	       -- ChangeMatrix => true,
	       -- Syzygies => true,
	       SyzygyRows => numgens source m))
     else gb(generators M, options))

	  -- handle the Hilbert numerator later, which might be here:
	  -- 

gbGetHilbertHint := (f,opts) -> (
     if opts.Hilbert =!= null then opts.Hilbert
     else if f.cache.?cokernel and f.cache.cokernel.cache.?poincare then f.cache.cokernel.cache.poincare
     )

ifSomething := method()
ifSomething(Thing  ,Function) := (x,f) -> f x
ifSomething(Nothing,Function) := (x,f) -> null

elseSomething := method()
elseSomething(Thing  ,Function) := (x,f) -> x
elseSomething(Nothing,Function) := (x,f) -> f()

newGB := (f,type,opts) -> (
     G := new GroebnerBasis;
     G.matrix = Bag{f};
     G.ring = ring f;
     G.target = target f;
     G.RawComputation = rawGB (
	  raw f,
	  type.Syzygies,
	  toEngineNat type.SyzygyRows,
	  checkListOfIntegers {},			    -- later: gb degree list
	  opts.HardDegreeLimit =!= computationOptionDefaults.HardDegreeLimit,
	  if opts.HardDegreeLimit =!= computationOptionDefaults.HardDegreeLimit then opts.HardDegreeLimit else 0,
	  processAlgorithm(opts.Algorithm,f),
	  processStrategy opts.Strategy
	  );
     f.cache#type = G;			  -- do this last, in case of an interrupt
     G)

setStopGB := (G,opts) -> (
     rawGBSetStop (G.RawComputation,
	  opts.StopBeforeComputation,
	  opts.DegreeLimit =!= stoppingOptionDefaults.DegreeLimit,
	  if opts.DegreeLimit =!= stoppingOptionDefaults.DegreeLimit then checkListOfIntegers opts.DegreeLimit else {},
     	  toEngineNat opts.BasisElementLimit,
     	  toEngineNat opts.SyzygyLimit,
     	  toEngineNat opts.PairLimit,
     	  toEngineNat opts.CodimensionLimit,
     	  toEngineNat opts.SubringLimit,
     	  toEngineNat opts.StopWithMinimalGenerators,
	  {}						    -- not used, just for resolutions
	  );
     )

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

gb Matrix := GroebnerBasis => opts -> (f) -> (
     checkArgGB f;
     type := gbTypeCode opts;
     G := elseSomething( gbGetSuitable(f,type), () -> newGB(f,type,opts) );
     ifSomething( gbGetHilbertHint(f,opts), hil -> rawGBSetHilbertFunction(G.RawComputation,raw hil) );
     setStopGB(G,opts);
     recordOptions(G,opts);
     rawStartComputation G.RawComputation;
     f.cache#type = G;
     G)

syz = method(Options => options gb)			    -- we seem to be ignoring these options!!

rawsort := m -> rawExtractColumns(m,rawSortColumns(m,1,1))

generators      GroebnerBasis := Matrix =>            (G) -> map(target unbag G.matrix,,rawGBGetMatrix G.RawComputation)
mingens         GroebnerBasis := Matrix => options -> (G) -> map(target unbag G.matrix,,rawsort rawGBMinimalGenerators G.RawComputation)
                -- rawGBMinimalGenerators doesn't sort its columns, so we do that here
syz             GroebnerBasis := Matrix => options -> (G) -> map(ring G, rawsort rawGBSyzygies G.RawComputation)
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
     g.matrix = Bag {f};
     g.ring = ring f;
     g.target = target f;
     g.returnCode = 0;
     g.RawComputation = rawGBForce(raw minmat, raw f, raw changemat, raw syzmat);
     f.cache#type = g;
     g)

Matrix // GroebnerBasis := Matrix => (n,g) -> quotient(n,g)
quotient(Matrix,GroebnerBasis) := Matrix => opts -> (n,g) -> (
     -- this gb might not be one with change of basis matrix attached...
     -- so it is best for the user not to use it
     R := ring g;
     map(R, last rawGBMatrixLift(raw g, raw n)))

quotientRemainder(Matrix,GroebnerBasis) := Matrix => (n,g) -> (
     -- this gb might not be one with change of basis matrix attached...
     -- so it is best for the user not to use it
     R := ring g;
     (rem,quo) := rawGBMatrixLift(raw g, raw n);
     (map(R, quo),map(R, rem)))

RingElement // GroebnerBasis := Matrix => (r,g) -> (r * id_(target g)) // g

Matrix % GroebnerBasis := Matrix => (n,g) -> (
     R := ring n;
     map(target n,, rawGBMatrixRemainder(raw g, raw n)));

RingElement % GroebnerBasis := RingElement =>
ZZ % GroebnerBasis := (r,g) -> ((r * id_(target g)) % g)_(0,0)

leadTerm GroebnerBasis := (g) -> map(ring g, rawGBGetLeadTerms(raw g,-1))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
