--		Copyright 1995-2002 by Daniel R. Grayson

load "statuscodes.m2"

GroebnerBasis = new Type of MutableHashTable
GroebnerBasis.synonym = "Groebner basis"
raw GroebnerBasis := G -> G.RawComputation
status GroebnerBasis := opts -> G -> status raw G
toString GroebnerBasis := g -> toString new FunctionApplication from { gb, unbag g.matrix }
net GroebnerBasis := g -> net gens g

summary GroebnerBasis := g -> (sendgg(ggPush g, ggstats);)

runGB := (G,ggcmds) -> (
     sendgg(ggPush G, ggcmds);
     sendgg ggcalc;
     G.returnCode = eePopInt();
     )

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

strategyCodes := new HashTable from {			    -- new: ok
     Sort => 16,
     LongPolynomial => 8
     }

processStrategy := (v) -> (
     if class v =!= List then v = {v};
     sum(v, s->(
	       if not strategyCodes#?s then error("unknown strategy encountered");
	       strategyCodes#s)))     

processAlgorithm := (a) -> (
     if a === Homogeneous then 1
     else if a === Inhomogeneous then 2
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
     else if f.cache.?cokernel and f.cache.cokernel.?poincare then f.cache.cokernel.poincare
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
     G.RawComputation = rawGB(
	  raw f,
	  type.Syzygies,
	  toEngineNat type.SyzygyRows,
	  checkListOfIntegers {},			    -- later: gb degree list
	  opts.HardDegreeLimit =!= null,
	  if opts.HardDegreeLimit =!= null then opts.HardDegreeLimit else 0,
	  processAlgorithm opts.Algorithm,
	  processStrategy opts.Strategy
	  );
     f.cache#type = G;			  -- do this last, in case of an interrupt
     G)

setStopGB := (G,opts) -> (
     rawGBSetStop(G.RawComputation,
	  opts.StopBeforeComputation,
	  opts.DegreeLimit =!= null,
	  if opts.DegreeLimit =!= null then checkListOfIntegers opts.DegreeLimit else {},
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
     if isPolynomialRing R and not (isField coefficientRing R or coefficientRing R === ZZ) then error "expected coefficient ring to be ZZ or a field"; -- remove later
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
     f.cache.type = G;
     G)

syz = method(Options => options gb)			    -- we seem to be ignoring these options!!
generators      GroebnerBasis := Matrix =>            (G) -> map(target unbag G.matrix,,rawGBGetMatrix(G.RawComputation,false))
mingens         GroebnerBasis := Matrix => options -> (G) -> map(target unbag G.matrix,,rawGBGetMatrix(G.RawComputation,true))
syz             GroebnerBasis := Matrix => options -> (G) -> notImplemented() -- rawGBSyzygies
getChangeMatrix GroebnerBasis := Matrix =>            (G) -> notImplemented() -- rawGBChangeOfBasis

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
               then map(target changemat, 0)
               else options.SyzygyMatrix;
     nsyz := numgens target changemat;
     if nsyz >= numgens source minmat then nsyz = -1;
     type := {
	  options.SyzygyMatrix =!= null,
          nsyz
	  };
     g := new GroebnerBasis;
     g.matrix = f;
     g.ring = ring f;
     g.target = target f;
     g.returnCode = 0;
     g.handle = newHandle(
          ggPush minmat,
          ggPush f, 
          ggPush changemat, 
          ggPush syzmat, 
          gggb);
     f.cache#type = g;
     g)

Matrix // GroebnerBasis := Matrix => (n,g) -> (
     -- this gb might not be one with change of basis matrix attached...
     -- so it is best for the user not to use it
     R := ring g;
     if R =!= ring n then error "expected matrix over the same ring";
     sendgg(ggPush g, ggPush n, ggreduce, ggPush 1, ggpick, ggpop);
     getMatrix R)
RingElement // GroebnerBasis := Matrix => (r,g) -> (r * id_(target g)) // g

Matrix % GroebnerBasis := Matrix => (n,g) -> (
     R := ring n;
     map(target n,, rawGBMatrixRemainder(raw g, raw n)));

RingElement % GroebnerBasis := RingElement =>
ZZ % GroebnerBasis := (r,g) -> ((r * id_(target g)) % g)_(0,0)

GroebnerBasis == GroebnerBasis := (g,h) -> (
     ring g === ring h
     and (
     	  sendgg(ggPush g, ggPush h, ggisequal);
     	  eePopBool()))

-- Auto-reduction
autoReduce = method()
autoReduce Matrix := (m) -> (
     sendgg(ggPush m, ggautoreduce);
     getMatrix ring m)

///
TEST
R = ZZ/101[a..f]
m = matrix{{a^2-b-c,b^2-c-d,c^4-b^3-2*d}}
autoReduce m
///     

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
