--------------------------------------------------------------------------
-- PURPOSE : Compute localization of rings with respect to prime ideals.
--
-- PROGRAMMERs : Localization at a maximal ideal was implemented by Mike Stillman and David Eisenbud.
--               Support for prime ideals added by Mike Stillman and Mahrud Sayrafi.
--
-- UPDATE HISTORY : created 1      July 2008 as packages/LocalRings.m2
-- 	     	    updated 4   January 2017 as packages/Localization.m2
-- 	     	    updated 12 November 2017 as m2/localring.m2
--
-- TODO : Add ZZ#(localRing, ideal(0)) := QQ
--        Localization at arbitrary multiplicatively closed sets
--        Localization at a function
--        Implement a prime filtration
--        symbol%, symbol// for the case when f != g * (f // g)
--        leadCoefficient, perhaps using the part with zero filtration?
--
-- NOTE : Elementary operations in a local ring are defined in e/localring.hpp and handled here.
--        Most operations (syz, res, mingens, etc.) are in packages/LocalRings.m2 and rely heavily
--        on liftUp in packages/LocalRings.m2 and on pruneComplex from packages/PruneComplex.m2
--        Legacy code from 2008 is stored in packages/LocalRings/legacy.m2 and is still loaded.
---------------------------------------------------------------------------

importFrom_Core {
    "getAttribute", "hasAttribute", "ReverseDictionary", "indexStrings", "indexSymbols",
    "generatorExpressions", "generatorSymbols", "commonEngineRingInitializations",
    "rawFraction", "rawNumerator", "rawDenominator", "rawIsLocalUnit", "rawLocalRing" }

-- defined in m2/localring.m2
--LocalRing = new Type of EngineRing
LocalRing.synonym = "local ring"
LocalRing#{Standard,AfterPrint} = RP -> (
     << endl << concatenate(interpreterDepth:"o") << lineNumber << " : "; -- standard template
     << "LocalRing, maximal " << RP.maxIdeal << endl;
     )

localRing = method(TypicalValue => LocalRing)
       describe LocalRing := RP -> Describe (expression localRing) (expression last RP.baseRings, expression RP.maxIdeal)
     expression LocalRing := RP -> if hasAttribute(RP, ReverseDictionary) then expression getAttribute(RP, ReverseDictionary) else describe RP
toExternalString LocalRing:= RP -> toString describe RP
coefficientRing LocalRing := RP -> coefficientRing ring RP.maxIdeal
  isWellDefined LocalRing := RP -> isPrime RP.maxIdeal
  isCommutative LocalRing := RP -> isCommutative ring RP.maxIdeal -- FIXME make sure this is correct
   degreeLength LocalRing := RP -> degreeLength ring RP.maxIdeal
   presentation LocalRing := RP -> map(RP^1, RP^0, 0)
     generators LocalRing := opts -> RP -> (if opts.CoefficientRing === ring RP.maxIdeal
                                            then generators(ring RP.maxIdeal) / (r -> promote(r, RP))
				            else generators(ring RP.maxIdeal, opts) / (r -> promote(r, RP)))
      precision LocalRing := RP -> precision ring RP.maxIdeal
        degrees LocalRing := RP -> degrees ring RP.maxIdeal
        numgens LocalRing := RP -> numgens ring RP.maxIdeal
           frac LocalRing := RP -> frac ring RP.maxIdeal
           char LocalRing := RP -> char ring RP.maxIdeal
            dim LocalRing := RP -> codim RP.maxIdeal
            max LocalRing := RP -> promote(RP.maxIdeal, RP)

-- TODO: use hooks?
--localRing(LocalRing, Ideal) := (R, P) -> (...)
localRing(Ring, Ideal) := (R, P) -> (
    if R#?(localRing, P) then R#(localRing, P) else error "No method found")
localRing(EngineRing, Ideal) := (R, P) ->
    if isField R then R else if R#?(localRing,P) then R#(localRing,P) else (
        if ring P =!= R then error "expected ideal of the same ring";
        RP := new LocalRing from rawLocalRing(raw R, raw gb P);
        RP.baseRings = append(R.baseRings, R);
        R#(localRing,P) = RP;
        RP.localRing    = RP;
        RP.maxIdeal     =  P;
        commonEngineRingInitializations RP;
	RP.residueMap   = map(frac(R/P), RP, vars R % P);
         expression RP := r -> expression numerator r / expression denominator r;
           toString RP := r -> toString expression r;
           baseName RP := r -> if denominator r == 1 then baseName numerator r
                               else error "expected a generator";
    leadCoefficient RP := r -> error "not implemented for local rings";
        denominator RP := r -> new R from rawDenominator raw r;
          numerator RP := r -> new R from rawNumerator   raw r;
             isUnit RP := r -> rawIsLocalUnit raw r;
             factor RP := r -> factor numerator r / factor denominator r;
                net RP := r -> net expression r;
                RP%RP  := (r,s) -> error "not implemented for local rings";
                RP/RP  :=
       fraction(RP,RP) := (r,s) -> if isUnit s then r * s^-1
                                   else if s != 0 then new frac R from
                                     rawFraction(raw frac R,
                                         raw (numerator r * denominator s),
                                         raw (denominator r * numerator s))
                                   else error "expected a unit or nonzero element in the denominator";
        if R.?generators           then RP.generators = apply(R.generators, r -> promote(r, RP));
        if R.?generatorSymbols     then RP.generatorSymbols     = R.generatorSymbols;
        if R.?generatorExpressions then RP.generatorExpressions = R.generatorExpressions;
        if R.?indexSymbols then RP.indexSymbols = applyValues(R.indexSymbols, r -> promote(r,RP));
        if R.?indexStrings then RP.indexStrings = applyValues(R.indexStrings, r -> promote(r,RP));
        RP
        )
