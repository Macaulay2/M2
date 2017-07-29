-- -*- coding: utf-8 -*-
--------------------------------------------------------------------------
-- PURPOSE : Compute localization of rings with respect to prime ideals.
--
-- PROGRAMMERs : Localization at a maximal ideal was implemented by Mike Stillman and David Eisenbud.
--               Support for prime ideals added by Mike Stillman and Mahrud Sayrafi.
--
-- UPDATE HISTORY : created 1      July 2008 as LocalRings.m2
-- 	     	    updated 4   January 2017
--                  updated 17 February 2017 to move legacy code here
--
-- TODO : Localization at a function
--        Localization at arbitrary multiplicatively closed sets
--
-- NOTE : Main definition of a local ring is in Localization.m2
--        Many operations rely heavily on PruneComplex.m2
---------------------------------------------------------------------------
newPackage(
    "Localization",
    Version => "2", 
    Date => "January 14, 2017",
    Authors => {
      {Name => "David Eisenbud", Email => "de@msri.org",           HomePage => "http://www.msri.org/~de/"},
      {Name => "Mike Stillman",  Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike/"},
      {Name => "Mahrud Sayrafi", Email => "mahrud@berkeley.edu",   HomePage => "http://ocf.berkeley.edu/~mahrud/"}
      },
    Headline => "Local rings at prime ideals",
    DebuggingMode => false,
    AuxiliaryFiles => true
    )

export {
    "LocalRing",
    "localRing",
    "MaximalIdeal",
    --
    "setMaxIdeal",
    "localComplement",
    "localsyz",
    "localMingens",
    "localModulo",
    "localPrune",
    "localResolution",
    "residueMap",
    "maxIdeal"
    }


debug Core

LocalRing = new Type of EngineRing
LocalRing.synonym = "Local ring"

--FIXME: isField RP should not be always true
-- ASK MIKE #4: Clarification: what's the difference between the two methods (localRing, Ring, Ideal) and (localRing, EngineRing, Ideal)?
localRing = method(TypicalValue => LocalRing)
localRing(Ring, Ideal) := (R, P) -> (if R#(localRing, P) then R#(localRing, P) else error "what?!") -- FIXME how does this work?

      localRing LocalRing := identity
     expression LocalRing := RP -> (expression localRing) (expression last RP.baseRings, expression max RP)
       describe LocalRing := RP -> net expression RP
       toString LocalRing := RP -> (if hasAttribute(RP, ReverseDictionary)
                                        then toString getAttribute(RP, ReverseDictionary)
                                        else "localRing(" | toString last RP.baseRings | ", " | toString RP.MaximalIdeal | ")")
            net LocalRing := RP -> (if hasAttribute(RP, ReverseDictionary)
                                        then toString getAttribute(RP, ReverseDictionary)
                                        else net new FunctionApplication from { localRing, last RP.baseRings, RP.MaximalIdeal})
coefficientRing LocalRing := RP -> coefficientRing last RP.baseRings
   degreeLength LocalRing := RP -> degreeLength last RP.baseRings
     generators LocalRing := opts ->
                             RP -> (if opts.CoefficientRing =!= RP
                                        then generators(last RP.baseRings, opts) / (r -> promote(r, RP)) else {})
      precision LocalRing := RP -> precision last RP.baseRings
        degrees LocalRing := RP -> degrees last RP.baseRings
        numgens LocalRing := RP -> numgens last RP.baseRings
           char LocalRing := RP -> char last RP.baseRings
            dim LocalRing := RP -> codim RP.MaximalIdeal
            max LocalRing := RP -> RP.MaximalIdeal

localRing(EngineRing, Ideal) := (R, P) -> 
    if isField R then R else if R#?(localRing,P) then R#(localRing,P) else (
        if ring P =!= R then error "expected ideal of the same ring";
        RP := new LocalRing from raw frac R;
        RP.baseRings = append(R.baseRings, R);
        R#(localRing,P) = RP;
        RP.localRing    = RP;
        RP.MaximalIdeal =  P;
        commonEngineRingInitializations RP;
         expression RP := r -> expression numerator r / expression denominator r;
           toString RP := r -> toString expression r;
           baseName RP := r -> if denominator r == 1
                                   then baseName numerator r else error "expected a generator";
--FIXME
    leadCoefficient RP := r -> error "not implemented for local rings";
        denominator RP := r -> new R from rawDenominator raw r;
          numerator RP := r -> new R from rawNumerator   raw r;
             isUnit RP := r -> numerator r % P != 0;
             factor RP := r -> factor numerator r / factor denominator r;
                net RP := r -> net expression r;
--FIXME
                RP%RP  := (r,s) -> error "not implemented for local rings";
                RP/RP  :=
       fraction(RP,RP) := (r,s) -> if (numerator s) % P != 0 
                                       then r // s else error "expected a unit in denominator";
--FIXME MIKE #5: How to fix this?
       fraction(R, R)  := (r,s) -> if s % P != 0
                                       then new RP from rawFraction(RP.RawRing, raw r, raw s)
                                       else error "expected a unit in denominator";

--FIXME MIKE #6: Why two definitions for RP.generators?
        RP.generators = apply(generators R, r -> promote(r, RP));
        if R.?generators           then RP.generators = apply(R.generators, r -> promote(r, RP));
        if R.?generatorSymbols     then RP.generatorSymbols     = R.generatorSymbols;
        if R.?generatorExpressions then RP.generatorExpressions = R.generatorExpressions;
        if R.?indexSymbols then RP.indexSymbols = applyValues(R.indexSymbols, r -> promote(r,RP));
        if R.?indexStrings then RP.indexStrings = applyValues(R.indexStrings, r -> promote(r,RP));
        RP
        )

--##################### Documentation ########################--
-- See LocalRings/doc.m2

--##################### Legacy Code ##########################--

setMaxIdeal = method()
setMaxIdeal(Ideal) := (maxI) -> (
     R := ring maxI;
     R.residueMap = map(R,R,vars R % maxI);
     R.maxIdeal = maxI
     )

localComplement = method()
localComplement Matrix := Matrix => (m) -> (
     n := transpose syz transpose ((ring m).residueMap m);
     id_(target n) // n
     )

defaultResolutionLength := (R) -> (
     numgens R + 1 + if ZZ === ultimate(coefficientRing, R) then 1 else 0
     )

resolutionLength := (R,options) -> (
     if options.LengthLimit == infinity then defaultResolutionLength R else options.LengthLimit
     )

localsyz = method()
localsyz(Matrix) := (m) -> (
     localMingens syz m
     --C = res(coker m,LengthLimit=>3);
     --C.dd_2 * localComplement(C.dd_3)
     )

localMingens = method()
localMingens(Matrix) := Matrix => (m) -> (
     -- warning: this routine should perhaps take a Module...
     m * localComplement syz m
     )

localModulo = method()
localModulo(Matrix,Matrix) := Matrix => (m,n) -> (
     P := target m;
     Q := target n;
     if P != Q then error "expected maps with the same target";
     if not isFreeModule P or not isFreeModule Q
     or not isFreeModule source m or not isFreeModule source n
     then error "expected maps between free modules";
     localMingens syz(m|n, SyzygyRows => numgens source m)
     )

localPrune = method()
localPrune Module := (M) -> (
     p := presentation M;
     p1 := localComplement p;
     p2 := localModulo(p1,p);
     N := coker(p2);
     N.cache.pruningMap = map(M,N,p1);
     N
     )

localResolution = method(Options => options resolution)
localResolution Ideal := options -> (I) -> localResolution coker gens I
localResolution Module := options -> (M) -> (
     R := ring M;
     if not R.?maxIdeal then error "use setMaxIdeal first!";
     maxlength := resolutionLength(R,options);
     if M.cache.?localResolution
     then (
         C := M.cache.localResolution;
         C.length = length C
         )
     else (
          C = new ChainComplex;
          C.ring = R;
          -- f := presentation M;
          -- we have replaced presentation in the previous line by minimalPresentation. This has fixed a reported bug where 
          -- localResolution returned a non-minimal presentation.
          -- We have included the above mentioned example in the tests section.
          f := relations minimalPresentation M;
          C#0 = target f;
          C#1 = source f;
          C.dd#1 = f;
          M.cache.localResolution = C;
          C.length = 1;
          );
     i := C.length;
     while i < maxlength and C.dd_i != 0 do (
          g := localsyz C.dd_i;
          shield (
               i = i+1;
               C.dd#i = g;
               C#i = source g;
               C.length = i;
               );
          );
     C)

TEST ///
     --loadPackage "LocalRings"
     R = QQ[x,y,z]
     setMaxIdeal ideal vars R
     m = matrix {{x-1, y, z-1}}
     LC = localResolution coker m
     LC.dd
     assert (length LC == 2)
     ///

TEST ///
     --loadPackage "LocalRings"
     S=ZZ/101[t,x,y,z]
     setMaxIdeal ideal vars S
     assert(S.residueMap === map(S,S,{0,0,0,0}))
     m=matrix"x,y2;z3,x4"
     M=coker m
     assert(localsyz m == 0)
     ///
     
TEST ///
     --loadPackage "LocalRings"
     R = QQ[a,b,c,d]
     setMaxIdeal(ideal(a-1,b-2,c-3,d-4))
     I = ideal((1+a), (1+b))
     G = localMingens gens I
     assert(numgens source G == 1)
     ///
     
TEST ///
     --loadPackage "LocalRings"
     kk = ZZ/32003
     R = kk[x,y,z,w,SkewCommutative=>true]
     m = matrix{{x,y*z},{z*w,x}}
     setMaxIdeal(ideal(x,y,z,w))
     C = localResolution(coker m, LengthLimit=>10)
     C = localResolution(coker m)
     for i from 1 to 10 do
     assert(zero(C.dd_(i-1) * C.dd_i))
     ///

end--

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=Local pre-install"
-- End: