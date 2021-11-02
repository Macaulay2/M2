-- -*- coding: utf-8 -*-
newPackage("Dmodules", 
     Version => "1.4.0.1",
     Date => "01/28/2011",
     Headline => "D-modules",
     HomePage => "http://people.math.gatech.edu/~aleykin3/Dmodules",
     AuxiliaryFiles => true,
     Authors => {
	  {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"},
	  {Name => "Harrison Tsai"}
	  },
     Keywords => {"D-modules"},
     DebuggingMode => false,
     PackageImports => {"PrimaryDecomposition","ReesAlgebra","Elimination"}
     )

export { "kappaAnnF1PlanarCurve", "reiffen", "kOrderAnnFa", "kOrderAnnFs",
     "localBFunction", "multiplierIdeal", "ViaElimination", "ViaColonIdeal", "ViaLinearAlgebra",
     "isInMultiplierIdeal", "generalizedBFunction", "mGeneralizedBFunction",
     "jumpingCoefficients", "hasRationalSing",
     "bFunctionRoots", "lct", "GeneralBernsteinSato", "ViaBFunction", "generalB", "Exponent", "InitialIdeal", "StarIdeal", 
     "ExternalProduct","TwistMap","twistMap","twistInvMap","projMap1","projMap2",
     "bFunction","NonGeneric","TryGeneric","IntRing","LocalizeMap",
     "globalB","globalBFunction","ViaAnnFs","ReducedB","factorBFunction","getIntRoots","Boperator","Bpolynomial","globalBoperator",
     "AnnFs","AnnIFs","Dtrace","getDtrace","setHomSwitch","getHomSwitch","localCohom","Walther","OaTa","LocStrategy",
     "OaTaWa","pruneLocalCohom","paramBpoly","GroundField","makeCyclic","Generator","AnnG","isHolonomic","DHom","DExt","Special",
     "None","Info","PolySols","Alg","GD","Duality","PolyExt","RatSols","RatExt","createDpairs","dpairInds","extractVarsAlgebra","extractDiffsAlgebra",
     "dpairVars","Fourier","Dtransposition","singLocus","charIdeal","holonomicRank","Ddim","makeWA"=>"makeWeylAlgebra","makeWeylAlgebra","Ddual","Dlocalize",
     "Oaku","OTW","OTWcyclic","Dlocalization","DlocalizationAll","DlocalizeMap","LocModule","GeneratorPower","LocMap","annFS",
     "DlocalizeAll","IntegrateBfunction","Bfunction","DlocalizationMap","Dresolution","Schreyer","Vhomogenize","Dres",
     "Drestriction","Drestrict","DrestrictionClasses","DrestrictClasses","DrestrictIdeal","DrestrictAll",
     "DrestrictionComplex","DrestrictionAll","DrestrictionIdeal","DrestrictComplex","HomologyModules",
     "GenCycles","Exponents","Cycles","Boundaries","BFunction","VResolution","Explicit","IntegrateComplex","Dintegration",
     "Dintegrate","DintegrateIdeal","DintegrationIdeal","DintegrationComplex","DintegrateClasses","DintegrateComplex",
     "DintegrationClasses","DintegrateAll","DintegrationAll","gkz","Vars","AppellF1","PolyAnn",
     "RatAnn","WeylClosure","deRham","deRhamAll","TransferCycles","CohomologyGroups","PreCycles","OmegaRes",
     "diffOps","PolyGens","BasisElts","putWeylAlgebra","inw","gbw",
     "Dprune","pInfo","optGB","FourierInverse","Output","stafford",
     "BMM","pruneCechComplexCC","populateCechComplexCC",
     "logCohomology","SetVariables", "eulerOperators", "toricIdealPartials", "genToDistractionGens", "distraction", "indicialIdeal",  
     "cssExpts", "cssExptsMult", "solveFrobeniusIdeal", "isTorusFixed", "ICmodule", "ICcohom", "LocCohomStrategy"
     }
   
scan({"Local", "Global"}, nm -> assert (isGlobalSymbol nm and value getGlobalSymbol nm === getGlobalSymbol nm))


-- Harry's basic files
load "./Dmodules/Dbasic.m2" 
load "./Dmodules/Gbw.m2"
load "./Dmodules/Dsystems.m2"
  
-- Anton's basic files
load "./Dmodules/switch.m2"
load "./Dmodules/newRings.m2"

-- Harry's algorithms
load "./Dmodules/Dresolution.m2"
load "./Dmodules/Drestriction.m2"
load "./Dmodules/Dlocalize.m2"
load "./Dmodules/WeylClosure.m2"
load "./Dmodules/Ddual.m2"
load "./Dmodules/DHom.m2"
load "./Dmodules/DeRham.m2"
load "./Dmodules/DiffOps.m2"

-- Anton's algorithms
load "./Dmodules/bFunction.ideal.m2"
load "./Dmodules/globalBFunction.m2"
load "./Dmodules/annFs.m2"
load "./Dmodules/bFunction.module.m2"
load "./Dmodules/localCohom.m2"
load "./Dmodules/paramBpoly.m2"
load "./Dmodules/makeCyclic.m2"
load "./Dmodules/stafford.m2"
load "./Dmodules/CC.m2"
load "./Dmodules/localBFunction.m2"
load "./Dmodules/multiplierIdeals.m2"

-- Christine's algorithms
load "./Dmodules/canonicalSeries.m2"

-- Andras' algorithms
load "./Dmodules/intersectionCohom.m2"

-- HOOKS

addHook((resolution, Module), Strategy => WeylAlgebra,
    (o,M) -> (
	  R := ring M;
	  op := options R;
	  o' := applyPairs(options Dresolution, (key,val) -> (key, o#key));
	  if op.?WeylAlgebra and op.WeylAlgebra =!= {} then Dresolution(o',M)))

addHook((codim, Module), Strategy => WeylAlgebra,
    (opts,M) -> (
	  R := ring M;
	  op := options R;
	  if op.?WeylAlgebra and op.WeylAlgebra =!= {} then (dim R - Ddim M)))

TEST /// input "Dmodules/TST/Drestriction.tst.m2" ///


beginDocumentation()
load "./Dmodules/DMODdoc.m2"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=Dmodules pre-install"
-- End:

