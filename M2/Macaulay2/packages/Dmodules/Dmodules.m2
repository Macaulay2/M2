export { "kappaAnnF1PlanarCurve", "reiffen", "kOrderAnnFa", "kOrderAnnFs", "kDiffFs",
     "localBFunction", "multiplierIdeal", "ViaElimination", "ViaColonIdeal", "ViaLinearAlgebra",
     "isInMultiplierIdeal", "generalizedBFunction", "mGeneralizedBFunction",
     "jumpingCoefficients", "hasRationalSing",
     "bFunctionRoots", "lct", "GeneralBernsteinSato", "ViaBFunction", "generalB", "Exponent", "InitialIdeal", "StarIdeal", 
     "ExternalProduct","TwistMap","twistMap","twistInvMap","projMap1","projMap2",
     "bFunction","NonGeneric","TryGeneric","IntRing","LocalizeMap",
     "globalB","globalBFunction","ViaAnnFs","ReducedB","factorBFunction","getIntRoots","Boperator","Bpolynomial","globalBoperator",
     "AnnFs","AnnIFs","Dtrace","getDtrace","setHomSwitch","getHomSwitch","localCohom","Walther","OaTa","LocStrategy",
     "OaTaWa","pruneLocalCohom","paramBpoly","GroundField","makeCyclic","Generator","AnnG","isHolonomic","DHom","DExt","Special",
     "None","Info","PolySols","Alg","GD","Duality","PolyExt","RatSols","RatExt","createDpairs","dpairInds",
     "dpairVars","Fourier","Dtransposition","singLocus","charIdeal","holonomicRank","Drank","Ddim","makeWA"=>"makeWeylAlgebra","makeWeylAlgebra","Ddual","Dlocalize",
     "Oaku","OTW","Dlocalization","DlocalizationAll","DlocalizeMap","LocModule","GeneratorPower","LocMap","annFS",
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
     "logCohomology","SetVariables"
     }
   
scan({"Local", "Global"}, nm -> assert (isGlobalSymbol nm and value getGlobalSymbol nm === getGlobalSymbol nm))

load "./Dloadfile.m2"

-- HOOKS

addHook(Module, symbol resolution, (o,M) -> (
	  R := ring M;
	  op := options R;
	  o' := applyPairs(options Dresolution, (key,val) -> (key, o#key));
	  if op.?WeylAlgebra and op.WeylAlgebra =!= {} then break Dresolution(o',M)))

addHook(Module, symbol codim, (opts,M) -> (
	  R := ring M;
	  op := options R;
	  if op.?WeylAlgebra and op.WeylAlgebra =!= {} then break (dim R - Ddim M)))

beginDocumentation()
load "./DMODdoc.m2"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=Dmodules pre-install"
-- End:
