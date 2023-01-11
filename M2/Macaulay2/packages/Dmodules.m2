-- -*- coding: utf-8 -*-
newPackage("Dmodules",
     Version => "1.4.1.1",
     Date => "01/31/2020",
     Headline => "D-modules",
     HomePage => "http://people.math.gatech.edu/~aleykin3/Dmodules",
     AuxiliaryFiles => true,
     Authors => {
	  {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"},
	  {Name => "Harrison Tsai"}
	  },
     Keywords => {"D-modules"},
     DebuggingMode => false,
     PackageImports => {
	 "PrimaryDecomposition",
	 "ReesAlgebra",
	 "Elimination",
	 "FourTiTwo"
	 }
     )
   
--------------------------------------------------------------------------------
-- Dmodules: the base package
--------------------------------------------------------------------------------

-- Harry's basic files
load "./Dmodules/Dbasic.m2"
export {
    "SetVariables",
    "optGB",
    "dpairInds",
    "dpairVars",
    "makeWeylAlgebra",
    "makeWA" => "makeWeylAlgebra",
    "createDpairs",
    "extractVarsAlgebra",
    "extractDiffsAlgebra",
    -- createCommAlgebra
    -- FourierLocal
    "Fourier",
    "FourierInverse",
    "Dtransposition",
    -- zeroize
    -- ensureQuotientModule
    "Ddim",
    "isHolonomic",
    "holonomicRank",
    -- GBprune
    "Dprune",
    -- reduceCompress
    -- + a lot of tests
    }
load "./Dmodules/Gbw.m2"
export {
    "Alg",
    "inw",
    "gbw",
    "charIdeal",
    "singLocus",
    "setHomSwitch",
    "getHomSwitch",
    }
-- Anton's basic files
load "./Dmodules/switch.m2"
export {
    "Dtrace",
    "getDtrace",
    "pInfo",
    }
load "./Dmodules/newRings.m2"
export {
    "IntRing",
    -- createHomWeylAlgebra
    -- createIntRing
    -- createAssCommRing
    -- createThetaRing
    }

-- TODO
-- Anton's algorithms
load "./Dmodules/paramBpoly.m2" -- seems to be outdated
export {
    "GroundField",
    "paramBpoly",
    }

-- Anton's algorithms
load "./Dmodules/stafford.m2" -- TODO
export {
    -- leadMonomial -- TODO: evict
    "stafford",
    }
load "./Dmodules/makeCyclic.m2"
export {
    "AnnG",
    "Generator",
    "makeCyclic",
    }

--------------------------------------------------------------------------------
-- HolonomicSystems
--------------------------------------------------------------------------------

-- Harry's basic files
load "./Dmodules/Dsystems.m2"
export {
    "gkz", -- needs toricIdeal from FourTiTwo
    "eulerOperators",
    "toricIdealPartials", -- needs toricMarkov from FourTiTwo
    "AppellF1",
    "reiffen",
    "Vars",
    }
-- Christine's algorithms
load "./Dmodules/canonicalSeries.m2"
export {
    "isTorusFixed",
    -- apbFactor
    -- thetaBracketSub
    -- solveMax
    -- beginExptComp
    -- makeMonomial
    -- makeRationalMonomial
    -- makeLogTerm
    -- makeLogMonomial
    -- factorial'
    "cssExpts",
    "cssExptsMult",
    "cssLeadTerm",
    "distraction",
    "genToDistractionGens",
    "indicialIdeal",
    -- solvePrimaryFrobeniusIdeal
    "solveFrobeniusIdeal",
    }

-- DifferentialOperators
-- Harry's algorithms
load "./Dmodules/DiffOps.m2"
export {
    "PolyGens",
    "BasisElts",
    "diffOps",
    "putWeylAlgebra",
    }
load "./Dmodules/WeylClosure.m2"
export { "WeylClosure" }

--------------------------------------------------------------------------------
-- BernsteinSato
--------------------------------------------------------------------------------
-- TODO: figure out what this package needs, hopefully only Dbasic

export {
    "Exponent",		-- bFunction.ideal, globalBFunction, multiplierIdeals
    --
    "GeneralBernsteinSato",	-- bFunction.ideal, globalBFunction, multiplierIdeals
    "NonGeneric",	-- bFunction.ideal, globalBFunction
    "TryGeneric",	-- also in DHom
    -- "IntRing",	-- from newRings
    --
    "InitialIdeal",	-- bFunction.ideal, globalBFunction
    "StarIdeal",	-- bFunction.ideal, globalBFunction
    --
    "Boperator",	-- used in Dlocalize
    "Bpolynomial",	-- used in Dlocalize, DHom
    }

-- Anton's algorithms
load "./Dmodules/globalBFunction.m2" -- FIXME: needs AnnFs, AnnIFs, TODO: has deduplicates
export {
    "ReducedB",
    -- makeMonic, duplicate
    -- star
    -- globalBFunctionIdeal
    -- globalRB
    "globalB",		-- TODO: also used in DHom, duplicate in paramBpoly
    "globalBoperator",
    "globalBFunction",	-- TODO: also used in Dsystems, localCohom, duplicate in paramBpoly
    "generalB",
    -- generalBideal
    }
load "./Dmodules/localBFunction.m2"
export {
    -- eliminateWA
    -- computeJf
    -- exceptionalLocusB
    -- localBFunctionStrata
    "localBFunction",
    }
load "./Dmodules/bFunction.ideal.m2" -- TODO: has deduplicates
load "./Dmodules/bFunction.module.m2" -- TODO: has deduplicate bFunction2
export {
    -- makeMonic, duplicate
    -- makeQQ, duplicate
    -- bfIntRing
    -- bfGenericOrNonGeneric
    "bFunction",	-- TODO: also used in Drestriction, Dlocalize, DHom, globalBFunction
    "bFunctionRoots",	-- TODO: also used in localBFunction
    "factorBFunction",	-- TODO: also used in Drestriction, Dlocalize, localBFunction
    "getIntRoots",	-- TODO: also used in Dsystems, Drestriction, Dlocalize, DHom, localCohom
    }
load "./Dmodules/multiplierIdeals.m2"
export {
    "ViaAnnFs", -- also globalBFunction
    "ViaBFunction",
    "ViaColonIdeal",
    "ViaElimination",
    "ViaLinearAlgebra", -- also globalBFunction
    --
    "multiplierIdeal",
    "generalizedBFunction",
    "mGeneralizedBFunction",
    "hasRationalSing",
    "isInMultiplierIdeal",
    "jumpingCoefficients",
    "lct",
    -- rlct
    -- isFsLocallyIntegrable
    }
load "./Dmodules/annFs.m2" -- FIXME: needs globalBFunction, charIdeal
export {
    "AnnFs",
    "AnnIFs", -- deduplicate
    "PolyAnn",
    "RatAnn",
    "diffRatFun",
    "kOrderAnnFa",
    "kOrderAnnFs",
    -- kCoeffVectorWRTs
    "kappaAnnF1PlanarCurve",
    }

--------------------------------------------------------------------------------
-- LocalCohomology
--------------------------------------------------------------------------------

-- Harry's algorithms
load "./Dmodules/Dresolution.m2"
export {
    -- shifts
    -- kerGB -- wrapped over rawKernelOfGB
    "Schreyer",
    "Vhomogenize",
    "Dresolution",
    "Dres" => "Dresolution",
    }
load "./Dmodules/Drestriction.m2" -- FIXME: needs factorBFunction, bFunction, getIntRoots
export {
    "Cycles",
    "Boundaries",
    "Drestriction",
    "DrestrictionClasses",
    "DrestrictionComplex",
    "DrestrictionIdeal",
    "DrestrictionAll",
    "Drestrict"         => "Drestriction",
    "DrestrictClasses"  => "DrestrictionClasses",
    "DrestrictComplex"  => "DrestrictionComplex",
    "DrestrictIdeal"    => "DrestrictionIdeal",
    "DrestrictAll"      => "DrestrictionAll",
    "Dintegration",
    "DintegrationClasses",
    "DintegrationComplex",
    "DintegrationIdeal",
    "DintegrationAll",
    "Dintegrate"        => "Dintegration",
    "DintegrateClasses" => "DintegrationClasses",
    "DintegrateComplex" => "DintegrationComplex",
    "DintegrateIdeal"   => "DintegrationIdeal",
    "DintegrateAll"     => "DintegrationAll",
    "IntegrateComplex",
    -- computeRestriction options
    "GenCycles",	-- Drestriction, DHom, DeRham
    "HomologyModules",	-- Drestriction, DHom, DeRham, localCohom
    "VResolution",	-- Drestriction, DHom, DeRham
    "BFunction",	-- Drestriction, Dlocalize, DeRham
    -- RestrictComplex
    "Exponents",
    "Explicit", -- also in DeRham
    }
load "./Dmodules/Ddual.m2"
export { "Ddual" }

-- tests
TEST get(currentFileDirectory | "Dmodules/TST/Drestriction.tst.m2")

-- Harry's algorithms
load "./Dmodules/Dlocalize.m2" -- FIXME: needs gbW2, charIdeal
export {
    "Dlocalize",
    "DlocalizeMap",
    "DlocalizeAll",
    "Dlocalization"    => "Dlocalize",
    "DlocalizationMap" => "DlocalizeMap",
    "DlocalizationAll" => "DlocalizeAll",
    "OTW", -- deduplicate with OaTaWa
    "OTWcyclic",
    "Oaku",
    -- "Boperator",	-- from globalBFunction
    -- "Bpolynomial",	-- from globalBFunction
    "GeneratorPower",	-- also in localCohom
    "IntegrateBfunction",
    "Bfunction",
    "LocMap",		-- also in DeRham, intersectionCohom
    "LocModule",	-- also in localCohom
    "annFS",		-- also in localCohom
    -- AnnIFs2 -- deduplicate
    }
load "./Dmodules/DHom.m2" -- FIXME: needs globalB, Bpolynomial, TryGeneric, Dlocalize
export { -- TODO: remove duplicate code
    "GD",
    "None",
    "Info",
    "Output",
    "Duality",
    "Special",
    "PolyExt",
    "PolySols",
    "TwistMap",
    "RatSols",
    "RatExt",
    -- TwistOperator
    "DHom",
    "DExt",
    "ExternalProduct",
    "twistInvMap",
    "twistMap",
    -- TODO: what are these?
    "projMap1",
    "projMap2",
    -- compareSpans
    }
load "./Dmodules/DeRham.m2"
export {
    "OmegaRes",
    "PreCycles",
    "LocalizeMap",
    "TransferCycles",
    "CohomologyGroups",
    "deRham",
    "deRhamAll",
    "logCohomology",
    -- iAllt, derLogF, getTransfer, getReducedTransfer
    }
-- Anton's algorithms
load "./Dmodules/localCohom.m2"
export {
    "Walther", -- also in intersectionCohom
    "LocStrategy", -- also in intersectionCohom
    "OaTa", "OaTaWa", -- deduplicate with OTW
    "localCohom",
    "pruneLocalCohom",
    -- preimage -- TODO: evict
    }
load "./Dmodules/CC.m2"
export {
    "BMM",
    "populateCechComplexCC",
    "pruneCechComplexCC",
    }
-- Andras' algorithms
load "./Dmodules/intersectionCohom.m2"
export {
    "ICmodule",
    "ICcohom",
    "LocCohomStrategy",
    }

--------------------------------------------------------------------------------

scan({"Local", "Global"}, nm -> assert (isGlobalSymbol nm and value getGlobalSymbol nm === getGlobalSymbol nm))

--------------------------------------------------------------------------------
-- computes the preimage of a submodule M of the target of f
-- (more precisely, M and <target f> should have the same ambient module)
--------------------------------------------------------------------------------
preimage(Matrix, Module) := (f, M) -> (
    T := target f;
    g := map(T/M, T);
    kernel (g * f))

--------------------------------------------------------------------------------

beginDocumentation()
load "./Dmodules/DMODdoc.m2"

end--

restart
uninstallPackage "Dmodules"

restart
needsPackage "Dmodules"
installPackage "Dmodules"
