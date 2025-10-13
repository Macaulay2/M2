-- -*- coding: utf-8 -*-
newPackage(
    "BernsteinSato",
    Version => "1.0",
    Date => "February 2023",
    Headline => "Bernstein-Sato",
    Authors => {
	{ Name => "Anton Leykin", Email => "leykin@math.gatech.edu" },
	{ Name => "Harrison Tsai" }
	},
    Keywords => {"D-modules"},
    PackageImports => {
	"PrimaryDecomposition",
	"ReesAlgebra",
	"Elimination",
	"FourTiTwo"
	},
    PackageExports => {
	"WeylAlgebras",
	"HolonomicSystems",
	"OldChainComplexes",
    },
    AuxiliaryFiles => true,
    DebuggingMode => false
    )

-- local symbols defined in WeylAlgebras
importFrom_"WeylAlgebras" {
    "raw", "WtoT", "isGeneric",
    "IntRing", "createIntRing", "RtoIR",
    "pInfo", "createThetaRing", "ThetaRing",
    "HomWeylAlgebra", "createHomWeylAlgebra", "WAtoHWA", "HWAtoWA",
    }

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
    "IntRing",		-- bFunction.ideal, globalBFunction (internally in newRings)
    --
    "InitialIdeal",	-- bFunction.ideal, globalBFunction
    "StarIdeal",	-- bFunction.ideal, globalBFunction
    --
    "Boperator",	-- used in Dlocalize
    "Bpolynomial",	-- used in Dlocalize, DHom
    }

-- Anton's algorithms
load "./BernsteinSato/globalBFunction.m2" -- TODO: has deduplicates
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
load "./BernsteinSato/localBFunction.m2"
export {
    -- eliminateWA
    -- computeJf
    -- exceptionalLocusB
    -- localBFunctionStrata
    "localBFunction",
    }
load "./BernsteinSato/bFunction.ideal.m2" -- TODO: has deduplicates
load "./BernsteinSato/bFunction.module.m2" -- TODO: has deduplicate bFunction2
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
load "./BernsteinSato/multiplierIdeals.m2"
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
load "./BernsteinSato/annFs.m2"
export {
    -- TODO: AnnG is in WeylAlgebras
    "AnnFs",
    "AnnIFs", -- deduplicate
    "polynomialAnnihilator",
    "PolyAnn" => "polynomialAnnihilator",
    "rationalFunctionAnnihilator",
    "RatAnn" => "rationalFunctionAnnihilator",
    "diffRatFun",
    "kOrderAnnFa",
    "kOrderAnnFs",
    -- kCoeffVectorWRTs
    "kappaAnnF1PlanarCurve",
    "reiffen",
    }

-- TODO
-- Anton's algorithms
load "./BernsteinSato/paramBpoly.m2" -- seems to be outdated
export { "paramBpoly" }
-- Harry's algorithms
load "./BernsteinSato/WeylClosure.m2"
export { "WeylClosure" }

--------------------------------------------------------------------------------
-- LocalCohomology
--------------------------------------------------------------------------------

-- Harry's algorithms
load "./BernsteinSato/Dresolution.m2"
export {
    -- shifts
    -- kerGB -- wrapped over rawKernelOfGB
    "Schreyer",
    "Vhomogenize",
    "Dresolution",
    "Dres" => "Dresolution",
    }
load "./BernsteinSato/Drestriction.m2"
export {
    -- findExps
    -- zeroize
    -- ensureQuotientModule
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

-- Harry's algorithms
load "./BernsteinSato/Dlocalize.m2"
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
load "./BernsteinSato/DHom.m2"
export { -- TODO: remove duplicate code
    "Alg",
    "GD",
    "None",
    "Info",
    "Output",
    "Duality",
    "Special",
    "polynomialExt",
    "PolyExt" => "polynomialExt",
    "polynomialSolutions",
    "PolySols" => "polynomialSolutions",
    "TwistMap",
    "rationalFunctionSolutions",
    "RatSols" => "rationalFunctionSolutions",
    "rationalFunctionExt",
    "RatExt" => "rationalFunctionExt",
    -- TwistOperator
    "Ddual",
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
load "./BernsteinSato/DeRham.m2"
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
load "./BernsteinSato/localCohom.m2"
export {
    "Walther", -- also in intersectionCohom
    "LocStrategy", -- also in intersectionCohom
    "OaTa", "OaTaWa", -- deduplicate with OTW
    "localCohom",
    "pruneLocalCohom",
    -- preimage -- TODO: evict
    }
load "./BernsteinSato/CC.m2"
export {
    "localizeCharacteristicCycle",
    "BMM" => "localizeCharacteristicCycle",
    "populateCechComplexCC",
    "pruneCechComplexCC",
    }
-- Andras' algorithms
load "./BernsteinSato/intersectionCohom.m2"
export {
    "IHmodule",
    "intersectionCohomology",
    "LocCohomStrategy",
    "IH"
    }

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

load "./BernsteinSato/TST/tests.m2"

--------------------------------------------------------------------------------
-- Documentation
--------------------------------------------------------------------------------

beginDocumentation()

load "./BernsteinSato/DOC/main.m2"
load "./BernsteinSato/DOC/DHom.m2"
load "./BernsteinSato/DOC/Dlocalize.m2"
load "./BernsteinSato/DOC/Drestriction.m2"
load "./BernsteinSato/DOC/WeylClosure.m2"
load "./BernsteinSato/DOC/annFs.m2"
load "./BernsteinSato/DOC/bFunctions.m2"
load "./BernsteinSato/DOC/localCohom.m2"
load "./BernsteinSato/DOC/intersectionCohom.m2"
load "./BernsteinSato/DOC/multiplierIdeals.m2"
load "./BernsteinSato/DOC/paco-anton-paper.m2"
load "./BernsteinSato/DOC/other.m2"

--------------------------------------------------------------------------------

end--
restart
uninstallPackage "BernsteinSato"
installPackage "WeylAlgebras"
installPackage "HolonomicSystems"
installPackage "BernsteinSato"
installPackage("BernsteinSato", RerunExamples => true)

restart
needsPackage "BernsteinSato"
check BernsteinSato
