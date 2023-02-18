-- -*- coding: utf-8 -*-
newPackage(
    "HolonomicSystems",
    Version => "1.0",
    Date => "February 2023",
    Headline => "Holonomic Systems",
    Authors => {
	{ Name => "Anton Leykin", Email => "leykin@math.gatech.edu" },
	{ Name => "Harrison Tsai" }
	},
    Keywords => {"D-modules"},
    PackageExports => { "Dmodules" },
    PackageImports => {
	"PrimaryDecomposition",
	"ReesAlgebra",
	"Elimination",
	"FourTiTwo"
	},
    AuxiliaryFiles => true,
    DebuggingMode => false
    )

-- local symbols defined in Dmodules
importFrom_"Dmodules" { }

--------------------------------------------------------------------------------
-- HolonomicSystems
--------------------------------------------------------------------------------

-- Harry's basic files
load "./HolonomicSystems/Dsystems.m2"
export {
    "gkz", -- needs toricIdeal from FourTiTwo
    "eulerOperators",
    "toricIdealPartials", -- needs toricMarkov from FourTiTwo
    "AppellF1",
    "Vars",
    }
-- Christine's algorithms
load "./HolonomicSystems/canonicalSeries.m2"
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
load "./HolonomicSystems/DiffOps.m2"
export {
    "PolyGens",
    "BasisElts",
    "diffOps",
    "putWeylAlgebra",
    }

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

load "HolonomicSystems/TST/tests.m2"

--------------------------------------------------------------------------------
-- Documentation
--------------------------------------------------------------------------------

beginDocumentation()

load "HolonomicSystems/DOC/main.m2"
load "HolonomicSystems/DOC/DiffOps.m2"
load "HolonomicSystems/DOC/Dsystems.m2"
load "HolonomicSystems/DOC/canonicalSeries.m2"

--------------------------------------------------------------------------------

end--
restart
uninstallPackage "HolonomicSystems"
installPackage "Dmodules"
installPackage "HolonomicSystems"
installPackage("HolonomicSystems", RerunExamples => true)

restart
needsPackage "HolonomicSystems"
check HolonomicSystems
