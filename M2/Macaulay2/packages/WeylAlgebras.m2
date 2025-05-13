-- -*- coding: utf-8 -*-
newPackage("WeylAlgebras",
     Version => "1.4.1.1",
     Date => "February 2023",
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
         },
     PackageExports => {
	 "OldChainComplexes",
	 }
     )

--------------------------------------------------------------------------------
-- WeylAlgebras: the base package
--------------------------------------------------------------------------------

-- Harry's basic files
load "./WeylAlgebras/Dbasic.m2"
export {
    "SetVariables",
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
    "Ddim",
    "isHolonomic",
    "holonomicRank",
    -- GBprune
    "Dprune",
    -- reduceCompress
    -- + a lot of tests
    }
load "./WeylAlgebras/Gbw.m2"
export {
    "inw",
    "gbw",
    "characteristicIdeal",
    "charIdeal" => "characteristicIdeal",
    "DsingularLocus",
    "singLocus" => "DsingularLocus",
    "setHomSwitch",
    "getHomSwitch",
    }
-- Anton's basic files
load "./WeylAlgebras/switch.m2"
export {
    -- pInfo
    "Dtrace",
    }
load "./WeylAlgebras/newRings.m2"
export {
    -- createHomWeylAlgebra
    -- createIntRing
    -- createAssCommRing
    -- TODO: export and document these
    -- "ThetaRing",
    -- "createThetaRing",
    }

-- TODO
-- Anton's algorithms
load "./WeylAlgebras/stafford.m2" -- TODO
export {
    -- leadMonomial -- TODO: evict
    "stafford",
    }
load "./WeylAlgebras/makeCyclic.m2"
export {
    "AnnG", -- TODO: move to BernsteinSato?
    "Generator",
    "makeCyclic",
    }

-- PZJ's factor
load "./WeylAlgebras/factorWA.m2"
export {
    "factorWeylAlgebra",
    "factorWA" => "factorWeylAlgebra",
    "StopAfter",
    "factorWeylAlgebra1",
    "factorWA1" => "factorWeylAlgebra1",
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
-- Tests
--------------------------------------------------------------------------------

load "WeylAlgebras/TST/Dbasic.m2"
load "WeylAlgebras/TST/Gbw.m2"

--------------------------------------------------------------------------------
-- Documentation
--------------------------------------------------------------------------------

beginDocumentation()

load "WeylAlgebras/DOC/main.m2"
load "WeylAlgebras/DOC/basics.m2"   -- basic commands
load "WeylAlgebras/DOC/general.m2"

--------------------------------------------------------------------------------

end--

restart
uninstallPackage "WeylAlgebras"
installPackage "WeylAlgebras"

restart
needsPackage "WeylAlgebras"
check WeylAlgebras
