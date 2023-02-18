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
    "inw",
    "gbw",
    "charIdeal" => "characteristicIdeal",
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
    -- createHomWeylAlgebra
    -- createIntRing
    -- createAssCommRing
    -- TODO: document these
    "ThetaRing",
    "createThetaRing",
    }

-- TODO
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

load "Dmodules/TST/Dbasic.m2"

--------------------------------------------------------------------------------
-- Documentation
--------------------------------------------------------------------------------

beginDocumentation()

load "Dmodules/DOC/main.m2"
load "Dmodules/DOC/tutorial.m2" -- basic tutorial
load "Dmodules/DOC/Dbasic.m2"   -- basic commands
load "Dmodules/DOC/general.m2"

--------------------------------------------------------------------------------

end--

restart
uninstallPackage "Dmodules"

restart
needsPackage "Dmodules"
check Dmodules
