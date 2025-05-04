newPackage(
    "OldChainComplexes",
    Version => "0.1",
    Date => "7 March 2025",
    Headline => "consolidating legacy chain complex code",
    Authors => { -* { Name => "", Email => "", HomePage => ""}*- },
    Keywords => {"Homological Algebra"},
    PackageImports => { "Complexes", "LLLBases" },
    AuxiliaryFiles => true,
    DebuggingMode => false
)

export {
    "ChainComplex",
    "ChainComplexMap",
    "GradedModule",
    "GradedModuleMap",
    --"Resolution", -- exported in chaincomplexes.m2
    "chainComplex",
    "gradedModuleMap",
    "resolution", "res" => "resolution",
    "syzygyScheme",
}

importFrom_Core {
    "raw",
    "storefuns",
    "leftarrow",
    "mtable",
    "isPackageLoaded",
}

-----------------------------------------------------------------------------

load "./OldChainComplexes/gradedmodules.m2"
load "./OldChainComplexes/chaincomplexes.m2"

load "./OldChainComplexes/res.m2"
load "./OldChainComplexes/betti.m2"
load "./OldChainComplexes/Ext.m2"
load "./OldChainComplexes/Tor.m2"

-----------------------------------------------------------------------------

-- ChainComplex to Complex conversion code
if isPackageLoaded "Complexes" then
  load "./OldChainComplexes/conversion.m2"

-----------------------------------------------------------------------------
-* Documentation section *-
beginDocumentation()

-- overview docs
load "./OldChainComplexes/docs/ov_chaincomplexes.m2"

-- random docs
load "./OldChainComplexes/docs/docs.m2" -- see TODO list here
load "./OldChainComplexes/docs/doc9.m2"
load "./OldChainComplexes/docs/doc10.m2"

-- method docs
load "./OldChainComplexes/docs/betti-doc.m2"
load "./OldChainComplexes/docs/chainComplex-doc.m2"
load "./OldChainComplexes/docs/kernel-doc.m2"
load "./OldChainComplexes/docs/map-doc.m2"
load "./OldChainComplexes/docs/regularity-doc.m2"
load "./OldChainComplexes/docs/resolution-doc.m2"
  -- TODO: setting C_3 = ... no longer allowed!

-- operator docs
load "./OldChainComplexes/docs/caret-doc.m2"
load "./OldChainComplexes/docs/underscore-doc.m2"

-- ChainComplex to Complex conversion docs
if isPackageLoaded "Complexes" then
  load "./OldChainComplexes/docs/conversion.m2"

doc ///
Key
  OldChainComplexes
Headline
  legacy chain complex code and doc
Description
  Text
    This package contains code we are moving from the Core.
    The first part of the plan is to consolidate all legacy chain complex
    code and doc in one package.  Later, we will make this not preloaded.
///

///
Key
Headline
Usage
Inputs
Outputs
Consequences
  Item
Description
  Text
  Example
  CannedExample
  Code
  Pre
ExampleFiles
Contributors
References
Caveat
SeeAlso
///

-----------------------------------------------------------------------------
-* Test section *-

TEST /// -* [insert short title for this test] *-
-- test code and assertions here
-- may have as many TEST sections as needed
///

end--

-----------------------------------------------------------------------------
-* Development section *-

restart
debug needsPackage "OldChainComplexes"
check "OldChainComplexes"

uninstallPackage "OldChainComplexes"
restart
installPackage "OldChainComplexes"
viewHelp "OldChainComplexes"
