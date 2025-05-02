newPackage(
    "OldChainComplexes",
    Version => "0.1",
    Date => "7 March 2025",
    Headline => "consolidating legacy chain complex code",
    Authors => { -* { Name => "", Email => "", HomePage => ""}*- },
    Keywords => {"Homological Algebra"},
    AuxiliaryFiles => true,
    DebuggingMode => false
)

export {
    "ChainComplex",
    "ChainComplexMap",
    "GradedModule",
    "GradedModuleMap",
    "Resolution",
    "chainComplex",
    "eagonNorthcott",
    "gradedModule",
    "gradedModuleMap",
    "minimalBetti",
    "poincareN",
    "resolution", "res" => "resolution",
    "syzygyScheme",
}

importFrom_Core {
    "raw",
    "storefuns",
    "leftarrow",
    "mtable",
}

-----------------------------------------------------------------------------

needs "./OldChainComplexes/gradedmodules.m2"
needs "./OldChainComplexes/chaincomplexes.m2"
needs "./OldChainComplexes/res.m2"
needs "./OldChainComplexes/betti.m2"
needs "./OldChainComplexes/Ext.m2"
needs "./OldChainComplexes/Tor.m2"

-----------------------------------------------------------------------------

-- used to be in hilbert.m2
pdim Module := M -> length resolution minimalPresentation M

-----------------------------------------------------------------------------
-* Documentation section *-
beginDocumentation()

load "./OldChainComplexes/docs/chainComplex-doc.m2"
load "./OldChainComplexes/docs/eagonNorthcott-doc.m2"
load "./OldChainComplexes/docs/Ext-doc.m2"
load "./OldChainComplexes/docs/minimalBetti-doc.m2"
load "./OldChainComplexes/docs/pdim-doc.m2"
load "./OldChainComplexes/docs/regularity-doc.m2"
load "./OldChainComplexes/docs/Tor-doc.m2"
load "./OldChainComplexes/docs/ov_chaincomplexes.m2"
load "./OldChainComplexes/docs/doc9.m2"
load "./OldChainComplexes/docs/from-kernel-doc.m2"
load "./OldChainComplexes/docs/from-symbol-underscore-doc.m2" -- TODO: tease out modules vs complexes?
load "./OldChainComplexes/docs/from-caret.m2"
load "./OldChainComplexes/docs/from-map-doc.m2"
load "./OldChainComplexes/docs/resolution-doc.m2"
  -- TODO: setting C_3 = ... no longer allowed!

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
