newPackage(
    "OldChainComplexes",
    Version => "0.1",
    Date => "7 March 2025",
    Headline => "consolidating legacy chain complex code",
    Authors => { -* { Name => "", Email => "", HomePage => ""}*- },
    Keywords => {"Homological Algebra"},
    PackageImports => { "LLLBases" },
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
Node
  Key
    OldChainComplexes
    "chain complexes"
  Headline
    legacy implementation of chain complexes
  Description
    Text
      This package contains legacy code that was extracted from the Core in order to preserve the existing
      functionality of chain complexes, graded modules, and resolutions for backwards compatibility.

      Beginning in the 1.25.11 release, this package will not be preloaded anymore and current routines
      involving chain complexes and graded modules will be superseded by new functorial homological algebra
      routines in the @TO "Complexes::Complexes"@ package.

      Users and package developers are encouraged to preview the upcoming changes by adding
    Pre
      HomologicalAlgebraPackage = "Complexes"
    Text
      to their @TO "initialization file"@ and reporting any bugs via email or
      @HREF{"https://github.com/Macaulay2/M2/issues/3778", "GitHub"}@.
      Once the transition is complete, adding
    Pre
      HomologicalAlgebraPackage = "OldChainComplexes"
    Text
      will provide a workaround for running old scripts or benchmarking
      against previous algorithms to detect potential regressions.

      For additional common operations and a comprehensive list of all routines provided
      in this package which return or use chain complexes or maps between chain complexes,
      see @TO "ChainComplex"@ and @TO "ChainComplexMap"@.
  SeeAlso
    "ChainComplexExtras :: ChainComplexExtras"
    "ChainComplexOperations :: ChainComplexOperations"
  Subnodes
    "free resolutions of modules"
    "extracting information from chain complexes"
    "making chain complexes by hand"
    "manipulating chain complexes"
    "maps between chain complexes"
    GradedModule
    GradedModuleMap
    ChainComplex
    ChainComplexMap
    Resolution
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
