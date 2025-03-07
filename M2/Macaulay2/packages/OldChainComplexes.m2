newPackage(
    "OldChainComplexes",
    Version => "0.1",
    Date => "7 March 2025",
    Headline => "consolidating legacy chain complex code",
    Authors => {{ Name => "", Email => "", HomePage => ""}},
    AuxiliaryFiles => true,
    DebuggingMode => false
    )

export {}


-* Documentation section *-
beginDocumentation()

load "./OldChainComplexes/doc9.m2"

///
Key
  OldChainComplexes
Headline
Description
  Text
  Tree
  Example
  CannedExample
Acknowledgement
Contributors
References
Caveat
SeeAlso
Subnodes
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

-* Test section *-
TEST /// -* [insert short title for this test] *-
-- test code and assertions here
-- may have as many TEST sections as needed
///

end--

-* Development section *-
restart
debug needsPackage "OldChainComplexes"
check "OldChainComplexes"

uninstallPackage "OldChainComplexes"
restart
installPackage "OldChainComplexes"
viewHelp "OldChainComplexes"
