newPackage(
    "Permutations",
    AuxiliaryFiles => true,
    Version => "1.1", 
    Date => "May 13, 2025",
    Keywords => {"Combinatorics"},
    Authors => {
        {Name => "Sean Grate", 
         Email => "sean.grate@auburn.edu", 
         HomePage => "https://seangrate.com/"}
    },
    Headline => "functions for working with permutations",
    PackageExports => {"Posets"})

export {
    -- types
    "Permutation",
    -- methods
    "permutation",
    "cycleDecomposition",
    "cycleType",
    "ascents",
    "descents",
    "ascendingRuns",
    "descendingRuns",
    "exceedances",
    "saliances",
    "records",
    "avoidsPattern",
    "avoidsPatterns",
    "isVexillary",
    "isCartwrightSturmfels",
    "isCDG",
    "isSeparable",
    "foataBijection",
    "ord",
    "isEven",
    "isOdd",
    "isDerangement",
    "fixedPoints",
    "inversions",
    "randomPermutation",
    "reducedWords",
    "weakBruhatOrder",
    "strongBruhatOrder",
    "symmetricGroupPoset",
    "transposition",
    -- symbols
    "Weak",
    "Side"
}


-----------------------------------------------------------------------------
-- **CODE** --
-----------------------------------------------------------------------------
load "./Permutations/Code/main.m2"
load "./Permutations/Code/operations.m2"
load "./Permutations/Code/patternAvoidance.m2"


-----------------------------------------------------------------------------
-- **DOCUMENTATION** --
-----------------------------------------------------------------------------
beginDocumentation()
load "./Permutations/Documentation/packageDocs.m2"
load "./Permutations/Documentation/mainDocs.m2"
load "./Permutations/Documentation/operationsDocs.m2"
load "./Permutations/Documentation/patternAvoidanceDocs.m2"


-----------------------------------------------------------------------------
-- **TESTS** --
-----------------------------------------------------------------------------
load "./Permutations/Tests/mainTests.m2"
load "./Permutations/Tests/operationsTests.m2"
load "./Permutations/Tests/patternAvoidanceTests.m2"
end


-----------------------------------------------------------------------------
--Development Section
-----------------------------------------------------------------------------
restart
uninstallPackage "Permutations"
restart
installPackage "Permutations"
restart
needsPackage "Permutations"
elapsedTime check "Permutations"
viewHelp "Permutations"
