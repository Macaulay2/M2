newPackage(
    "Permutations",
    AuxiliaryFiles => true,
    Version => "1.0", 
    Date => "June 4, 2024",
    Keywords => {"Combinatorics"},
    Authors => {
        {Name => "Sean Grate", 
         Email => "sean.grate@auburn.edu", 
         HomePage => "https://seangrate.com/"}
  },
    Headline => "functions for working with permutations",
    PackageExports => {
        -- "SimplicialComplexes",
        -- "SimplicialDecomposability",
        -- "Posets",
        -- "MinimalPrimes"
    },
    DebuggingMode => true
)

export{
    -- types
    "Permutation",
    -- methods
    "permutation",
    "isValidPermutation",
    "reduce",
    "expand",
    "toMatrix",
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
    "foataBijection",
    -- "inverse",
    "ord",
    "sign",
    "isEven",
    "isOdd",
    "isDerangement",
    "fixedPoints",
    "inversions",
    -- symbols
    "Weak"
}

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- **CODE** --
------------------------------------------------------------------------------
------------------------------------------------------------------------------
load "./Permutations/Permutations.m2"
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- **DOCUMENTATION** --
------------------------------------------------------------------------------
------------------------------------------------------------------------------
beginDocumentation ()    
load "./Permutations/PermutationsDOC.m2"
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- **TESTS** --
------------------------------------------------------------------------------
------------------------------------------------------------------------------
load "./Permutations/PermutationsTests.m2"

end---------------------------------------------------------------------------     

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- **SCRATCH SPACE** --
------------------------------------------------------------------------------
------------------------------------------------------------------------------


------------------------------------
--Development Section
------------------------------------

restart
uninstallPackage "Permutations"
restart
installPackage "Permutations"
restart
needsPackage "Permutations"
elapsedTime check "Permutations"
viewHelp "Permutations"