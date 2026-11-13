newPackage(
    "EquivariantEhrhart",
    Version => "1.0",
    Date => "June 30, 2025",
    Headline => "compute equivariant Ehrhart series of lattice polytopes",
    Authors => {
        {
            Name => "Oliver Clarke",
            Email => "oliver.clarke@durham.ac.uk",
            HomePage => "https://oliverclarkemath.com"
        },
        {
            Name => "Mike Cummings",
            Email => "mike.cummings@uwaterloo.ca",
            HomePage => "https://mikecummings.ca"
        },
        {
            Name => "Sean Grate",
            Email => "sgrate@iastate.edu",
            HomePage => "https://seangrate.com"
        }
    },
    Keywords => {"Combinatorics", "Convex Geometry"},
    PackageExports => {"BettiCharacters", "Permutations", "RationalPolytopes", "Cyclotomic"},
    AuxiliaryFiles => true,
    DebuggingMode => true,
    HomePage => "https://github.com/Macaulay2/Workshop-2025-Madison/tree/EhrhartTheory" -- Temporary Homepage

)

export {
    -- methods
    "cycleTypeRepresentatives",
    "equivariantEhrhartSeries",
    "fixedPolytope",
    "generateGroup",
    "isEffective",
    "isSymmetric",
    "orbitPolytope",
    "representationRing",
    -- options
    "ReturnHStarList",
    "ReturnTable",
    "ReturnClassReps",
    "MaxComputeTime"
}

-* Code section *-
load "./EquivariantEhrhart/code.m2"

-* Documentation section *-
beginDocumentation()
load "./EquivariantEhrhart/docs.m2"

-* Test section *-
load "./EquivariantEhrhart/tests.m2"

end--

-* Development section *-
restart
debug needsPackage "EquivariantEhrhart"
check "EquivariantEhrhart"

uninstallPackage "EquivariantEhrhart"
restart
installPackage "EquivariantEhrhart"
viewHelp "EquivariantEhrhart"
