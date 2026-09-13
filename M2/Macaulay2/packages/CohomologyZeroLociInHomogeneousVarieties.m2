newPackage(
    "CohomologyZeroLociInHomogeneousVarieties",
    Version => "0.1",
    Date => "Sep 9, 2026",
    Headline => "homogeneous vector bundles",
    Keywords => {"Algebraic Geometry"},
    Authors => {{ Name => "Alessandro Frassineti", Email => "alessandro.frassineti@edu.unige.it", HomePage => "https://sites.google.com/view/alessandrofrassineti-math/home-page/"}},
    HomePage => "https://github.com/K3Ale/package-HomogeneousVarieties", 
    AuxiliaryFiles => true,
    DebuggingMode => false,
    Reload => false,
    PackageExports => {"Schubert2","Python","WeylGroups"}
    )


export {
    "HomogeneousVariety",
    "EmbeddedVariety",
    "ParabolicGroup",
    "HomogeneousVectorBundle",
    "FiltrationBundle", -- end list of variables names
    "newParabolic",
    "homogeneousVariety",
    "Gr",
    "Fl",
    "OGr",
    "SGr",
    "homogeneousVectorBundle",
    "filtrationBundle",
    "embeddedVariety",
    "homogeneousTangentBundle",
    "homogeneousCotangentBundle",
    "structureSheaf",
    "summands",
    "dominantConjugate",
    "isDominant",
    "isSingular",
    "isAmple",
    "isGloballyGenerated",
    "adjointRepresentation",
    "weylFormula",
    "eulerCharacteristic",
    "eulerCharacteristicCotangent",
    "chiCotangent",
    "eulerCharacteristicTangent",
    "chiTangent",
    "eulerCharacteristicTangentTwisted",
    "eulerCharacteristicStructure",
    "chiStructure",
    "volumeFano",
    "cohomologyRestriction",
    "tensorProduct",
    "hodgeNumbers",
    "displayHN",
    "hochschildNumbers",
    "hochschildNumbersTwisted",
    "displayHochN",
    "invariants", -- end list of methods
    "DisplayBounds",
    "doHodge",
    "maxChiT"
    }
    


-* Code section *-
packageDir := currentFileDirectory;

------------------------------------------------------------------
-- Check Python
------------------------------------------------------------------
if not (options Python).OptionalComponentsPresent then
    error ///CohomologyZeroLociInHomogeneousVarieties requires a version of
Macaulay2 built with Python support, which is not available in this
installation.

Most official Macaulay2 binary releases already include Python support:
    https://github.com/Macaulay2/M2/releases
If you compiled Macaulay2 from source, reconfigure and rebuild with:
    ./configure --with-python
and reinstall this package afterwards.///;

------------------------------------------------------------------
-- Check numpy 
------------------------------------------------------------------
hasNumPy := true;
try (import "numpy";) else hasNumPy = false;

if not hasNumPy then (
    print "CohomologyZeroLociInHomogeneousVarieties: numpy was not found; attempting automatic installation with pipInstall...";
    installOK := true;
    try (pipInstall "numpy";) else installOK = false;
    if installOK then (
        hasNumPy = true;
        try (import "numpy";) else hasNumPy = false;
        );
    );

if not hasNumPy then
    error ///CohomologyZeroLociInHomogeneousVarieties requires the Python
package numpy, which could not be found or installed automatically.

Please install it manually by running one of the following commands in a
terminal (outside Macaulay2), then restart Macaulay2 and reload this
package:
    python3 -m pip install numpy
    pip install numpy

If Macaulay2 is configured to use a specific Python executable (for
example inside a virtual environment), make sure numpy is installed in
that same environment. See the Python package's documentation on
setupVirtualEnvironment and the "executable" Configuration option.///;

((import "sys")@@("path"))@@append(toPython (packageDir | "CohomologyZeroLociInHomogeneousVarieties/pythonFiles/"));


((import "sys")@@("path"))@@append(toPython (packageDir | "CohomologyZeroLociInHomogeneousVarieties/pythonFiles/"));

--"This package contains several methods for computing several invariants of homogeneous varieties and of zero loci of homogeneous vector bundles.",

load (currentFileDirectory | "CohomologyZeroLociInHomogeneousVarieties/Definitions.m2")
load (currentFileDirectory | "CohomologyZeroLociInHomogeneousVarieties/FirstPart.m2")
load (currentFileDirectory | "CohomologyZeroLociInHomogeneousVarieties/LieFunctions.m2")
load (currentFileDirectory | "CohomologyZeroLociInHomogeneousVarieties/Plethysms.m2")
load (currentFileDirectory | "CohomologyZeroLociInHomogeneousVarieties/Cohomology.m2")

-----------------------------------------------------------------------------------------------

-* Documentation section *-

load (currentFileDirectory | "CohomologyZeroLociInHomogeneousVarieties/docs.m2")
-* Test section *-

load (currentFileDirectory | "CohomologyZeroLociInHomogeneousVarieties/tests.m2")

end

-* Development section *-
for i from 0 to 9 do check (i,"CohomologyZeroLociInHomogeneousVarieties")
for i from 10 to 19 do check (i,"CohomologyZeroLociInHomogeneousVarieties")
for i from 20 to 29 do check (i,"CohomologyZeroLociInHomogeneousVarieties")
for i from 30 to 36 do check (i,"CohomologyZeroLociInHomogeneousVarieties")

restart
uninstallPackage "CohomologyZeroLociInHomogeneousVarieties"
restart
installPackage "CohomologyZeroLociInHomogeneousVarieties"
needsPackage "CohomologyZeroLociInHomogeneousVarieties"
viewHelp "CohomologyZeroLociInHomogeneousVarieties"
