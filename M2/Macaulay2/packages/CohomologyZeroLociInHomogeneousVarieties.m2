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

hasNumPy := true; 
try (runSimpleString "import numpy; import numpy as np";) else hasNumPy = false; 
if not hasNumPy then error ///CohomologyZeroLociInHomogeneousVarieties requires the Python package numpy, 
which could not be found. Please install it by following the instructions in the Python package's tutorial on creating a virtual environment and installing NumPy: 
https://www.macaulay2.com/doc/Macaulay2/share/doc/Macaulay2/Python/html/___Python_sptutorial_co_spcreating_spa_spvirtual_spenvironment_spand_spinstalling_sp__Num__Py.html 
Then restart Macaulay2 with the Python package's "executable" option set to the virtual environment's Python executable, and reload this package.///;

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
