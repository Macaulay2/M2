newPackage(
    "CohomologyZeroLociInHomogeneousVarieties",
    Version => "0.1",
    Date => "Oct 31, 2026",
    Headline => "homogeneous vector bundles",
    Keywords => {"Algebraic Geometry"},
    Authors => {{ Name => "Alessandro Frassineti", Email => "alessandro.frassineti@edu.unige.it", HomePage => "https://sites.google.com/view/alessandrofrassineti-math/home-page/"}},
    HomePage => "https://github.com/K3Ale/package-HomogeneousVarieties", 
    AuxiliaryFiles => true,
    DebuggingMode => false,
    Reload => false,
    PackageImports => {"LieAlgebraRepresentations"},
    PackageExports => {"Schubert2","WeylGroups"}
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
    "factors",
    "homogeneousTangentBundle",
    "homogeneousCotangentBundle",
    "structureSheaf",
    "pullBack",
    "summands",
    "dominantConjugate",
    "isDominant",
    "isSingular",
    "isAmple",
    "isGloballyGenerated",
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
for i from 30 to 35 do check (i,"CohomologyZeroLociInHomogeneousVarieties")

restart
uninstallPackage "CohomologyZeroLociInHomogeneousVarieties"
restart
installPackage "CohomologyZeroLociInHomogeneousVarieties"
needsPackage "CohomologyZeroLociInHomogeneousVarieties"
viewHelp "CohomologyZeroLociInHomogeneousVarieties"


