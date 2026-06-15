
-*
   Copyright 2020-2026, Giovanni Staglianò.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or (at your option) any later version.
*-

newPackage(
    "SpecialFanoFourfolds",
    Version => "2.8",
    Date => "Apr 13, 2026",
    Authors => {{Name => "Giovanni Staglianò", Email => "giovanni.stagliano@unict.it" }},
    Headline => "Hodge-special fourfolds",
    Keywords => {"Algebraic Geometry"},
    AuxiliaryFiles => true,
    PackageImports => {"PrimaryDecomposition","TangentCone"},
    PackageExports => {"MultiprojectiveVarieties"},
    DebuggingMode => false,
    Reload => false,
    Certification => {
	"journal name" => "Journal of Software for Algebra and Geometry",
	"journal URI" => "https://msp.org/jsag/",
	"article title" => "The SpecialFanoFourfolds package in Macaulay2",
	"acceptance date" => "2024-04-14",
	"published article URI" => "https://msp.org/jsag/2024/14-1/p12.xhtml",
	"published article DOI" => "10.2140/jsag.2024.14.111",
	"published code URI" => "https://msp.org/jsag/2024/14-1/jsag-v14-n1-x12-SpecialFanoFourfolds.m2",
	"release at publication" => "67f7b2f777314d7f85c02a661d8e54f9d2c5e8d3",
	"version at publication" => "2.7.1",
	"volume number" => "14",
	"volume URI" => "https://msp.org/jsag/2024/14-1/"
	}
)

requiredMultiprojectiveVarietiesVersion := "2.7.1";
if MultiprojectiveVarieties.Options.Version < requiredMultiprojectiveVarietiesVersion then (
    <<endl<<"Your version of the MultiprojectiveVarieties package is outdated (required version "<<requiredMultiprojectiveVarietiesVersion<<" or newer);"<<endl;
    <<"you can manually download the latest version from"<<endl;
    <<"https://github.com/Macaulay2/M2/tree/development/M2/Macaulay2/packages."<<endl;
    <<"To automatically download the latest version of MultiprojectiveVarieties in your current directory,"<<endl;
    <<"you may run the following Macaulay2 code:"<<endl<<"***"<<endl<<endl;
    <<///run "curl -s -o MultiprojectiveVarieties.m2 https://raw.githubusercontent.com/Macaulay2/M2/development/M2/Macaulay2/packages/MultiprojectiveVarieties.m2";///<<endl<<endl<<"***"<<endl;
    error("required MultiprojectiveVarieties package version "|requiredMultiprojectiveVarietiesVersion|" or newer");
);

export{
    "HodgeSpecialFourfold",
    "specialFourfold",
    "CubicFourfold",
    "cubicFourfold",
    "GushelMukaiFourfold",
    "gushelMukaiFourfold",
    "DoublySpecialCubicFourfold",
    "IntersectionOfThreeQuadricsInP7",
    "CongruenceOfCurves",
    "detectCongruence",
    "ambientFivefold",
    "surface",
    "surfaces",
    "NumNodes",
    "InputCheck",
    "FanoMapType",
    "parameterCount",
    "normalSheaf",
    "unirationalParametrization",
    "toGrass",
    "isAdmissible",
    "isAdmissibleGM",
    "mirrorFourfold",
    "Singular",
    "associatedK3surface",
    "associatedCastelnuovoSurface",
    "building",
    "trisecantFlop",
    "GMtables",
    "fanoFourfold",
    "parametrizeFanoFourfold",
    "fromOrdinaryToGushel",
    "beauvilleMap",
    "polarizedK3surface",
    "latticePolarization",
    "swap"
}

needsPackage "IntegralClosure"; -- for method: normalization
needsPackage "CharacteristicClasses"; -- for method: eulerCharacteristic
needsPackage("RationalMaps",DebuggingMode=>false); -- for method: inverse3

debug SparseResultants
debug MultiprojectiveVarieties

load "./SpecialFanoFourfolds/HodgeSpecialFourfolds.m2";

load "./SpecialFanoFourfolds/CubicFourfolds.m2";

load "./SpecialFanoFourfolds/GushelMukai.m2";

load "./SpecialFanoFourfolds/IntersectionOfThreeQuadrics.m2";

load "./SpecialFanoFourfolds/DSCF.m2";

load "./SpecialFanoFourfolds/FanoMaps.m2";

load "./SpecialFanoFourfolds/Congruences.m2";

load "./SpecialFanoFourfolds/AssociatedSurfaces.m2";

load "./SpecialFanoFourfolds/contractionMaps.m2";

load "./SpecialFanoFourfolds/mirrorFourfolds.m2";

load "./SpecialFanoFourfolds/LatticePolarizedK3.m2";

load "./SpecialFanoFourfolds/examples.m2";

load "./SpecialFanoFourfolds/utils.m2";

load "./SpecialFanoFourfolds/HodgeSpecialSurfaces.m2";

load "./SpecialFanoFourfolds/docs.m2";

load "./SpecialFanoFourfolds/tests.m2";
