-- -*- coding: utf-8 -*-
-- licensed under GPL v2 or any later version
newPackage(
    "LieAlgebraRepresentations",
    Version => "1.0",
    Date => "Nov 8, 2025",
    AuxiliaryFiles=>true,
    Headline => "Lie algebra representations and characters",
    Authors => {
	  {   Name => "Dave Swinarski",
	      Email => "dswinarski@fordham.edu",
	      HomePage => "https://faculty.fordham.edu/dswinarski/"
	  },
	  {
	      Name => "Paul Zinn-Justin", -- starting with version 0.6
	      Email => "pzinn@unimelb.edu.au",
	      HomePage => "http://blogs.unimelb.edu.au/paul-zinn-justin/"}
	  },
    Keywords => {"Lie Groups and Lie Algebras"},
    PackageImports => {"ReesAlgebra"},
    PackageExports => {"SpechtModule","Polyhedra","Isomorphism","AssociativeAlgebras"},
    DebuggingMode => false,
    Certification => {
	-- same article as for package ConformalBlocks
	"journal name" => "The Journal of Software for Algebra and Geometry",
	"journal URI" => "https://msp.org/jsag/",
	"article title" => "Software for computing conformal block divisors on bar M_0,n",
	"acceptance date" => "2 August 2018",
	"published article URI" => "https://msp.org/jsag/2018/8-1/p08.xhtml",
	"published article DOI" => "10.2140/jsag.2018.8.81",
	"published code URI" => "https://msp.org/jsag/2018/8-1/jsag-v8-n1-x08-LieTypes.m2",
	"release at publication" => "923fbcc7c77b23f510bb0d740e00fc1722a2f397",	    -- git commit number in hex
	"version at publication" => "0.5",
	"volume number" => "8",
	"volume URI" => "https://msp.org/jsag/2018/8-1/",
	"legacy name" => "LieTypes"
	}
    )


-- See version history in "./LieAlgebraRepresentations/versionhistory.txt"

needs "./LieAlgebraRepresentations/importsandexports.m2"
needs "./LieAlgebraRepresentations/lieAlgebras.m2"
needs "./LieAlgebraRepresentations/lieAlgebraModules.m2"
needs "./LieAlgebraRepresentations/lieAlgebraBases.m2"
needs "./LieAlgebraRepresentations/representationsCasimirReynolds.m2"
needs "./LieAlgebraRepresentations/basesAsWords.m2"
needs "./LieAlgebraRepresentations/deGraafAlgorithm.m2"
needs "./LieAlgebraRepresentations/gelfandTsetlin.m2"
needs "./LieAlgebraRepresentations/symWedgeTensor.m2"
needs "./LieAlgebraRepresentations/spinRepresentations.m2"
needs "./LieAlgebraRepresentations/highestWeightVectorsAndSubmodules.m2"

beginDocumentation()
needs "./LieAlgebraRepresentations/documentation.m2"


endPackage "LieAlgebraRepresentations" 
