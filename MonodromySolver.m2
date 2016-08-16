-- -*- coding: utf-8 -*-
newPackage(
	"MonodromySolver",
    	Version => "1.0", 
    	Date => "June, 2016",
    	Authors => {
	     {Name => "Anton Leykin", Email => "doe@math.uiuc.edu"},
	     {Name => "Timothy Duff", Email => "doe@math.uiuc.edu"},
	     {Name => "Kisun Lee", Email => "doe@math.uiuc.edu"},
	     {Name => "Cvetelina Hill", Email => "doe@math.uiuc.edu"}
	     },
    	HomePage => "http://www.math.uiuc.edu/~doe/",
    	Headline => "Solving polynomial systems via monodromy",
	PackageImports => {"PHCpack"},
	PackageExports => {"NumericalAlgebraicGeometry"},
	AuxiliaryFiles => true, -- set to true if package comes with auxiliary files
    	DebuggingMode => true		 -- set to true only during development
    	)


-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists

export{"Edges", "Graph", "Node1", "Node2", "TargetSolutionCount", "Potential", "Vertices", "PartialSols", "SpecializedSystem", "Potential12",
     "MasterFactor", "gamma1", "gamma2", "Correspondence21", "Family", "MasterNode", "Potential21", "Correspondence12"}

debug NAGtypes
debug NumericalAlgebraicGeometry
debug Core
needs "./MonodromySolver/PointArray.m2"
needs "./MonodromySolver/HomotopyGraphTypes.m2"
needs "./MonodromySolver/random_methods.m2"
needs "./MonodromySolver/solveViaMonodromy.m2"


