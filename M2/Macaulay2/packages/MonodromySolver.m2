-- -*- coding: utf-8 -*-
newPackage(
	"MonodromySolver",
    	Version => "1.12", 
    	Date => "Nov 2017",
    	Authors => {
	     {Name => "Timothy Duff", Email => "tduff3@gatech.edu"},
	     {Name => "Cvetelina Hill", Email => "cvetelina.hill@math.gatech.edu"},
	     {Name => "Anders Nedergaard Jensen", Email => "jensen@math.au.dk"},
	     {Name => "Kisun Lee", Email => "klee669@math.gatech.edu"},
	     {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"},
	     {Name => "Jeff Sommars", Email => "sommars1@uic.edu"}
	     },
    	HomePage => "http://www.math.gatech.edu/~leykin",
    	Headline => "solving polynomial systems via monodromy",
	PackageImports => {"PHCpack","NAGtypes"},
	PackageExports => {"NumericalAlgebraicGeometry"},
	AuxiliaryFiles => true, -- set to true if package comes with auxiliary files
  	DebuggingMode => false,		
  	--DebuggingMode => true,		 -- set to true only during development
  	CacheExampleOutput => true
    	)


-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists

export{"Edges", "Graph", "Node1", "Node2", "TargetSolutionCount", "Potential", "Vertices", "PartialSols", "SpecializedSystem", "Potential12",
     "gamma1", "gamma2", "Correspondence21", "Family", "Potential21", "Correspondence12", "homotopyGraph", "MonodromySolverOptions"}

debug NAGtypes
debug NumericalAlgebraicGeometry
debug Core
needs "./MonodromySolver/PointArray.m2"
needs "./MonodromySolver/HomotopyGraphTypes.m2"
needs "./MonodromySolver/solveViaMonodromy.m2"


beginDocumentation()
needs "./MonodromySolver/Documents/DocMonodromysolver.m2"
end

restart
uninstallPackage "MonodromySolver"
installPackage "MonodromySolver"
installPackage("MonodromySolver", RemakeAllDocumentation=>true)
check "MonodromySolver"
peek MonodromySolver
--help "OnesiteModificationA"
--viewHelp "OnesiteModificationA"
--examples "OnesiteModificationA"
viewHelp MonodromySolver
viewHelp potentialE

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=PackageTemplate pre-install"
-- End:
