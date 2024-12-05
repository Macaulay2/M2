-- -*- coding: utf-8 -*-
newPackage(
	"MonodromySolver",
    	Version => "1.16", 
    	Date => "May 2023",
    	Authors => {
	     {Name => "Timothy Duff", Email => "timduff@uw.edu"},
	     {Name => "Cvetelina Hill", Email => "cvetelina.hill@math.gatech.edu"},
	     {Name => "Anders Nedergaard Jensen", Email => "jensen@math.au.dk"},
	     {Name => "Kisun Lee", Email => "klee669@math.gatech.edu"},
	     {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"},
	     {Name => "Jeff Sommars", Email => "sommars1@uic.edu"}
	     },
    	HomePage => "http://www.math.gatech.edu/~leykin",
    	Headline => "solving polynomial systems via monodromy",
	Keywords => {"Numerical Algebraic Geometry"},
	PackageImports => {"gfanInterface","NAGtypes"},
	PackageExports => {"NumericalAlgebraicGeometry"},
	AuxiliaryFiles => true,
  	DebuggingMode => false,		
  	--DebuggingMode => true,		 -- set to true only during development
  	CacheExampleOutput => false
--        OptionalComponentsPresent => true --  set to true only during development
    	)


-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists

export{"Edges", "Graph", "Node1", "Node2", "TargetSolutionCount", "Potential", "Vertices", "PartialSols", "SpecializedSystem", "Potential12", "gamma1", "gamma2", "Correspondence21", "Family", "Potential21", "Correspondence12", "homotopyGraph", "MonodromySolverOptions", "monodromyGroup"}

debug NAGtypes
debug NumericalAlgebraicGeometry
debug Core
needs "./MonodromySolver/misc.m2"
needs "./MonodromySolver/Systems.m2"
needs "./MonodromySolver/PointArray.m2"
needs "./MonodromySolver/HomotopyGraphTypes.m2"
needs "./MonodromySolver/solveViaMonodromy.m2"
needs "./MonodromySolver/galois-group.m2"
needs "./MonodromySolver/Tests.m2"


beginDocumentation()
needs "./MonodromySolver/Documentation.m2"
end

restart
load "./MonodromySolver.m2"
check "MonodromySolver"
vars(x,y,a)
f=gateSystem(gateMatrix{{a}}, gateMatrix{{x,y}}, transpose gateMatrix{{x^2-a^2, x-a, y-1}})
a0 = point{{2.0}}
xy0 = point{{2.0,1.0}}
debug MonodromySolver
squareDown(a0,xy0,f)

elapsedTime load "monodromy-rotary.m2"
setRandomSeed 0
(p0,x0)=seedNewton(G, Iterations=>infinity)

elapsedTime load "monodromy-rotary.m2"
setRandomSeed 0
(p0,x0)=seedNewton(G, Iterations=>infinity)
debug MonodromySolver
(p0,x0)=newtonHomotopy(G)


areEqual(0, norm evaluate(G, point((random CC) * matrix p0), x0))
assert(norm evaluate(G,point p0,point x0) < 1e-6)
assert(S:=first SVD evaluateJacobian(G, p0, x0); min S > 1e-6)
elapsedTime V=first monodromySolve(G, p0, {x0}, Verbose=>true)


uninstallPackage "MonodromySolver"
restart
installPackage "MonodromySolver"
check "MonodromySolver"

uninstallPackage "MonodromySolver"
restart
installPackage("MonodromySolver", RemakeAllDocumentation=>true, RerunExamples=>true)
viewHelp MonodromySolver
check "MonodromySolver"
