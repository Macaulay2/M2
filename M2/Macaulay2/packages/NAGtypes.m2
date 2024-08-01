-- -*- coding: utf-8 -*-
-- licensed under GPL v2 or any later version
newPackage(
     "NAGtypes",
     Version => "1.21",
     Date => "Nov 2022",
     Headline => "types used in Numerical Algebraic Geometry",
     HomePage => "http://people.math.gatech.edu/~aleykin3/NAG4M2",
     AuxiliaryFiles => true,
     Authors => {
	  {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"}
	  },
     Keywords => {"Numerical Algebraic Geometry"},
     PackageExports => {"NumericalLinearAlgebra"}, 
     -- DebuggingMode should be true while developing a package, 
     --   but false after it is done
     DebuggingMode => false 
     --DebuggingMode => true 
     )

export {
     -- service functions
     "generalEquations", 
     -- witness set
     "WitnessSet", "witnessSet", "equations", "slice", "points", "declareIrreducible", 
     "Equations", "Slice", "Points", "ProjectionDimension", 
     "sliceEquations", "projectiveSliceEquations", "IsIrreducible", 
     "ProjectiveWitnessSet", "AffineChart", "projectiveWitnessSet",
     -- WSet
     "WSet", "Ambient", "SlicingVariety",
     -- numerical variety
     "NumericalVariety", "numericalVariety", "numericalAffineSpace",
     "ProjectiveNumericalVariety", "projectiveNumericalVariety",
     -- point (solution)
     "AbstractPoint", "Point", "point", "coordinates",
     "project",
     "isRealPoint", "realPoints", "residual", "origin",
     "Norm", 
     "toAffineChart",
     "sortSolutions", "areEqual", "isGEQ", "solutionsWithMultiplicity",
     "Coordinates", "SolutionStatus", "LastT", "ConditionNumber", "Multiplicity", 
     "NumberOfSteps", "ErrorBoundEstimate",
     "MaxPrecision", "WindingNumber", "DeflationNumber",
     -- values for status(AbstractPoint) 
     "Regular", "Singular", "Infinity", 
     "MinStepFailure", "NumericalRankFailure", "RefinementFailure", 
     "Origin", "IncreasePrecision", "DecreasePrecision", 
     -- point sets 
     "PointSet", "pointSet", "unionPointSet", "differencePointSet",
     -- systems
     "System", "numVariables", "numFunctions", "numParameters", 
     "evaluate", "evaluateJacobian",
     "PolySystem", "NumberOfPolys", "NumberOfVariables", "PolyMap", 
     "ContinuationParameter", "SpecializationRing",
     "polySystem", "parameters"
     }

-- DEBUG Core ----------------------------------------
debug Core -- to enable engine routines

load "./NAGtypes/Point-abstract.m2"
load "./NAGtypes/WSet-abstract.m2"
System = new Type of MutableHashTable -- TODO: make it a HashTable
PolySystem = new Type of System
WitnessSet = new Type of WSet 
ProjectiveWitnessSet = new Type of WitnessSet
NumericalVariety = new Type of MutableHashTable 

-- methods involving several abstract types
residual = method(Options=>{Norm=>2})
residual (System,AbstractPoint) := o -> (s,p) -> error "not implemented"

load "./NAGtypes/Point.m2"
load "./NAGtypes/PolySystem.m2"
load "./NAGtypes/PointSet.m2"

-* not exported. obsolete?

diffSolutions = method(TypicalValue=>Sequence, Options=>{Tolerance=>1e-3})
-- in:  A, B (presumably sorted)
-- out: (a,b), where a and b are lists of indices where A and B differ
diffSolutions (List,List) := o -> (A,B) -> (
     i := 0; j := 0;
     a := {}; b := {};
     while i<#A and j<#B do 
     if areEqual(A#i,B#j) then (i = i+1; j = j+1)
     else if isGEQ(A#i,B#j) then (b = append(b,j); j = j+1)
     else (a = append(a,i); i = i+1);	  
     (a|toList(i..#A-1),b|toList(j..#B-1))	      	    
     )

*-

toAffineChart = method() -- coordinates of the point (x_0:...:x_n) in the k-th affine chart
toAffineChart (ZZ,List) := List => (k,x) -> (
     if k<0 or k>#x then error "chart number is out of range ";
     if x#k == 0 then return infinity;
     y := apply(x, c->c/x#k);
     take(y,k) | drop(y,k+1)
     ) 

-- !!! this seems to be unused
-- projectiveDistance = method()
-- projectiveDistance (List,List) := (a,b) -> acos((abs sum(a,b,(x,y)->x*conjugate y)) / ((norm2 a) * (norm2 b)))
-- projectiveDistance (AbstractPoint,AbstractPoint) := (a,b) -> projectiveDistance(coordinates a, coordinates b)

solutionDuplicates = method(TypicalValue=>MutableHashTable, Options=>{Tolerance=>1e-6})
solutionDuplicates List := o -> sols -> ( 
-- find positions of duplicate solutions
-- IN: list of solutions
-- OUT: H = MutableHashTable with entries of the form i=>j (sols#i is a duplicate for sols#j);
--      connected components (which are cycles) in the graph stored in H correspond to clusters of "duplicates" 
--      i=>i indicates a nonduplicate
     H := new MutableHashTable;
     for j from 0 to #sols-1 do (
	  H#j = j;
	  i := j-1;
	  while i>=0 do
	  if areEqual(sols#i,sols#j,o) then (
	       H#j = H#i;
	       H#i = j;
	       i = -1
	       ) 
	  else i = i - 1;
	  );
     H
     )

groupClusters = method()
groupClusters MutableHashTable := H -> (
-- processes the output of solutionDuplicates to get a list of clusters of solutions
     cs := {};
     apply(keys H, a->if H#a=!=null then (
	       c := {a};
	       b := H#a; 
	       H#a = null;
	       while b != a do (
	       	    c = c | {b};
	       	    bb := H#b;
		    H#b = null;
		    b = bb;
	       	    );
	       cs = cs | {c};
	       ));
     cs
     )

clusterSolutions = method(TypicalValue=>List, Options=>{Tolerance=>1e-6})
clusterSolutions List := o-> sols -> ( 
    -- time sorted' := sortSolutions(sols,o);
    -- time sorted'' := sort sols;
    sorted := sortSolutions(sols,Weights=>{});
    i := 0; 
    while i<#sorted list (
	si := sorted#i;
	mult := 1;
	j := i + 1;
	while j < #sorted and areEqual(sorted#j,si,o) do (
	    mult = mult + 1;
	    j = j + 1;
	    );
	i = j;
	si.cache.Multiplicity = mult;
	si
	) 
    )
solutionsWithMultiplicity = method(TypicalValue=>List, Options=>{Tolerance=>1e-6})
solutionsWithMultiplicity List := o-> sols -> sortSolutions clusterSolutions(sols,o)

TEST ///
a = point {{0,1}}
b = point {{0.000000001,1+0.00000000001*ii}}
c = point {{0.001*ii,1}}
assert (# solutionsWithMultiplicity {a,b,c} == 2)
///

load "./NAGtypes/WitnessSet.m2"

---------------------------------------------
-- AUXILIARY FUNCTIONS
---------------------------------------------
checkCCpolynomials = method()
checkCCpolynomials List := F -> (    
    if #F > 0 then R := commonRing F else error "expected a nonempty list of polynomials";
    if not instance(R, PolynomialRing) then error "expected input in a polynomial ring"; 
    coeffR := coefficientRing R; 
    if not(
	instance(ring 1_coeffR, ComplexField) 
	or instance(ring 1_coeffR, RealField)
	or coeffR===QQ or coeffR ===ZZ
	) then error "expected coefficients that can be converted to complex numbers";  
    if any(F, f->ring f =!= R) then error "expected all polynomials in the same ring";
    )
checkCCpolynomials (List,List) := (S,T) -> (
    n := #T;
    if #S != n then error "expected same number of polynomials in start and target systems";
    ST := checkCCpolynomials(S|T);
    )

generalEquations = method()
-- make k random linear combinations of gens I
generalEquations (ZZ,Ideal) := (k,I) -> (
     R := ring I;
     ngens := numgens I;
     ideal ( (gens I) * random(R^ngens, R^k) )
     )
generalEquations (ZZ,List) := (k,F) -> (generalEquations(k,ideal F))_*;


-- change the equations to be general change of vars, if not a CI
-- the output is a new witness set, with the same points and slice.
generalEquations WitnessSet := (W) -> (
     R := ring W;
     n := numgens R;
     d := dim W;
     ngens := numgens ideal W;
     if ngens === n-d then W
     else (
	  -- take random combinations of the equations
	  neweqns := (generators ideal W) * random(R^ngens, R^(n-d));
	  witnessSet(ideal neweqns, slice W, points W))
     )

TEST /// -- miscellaneous tests
CC[x,y]
S = polySystem {x^2+y^2-6, 2*x^2-y}
p = point({{1.0_CC,2.3_CC}, ConditionNumber=>1000, ErrorBoundEstimate =>0.01});
assert (round (1000*norm(4.5,p)) == 2312)
assert isRealPoint p
assert(round (10000*residual(S,p)) == 4173)
p2 =  point {{1.001,2.3+ii}}
p3 =  point {{.999,2.3+ii}}
assert areEqual(sortSolutions {p,p2,p3}, {p3,p,p2})
///

load "./NAGtypes/PolyDualSpaces.m2"
load "./NAGtypes/WSet-ambient.m2"
load "./NAGtypes/WSet-proxy.m2"

toChart = method()
-- load "./NAGtypes/WSet-projective.m2" -- convert to HashTable
load "./NAGtypes/WSet-multiaffine.m2"
 
-- DOCUMENTATION ------------------------------------------------------
beginDocumentation()

load "./NAGtypes/doc-NAGtypes.m2"

undocumented {BasePoint,origin,(origin,Ring),Gens,Space} --Robert???

undocumented {(toExternalString,Point), (toExternalString,PolySystem),
    unionPointSet,  (unionPointSet,PointSet,PointSet), pointSet, (pointSet,Thing), (areEqual,PointSet,PointSet), PointSet,
    differencePointSet, (differencePointSet,PointSet,PointSet), 
    (symbol ==,PointSet,PointSet), (net,PointSet), 
    (symbol +,PointSet,PointSet), (symbol -,PointSet,PointSet),
    }

endPackage "NAGtypes" 

end

restart
loadPackage "NAGtypes"
uninstallPackage "NAGtypes"
installPackage "NAGtypes"
installPackage ("NAGtypes",RerunExamples=>true, RemakeAllDocumentation=>true)
installPackage ("NAGtypes",RerunExamples=>false, RemakeAllDocumentation=>true)
-- install docs with no absolute links
uninstallPackage "Style"
installPackage("Style")
installPackage("NAGtypes")

installPackage ("NAGtypes", MakeDocumentation=>false)
check "NAGtypes"
