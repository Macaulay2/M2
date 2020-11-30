-- -*- coding: utf-8 -*-
-- licensed under GPL v2 or any later version
newPackage(
     "NAGtypes",
     Version => "1.14",
     Date => "Aug 2019",
     Headline => "types used in Numerical Algebraic Geometry",
     HomePage => "http://people.math.gatech.edu/~aleykin3/NAG4M2",
     AuxiliaryFiles => true,
     Authors => {
	  {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"}
	  },
     Keywords => {"Numerical Algebraic Geometry"},
     PackageExports => {"ReesAlgebra"}, -- avoids collision with "Jacobian" 
     -- DebuggingMode should be true while developing a package, 
     --   but false after it is done
     DebuggingMode => false 
     --DebuggingMode => true 
     )

export {
     -- service functions
     "generalEquations", 
     -- witness set
     "WitnessSet", "witnessSet", "equations", "slice", "points", 
     "Equations", "Slice", "Points", "ProjectionDimension", 
     "sliceEquations", "projectiveSliceEquations", "IsIrreducible", 
     "ProjectiveWitnessSet", "AffineChart", "projectiveWitnessSet",
     -- WSet
     "WSet", "Ambient", "SlicingVariety",
     -- numerical variety
     "NumericalVariety", "numericalVariety", "numericalAffineSpace",
     "ProjectiveNumericalVariety", "projectiveNumericalVariety",
     -- point (solution)
     "Point", "point", "coordinates",
     "project",
     "isRealPoint", "realPoints", "residual", "origin",
     "Norm", 
     "toAffineChart",
     "Tolerance", "sortSolutions", "areEqual", "isGEQ", "solutionsWithMultiplicity",
     "Coordinates", "SolutionStatus", "LastT", "ConditionNumber", "Multiplicity", 
     "NumberOfSteps", "ErrorBoundEstimate",
     "MaxPrecision", "WindingNumber", "DeflationNumber",
     -- values for status(Point) 
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
     "polySystem", "parameters",
     -- "segmentHomotopy"(defined in extraNAGtypes), "substituteContinuationParameter"(delete???), "specializeContinuationParameter"(delete???),
     -- dual space
     "DualSpace", "BasePoint", "dualSpace", "PolySpace", "polySpace", "Reduced", "Gens", "Space"
     }

-- DEBUG Core ----------------------------------------
debug Core -- to enable engine routines

Point = new Type of MutableHashTable 
load "./NAGtypes/WSet-abstract.m2"
System = new Type of MutableHashTable -- TODO: make it a HashTable
PolySystem = new Type of System
WitnessSet = new Type of WSet 
ProjectiveWitnessSet = new Type of WitnessSet
NumericalVariety = new Type of MutableHashTable 

load "./NAGtypes/PolySystem.m2"
load "./NAGtypes/Point.m2"

PointSet = new Type of HashTable
pointSet = method()
pointSet Thing := L -> (
    if not instance(L,List) and not instance(L,Set) then error "a list/set is expected"; 
    LL := toList L;
    if not all(LL, x->instance(x,Point)) then error "a list/set of Points is expected"; 
    S := solutionsWithMultiplicity LL;
    new PointSet from apply(#S, i->((S#i).Multiplicity=1; i=>S#i))	 
    ) 
net PointSet := P -> net values P

areEqual (PointSet,PointSet) := o-> (a,b) -> areEqual(values a, values b, o)
PointSet == PointSet := (A,B) -> areEqual(A,B)

unionPointSet = method(Options=>{Tolerance=>1e-6})
unionPointSet (PointSet,PointSet) := o -> (A,B) -> (
    S := solutionsWithMultiplicity(values A | values B, Tolerance=>o.Tolerance); 
    new PointSet from apply(#S, i->((S#i).Multiplicity=1; i=>S#i))	 
    )
PointSet + PointSet := (A,B) -> unionPointSet(A,B)  
PointSet - PointSet := (A,B) -> differencePointSet(A,B)
differencePointSet = method(Options=>{Tolerance=>1e-6})
differencePointSet (PointSet,PointSet) := o -> (A,B) -> (
    D := new MutableHashTable;
    i := 0; 
    j := 0;
    c := 0;
    while i<#A and j<#B do (
	a := isGEQ(A#i,B#j,Tolerance=>o.Tolerance);
	if not a then (D#c = A#i; i=i+1; c=c+1)
	else (
	    if areEqual(A#i,B#j,Tolerance=>o.Tolerance) then i=i+1;
	    j=j+1;
	    ) 
	);
    if j==#B then for i' from i to #A-1 do (D#c = A#i'; c=c+1);
    new PointSet from D
    )

TEST /// 
    needsPackage "NAGtypes"
    A = set {{{1,3}},{{2,5}},{{0,3}},{{1+ii,3}}} /point // pointSet
    B = {{{1,3.1}},{{0,3}}}/point//pointSet
    assert(A + B == {{{1,3}},{{2,5}},{{0,3}},{{1+ii,3}},{{1,3.1}}} /point // pointSet)	
    assert(A - B == {{{1, 3}}, {{1+ii, 3}}, {{2, 5}}}/point//pointSet)
///

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
-- projectiveDistance (Point,Point) := (a,b) -> projectiveDistance(coordinates a, coordinates b)

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
	si.Multiplicity = 1;
	j := i + 1;
	while j < #sorted and areEqual(sorted#j,si,o) do (
	    si.Multiplicity = si.Multiplicity + 1;
	    j = j + 1;
	    );
	i = j;
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

-------------------------------------------
-- PolySpace
--   Basis: a row matrix of polynomials representing dual functionals
-- (Basis is expected to be linearly independent and reduced w.r.t. the monomial order)
PolySpace = new Type of MutableHashTable
globalAssignment PolySpace
polySpace = method(Options => {Reduced => false})
polySpace PolySpace := S -> new PolySpace from S
polySpace Matrix := o -> M -> (
    assert(numrows M == 1);
    new PolySpace from {Gens=>M,Reduced=>o.Reduced}
    )

-------------------------------------------
-- DualSpace 
--   Space: a PolySpace (closed under derivation) 
--   BasePoint: a point of localization
--   Tolerance: tolerance used to compute the DualSpace or associated to it; 0 in the exact setting
DualSpace = new Type of MutableHashTable
globalAssignment DualSpace
dualSpace = method()
dualSpace DualSpace := L -> new DualSpace from L
dualSpace (PolySpace, Point) := (S,p)-> (
    assert(numgens ring S.Gens == #coordinates p);
    new DualSpace from {Space=>S,BasePoint=>p}
    )
dualSpace (Matrix, Point) := (M,p)-> dualSpace(polySpace M,p)
-- what other constructors would we have?

gens PolySpace := o -> S -> S.Gens
gens DualSpace := o -> L -> gens L.Space

net PolySpace := S -> net gens S
net DualSpace := L -> net gens L

dim PolySpace := S -> numcols gens S
dim DualSpace := L -> numcols gens L

ring PolySpace := S -> ring gens S
ring DualSpace := L -> ring gens L

point DualSpace := L -> L.BasePoint


-- extra types used (at this point) only by NumericalAlgebraicGeometry 
export { "Homotopy", "ParameterHomotopy", "SpecializedParameterHomotopy", 
    "evaluateH", "evaluateHt", "evaluateHx", "Parameters", "specialize"}

Homotopy = new Type of MutableHashTable -- abstract type
evaluateH = method()
evaluateH (Homotopy,Matrix,Number) := (H,x,t) -> error "not implemented"
evaluateHt = method()
evaluateHt (Homotopy,Matrix,Number) := (H,x,t) -> error "not implemented"
evaluateHx = method()
evaluateHx (Homotopy,Matrix,Number) := (H,x,t) -> error "not implemented"

ParameterHomotopy = new Type of MutableHashTable -- abstract type
evaluateH (ParameterHomotopy,Matrix,Matrix,Number) := (H,parameters,x,t) -> error "not implemented"
evaluateHt (ParameterHomotopy,Matrix,Matrix,Number) := (H,parameters,x,t) -> error "not implemented"
evaluateHx (ParameterHomotopy,Matrix,Matrix,Number) := (H,parameters,x,t) -> error "not implemented"

SpecializedParameterHomotopy = new Type of Homotopy
specialize = method()
specialize (ParameterHomotopy,Matrix) := (PH, M) -> (
    SPH := new SpecializedParameterHomotopy;
    SPH.ParameterHomotopy = PH;
    SPH.Parameters = M;
    SPH
    ) 
evaluateH (SpecializedParameterHomotopy,Matrix,Number) := (H,x,t) -> evaluateH(H.ParameterHomotopy,H.Parameters,x,t) 
evaluateHt (SpecializedParameterHomotopy,Matrix,Number) := (H,x,t) -> evaluateHt(H.ParameterHomotopy,H.Parameters,x,t) 
evaluateHx (SpecializedParameterHomotopy,Matrix,Number) := (H,x,t) -> evaluateHx(H.ParameterHomotopy,H.Parameters,x,t) 

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


load "./NAGtypes/WSet-ambient.m2"
load "./NAGtypes/WSet-proxy.m2"
load "./NAGtypes/WSet-projective.m2"
load "./NAGtypes/WSet-multiaffine.m2"
 
-- DOCUMENTATION ------------------------------------------------------
beginDocumentation()

load "./NAGtypes/doc-NAGtypes.m2"

undocumented {BasePoint,origin,(origin,Ring),Gens,Space} --Robert???
undocumented {
    evaluateHt, (evaluateHt,Homotopy,Matrix,Number), (evaluateHt,ParameterHomotopy,Matrix,Matrix,Number), (evaluateHt,SpecializedParameterHomotopy,Matrix,Number), 
    evaluateHx, (evaluateHx,Homotopy,Matrix,Number), (evaluateHx,ParameterHomotopy,Matrix,Matrix,Number), (evaluateHx,SpecializedParameterHomotopy,Matrix,Number),
    evaluateH, (evaluateH,Homotopy,Matrix,Number), (evaluateH,ParameterHomotopy,Matrix,Matrix,Number), (evaluateH,SpecializedParameterHomotopy,Matrix,Number)
    }

undocumented {(toExternalString,Point), (toExternalString,PolySystem),
    unionPointSet,  (unionPointSet,PointSet,PointSet), pointSet, (pointSet,Thing), (areEqual,PointSet,PointSet), PointSet,
    differencePointSet, (differencePointSet,PointSet,PointSet), specialize, (specialize,ParameterHomotopy,Matrix),
    (symbol ==,PointSet,PointSet), (symbol ?,Point,Point), (net,PointSet), 
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
