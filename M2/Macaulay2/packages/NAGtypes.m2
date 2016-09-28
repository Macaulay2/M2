-- -*- coding: utf-8 -*-
-- licensed under GPL v2 or any later version
newPackage(
     "NAGtypes",
     Version => "1.9",
     Date => "Apr 2016",
     Headline => "Common types used in Numerical Algebraic Geometry",
     HomePage => "http://people.math.gatech.edu/~aleykin3/NAG4M2",
     Authors => {
	  {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"}
	  },
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
     -- polynomial systems
     "PolySystem", "NumberOfPolys", "NumberOfVariables", "PolyMap", "Jacobian", -- "JacobianAndPolySystem", 
     "ContinuationParameter", "SpecializationRing",
     "polySystem", 
     -- "segmentHomotopy", "substituteContinuationParameter", "specializeContinuationParameter",
     "evaluate",
     -- dual space
     "DualSpace", "BasePoint", "dualSpace", "PolySpace", "polySpace", "Reduced", "Gens", "Space"
     }

-- DEBUG Core ----------------------------------------
debug Core -- to enable engine routines

PolySystem = new Type of MutableHashTable
Point = new Type of MutableHashTable 
-- ProjectivePoint = new Type of Point -- do we really need this?
WitnessSet = new Type of MutableHashTable 
ProjectiveWitnessSet = new Type of WitnessSet
NumericalVariety = new Type of MutableHashTable 
ProjectiveNumericalVariety = new Type of NumericalVariety

-----------------------------------------------------------------------
-- POLYSYSTEM = {
--   NumberOfVariables => ZZ,
--   NumberOfPolys => ZZ,
--   PolyMap => Matrix, a column matrix over a polynomial ring (usually with complex coeffiecients)
--           or SLP;
--   Jacobian => Matrix or SLP, the jacobian of PolyMap
--   JacobianAndPolySystem => SLP, a circuit evaluating PolyMap and Jacobian (the two are not necessary then)
--   }
PolySystem.synonym = "polynomial system"
net PolySystem := p -> (
    if hasAnAttribute p then (
	if hasAttribute(p,PrintNet) then return getAttribute(p,PrintNet);
  	if hasAttribute(p,PrintNames) then return net getAttribute(p,PrintNames);
  	if hasAttribute(p,ReverseDictionary) then return toString getAttribute(p,ReverseDictionary);
  	);
     if p.?PolyMap then net p.PolyMap
     else if p.?NumberOfPolys and p.?NumberOfVariables 
     then net "a system of " | net p.NumberOfPolys | " polynomials in " | net p.NumberOfVariables | " variables" 
     else error "the polynomial system is corrupted"
    ) 
globalAssignment PolySystem

polySystem = method()
polySystem PolySystem := P -> new PolySystem from P
polySystem List := L -> (
    checkCCpolynomials L;
    polySystem transpose matrix {L} 
    )
polySystem Matrix := M -> (
    assert(numcols M == 1);
    new PolySystem from {PolyMap=>M, NumberOfVariables=>numgens ring M, NumberOfPolys=>numrows M}
    )
polySystem Ideal := I -> polySystem transpose gens I

ring PolySystem := P -> ring P.PolyMap -- change this for SLP!!!
equations = method() -- returns list of equations
equations PolySystem := P -> flatten entries P.PolyMap -- change this for SLP!!!
ideal PolySystem := P -> ideal P.PolyMap -- change this for SLP!!!
toExternalString PolySystem := P -> "polySystem " | toExternalString equations P 

isHomogeneous PolySystem := P -> isHomogeneous ideal P.PolyMap -- change this for SLP!!!
XXXapply = method()
XXXapply(PolySystem,Function) := (P,f) -> polySystem apply(XXXtoList P, f) -- does not work for SLPs
substitute(PolySystem,Ring) := (P,R) -> polySystem sub(P.PolyMap, R) -- does not work for SLPs
XXXtoList = method()
XXXtoList PolySystem := P -> if P.?PolyMap then flatten entries P.PolyMap else error "polynomial system is not represented by a matrix"
homogenize (PolySystem,Ring,RingElement) := (P,R,h) -> polySystem homogenize(sub(P.PolyMap,R),h)
isSquare = method()
isSquare PolySystem := P -> P.NumberOfPolys == P.NumberOfVariables
evaluate = method()
evaluate (PolySystem,Point) := (P,p) -> evaluate(P, matrix p)
evaluate (Matrix,Point) := (M,p) -> evaluate(M, matrix p)
evaluate (PolySystem,Matrix) := (P,X) -> (
    if class P.PolyMap === Matrix 
    then evaluate(P.PolyMap,X)
    else error "evaluation not implemented for this type of PolyMap"
    )    
evaluate (Matrix,Matrix) := (M,X) ->  (
    C := coefficientRing ring M;
    if numColumns X == 1 then sub(M,sub(transpose X,C))
    else if numRows X == 1 then sub(M,sub(X,C))
    else error "expected a row or a column vector"
    )
evaluate (PolySystem,Point) := (P,p) -> evaluate(P,matrix p)

jacobian PolySystem := P -> (
    if P.?Jacobian then P.Jacobian
    else P.Jacobian = transpose jacobian(transpose P.PolyMap) -- TO DO: make "jacobian" work for SLPs
    )


-----------------------------------------------------------------------
-- POINT = {
--   Coordinates => List of CC,
--   NumberOfSteps => ZZ, -- number of steps made while tracking the path
--   SolutionStatus => {Regular, Singular, Infinity, MinStepFailure, NumericalRankFailure, RefinementFailure, Origin, null}
--   LastT => RR in [0,1]
--   ConditionNumber => condition number of the Jacobian
--   ErrorBoundEstimate => absolute error bound estimate (from Newton's method)
--   Multiplicity => the multiplicity (sometimes: the number of paths converging to the point) 
--   MaxPrecision => max precision used during the homotopy tracking
--   WindingNumber => used in the end-games
--   DeflationNumber => number of first-order deflations 
--   [SolutionSystem]        -- a square system that the point satisfies (used to compute the point)
--   [LiftedSystem]          -- a regularization of SolutionSystem (in case the point is not regular)
--   [LiftedPoint]           -- the corresponding solution of the LiftedSystem
--   }
Point.synonym = "point"
net Point := p -> (
    if hasAnAttribute p then (
	if hasAttribute(p,PrintNet) then return getAttribute(p,PrintNet);
  	if hasAttribute(p,PrintNames) then return net getAttribute(p,PrintNames);
  	if hasAttribute(p,ReverseDictionary) then return toString getAttribute(p,ReverseDictionary);
  	);
    s := if p.?SolutionStatus then p.SolutionStatus else Regular;
    if s === Regular then net p.Coordinates 
    else if s === Singular then net toSequence p.Coordinates
    else if s === MinStepFailure then net "[M,t=" | net p.LastT | net "]"
    else if s === Infinity then net "[I,t=" | net p.LastT | net "]"
    else if s === NumericalRankFailure then net "[NF]"
    else if s === RefinementFailure then net "[RF]"
    else if s === IncreasePrecision then net "[P+]"
    else if s === DecreasePrecision then net "[P-]"         
    else if s === Origin then net "[0]"         
    else error "the point is corrupted"
    ) 
globalAssignment Point

point = method()
point Point := p -> new Point from p
point List := s -> new Point from {Coordinates=>(
	c := first s;
	if instance(c,List) then c	
	else if instance(c,Matrix) or instance(c,MutableMatrix) then flatten entries c
	else error "wrong type of coordinates: List or Matrix expected"   
	)} | drop(s,1)
point Matrix := M -> point {flatten entries M} 
toExternalString Point := p -> "point { " | toExternalString coordinates p |" }"

Point == Point := (a,b) -> areEqual(a,b) -- the default Tolerance is used
Point ? Point := (a,b) -> if isGEQ(a,b) then symbol > else symbol < 


coordinates = method()
coordinates Point := p -> p.Coordinates

status Point := o -> p -> if p.?SolutionStatus then p.SolutionStatus else null
matrix Point := o -> p -> matrix {coordinates p}

project = method()
-- project point to the first n coordinates
project (Point,ZZ) := (p,n) -> (
    p' := point { take(coordinates p, n) };
    if p.?ErrorBoundEstimate then p'.ErrorBoundEstimate = p.ErrorBoundEstimate;
    p'
    )
   
norm (Thing, Point) := (no,p) -> norm(no, coordinates p)
norm (Thing, Matrix) := (no,M) -> norm(no, flatten entries M)
norm (Thing, List) := (no,p) -> (
     if instance(no,InfiniteNumber) and no === infinity then return max(p/abs);
     assert((instance(no, ZZ) or instance(no, QQ) or instance(no, RR)) and no>0);
     (sum(p, c->abs(c)^no))^(1/no)
     )
 
residual = method(Options=>{Norm=>2})
residual (List,Point) := o->(S,p)-> residual(polySystem S,p,o)
residual (PolySystem,Point) := o->(P,p)-> residual(P.PolyMap,matrix p,o)
residual (Matrix,Matrix) := o->(S,p)->norm(o.Norm, point evaluate(S,p))

isRealPoint = method(Options=>{Tolerance=>1e-6})
isRealPoint Point := o -> p -> norm (coordinates p / imaginaryPart) < o.Tolerance

realPoints = method(Options=>{Tolerance=>1e-6})
realPoints List := o -> pp -> select(pp, isRealPoint)

areEqual = method(TypicalValue=>Boolean, Options=>{Tolerance=>1e-6, Projective=>false})
areEqual (List,List) := o -> (a,b) -> #a == #b and all(#a, i->areEqual(a#i,b#i,o))
areEqual (BasicList,BasicList) := o-> (a,b) -> areEqual(toList a, toList b, o)
areEqual (Number,Number) := o -> (a,b) -> areEqual(toCC a, toCC b, o)
areEqual (CC,CC) := o -> (a,b) -> (
     abs(a-b) < o.Tolerance
     ) 
areEqual (Matrix,Matrix) := o -> (a,b) -> (
     areEqual(flatten entries a, flatten entries b, o)
     ) 
areEqual (MutableMatrix,MutableMatrix) := o -> (a,b) -> (
     areEqual(flatten entries a, flatten entries b, o)
     ) 
areEqual (Point,Point) := o -> (a,b) -> (
    a = a.Coordinates; 
    b = b.Coordinates;
    if o.Projective 
    then (1 - abs sum(a,b,(x,y)->x*conjugate y))/((norm(2,a)) * (norm(2,b))) < o.Tolerance  -- projective distance is too rough in practice
    else (
	na := norm(2,a); 
	nb := norm(2,b);
	if na > 1 or nb > 1 then norm(2,(a-b)) < o.Tolerance * max(na,nb)
	else norm(2,a-b) < o.Tolerance -- in case both points are close to the origin, absolute error is looked at 
	)
    )
areEqual (Point,BasicList) := o -> (a,b) -> areEqual(coordinates a, toList b)
areEqual (BasicList,Point) := o -> (a,b) -> areEqual(toList a, coordinates b)

origin = method(TypicalValue=>Point)
origin Ring := R -> point matrix{toList(numgens R:0_(ultimate(coefficientRing, R)))};

isGEQ = method(TypicalValue=>Boolean, Options=>{Tolerance=>1e-6})
isGEQ(Point,Point) := o->(t,s)-> isGEQ(coordinates t, coordinates s, o)
isGEQ(List,List) := o->(t,s)-> (
     n := #t;
     for i from 0 to n-1 do ( 
	  if not areEqual(t#i,s#i, Tolerance=>o.Tolerance) 
	  then 
	  if abs(realPart t#i - realPart s#i) < o.Tolerance then 
	  return imaginaryPart t#i > imaginaryPart s#i
	  else return realPart t#i > realPart s#i
	  ); 
     true -- if approx. equal 
     )

sortSolutionsWithWeights = method()
sortSolutionsWithWeights (List, List) := (sols,w) -> (
    n := #coordinates first sols;
    solsCoords := sols/coordinates; 
    R := commonRing solsCoords;
    if #w === 0 then w = for i to n list random R
    else if n =!= #w then error "weight list is of wrong length";
    dot := (a,b) -> sum(n,i->a#i*b#i);
    --print "-- in sortSolutionsWithWeights ----------";
    L := matrix{for s in solsCoords list dot(w,s)};    
    --print(w,L); 
    sortedCols := sortColumns L;
    sols_sortedCols
    ) 

{*
sortSolutionsWithWeights = method()
sortSolutionsWithWeights (List, List) := (sols,w) -> (
    n := #coordinates first sols;
    R := ring matrix first sols;
    if #w === 0 then w = random(R^1,R^n)
    else (
	if n =!= #w then error "weight list is of wrong length";
	w = matrix{w};
	);
    print "-- in sortSolutionsWithWeights ----------";
    time M := transpose matrix(sols/coordinates); 
    time L := w*M;    
    time sortedCols := sortColumns L;
    time sols_sortedCols
    ) 
*}

sortSolutions = method(TypicalValue=>List, Options=>{Tolerance=>1e-6,Weights=>null})
sortSolutions List := o -> sols -> (
-- sorts numerical solutions     
     if #sols == 0 then (
	 sorted := {};
	 sols
	 )
     else (
     	 if o.Weights =!= null then return sortSolutionsWithWeights(sols,o.Weights);
	 sorted = {0};
	 get'coordinates := sol -> if class sol === Point then coordinates sol else 
	     if ancestor(BasicList, class sol) then toList sol
	     else error "expected Points or BasicLists";
	 scan(#sols-1, s->(
		    -- find the first element that is "larger";
		    -- "larger" means the first coord that is not (approx.) equal 
		    -- has (significantly) larger realPart, if tie then larger imaginaryPart
		    --l := position(sorted, t->isGEQ(first t, first s));
     	       	    s = s + 1;
		    t := get'coordinates sols#s;
		    l := 0; r := #sorted-1;
		    if isGEQ(t, get'coordinates sols#(sorted#r)) then  sorted = sorted | {s}
		    else if isGEQ(get'coordinates sols#(sorted#l),t) then  sorted = {s} | sorted 
		    else (
		    	 while r-l>0 do (
			      m := (l+r)//2;
			      if isGEQ(get'coordinates sols#(sorted#m), t) then r=m
			      else l=m+1; 
			      );
		    	 sorted = take(sorted,r) | {s} | drop(sorted,r);
		    	 )
		    ));      
	  );
     apply(sorted, i->sols#i)
     )

TEST /// -- point comparison and sorting
e = 0.0001
assert areEqual(point{{0,e}},point{{0,0}},Tolerance=>2*e)
assert areEqual(point{{0,e}},point{{e,0}},Tolerance=>2*e)
assert areEqual(point{{10,e}},point{{10+5*e,0}},Tolerance=>e)
assert not areEqual(point{{10,e}},point{{10+10*e,0}},Tolerance=>e)

A = point{{1,0}}; B = point{{0,2}}; A' = point{{1+e,0}}; B' = point{{0,2+e}};
assert( sortSolutions({A,A',B,B'}, Tolerance=>10*e) == sortSolutions({B',A,B,A'}, Tolerance=>10*e) )
assert areEqual(
    sortSolutions({A,B,B'},Tolerance=>2*e),
    sortSolutions({B',B,A'},Tolerance=>2*e),
    Tolerance=>2*e
    )
///

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
    restart
    needsPackage "NAGtypes"
    A = set {{{1,3}},{{2,5}},{{0,3}},{{1+ii,3}}} /point // pointSet
    B = {{{1,3.1}},{{0,3}}}/point//pointSet
    assert(A + B == {{{1,3}},{{2,5}},{{0,3}},{{1+ii,3}},{{1,3.1}}} /point // pointSet)	
    assert(A - B == {{{1, 3}}, {{1+ii, 3}}, {{2, 5}}}/point//pointSet)
///

{* not exported. obsolete?

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

*}

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

-----------------------------------------------------------------------
-- WITNESS SET = {
--   Equations,            -- an ideal or a polynomial system or a list
--   Slice,                -- a matrix of coefficients of linear equations 
--                            (e.g., row [1,2,3] corresponds to x+2y+3=0)
--   Points,	           -- a list of points (in the format of the output of solveSystem/track) 
--   IsIrreducible,        -- true, false, or null
--   [ProjectionDimension]   -- an integer n, the set describes a projection a variety to the first n coordinates
--   [SolutionSystem]        -- a square system built from Equations and Slice that Points satisfy
--   [LiftedSystem]          -- a regularization of SolutionSystem (in case Points are not regular)
--   }
-- caveat: we do not assume that #Equations = dim(Slice) 
--
-- PROJECTIVE WITNESS SET = { ... same as WITNESS SET ..., 
--     AffineChart         -- one-row matrix of coefficients of a the linear equation of the chart
--                            (e.g., [1,2,3] corresponds to x+2y+3=1)  
--     }
WitnessSet.synonym = "witness set"
net WitnessSet := W -> (
    if hasAnAttribute W then (
	if hasAttribute(W,PrintNet) then return getAttribute(W,PrintNet);
  	if hasAttribute(W,PrintNames) then return net getAttribute(W,PrintNames);
  	if hasAttribute(W,ReverseDictionary) then return toString getAttribute(W,ReverseDictionary);
  	);
    if not W.?IsIrreducible or W.IsIrreducible===null or not W.IsIrreducible 
    then "[dim=" | net dim W |",deg="| net degree W | "]" 
    else "(dim=" | net dim W |",deg="| net degree W | ")"
    ) 
globalAssignment WitnessSet

dim WitnessSet := W -> ( if class W.Slice === List then #W.Slice 
     else if class W.Slice === Matrix then numrows W.Slice 
     else error "ill-formed slice in WitnessSet" )
codim WitnessSet := {} >> o -> W -> numgens ring W - dim W
ring WitnessSet := W -> if class W.Equations === Ideal then ring W.Equations else ring ideal equations W
degree WitnessSet := W -> #W.Points
ideal WitnessSet := W -> if class W.Equations === PolySystem then ideal W.Equations else W.Equations

witnessSet = method(TypicalValue=>WitnessSet)
witnessSet (Ideal,Ideal,List) := (I,S,P) -> 
  new WitnessSet from { Equations => I, Slice => sliceEquationsToMatrix S, Points => VerticalList P, IsIrreducible=>null }
witnessSet (Ideal,Matrix,List) := (I,S,P) -> 
  new WitnessSet from { Equations => I, Slice => S, Points => VerticalList P, IsIrreducible=>null}
witnessSet (PolySystem,Matrix,List) := (F,S,P) -> 
  new WitnessSet from { Equations => F, Slice => S, Points => VerticalList P, IsIrreducible=>null}
witnessSet (PolySystem,PolySystem,List) := (I,S,P) -> 
  new WitnessSet from { 
      Equations => ideal equations I, 
      Slice => sliceEquationsToMatrix ideal equations S, 
      Points => VerticalList P, 
      IsIrreducible=>null 
      }

points = method() -- strips all info except coordinates, returns a doubly-nested list
points WitnessSet := W -> apply(W.Points, coordinates)

equations WitnessSet := (W) -> if class W.Equations === PolySystem then XXXtoList W.Equations else 
if class W.Equations === Ideal then (W.Equations)_* else 
W.Equations

slice = method() -- returns linear equations for the slice (in both cases)   
slice WitnessSet := (W) -> ( 
    --if class W.Slice === List then W.Slice
    --else 
    if class W.Slice === Matrix then (
	if class W === ProjectiveWitnessSet 
	then projectiveSliceEquations(W.Slice, ring W)
	else sliceEquations(W.Slice, ring W)
	)
    else error "ill-formed slice in WitnessSet" )

sliceEquations = method(TypicalValue=>List) -- make slicing plane equations 
sliceEquations (Matrix,Ring) := (S,R) -> 
  apply(numrows S, i->(sub(S^{i},R) * transpose(vars R | matrix{{1_R}}))_(0,0)) 

projectiveSliceEquations = method(TypicalValue=>List) -- make slicing plane equations 
projectiveSliceEquations (Matrix,Ring) := (S,R) -> 
  apply(numrows S, i->(sub(S^{i},R) * transpose vars R)_(0,0)) 

sliceEquationsToMatrix = method()
sliceEquationsToMatrix Ideal := I -> (
    R := ring I;
    if numgens I > 0 then 
    matrix apply(I_*, f -> apply(gens R, x->coefficient(x,f))|{coefficient(1_R,f)})
    else 
    map(R^0,R^(numgens R + 1),0)
    )  

projectiveWitnessSet = method(TypicalValue=>ProjectiveWitnessSet)
projectiveWitnessSet (Ideal,Matrix,Matrix,List) := (I,C,S,P) -> 
  new WitnessSet from { Equations => I, AffineChart => C, Slice => S, Points => VerticalList P, IsIrreducible=>null}

TEST /// --WitnessSet
CC[x,y,z]
I = ideal {z-x*y, x^2-y}
S = ideal (z-1)
P = apply(3, i->(
	x := exp(2*i*pi*ii/3);
	point {{x,x^2,x^3}}
	))
W = witnessSet(I,S,P)
M = matrix{{0,0,1,-1}}
W = witnessSet(I,M,P)
points W
equations W
slice W
assert (dim W == 1 and degree W ==3)
///

{**********************************************************************
NumericalVariety = {
     0 => list of (irreducible) witness sets
     1 => list of (irreducible) witness sets
     ...
     dim => list of (irreducible) witness sets
     ...
     }

SERVICE FUNCTIONS:
  dim
  degree
  isReduced
  NumericalVariety union NumericalVariety (binary)
  
*}
NumericalVariety.synonym = "numerical variety"
ProjectiveNumericalVariety.synonym = "projective numerical variety"
net NumericalVariety := V -> (
    if hasAnAttribute V then (
	if hasAttribute(V,PrintNet) then return getAttribute(V,PrintNet);
  	if hasAttribute(V,PrintNames) then return net getAttribute(V,PrintNames);
  	if hasAttribute(V,ReverseDictionary) then return toString getAttribute(V,ReverseDictionary);
  	);
    out := net ofClass class V | " with components in";
    scan(keys V, k->if class k === ZZ then (
	    row := "dim "|net k|": ";
	    scan(V#k, W->row = row|" "|net W);
	    out = out || row;
	    ));
    out
    )
globalAssignment NumericalVariety
globalAssignment ProjectiveNumericalVariety

dim NumericalVariety := V -> max select(keys V, k->class k === ZZ)
degree NumericalVariety := V -> (
     d := dim V;
     sum(keys V, k->if k =!= d then 0 else sum(V#k,degree))
     )
components NumericalVariety := V -> flatten apply(select(keys V, i->class i === ZZ), i->V#i)
components (NumericalVariety,ZZ) := (V,d) -> V#d
components (NumericalVariety,ZZ,ZZ) := (V,a,b) -> (
    a = max(0,a);
    b = min(b,dim V);
    flatten apply(select(keys V, d->a<=d and d<=b), d->V#d)  
    )
components (NumericalVariety,ZZ,InfiniteNumber) := (V,a,b) -> components(V,a,min(b,dim V))
    
numericalVariety = method(TypicalValue=>NumericalVariety)
numericalVariety List := Ws -> if #Ws==0 then new NumericalVariety else (
     T := class first Ws;
     if not ancestor(WitnessSet,T) then error "a list of WitnessSet-s expected";
     V := new NumericalVariety;
     scan(Ws, W->(
	     if class W =!= T then error "a list of witness sets of same type expected";
	     d := dim W;
	     if V#?d then V#d = V#d | {W} else V#d = {W};
	     ));     
     check V;
     V
     )
numericalAffineSpace = method()
numericalAffineSpace PolynomialRing := R -> (
    n := numgens R;
    C := coefficientRing R; 
    A := random(C^n,C^n);
    b := random(C^n,C^1);
    numericalVariety {witnessSet(ideal R, A|(-b), {point entries transpose solve(A,b)})}
    )
projectiveNumericalVariety = method(TypicalValue=>ProjectiveNumericalVariety)
projectiveNumericalVariety List := Ws -> new ProjectiveNumericalVariety from numericalVariety Ws

check NumericalVariety := o-> V -> (
     if any(keys V, k->(class k =!= ZZ or k<0)) 
     then error "the keys of a NumericalVariety should be nonnegative integers";
     scan(keys V, k->if class k === ZZ then scan(V#k, W->(
		    if dim W != k then 
		    error "dimension of a witness set does not match the key in NumericalVariety";
		    )));
     )

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


-- DOCUMENTATION ------------------------------------------------------
beginDocumentation()

document {
     Key => NAGtypes,
     Headline => "Common types used in Numerical Algebraic Geometry",
     PARA{
     	  "The package defines types used by the package ", TO "NumericalAlgebraicGeometry::NumericalAlgebraicGeometry", 
     	  " as well as other numerical algebraic geometry packages: e.g., interface packages ", 
     	  TO "PHCpack::PHCpack", " and ", TT "Bertini::Bertini", "."
	  },  
     PARA{"Datatypes: "},
     UL{    
	 {TO Point, " -- a numerical approximation of a point in a complex space"},
	 {TO PolySystem, " -- a polynomial system (usually with complex coefficients)"},
	 {TO WitnessSet, " -- a witness set representing (possibly positive-dimensional) solution components"},
	 {TO NumericalVariety, " -- a numerical description of a variety"},
	 TO PolySpace,
	 TO DualSpace
	 },
     PARA{"See the corresponding documentation nodes for description of provided service functions."},
     PARA {
     	 "We display the objects of all new types showing only partial data. 
     	 Moreover, if an object is assigned to a global variable, only the name of the variable is shown. Use ", TO peek, 
     	 " for more information."
     	 },
     EXAMPLE lines ///
R = CC[x,y]	
I = ideal((x^2+y^2+2)*x,(x^2+y^2+2)*y);
w1 = witnessSet(I , ideal(x-y), {point {{0.999999*ii,0.999999*ii}}, point {{-1.000001*ii,-1.000001*ii}}} )
O = point {{0.,0.}}
numericalVariety {witnessSet(I, ideal R, {O}),w1}
V = oo
peek V
peek w1
peek O
///
     }

-- Point ---------------------------------------------------------------------------
document {
     Key => {Point, coordinates, (coordinates,Point), (status,Point), (matrix,Point), (net, Point),
	  Regular, Singular, Infinity, MinStepFailure, NumericalRankFailure, RefinementFailure,
	  Origin, IncreasePrecision, DecreasePrecision,
	  Multiplicity,
	  Coordinates, SolutionStatus, LastT, ConditionNumber, NumberOfSteps, ErrorBoundEstimate,
	  MaxPrecision, WindingNumber, DeflationNumber
	  },
     Headline => "a type used to store a point in complex space",
     "This type is used to store a solution to a polynomial system obtained by such fuctions as ", 
     TO "NumericalAlgebraicGeometry::solveSystem", ", ", TO "NumericalAlgebraicGeometry::track",
     ". The following methods can be used to access a ", 
     TO "Point", ":",
     UL{
	  {"coordinates", " -- get the coordinates (returns a list)"},
	  {"matrix", " -- get the coordinates (returns a matrix)"},
	  {"status", " -- get the type of solution (e.g., Regular)"}
	  },
     "Possible types of Points (accessed by ", TO "status", "): ",
     UL { {"Regular", " -- the jacobian of the polynomial system is regular at the point"}, 
	  {"Singular", " -- the jacobian of the polynomial system is (near)singular at the point"}, 
	  {"Infinity", " -- the solution path is deemed divergent"},
	  {"MinStepFailure", " -- the tracker failed to stay above the minimal step increment threshold"},
	  {"NumericalRankFailure", " -- it is likely that in a sequence of deflations numerical rank did not give the correct rank"},
	  {"RefinementFailure", " -- a solution refinement function failed"},
	  {"Origin", " -- the solution path approaches the origin (impossible to give a relative error estimate)"},
	  {"IncreasePrecision", " -- the current precision is deemed inadequate for robust computation"},
	  {"DecreasePrecision", " -- the current precision is deemed excessive (more than the double of sufficient precision)"},
	  {"null", " -- the point has not been classified"}
	  },
     "Only coordinates are displayed (by ", TO "net", "); to see the rest use ", 
     TO "peek", ".  Different algorithms attach different information describing the point. For example, ", TO "NumericalAlgebraicGeometry::solveSystem", " produces the following.",
     PARA{},
     EXAMPLE lines ///
       loadPackage "NumericalAlgebraicGeometry";
       R = CC[x,y];
       sols = solveSystem{x^2+y^2-3, x^3-y^3-7}
       pt = first sols
       peek pt
       coordinates pt
       status pt
     ///,
     {* condition number is not computed by default anymore !!!
     PARA{"For example, one may see the condition number of the Jacobian of the polynomial system, evaluated at this point
      (the smaller the value, the better) as follows."},
     EXAMPLE lines ///
       pt.ConditionNumber
     ///,
     *}
     PARA{"The other keys that may be attached include "}, 
     UL{
	  {TO NumberOfSteps, " -- the number of steps in made by the continuation procedure"}, 
     	  {TO LastT, " -- the last value of the continuation parameter produced during tracking (equals 1 for a regular solution)"},
	  {TO ErrorBoundEstimate, " -- an estimate of the distance from the approximation to the actual solution"},
	  {TO MaxPrecision, " -- max precision used during the homotopy tracking"}, 
	  {TO Multiplicity, " -- the multiplicity of an isolated solution"}, 
	  {TO WindingNumber, " -- the winding numeber of a singular solution determined in the end-games"}, 
	  {TO DeflationNumber, " -- number of first-order deflations in the regularization of a singular solution"},
	  {TT "Tracker", " -- reserved for developers"}
	  },
     PARA {"Basic service functions: "},
     UL{
     	 TO areEqual,
	 TO sortSolutions,
	 TO isRealPoint,
	 TO realPoints,
	 TO solutionsWithMultiplicity,
	 TO (norm,Thing,Point),
	 TO toAffineChart
	 }
     }

document {
	Key => {point, (point,List), (point,Matrix), (point,Point)},
	Headline => "construct a Point",
	Usage => "p = point c",
	Inputs => { 
	     "c"=> {ofClass List, "containing  elements in the form {{list of complex coordinates}, other data} or ",
		 ofClass Matrix, " (only coordinates) or ", ofClass Point}
	     },
	Outputs => {"p"=>Point},
	PARA{"Used to construct a ", TO2{Point, "point"}, " from the old format of output."},
        EXAMPLE lines ///
p := point {{1+0.2*ii, 0.5}, SolutionStatus=>Regular, LastT=>1., NumberOfSteps=>10, ConditionNumber=>2.3}
peek p 
q := point p
     	///
	}

document {
	Key => {(sortSolutions,List), sortSolutions, [sortSolutions,Weights]},
	Headline => "sort the list of solutions",
	Usage => "t = sortSolutions s",
	Inputs => { 
	     "s"=>{"contains solutions (represented either by lists of coordinates or ", TO2{Point,"points"}, ")"}
	     },
	Outputs => {"t"=> "contains solutions sorted as described below"},
	"The sorting is done lexicographically regarding each complex n-vector as real 2n-vector. ",
	"The output format of ", TO "track", " and ", TO "solveSystem", " is respected.", BR{}, 
	"For the corresponding coordinates a and b (of two real 2n-vectors) a < b if b-a is larger than ", 
	TO Tolerance, ". ", 
     	PARA {},
        EXAMPLE lines ///
needsPackage "NumericalAlgebraicGeometry"
R = CC[x,y];
s = solveSystem {x^2+y^2-1, x*y}
sortSolutions s
     	///,
	Caveat => {"The sorting described above does not possess good properties, since there may be near ties in specific coordinate values between several points. ",
	    "A better way is to specify a random weight (of length 2n where n=#points) as an optional parameter ", TO [sortSolutions,Weights], 
	    ", which provides a linear functional that evaluates to distinct (and sufficiently) real numbers on the given points. " }, 
	SeeAlso => {"solveSystem", "track", "areEqual"}
	}

document { Key => {Tolerance, 
	[sortSolutions,Tolerance], 
	[areEqual,Tolerance], 
	[isGEQ,Tolerance], 
	[isRealPoint,Tolerance], 
	[realPoints,Tolerance], 
	[solutionsWithMultiplicity,Tolerance],
	[differencePointSet,Tolerance], [unionPointSet,Tolerance]
	},
     Headline => "the tolerance of a numerical computation" 
     }

document { Key => {Norm, 
    	[residual,Norm]
	},
     Headline => "p in the p-norm",
     "Specifies p-norm, where p is either ", ofClass ZZ, " or ", TO infinity
     }

document {
	Key => {isGEQ, (isGEQ,List,List),(isGEQ,Point,Point)},
	Headline => "compare two points",
	Usage => "b = isGEQ(x,y)",
	Inputs => {
	     "x" => {ofClass Point, "or a list of complex (floating point) numbers"},
	     "y" => {ofClass Point, "or a list of complex (floating point) numbers"}
	     },
	Outputs => {"b"=>{"tells if ", TT "x", " is (approximately) greater or equal than ", TT "y"}},
	PARA {"The inputs are lists of complex numbers, the order is (approximately) lexicographic: regard each complex n-vector as real 2n-vector, 
	      for the corresponding coordinates a and b (of two real 2n-vectors) a < b if b-a is larger than ", TO Tolerance, ". "},
	EXAMPLE lines ///
isGEQ({1,1,1},{1,0,2})
isGEQ({1,1e-7},{1, 0})
     	///,
	SeeAlso => {"areEqual"}
	}
document {
	Key => {areEqual, (areEqual,CC,CC), (areEqual,Number,Number), 
	    (areEqual,List,List), (areEqual,BasicList,BasicList),
	    (areEqual,Matrix,Matrix), (areEqual,MutableMatrix,MutableMatrix), (areEqual,Point,Point), 
	    (areEqual,BasicList,Point), (areEqual,Point,BasicList),
	    (symbol ==,Point,Point),
	    [areEqual,Projective]},
	Headline => "determine if solutions are equal",
	Usage => "b = areEqual(x,y)",
	Inputs => {
	     "x" => "a solution or a list of solutions",
	     "y" => "a solution or a list of solutions",
	     Projective=>{"if ", TO true, " then solutions are considered as representatives of points 
		  in the projective space"}
	     },
	Outputs => {"b"=>{"tells if ", TT "x", " and ", TT "y", " are approximately equal"}},
	PARA {
	    "The inputs can be complex numbers, ", TO2{Point, "points"}, ", ", 
	    " or lists of points (presented as ", TO2{Point, "points"}, " or lists of coordinates). ",
	    "The function returns false if the distance between ", TT "x", " and ", TT "y", 
	    " exceeds ", TO Tolerance, " and true, otherwise."
	    },
	PARA {
	    "If ", TT "Projective=>true", " then ", 
	    TEX "1-\\cos\\alpha", " is compared with the ", TO Tolerance, ", where ",
	    TEX "\\alpha", " is the angle between ", TT "x", " and ", TT "y", "." 
	    },
	EXAMPLE lines ///
areEqual({{-1,1e-7},{1e-7*ii,-1}}, {{-1, 0}, {0, -1}})
areEqual({3*ii,2*ii,1+ii}, {-6,-4,-2+2*ii}, Projective=>true)  
     	///,
	PARA {
	    "For two ", TO2(Point, "points"), " ", TT "A", " and ", TT "B", 
	    "calling ", TT "A == B", "is equivalent to ", TT "areEqual(A,B)", 
	    ", however, there is no way to specify the optional parameter."
	    },
	EXAMPLE lines ///
A = point {{-1,1e-7}, {1e-7*ii,-1}}
B = point {{-1,0}, {0, -1}}
A == B
        ///,
	SeeAlso => {"solveSystem", "track", sortSolutions}
	}


document {
     Key => {isRealPoint, (isRealPoint,Point)},
     Headline => "determine whether a point is real",
     Usage => "b = isRealPoint p",
     Inputs => {
	     "p"
	     },
     Outputs => {"b"=>{"tells if ", TT "p", " is within ", TO Tolerance, " to a real point"}},
     PARA{},
     EXAMPLE lines ///
     needsPackage "NumericalAlgebraicGeometry"
     R = CC[x,y];
     sols = solveSystem{x^3-y^2, x-y-2}
     sols / isRealPoint
     ///
     }
document {
     Key => {realPoints, (realPoints,List)},
     Headline => "select real points",
     Usage => "R = realPoints L",
     Inputs => {
	     "L" => {TO2{Point,"points"}}
	     },
     Outputs => {"R"=>{TO2{Point,"points"}, " that are real (up to ", TO Tolerance, ")"}},
     PARA{"Selects real points from a list of points using the function ", TO isRealPoint, "."},
     EXAMPLE lines ///
     needsPackage "NumericalAlgebraicGeometry"
     R = CC[x,y];
     sols = solveSystem{x^6-y^4, x-y-2}
     realPoints sols
     ///,
     SeeAlso => {realPoints}
     }
document {
     Key => {(norm,Thing,Point)},
     Headline => "p-norm of the point",
     Usage => "a = norm(p,pt)",
     Inputs => {
	     "p"=>{"a positive real number or ", TO infinity},
	     "pt"
	     },
     Outputs => {"a"=>{"the ", TT "p", "-norm of the point ", TT "pt"}},
     PARA{},
     EXAMPLE lines ///
     needsPackage "NumericalAlgebraicGeometry"
     R = CC[x,y];
     sols = solveSystem{x^2+y^2-3, x^3-y^3-7}
     norm(infinity, first sols)
     norm(2.5, last sols) 
     ///
     }

document {
     Key => {solutionsWithMultiplicity, (solutionsWithMultiplicity,List)},
     Headline => "replaces clusters of approximately equal points by single points with multiplicity",
     Usage => "M = solutionsWithMultiplicity S",
     Inputs => {
	     "S" => {TO2{Point,"points"}}
	     },
     Outputs => {"M"=>{TO2{Point,"points"}, " with a multiplicity field"}},  
     PARA{"Clusters the points and outputs a list with one point ", TT "p", " per cluster with ", TT "p.", TO Multiplicity, 
	 " equal to the size of the cluster. If the multiplicity is not 1, then ", TT "p.", TO SolutionStatus, " is set to ", TO Singular, 
	 "; otherwise, it is inherited from one of the points in the cluster."},
     PARA{"Whether two points are approximately equal is decided by the function ", TO areEqual, " that depends on ", TO Tolerance, "."},
     EXAMPLE lines ///
     a = point {{0,1}}
     b = point {{0.000000001,1+0.00000000001*ii}}
     c = point {{0.001*ii,1}}
     M = solutionsWithMultiplicity {a,b,c}
     peek M
     ///,
     Caveat => {"A point in a cluster may be farther than ", TO Tolerance, 
	 " from another point in the cluster. (In that case there has to be another point in the cluster that is within the ", 
	 TO Tolerance, ".)"}
     }


document {
	Key => {(project,Point,ZZ), project},
	Headline => "project a point",
	Usage => "q = project(p,n)",
	Inputs => {
	     "p",
	     "n"
	     },
	Outputs => {"q"=>{"projection of ", TT "p", " to the first ", TT "n", " coordinates"}},
	PARA {
	    "Projects a point to the subspace corresponding to the first ", TT "n", " coordinates. "
	    },
	EXAMPLE lines ///
p = point({{1+ii,2.3,2*ii}, ConditionNumber=>1000, ErrorBoundEstimate =>0.01});
project(p,2)
     	///,
	SeeAlso => {WitnessSet,ProjectionDimension}
	}

document {
	Key => {(toAffineChart, ZZ, List), toAffineChart},
	Headline => "coordinates of a point in the projective space in an affine chart",
	Usage => "y = toAffineChart(i,x)",
	Inputs => {
	     "i" => "the number of the standard chart",
	     "x" => "projective coordinates of a point"
	     },
	Outputs => {"y"=>{"coordinates of ", TT "x", " in the ", TT "i", "-th affine chart"}},
	Caveat => {"Returns ", TT "infinity", " if the ", TT "i", "-th coordinate of ", TT "x", " is zero."},
	EXAMPLE lines ///
toAffineChart(2,{1,2,3,4,5,6}) 
toAffineChart(2,{1,2,0,4,5,6}) 
     	///,
	SeeAlso => {areEqual}
	}

-- PolySystem ------------------------------------------------------------------------------
document {
    Key => {PolySystem, 
	(ideal,PolySystem), (isHomogeneous,PolySystem), (jacobian,PolySystem), (net,PolySystem),
	(ring,PolySystem), (equations,PolySystem),
	NumberOfPolys, NumberOfVariables, PolyMap, Jacobian, ContinuationParameter, 
	SpecializationRing
	},
    Headline => "a polynomial system",
    "This type stores a polynomial system, ",
    "the following methods can be used to access a ", 
    TT "PolySystem", ":",
    UL{
	{"ideal", " -- the ideal generated by the system"},
	{"equations", " -- the list of polynomials in the system"},	
	{"ring", " -- the ring containing the polynomials"},
	{"jacobian", " -- the jacobian of the polynomial map"}
	},
    "Only polynomials are displayed (by ", TO "net", "); ",
    "to see the data stored in a witness set use ", TO "peek", ".",
    SUBSECTION "For developers:",
    "Required entries in a ", TO PolySystem, " are",
     UL {
	 {TT "NumberOfVariables", " of type ", TO ZZ},
	 {TT "NumberOfPolys", " of type ", TO ZZ},
	 {TT "PolyMap", " of type ", TO Matrix, ", a column matrix over a polynomial ring"},
    	 {TT "Jacobian", " of type ", TO Matrix, ", the jacobian of ", TT "PolyMap"},
	 },
{*     "Basic methods for ", TO "polynomial homotopy", " use additional keys: ",
     UL {
	 {TT "ContinuationParameter", " -- stores one variable of the ring" },
	 {TT "SpecializationRing", 
	     " -- stores the subring generated my all variables except the additional parameter",
	     " (e.g., used by ", TO specializeContinuationParameter, ")"}
	 },
     *}
     EXAMPLE lines ///
CC[x,y]
S = polySystem {x^2+y^2-6, 2*x^2-y}
p = point {{1.0+3*ii,2.3+ii}};
evaluate(S,p)
evaluate(jacobian S, p)
     ///,
     PARA {"Basic service functions: "},
     UL{
    	TO polySystem,
	TO evaluate,
	--TO segmentHomotopy,
	--TO specializeContinuationParameter,
	},     
     SeeAlso => {WitnessSet}
     }

document {
    Key => {evaluate, (evaluate,Matrix,Matrix), (evaluate,Matrix,Point), (evaluate,PolySystem,Matrix), (evaluate,PolySystem,Point)},
    Headline => "evaluate a polynomial system or matrix at a point",
    Usage => "y = evaluate(f,x)",
    Inputs => { 
	"f" => {ofClass PolySystem, " or ", ofClass Matrix},
	"x" => {ofClass Point, " or ", ofClass Matrix},
	},
    Outputs => {"y"=> {"the value ", TT "f(x)"}},
    PARA {"Evaluates a ", TO PolySystem, " or a matrix with polynomial entries at a point."},
    EXAMPLE lines ///
R = CC[x,y]; S = polySystem {x^2+y^2-6, 2*x^2-y};
p = point {{1.0+3*ii,2.3+ii}};
evaluate(S,p)
evaluate(jacobian S, p)
    ///,
    SeeAlso => {PolySystem}
    }

document {
    Key => {residual, 
	(residual,List,Point),
	(residual,Matrix,Matrix),
	(residual,PolySystem,Point)
	},
    Headline => "residual of a polynomial function at a point",
    Usage => "y = residual(f,x)",
    Inputs => { 
	"f" => {ofClass PolySystem, " or ", ofClass Matrix},
	"x" => {ofClass Point, " or ", ofClass Matrix},
	},
    Outputs => {"y"=> {"the norm of ", TT "f(x)"}},
    PARA {
	"Evaluates a ", TO PolySystem, 
	" or a matrix with polynomial entries at a point and returns the norm of the result."
	},
    EXAMPLE lines ///
R = CC[x,y]; S = polySystem {x^2+y^2-5, 2*x^2-y};
p = point {{1.001-0.0001*ii,2.+0.0001*ii}};
evaluate(S,p)
residual(S,p)
residual(S,p,Norm=>3)
residual(S,p,Norm=>infinity)
    ///,
    SeeAlso => {PolySystem}
    }

document {
    Key => {polySystem, (polySystem,List), (polySystem,Matrix), (polySystem,PolySystem), (polySystem,Ideal)},
    Headline => "construct a polynomial system",
    Usage => "P = polysystem F",
    Inputs => { 
	"F" => {ofClass List, " or ", ofClass Ideal, " or ", ofClass Matrix, 
	    " (column matrix) with polynomial entries or ", ofClass PolySystem},
	},
    Outputs => {"P"=> PolySystem},
    PARA {"Constructs a ", TO PolySystem, " from the given polynomials."},
    EXAMPLE lines ///
R = CC[x,y]; S := polySystem {x^2+y^2-6, 2*x^2-y}
S = polySystem transpose matrix {{x^2+y^2-6, 2*x^2-y}}
T = polySystem S
    ///,
    SeeAlso => {PolySystem}
    }

document {
    Key => {(substitute,PolySystem,Ring)},
    Headline => "substitute a ring in a polynomial system",
    Usage => "G = sub(F,R)",
    Inputs => { 
	"F" => PolySystem,
	"R" => Ring
	},
    Outputs => {"G"=> PolySystem},
    PARA {
	"Constructs ", ofClass PolySystem, " by attempting to map polynomials of a given system to a given ring."
	},
    EXAMPLE lines ///
R = QQ[x,y]; S := polySystem {x^2+y^2-6, 2*x^2-y}
T := sub(S,CC[x,y])
ring T
    ///,
    SeeAlso => {polySystem,PolySystem}
    }

document {
    Key => {(homogenize,PolySystem,Ring,RingElement)},
    Headline => "homogenize a polynomial system",
    Usage => "G = sub(F,R,t)",
    Inputs => { 
	"F" => PolySystem,
	"R" => Ring,
	"t" => {ofClass RingElement, ", a variable in ", TT "R"} 
	},
    Outputs => {"G"=> PolySystem},
    PARA {
	"Constructs ", ofClass PolySystem, " that is a homogenization of the given system. "
	},
    EXAMPLE lines ///
R = CC[x,y]; S := polySystem {x^2+y^2-6, 2*x^4-y}
T := homogenize(S,CC[x,y,t],t)
ring T
    ///,
    SeeAlso => {homogenize,PolySystem}
    }

{*
document {
    Key => {"polynomial homotopy", 
	segmentHomotopy, (segmentHomotopy,PolySystem,PolySystem), 
	substituteContinuationParameter, (substituteContinuationParameter,PolySystem,RingElement),
	specializeContinuationParameter, (specializeContinuationParameter,PolySystem,Number)
	},
    Headline => "basic methods for manipulating polynomial homotopies",
    Usage => "H = segmentHomotopy(S,T)\nH2=substituteContinuationParameter(H1,s)\nspecializeContinuationParameter(H,t0)",
    Inputs => { 
	"S" => PolySystem,
	"T" => PolySystem,
	"H1" => PolySystem
	},
    Outputs => {"H"=> PolySystem},
    PARA {
	"Construct ", ofClass PolySystem, " representing a segment homotopy ", 
	TEX "H = (1-t) F + t G", " for ", TEX "t \\in [0,1]", "."
	},
    EXAMPLE lines ///
R = CC[x,y]; 
S = polySystem {x^2-1, y^2-1};
T = polySystem {x^2+y^2-6, 2*x^2-y};
H := segmentHomotopy(S,T)
    ///,    
    PARA {
	"Specialize the continuation parameter:"
	},
    EXAMPLE lines ///
specializeContinuationParameter(H,0)
specializeContinuationParameter(H,1)
specializeContinuationParameter(H,2+3*ii)
    ///,    
    PARA {
	"Substitute ", TEX "1-t", " for the continuation parameter to swap the ends of the homotopy."
	},
    EXAMPLE lines ///
t := H.ContinuationParameter
H' := substituteContinuationParameter(H,1-t)
    ///,    
    SeeAlso => {ContinuationParameter,SpecializationRing}
    }
*}
-- WitnessSet ------------------------------------------------------------------------------
document {
     Key => {WitnessSet,equations,(equations,WitnessSet),slice,(slice,WitnessSet),
	  points,(points,WitnessSet),(ideal,WitnessSet),Equations,Slice,Points,IsIrreducible,ProjectionDimension,
     	  (codim,WitnessSet),(degree,WitnessSet),(dim,WitnessSet),(ring,WitnessSet),(net,WitnessSet) 
     	  },
     Headline => "a witness set",
     "This type stores a witness set of an equidimensional solution component. ", 
     "The following methods can be used to access a ", 
     TO WitnessSet, ":",
     UL{
     	  {"ideal", " -- get the defining ideal of the algebraic superset"},
	  {"equations", " -- get the list of defining polynomials of the algebraic superset"},
	  {"slice", " -- get linear functions defining the slicing plane"},
	  {"points", " -- get the list of witness points (which are zeroes of all above)"}
	  },
     "Also one may determine",
     UL {
	  {"dim", " -- the dimension"},
	  {"codim", " -- the codimension"},
	  {"deg", " -- the degree (the number of witness points)"},
	  {"ring", " -- the ring of the defining polynomials"}
	  }, 
     "Only dimension and degree are displayed (by ", TO "net", "); to see the data stored in a witness set use ", 
     TO "peek", ".",
     SUBSECTION "For developers:",
     "Required keys in a ", TO WitnessSet, " are",
     UL {
	  {TT "Equations", " -- ", ofClass Ideal},
	  {TT "Slice", " -- ", ofClass List, " or ", ofClass Matrix},
	  {TT "Points", "--  a list of ", TO2(Point, "points")},
	  {TT "IsIrreducible", " -- takes values ", TO "null", "(not determined), ", TO "true", ", or ", TO "false"}
	  },
     "Optional keys:",
     UL {
	  {TT "ProjectionDimension", " -- ", ofClass ZZ, 
	      ", the witness set describes a lifted variety (its projection on the first ", 
	      TT "ProjectionDimension", " coordinates is the variety the witness set represents)"},
	  },     
     SeeAlso => {witnessSet, ProjectiveWitnessSet, NumericalVariety}
     }

document {
	Key => {witnessSet,
	    (witnessSet,Ideal,Ideal,List),(witnessSet,Ideal,Matrix,List),
	    (witnessSet,PolySystem,Matrix,List),(witnessSet,PolySystem,PolySystem,List)
	    },
	Headline => "construct a WitnessSet",
	Usage => "w = witnessSet(E,S,P)",
	Inputs => { 
	     "E" => {ofClass Ideal, " or ", ofClass PolySystem},
	     "S" => {ofClass Ideal, " generated by linear polynomials (or ", 
		 ofClass PolySystem, " of the generators or ", 
		 ofClass Matrix, " of their coefficients)"},
	     "P" => List => {"contains witness points (of type ", TO "Point", ")"}
	     },
	Outputs => {"w"=> WitnessSet},
	PARA {"Used to construct a witness set of a component of the variety ", TT "V(E)", ". It is expected that ", TT "codim E == dim S", 
	     " and that ", TT "P", " is a subset of the intersection of ", TT "V(E)", " and ", TT "V(S)", "."},
        EXAMPLE lines ///
R = CC[x,y]	
w = witnessSet( ideal(x^2+y^2+2), ideal(x-y), {point {{0.999999*ii,0.999999*ii}}, point {{-1.000001*ii,-1.000001*ii}}} )
peek w
///
	}

document {
     Key => {ProjectiveWitnessSet, AffineChart},
     Headline => "a projective witness set",
     "This type stores a witness set of an equidimensional projective solution component. ", 
     SeeAlso => {WitnessSet, projectiveWitnessSet}
     }

-- !!! something strange is going on with EXAMPLE in this node:
-- stdio:1:1:(3): error: example results terminate prematurely: projectiveWitnessSet
document {
	Key => {projectiveWitnessSet,(projectiveWitnessSet,Ideal,Matrix,Matrix,List)},
	Headline => "construct a ProjectiveWitnessSet",
	Usage => "w = projectiveWitnessSet(E,C,S,P)",
	Inputs => { 
	     "E" => Ideal => {"in a polynomial ring over ", TO CC },
	     "C" => Matrix => {"in a polynomial ring over ", TO CC },
	     "S" => Matrix => {" complex coefficients of a linear system"},
	     "P" => List => {"contains witness points (of type ", TO "Point", ")"}
	     },
	Outputs => {"w"=> ProjectiveWitnessSet},
	PARA {"Used to construct a witness set for a component of the variety ", TT "V(E)", 
	    ". ", " An affine chart is specified by the matrix of the coefficients of the (normalized) linear equation defining the chart: e.g., ",
	    TT "ax+by+cz=1", " is encoded as ", TT "[a,b,c]", "." }, 
	PARA {"It is expected that the, ", TT "V(E)", " and the plane ", TT "V(S)", " defined by ", TT "S", 
	    " are of complementary dimensions and that ", TT "P", " is contained in the intersection of ", TT "V(E+C)", " and ", TT "V(S)", "."}
	,
	EXAMPLE lines ///
R = CC[x,y,z]	
w = projectiveWitnessSet( ideal(x^2+y^2+2*z^2), matrix{{0,0,1}}, matrix{{1,-1,0}}, {point {{0.999999*ii,0.999999*ii,1.}}, point {{ -1.000001*ii,-1.000001*ii,1.}}} )
peek w///
-- 	,
--         EXAMPLE lines ///
-- R = CC[x,y,z]
-- w = projectiveWitnessSet(
--     ideal(x^2+y^2+2*z^2),
--     matrix{{0,0,1}}, -- chart: Z=1
--     matrix{{1,-1,0}},
--     {point {{1.000001*ii,0.999999*ii,1}}, point {{ -1.000001*ii,-1.000001*ii,1}}} 
--     )
-- peek w
-- ///
}

document {
	Key => {(sliceEquations,Matrix,Ring),sliceEquations,
	    (projectiveSliceEquations,Matrix,Ring),projectiveSliceEquations},
	Headline => "slicing linear functions",
	Usage => "S = sliceEquations(M,R)\nS = projectiveSliceEquations(M,R)",
	Inputs => { 
	     "M"=> Matrix => " contains the coefficients of the slicing linear polynomials",
	     "R"=> Ring => " where the output polynomials belong"
	     },
	Outputs => {"S"=>List=>"contains linear polynomials"},
        PARA {"A service function used  in ", TO "NumericalAlgebraicGeometry::NumericalAlgebraicGeometry", "."},
	EXAMPLE lines ///
R = CC[x,y]	
sliceEquations(matrix{{1,2,3},{4,5,6*ii}}, R)
projectiveSliceEquations(matrix{{1,2,3},{4,5,6*ii}}, CC[x,y,z])
     	///
	}

-- NumericalVariety --------------------------------------------------------------------
document {
     Key => {NumericalVariety, 
	 (dim,NumericalVariety), (degree,NumericalVariety), 
	 (net,NumericalVariety), (check,NumericalVariety)
	 },
     Headline => "a numerical variety",
     PARA {"This type stores a collection of witness sets representing a complex affine variety. "},
     "Note that",
     UL {     	  
	  {"The ambient space is expected to be the same, i.e., ", 
	      TO2((dim,WitnessSet),"dimension"), " (or ", TO ProjectionDimension, ") of ", TO2(WitnessSet, "witness sets"),
	      " should be the same."},
	  -- "However, the witness sets need not come from the decomposition of the same variety.",
	  {"The constructor ", TO (numericalVariety,List), " does not check the sensibility of the input; run ", 
	  TO (check, NumericalVariety), " to verify the validity of a numerical variety."} 
	  },
     "Basic service routines:",
     UL {
	 {"dim", " -- the dimension"},
	 {"codim", " -- the codimension"},
	 {"deg", " -- the degree"},
	 {TO (components,NumericalVariety)}
	 },
     EXAMPLE lines ///
R = CC[x,y]; I = ideal((x^2+y^2+2)*x,(x^2+y^2+2)*y*(y-1));
w1 := witnessSet(I , ideal(x-y), {point {{0.999*ii,0.999*ii}}, point {{-1.001*ii,-1.001*ii}}} )
w0 := witnessSet(I, ideal R, {point {{0.,0.}}})
w0' := witnessSet(I, ideal R, {point {{0.,1.}}})
V := numericalVariety {w0,w1,w0'}
dim V
degree V
     ///,
     SeeAlso => {WitnessSet}
     }
document {
	Key => {(numericalVariety,List), numericalVariety, (projectiveNumericalVariety,List), projectiveNumericalVariety},
	Headline => "construct a numerical variety",
	Usage => "V = numericalVariety Ws; V = projectiveNumericalVariety Ws; ",
	Inputs => { 
	     "Ws" => {"contains (projective) witness sets representing components of a variety"}
	     },
	Outputs => {"V"=> NumericalVariety},
	PARA {"Constructs a numerical (affine or projective) variety. It is NOT expected that every witness set ", TT "W", 
	     " in the list ", TT "Ws", " has the same ", TT "W.Equations", "."},
        EXAMPLE lines ///
R = CC[x,y]; I = ideal((x^2+y^2+2)*x,(x^2+y^2+2)*y);
w1 = witnessSet(I , ideal(x-y), {point {{0.999999*ii,0.999999*ii}}, point {{-1.000001*ii,-1.000001*ii}}} )
w0 = witnessSet(I, ideal R, {point {{0.,0.}}})
V = numericalVariety {w0,w1}
     	///,
	SeeAlso => {WitnessSet, ProjectiveWitnessSet, numericalAffineSpace}
	}

document {
    Key => {
	(components,NumericalVariety),
	(components,NumericalVariety,ZZ),
	(components,NumericalVariety,ZZ,InfiniteNumber),
	(components,NumericalVariety,ZZ,ZZ)
	},
    Headline => "list components of a numerical variety",
    Usage => "components(V)\ncomponents(V,a)\ncomponents(V,a,b)",
    Inputs => { 
	"V" => NumericalVariety,
	},
    Outputs => {{ofClass List, " of ", TO2(WitnessSet,"witness sets")}},
    PARA {
	"Returns a list of components of a numerical variety. ",
	"If ", TT "a", " (", ofClass ZZ, ") and/or ", 
	TT "b", " (", ofClass ZZ, " or ", TO infinity, 
	") are specified, then components of dimension ", TT "a", 
	" (respectively, components of dimension at least ", TT "a", 
	" and at most ", TT "b", ") are returned."
	},
    EXAMPLE lines ///
R = CC[x,y]; I = ideal((x^2+y^2+2)*x,(x^2+y^2+2)*y);
w1 := witnessSet(I , ideal(x-y), {point {{0.999999*ii,0.999999*ii}}, point {{-1.000001*ii,-1.000001*ii}}} )
w0 := witnessSet(I, ideal R, {point {{0.,0.}}})
V := numericalVariety {w0,w1}
components V    
    ///,
    SeeAlso => {NumericalVariety}
    }

document {
	Key => {numericalAffineSpace, (numericalAffineSpace,PolynomialRing)},
	Headline => "affine space as a numerical variety",
	Usage => "numericalAffineSpace R",
	Inputs => { 
	     "R"=> PolynomialRing
	     },
	Outputs => {NumericalVariety},
        PARA {"Constructs a numerical variety representing the complex affine space corresponding to the given coordinate ring."},
	EXAMPLE lines ///
R = CC[x,y]	
V := numericalAffineSpace R
C := first components V
equations C
slice C
points C
     	///,
	SeeAlso => {NumericalVariety}
	}

document {
     Key => {ProjectiveNumericalVariety},
     Headline => "a projective numerical variety",
     "An object of this type stores a collection of ", TO2(projectiveWitnessSet, "projective witness sets"),
     SeeAlso => {projectiveWitnessSet}
     }

-- legacy stuff ------------------------------------------------------------------------------
doc ///
  Key
    generalEquations
    (generalEquations,ZZ,Ideal)
    (generalEquations,ZZ,List)
    (generalEquations,WitnessSet)
  Headline
    random linear combinations of equations/generators 
  Usage
    L = generalEquations(k,F)
    L = generalEquations(k,I)
  Inputs
    k:ZZ
    F:List
      a list of polynomials
    I:Ideal
  Outputs
    L:List
      {\tt k} linear combinations of polynomials in {\tt F} (of generators of {\tt I})
  Description
    Text
      A variety {\em V} (that is not necessarily a complete intersection) of codimension {\tt k} 
      is a component of a complete intersection of codimension {\tt k} defined by 
      {\tt k} general linear combinations of any generating set of the defining ideal of {\em V}.
      
      This function automates the above construction.   
    
    Example
      R = CC[x,y,z]; 
      F = {x*y, x^2 - y, x*z};
      L = generalEquations(2,F)      
///

-- PolySpace ------------------------------------------------------------------------------
doc ///
  Key
    PolySpace
    (gens,PolySpace)
    (dim,PolySpace)
    (ring,PolySpace)
    (net,PolySpace)
  Headline
    a polynomial vector subspace
  Description
    Text
      This type stores a finite dimensional vector subspace of a polynomial ring, given by a spanning set. 
      The spanning set is generally assumed to be a basis although this is not enforced.  
      The following methods can be used to access a {\tt PolySpace}: 

      @UL {
	  {"gens -- a one-row matrix of the generators"},
	  {"dim -- the number of generators"},
	  {"ring -- the ring of the generators"}
	  }@

  SeeAlso
    polySpace
    DualSpace
///

doc ///
  Key
    polySpace
    (polySpace,Matrix)
    (polySpace,PolySpace)
  Headline
    construct a PolySpace
  Usage
    S = polySpace M
  Inputs
    M:Matrix
      with one row of generators
  Outputs
    S:PolySpace
  Description
    Text
      Used to construct a finite dimensional vector subspace of a polynomial ring.
    Example
      R = CC[x,y];	
      M = matrix{{y^2,x^2+x*y}};
      S = polySpace M
///

doc ///
  Key
    DualSpace
    (gens,DualSpace)
    (dim,DualSpace)
    (ring,DualSpace)
    (net,DualSpace)
    (point,DualSpace) 
  Headline
    a dual functional vector subspace
  Description
    Text
      This type stores a finite dimensional subspace of the local dual of a polynomial ring at a point.
      In practice, the subspace is stored as a @TO PolySpace@ with functionals represented by the
      corresponding polynomial, along with a @TO Point@. 
      The following methods can be used to access a {\tt DualSpace}: 

      @UL {
	  {"gens -- a one-row matrix of the generators"},
	  {"dim -- the number of generators"},
	  {"ring -- the ring of the generators"},
	  {"point -- the base point of the dual space"}
	  }@
  SeeAlso
    dualSpace
    PolySpace
///

doc ///
  Key
    dualSpace
    (dualSpace,Matrix,Point)
    (dualSpace,PolySpace,Point)
    (dualSpace,DualSpace)
  Headline
    construct a DualSpace
  Usage
    D = dualSpace(M,p)
    D = dualSpace(S,p)
  Inputs 
    M:Matrix
      with one row of generators
    S:PolySpace
    p:Point
  Outputs
    D:DualSpace
  Description
    Text
      Used to construct a finite dimensional subspace of the local dual space of polynomial ring at a point.
    Example
      R = CC[x,y];
      M = matrix{{1,x,x^2-y}}
      p = point matrix{{1,0}};
      D = dualSpace(M,p)
///

undocumented {Reduced,BasePoint,origin,(origin,Ring),Gens,Space,[polySpace,Reduced]} --Robert???
undocumented {
    ParameterHomotopy, 
    Parameters, SpecializedParameterHomotopy, Homotopy,
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
installPackage("Style", AbsoluteLinks=>false)
installPackage("NAGtypes", AbsoluteLinks=>false)

installPackage ("NAGtypes", MakeDocumentation=>false)
check "NAGtypes"
