-- -*- coding: utf-8 -*-
-- licensed under GPL v2 or any later version
newPackage(
     "NAGtypes",
     Version => "1.6.0.1",
     Date => "June, 2013",
     Headline => "Common types used in Numerical Algebraic Geometry",
     HomePage => "http://people.math.gatech.edu/~aleykin3/NAG4M2",
     Authors => {
	  {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"}
	  },
     -- DebuggingMode should be true while developing a package, 
     --   but false after it is done
     DebuggingMode => true 
     )

export {
     Norm, MaxConditionNumber, -- options
     -- service functions
     generalEquations, 
     -- witness set
     "WitnessSet", "witnessSet", "equations", "slice", "points", 
     "Equations", "Slice", "Points", "sliceEquations", "projectiveSliceEquations", "IsIrreducible", 
     "ProjectiveWitnessSet", "AffineChart", "projectiveWitnessSet",
     -- numerical variety
     "NumericalVariety", "numericalVariety",
     "ProjectiveNumericalVariety", "projectiveNumericalVariety",
     -- point (solution)
     "Point", "point", "coordinates",
     "isRealPoint", "realPoints", "residual", "relativeErrorEstimate", "classifyPoint",
     "toAffineChart",
     "Tolerance", "sortSolutions", "areEqual", "isGEQ", "solutionsWithMultiplicity",
     "Coordinates", "SolutionStatus", "LastT", "ConditionNumber", "Multiplicity", 
     "NumberOfSteps", "ErrorBoundEstimate",
     "MaxPrecision", "WindingNumber", "DeflationNumber",
     Regular, Singular, Infinity, MinStepFailure, NumericalRankFailure, RefinementFailure,
     -- polynomial systems
     PolySystem, NumberOfPolys, NumberOfVariables, PolyMap, Jacobian, JacobianAndPolySystem, 
     ContinuationParameter, SpecializationRing,
     polySystem, segmentHomotopy, substituteContinuationParameter, specializeContinuationParameter,
     "evaluate",
     -- dual space
     "DualSpace", "DualBasis", "BasePoint", "dualSpace", "addition", "intersection", "reduceSpace",
     "PolySpace", "polySpace", "Reduced", "colon", "Gens", "Space", "innerProduct", "isContained",
     "numericalKernel", "numericalImage", "colReduce"
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
ring PolySystem := P -> ring P.PolyMap -- change this for SLP!!!
equations = method() -- returns list of equations
equations PolySystem := P -> flatten entries P.PolyMap -- change this for SLP!!!
ideal PolySystem := P -> ideal equations P.PolyMap -- change this for SLP!!!

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
    if numColumns X == 1 then sub(M,transpose X)
    else if numRows X == 1 then sub(M,X)
    else error "expected a row or a column vector"
    )
evaluate (PolySystem,Point) := (P,p) -> evaluate(P,matrix p)

jacobian PolySystem := P -> (
    if P.?Jacobian then P.Jacobian
    else P.Jacobian = transpose jacobian(transpose P.PolyMap) -- TO DO: make "jacobian" work for SLPs
    )

segmentHomotopy = method()
segmentHomotopy (PolySystem,PolySystem) := (S,T) -> (
    R := ring S;
    if R =!= ring T then error "systems over the same ring expected";
    K := coefficientRing R;
    t := symbol t;
    Rt := K(monoid[gens R, t]);
    toRt := map(Rt,R,drop(gens Rt,-1));
    t = last gens Rt;       
    H := polySystem((1-t)*toRt S.PolyMap + t*toRt T.PolyMap);
    H.ContinuationParameter = t;
    H.SpecializationRing = R;
    H 
    )

substituteContinuationParameter = method()
substituteContinuationParameter (PolySystem,RingElement) := (H,t') -> (
    -- t' contains typically a linear expression in t=H.ContinuationParameter
    -- but will work for any polynomial expression in ring H.
    if not H.?ContinuationParameter then error "system has no continuation parameter";
    R := ring H;
    H' := polySystem (map(R,R,drop(gens R,-1)|{t'})) H.PolyMap;
    H'.ContinuationParameter = H.ContinuationParameter;
    H'.SpecializationRing = H.SpecializationRing;
    H'	
    )

specializeContinuationParameter = method()
specializeContinuationParameter (PolySystem, Number) := (H,t0) -> (
    if not H.?ContinuationParameter then error "system has no continuation parameter";
    Rt := ring H;
    K := coefficientRing Rt;
    i := position(gens Rt, x->x===H.ContinuationParameter);
    R := H.SpecializationRing;
    specializeTo't0 := map(R,Rt,apply(numgens Rt, j->
	    if j<i then R_j 
	    else if j>i then R_(j-1) 
	    else t0
	    ));
    polySystem specializeTo't0 H.PolyMap
    )

TEST ///
CC[x,y]
polySystem transpose matrix{{x,y^2+1,x+1}}
QQ[x,y]
polySystem {x,y^2+1,x+1}
RR[x,y]
polySystem {x,y^2+1,x+1}
CC[x,y]
S = polySystem {x^2+y^2-6, 2*x^2-y}
p = point {{1.0_CC,2.3_CC}};
assert (
    clean_0.1 ( (100*evaluate(S,p)) - transpose matrix{{29, -30}} )==0
    )
T = polySystem {x^2-1, y^2-1}
H = segmentHomotopy(S,T)
H' = substituteContinuationParameter(H,1-H.ContinuationParameter)
S' = specializeContinuationParameter(H,0_CC) 
assert(S'.PolyMap - S.PolyMap == 0)
T' = specializeContinuationParameter(H',0_CC) 
assert(T'.PolyMap - T.PolyMap == 0)
///

-----------------------------------------------------------------------
-- POINT = {
--   Coordinates => List of CC,
--   NumberOfSteps => ZZ, -- number of steps made while tracking the path
--   SolutionStatus => {Regular, Singular, Infinity, MinStepFailure, NumericalRankFailure, null}
--   LastT => RR in [0,1]
--   ConditionNumber => condition number of the Jacobian
--   ErrorBoundEstimate => absolute error bound estimate (from Newton's method)
--   Multiplicity => the multiplicity (sometimes: the number of paths converging to the point) 
--   MaxPrecision => max precision used during the homotopy tracking
--   WindingNumber => used in the end-games
--   DeflationNumber => number of first-order deflations 
--   }
Point.synonym = "point"
net Point := p -> (
    if hasAnAttribute p then (
	if hasAttribute(p,PrintNet) then return getAttribute(p,PrintNet);
  	if hasAttribute(p,PrintNames) then return net getAttribute(p,PrintNames);
  	if hasAttribute(p,ReverseDictionary) then return toString getAttribute(p,ReverseDictionary);
  	);
     if not p.?SolutionStatus or p.SolutionStatus === Regular then net p.Coordinates 
     else if p.SolutionStatus === Singular then net toSequence p.Coordinates
     else if p.SolutionStatus === MinStepFailure then net "[M,t=" | net p.LastT | net "]"
     else if p.SolutionStatus === Infinity then net "[I,t=" | net p.LastT | net "]"
     else if p.SolutionStatus === NumericalRankFailure then net "[N]"
     else error "the point is corrupted"
    ) 
globalAssignment Point

point = method()
point Point := p -> new Point from p
point List := s -> new Point from {Coordinates=>first s} | drop(s,1)
point Matrix := M -> point {flatten entries M} 

Point == Point := (a,b) -> areEqual(a,b) -- the default Tolerance is used

coordinates = method()
coordinates Point := p -> p.Coordinates

status Point := o -> p -> if p.?SolutionStatus then p.SolutionStatus else null
matrix Point := o -> p -> matrix {coordinates p}

norm (Thing, Point) := (no,p) -> norm(no, coordinates p)
norm (Thing, Matrix) := (no,M) -> norm(no, flatten entries M)
norm (Thing, List) := (no,p) -> (
     if instance(no,InfiniteNumber) and no === infinity then return max(p/abs);
     assert((instance(no, ZZ) or instance(no, QQ) or instance(no, RR)) and no>0);
     (sum(p, c->abs(c)^no))^(1/no)
     )
 
residual = method(Options=>{Norm=>2})
residual (List,Point) := o->(S,p)-> residual(polySystem S,p)
residual (PolySystem,Point) := o->(P,p)-> residual(P.PolyMap,matrix p)
residual (Matrix,Matrix) := o->(S,p)->norm(o.Norm,evaluate(S,p))

relativeErrorEstimate = method(Options=>{Norm=>2})
relativeErrorEstimate(Point) := o->p->p.ErrorBoundEstimate/norm(o.Norm,p) 

isRealPoint = method(Options=>{Tolerance=>1e-6})
isRealPoint Point := o -> p -> norm (coordinates p / imaginaryPart) < o.Tolerance

realPoints = method(Options=>{Tolerance=>1e-6})
realPoints List := o -> pp -> select(pp, isRealPoint)

classifyPoint = method(Options=>{MaxConditionNumber=>1e6})
classifyPoint(Point) := o -> p -> if status p === null and p.?ConditionNumber then (
     p.SolutionStatus = 
     if p.ConditionNumber < o.MaxConditionNumber 
     then Regular  
     else Singular
     )  

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

isGEQ = method(TypicalValue=>Boolean, Options=>{Tolerance=>1e-6})
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

sortSolutions = method(TypicalValue=>List, Options=>{Tolerance=>1e-6})
sortSolutions List := o -> sols -> (
-- sorts numerical solutions     
     if #sols == 0 then sols
     else (
	  sorted := {0};
	  get'coordinates := sol -> if class sol === Point then coordinates sol 
	                       else if ancestor(BasicList, class sol) then toList sol
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

solutionsWithMultiplicity = method(TypicalValue=>List, Options=>{Tolerance=>1e-6})
solutionsWithMultiplicity List := o-> sols -> ( 
     clusters := groupClusters solutionDuplicates(sols,o);
     apply(clusters, c->(
	       s := new Point from sols#(first c);
	       if (s.Multiplicity = #c)>1 then s.SolutionStatus = Singular;
	       s
	       ))
     )

TEST ///
a = point {{0,1}}
b = point {{0.000000001,1+0.00000000001*ii}}
c = point {{0.001*ii,1}}
assert (# solutionsWithMultiplicity {a,b,c} == 2)
///

-----------------------------------------------------------------------
-- WITNESS SET = {
--   Equations,            -- an ideal or a polynomial system 
--   Slice,                -- a matrix of coefficients of linear equations 
--                            (e.g., row [1,2,3] corresponds to x+2y+3=0)
--   Points,	           -- a list of points (in the format of the output of solveSystem/track) 
--   IsIrreducible         -- true, false, or null
--   }
-- caveat: we assume that #Equations = dim(Slice)   
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
codim WitnessSet := W -> numgens ring W - dim W
ring WitnessSet := W -> ring W.Equations
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
points WitnessSet := (W) -> apply(W.Points, coordinates)

equations WitnessSet := (W) -> if class W.Equations === PolySystem then XXXtoList W.Equations else (W.Equations)_*

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

{**********************************************************************
NumericalVariety = {
     Equations => the defining equations
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
    out := net ofClass class V | " of dimension " | net dim V |" with components in";
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
numericalVariety = method(TypicalValue=>NumericalVariety)
numericalVariety List := Ws -> (
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
    if #F > 0 then R := ring first F else error "expected a nonempty list of polynomials";
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

hilbertFunction DualSpace := L -> (
    if not L.Space.Reduced then L = reduceSpace L;
    tally(flatten entries gens L / first @@ degree)
    )
hilbertFunction (List,DualSpace) := (LL,L) -> (
    h := hilbertFunction L;
    apply(LL, d->(if h#?d then h#d else 0))
    )
hilbertFunction (ZZ,DualSpace) := (d,L) -> first hilbertFunction({d},L)

check DualSpace :=  L -> error "not implemented"

areEqual (PolySpace,PolySpace) := o -> (S,T) -> (
    n := dim addition(S,T,Tolerance=>o.Tolerance);
    n == dim S and n == dim T
    )
areEqual (DualSpace,DualSpace) := o -> (L,K) ->
    areEqual(L.BasePoint,K.BasePoint,Tolerance=>o.Tolerance) and areEqual(L.Space,K.Space,Tolerance=>o.Tolerance)
    
isContained = method(TypicalValue => Boolean, Options => {Tolerance=>1e-6})
isContained (PolySpace,PolySpace) := o -> (S,T) ->
    dim addition(S,T,Tolerance=>o.Tolerance) == dim T
isContained (DualSpace,DualSpace) := o -> (L,K) ->
    areEqual(L.BasePoint,K.BasePoint,Tolerance=>o.Tolerance) and isContained(L.Space,K.Space,Tolerance=>o.Tolerance)

intersection = method(TypicalValue => PolySpace, Options => {Tolerance=>1e-6})
intersection (PolySpace,PolySpace) := o -> (S,T) -> (
    (mons,coefs) := coefficients (gens S|gens T);
    Scoefs := submatrix(coefs,(0..dim S-1));
    Tcoefs := submatrix'(coefs,(0..dim T-1));
    Sorth := numericalKernel(transpose Scoefs,o.Tolerance);
    Torth := numericalKernel(transpose Tcoefs,o.Tolerance);
    M := mons*numericalKernel(transpose (Sorth|Torth),o.Tolerance);
    polySpace M
    )

addition = method(TypicalValue => PolySpace, Options => {Tolerance=>1e-6})
addition (PolySpace,PolySpace) := o -> (S,T) -> (
    (mons,C) := coefficients (gens S | gens T);
    polySpace(mons*numericalImage(C,o.Tolerance))
    )

reduceSpace = method(Options => {Tolerance=>1e-6})
reduceSpace PolySpace := o -> S -> (
    (mons,coefs) := coefficients gens S;
    M := mons*(colReduce(coefs,o.Tolerance));
    polySpace(M,Reduced=>true)
    )
reduceSpace DualSpace := o -> L -> dualSpace(reduceSpace L.Space,L.BasePoint)

colon = method(TypicalValue => DualSpace, Options => {Tolerance=>1e-6})
colon (DualSpace, RingElement) := o-> (L,g) -> (
    (gmons,gcoefs) := coefficients g;
    (Lmons,Lcoefs) := coefficients gens L;
    M := matrix apply(flatten entries gmons, gm->(
	    apply(flatten entries Lmons, Lm->(
		    d := diff(gm,Lm);
		    if d == 0 then d else leadMonomial d
		    ))
	    ));
    M = (transpose gcoefs)*M*Lcoefs;
    (Mmons,Mcoefs) := coefficients M;
    M = Mmons*numericalImage(Mcoefs,o.Tolerance);
    dualSpace(polySpace M, L.BasePoint)
    )
colon (DualSpace, Ideal) := (L,J) -> error "not implemented"

-- Matrix of inner products
-- PolySpace generators as rows, DualSpace generators as columns
innerProduct = method()
innerProduct (PolySpace, PolySpace) := (S, T) -> (
    M := last coefficients(gens S | gens T);
    Svec := submatrix(M,0..dim S-1);
    Tvec := submatrix'(M,0..dim S-1);
    (transpose Svec)*Tvec
    )
innerProduct (PolySpace, DualSpace) := (S, L) -> (
    Sshift := polySpace sub(gens S, matrix{(gens ring L) + coordinates L.BasePoint});
    innerProduct(Sshift, L.Space)
    )
innerProduct (RingElement, DualSpace) := (f, L) -> innerProduct(polySpace matrix{{f}}, L)
innerProduct (RingElement, RingElement) := (f, l) -> (
    M := last coefficients(matrix{{f,l}});
    ((transpose M_{0})*M_{1})_(0,0)
    )

interpolatedIdeal = method()
interpolatedIdeal DualSpace := L -> error "not implemented"
interpolatedIdeal List := LL -> error "not implemented"

---------------------------------------------
-- AUXILIARY FUNCTIONS
---------------------------------------------

numericalImage = method()
numericalImage (Matrix, Number) := (M, tol) -> (
    R := ultimate(coefficientRing, ring M);
    M = sub(M, R);
    if numcols M == 0 then return M;
    if numrows M == 0 then return map(R^0,R^0,0);
    if precision 1_(ring M) < infinity then (
	(svs, U, Vt) := SVD M;
	cols := positions(svs, sv->(sv > tol));
	submatrix(U,,cols)
	) else (
	gens image M
	)
    )

numericalKernel = method()
numericalKernel (Matrix, Number) := (M, tol) -> (
    M = sub(M, ultimate(coefficientRing, ring M));
    if numrows M == 0 then return id_(source M);
    if precision 1_(ring M) < infinity then (
	(svs, U, Vt) := SVD M;
	cols := positions(svs, sv->(sv > tol));
	submatrix'(adjointMatrix Vt,,cols)
	) else (
	gens kernel M
	)
    )

-- produces the conjugate transpose
adjointMatrix = method(TypicalValue => Matrix)
adjointMatrix Matrix := M -> (
    M' := mutableMatrix transpose M;
    for i from 0 to (numrows M')-1 do (
	for j from 0 to (numcols M')-1 do M'_(i,j) = conjugate(M'_(i,j));
	);
    matrix M'
    )

--performs Gaussian reduction on M
colReduce = method(TypicalValue => Matrix)
colReduce (Matrix, Number) := (M, tol) -> (
    M = new MutableMatrix from sub(transpose M, ultimate(coefficientRing, ring M));
    (m,n) := (numrows M, numcols M);
    i := 0;
    for j from 0 to n-1 do (
	if i == m then break;
	a := i + maxPosition apply(i..m-1, l->(abs M_(l,j)));
	c := M_(a,j);
	if abs c <= tol then continue;
	rowSwap(M,a,i);
	for l from 0 to n-1 do M_(i,l) = M_(i,l)/c; --rowMult(M,i,1/c); is bugged
	for k from 0 to m-1 do rowAdd(M,k,-M_(k,j),i);
	i = i+1;
	);
    M = transpose new Matrix from M;
    if tol > 0 then clean(tol,M) else M
    )

TEST ///
restart
loadPackage "NAGtypes"
p = point matrix{{1.,0.}}
R = CC[x,y]
L = dualSpace(matrix{{y^2+x,y^2}},p,0.0001)
K = dualSpace(matrix{{x+.000000001}},p,0.0001)
assert(areEqual(L+K,L))
assert(areEqual(intersection(L,K),K))
hilbertFunction({0,1,2,3},L)
///

beginDocumentation()

undocumented {(generalEquations,WitnessSet)}
--warning: symbol has no documentation: NAGtypes :: Norm
--warning: symbol has no documentation: NAGtypes :: MaxConditionNumber

document {
     Key => NAGtypes,
     Headline => "Common types used in Numerical Algebraic Geometry",
     PARA{
     	  "The package defines types used by the package ", TO "NumericalAlgebraicGeometry::NumericalAlgebraicGeometry", 
     	  " as well as other numerical algebraic geometry packages: e.g., an interface package ", 
     	  TO "PHCpack::PHCpack", "."
	  },  
     PARA{"Main datatypes: "},
     UL{    
	 {TO "Point", " -- numerical approximation of a point in a complex space (and related methods)"},
	 {TO "WitnessSet", " -- a witness set representing (possibly positive-dimensional) solution components"},
	 {TO "NumericalVariety", " -- a numerical description of a variety"}
	 },
     PARA{"See the corresponding documentation nodes for description of provided service functions."},
     PARA {"Other service functions: "},
     UL{
	 {TO "areEqual", " -- compare numbers, points, lists of points"},
	 {TO "sortSolutions", " -- sort lists of points"},
	 {TO "generalEquations", " -- "}
	 },
     PARA {
     	 "We display the objects of all new types showing only partial data. 
     	 Moreover, if an object is assigned to a global variable, only the name of the variable is shown. Use ", TO peek, 
     	 " for more information."
     	 },
     EXAMPLE lines ///
R = CC[x,y]	
I = ideal((x^2+y^2+2)*x,(x^2+y^2+2)*y);
w1 = witnessSet(I , ideal(x-y), {point {{0.999999*ii,0.999999*ii}}, point {{-1.000001*ii,-1.000001*ii}}} )
origin = point {{0.,0.}}
numericalVariety {witnessSet(I, ideal R, {origin}),w1}
V = oo
peek V
peek w1
peek origin
///
     }

document {
     Key => {Point, coordinates, (coordinates,Point), (status,Point), (matrix,Point), 
	  Regular, Singular, Infinity, MinStepFailure, NumericalRankFailure, (net, Point),
	  Coordinates, SolutionStatus, LastT, ConditionNumber, NumberOfSteps, ErrorBoundEstimate,
	  MaxPrecision, WindingNumber, DeflationNumber
	  },
     Headline => "a type used to store a point in complex space",
     "This type is used to store a solution to a polynomial system obtained by such fuctions as ", 
     TO "solveSystem", ", ", TO "track",". The following methods can be used to access a ", 
     TO "Point", ":",
     UL{
	  {"coordinates", " -- get the coordinates (returns a list)"},
	  {"status", " -- get the type of solution (e.g., Regular)"},
	  {"matrix", " -- get the coordinates (returns a matrix)"}
	  },
     "Possible types of Points (accessed by ", TO "status", "): ",
     UL { {"Regular", " -- the jacobian of the polynomial system is regular at the point"}, 
	  {"Singular", " -- the jacobian of the polynomial system is (near)singular at the point"}, 
	  {"Infinity", " -- the solution path has been deemed divergent"},
	  {"MinStepFailure", " -- the tracker failed to stay above the minimal step increment threshold"},
	  {"NumericalRankFailure", " -- it is likely that in a sequence of deflations numerical rank did not give the correct rank"},
	  {null, " -- the point has not been classified"}
	  },
     "Only coordinates are displayed (by ", TO "net", "); to see the rest use ", 
     TO "peek", ".  Different algorithms attach different information describing the point. For example, the
     solveSystem function with default options produces the following.",
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
     PARA{"For example, one may see the condition number of the Jacobian of the polynomial system, evaluated at this point
      (the smaller the value, the better) as follows."},
     EXAMPLE lines ///
       pt.ConditionNumber
     ///,
     PARA{"The other keys that may be attached include "}, 
     UL{
	  {TO NumberOfSteps, " -- the number of steps in made by the continuation procedure"}, 
     	  {TO LastT, " -- the last value of the continuation parameter produced during tracking (equals 1 for a regular solution)"},
	  {TO ErrorBoundEstimate, " -- an estimate of the distance from the approximation to the actual solution"},
	  {TO MaxPrecision, " -- max precision used during the homotopy tracking"}, 
	  {TO WindingNumber, " -- the winding numeber of a singular solution determined in the end-games"}, 
	  {TO DeflationNumber, " -- number of first-order deflations in the regularization of a singular solution"},
	  {TT "Tracker", " -- reserved for developers"}
	  }
     }

document {
	Key => {(point,List), point},
	Headline => "construct a Point",
	Usage => "p = point c",
	Inputs => { 
	     "c"=> {"contains elements in the form {{list of complex coordinates}, other data}"}
	     },
	Outputs => {"p"=>Point},
	PARA{"Used to construct a ", TO2{Point, "point"}, " from the old format of output."},
        EXAMPLE lines ///
p = point {{1+0.2*ii, 0.5}, SolutionStatus=>Regular, LastT=>1., NumberOfSteps=>10, ConditionNumber=>2.3}
peek p 
     	///
	}

document {
     Key => {WitnessSet,equations,(equations,WitnessSet),slice,(slice,WitnessSet),
	  points,(points,WitnessSet),(ideal,WitnessSet),Equations,Slice,Points,
     	  (codim,WitnessSet),(degree,WitnessSet),(dim,WitnessSet),(ring,WitnessSet),(net,WitnessSet) 
     	  },
     Headline => "a witness set",
     "This type stores a witness set of an equidimensional solution component. ", 
     "The following methods can be used to access a ", 
     TT "WitnessSet", ":",
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
     "Required entries in a ", TO WitnessSet, " are",
     UL {
	  {TT "Equations", " of type ", TO Ideal},
	  {TT "Slice", " of type either ", TO List, " or ", TO Matrix},
	  {TT "Points", ", a list of ", TO2(Point, "points")},
	  {TT "IsIrreducible", " that takes values ", TO "null", "(not determined), ", TO "true", ", or ", TO "false"}
	  },
     SeeAlso => {witnessSet, ProjectiveWitnessSet, NumericalVariety}
     }

document {
	Key => {witnessSet,(witnessSet,Ideal,Ideal,List),(witnessSet,Ideal,Matrix,List)},
	Headline => "construct a WitnessSet",
	Usage => "w = witnessSet(E,S,P)",
	Inputs => { 
	     "E" => Ideal => {"in a polynomial ring over ", TO CC },
	     "S" => {ofClass Ideal, " generated by linear polynomials or ", ofClass Matrix, " with complex coefficients of these generators"},
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
	Usage => "S = sliceEquations(M,R), S = projectiveSliceEquations(M,R)",
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

document {
	Key => {(sortSolutions,List), sortSolutions},
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
	SeeAlso => {"solveSystem", "track", areEqual}
	}

document { Key => {Tolerance, [sortSolutions,Tolerance], [areEqual,Tolerance], [isGEQ,Tolerance], [isRealPoint,Tolerance], [realPoints,Tolerance]},
     Headline => "specifies the tolerance of a numerical computation" 
     }

document {
	Key => {isGEQ, (isGEQ,List,List)},
	Headline => "compare two points",
	Usage => "b = isGEQ(x,y)",
	Inputs => {
	     "x" => "complex (floating point) numbers",
	     "y" => "complex (floating point) numbers"
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
	Key => {areEqual, (areEqual,CC,CC), (areEqual,Number,Number), (areEqual,List,List), (areEqual,Matrix,Matrix), (areEqual,Point,Point), 
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
	PARA {"The inputs can be complex numbers, ", TO2{Point, "points"}, ", ", " or lists of points (presented as ", TO2{Point, "points"}, " or lists of coordinates)."},
	"The function returns false if the distance between ", TT "x", " and ", TT "y", " exceeds ", TO Tolerance, " and true, otherwise.",
	PARA {"If ", TT "Projective=>true", " then ", TEX "1-\\cos\\alpha", " is compared with the ", TO Tolerance, ", where ",
	     TEX "\\alpha", " is the angle between ", TT "x", " and ", TT "y", "." },
	EXAMPLE lines ///
areEqual({{-1,1e-7},{1e-7*ii,-1}}, {{-1, 0}, {0, -1}})
areEqual({3*ii,2*ii,1+ii}, {-6,-4,-2+2*ii}, Projective=>true)  
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
     Key => {NumericalVariety},
     Headline => "a numerical variety",
     PARA {"This type stores a collection of witness sets representing a variety. "},
     "Note that",
     UL {     	  
	  {"The ambient space is expected to be the same, i.e., ", TO "Equations", " of ", TO2(WitnessSet, "witness sets"),
	       " are should come from the same ring."},
	  "The witness sets need not come from the decomposition of the same variety.",
	  {"The constructor ", TO (numericalVariety,List), " does not check the sensibility of the input; run ", 
	  TO (check, NumericalVariety), " to verify the validity of a numerical variety."} 
	  },
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
R = CC[x,y]	
I = ideal((x^2+y^2+2)*x,(x^2+y^2+2)*y);
w1 = witnessSet(I , ideal(x-y), {point {{0.999999*ii,0.999999*ii}}, point {{-1.000001*ii,-1.000001*ii}}} )
w0 = witnessSet(I, ideal R, {point {{0.,0.}}})
V = numericalVariety {w0,w1}
     	///,
	SeeAlso => {WitnessSet, ProjectiveWitnessSet}
	}

doc ///
  Key
    generalEquations
    (generalEquations,ZZ,Ideal)
    (generalEquations,ZZ,List)
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

TEST /// -- miscellaneous tests
CC[x,y]
S = polySystem {x^2+y^2-6, 2*x^2-y}
p = point({{1.0_CC,2.3_CC}, ConditionNumber=>1000, ErrorBoundEstimate =>0.01});
assert (round (1000*norm(4.5,p)) == 2312)
assert isRealPoint p
classifyPoint p
assert(round (10000*residual(S,p)) == 4173)
p2 =  point {{1.001,2.3+ii}}
p3 =  point {{.999,2.3+ii}}
assert areEqual(sortSolutions {p,p2,p3}, {p3,p,p2})
///

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
