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
texMath Point := x -> texMath coordinates x
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

-*
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
*-

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
