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
Point = new Type of AbstractPoint

net Point := p -> (
    if hasAnAttribute p then (
	if hasAttribute(p,PrintNet) then return getAttribute(p,PrintNet);
  	if hasAttribute(p,PrintNames) then return net getAttribute(p,PrintNames);
  	if hasAttribute(p,ReverseDictionary) then return toString getAttribute(p,ReverseDictionary);
  	);
    s := if p.cache.?SolutionStatus then p.cache.SolutionStatus else Regular;
    if s === Regular then net p.Coordinates 
    else if s === Singular then net toSequence p.Coordinates
    else if s === MinStepFailure then net "[M,t=" | net p.cache.LastT | net "]"
    else if s === Infinity then net "[I,t=" | net p.cache.LastT | net "]"
    else if s === NumericalRankFailure then net "[NF]"
    else if s === RefinementFailure then net "[RF]"
    else if s === IncreasePrecision then net "[P+]"
    else if s === DecreasePrecision then net "[P-]"         
    else if s === Origin then net "[0]"         
    else error "the point is corrupted"
    ) 
globalAssignment Point

point = method()
point AbstractPoint := p -> point {coordinates p} 
prepareCoordinates := c	-> (
    if instance(c,List) then c 
    else if instance(c,Matrix) or instance(c,MutableMatrix) then flatten entries c
    else error "wrong type of coordinates: List or Matrix expected"   
    )
point (List,CacheTable) := (c,h) -> new Point from {
    Coordinates=>prepareCoordinates c,
    cache => copy h
    }
point List := s -> point(prepareCoordinates first s, new CacheTable from drop(s,1))
point Matrix := M -> point {flatten entries M} 
        
toExternalString Point := p -> "point { " | toExternalString coordinates p |" }"

coordinates Point := p -> p.Coordinates

status Point := o -> p -> if p.cache.?SolutionStatus then p.cache.SolutionStatus else null

-- project point to the first n coordinates
project (Point,ZZ) := (p,n) -> (
    p' := point { take(coordinates p, n) };
    if p.cache.?ErrorBoundEstimate then p'.cache.ErrorBoundEstimate = p.cache.ErrorBoundEstimate;
    p'
    )
    
residual (List,Point) := o->(S,p)-> residual(polySystem S,p,o)
residual (System,Point) := o->(P,p)->norm(o.Norm, point evaluate(P,p))
residual (Matrix,Matrix) := o->(S,p)->norm(o.Norm, point evaluate(S,p))

origin = method(TypicalValue=>Point)
origin Ring := R -> point matrix{toList(numgens R:0_(ultimate(coefficientRing, R)))};

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
