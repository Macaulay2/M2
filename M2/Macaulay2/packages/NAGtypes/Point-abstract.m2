-- Abstract classes ------------------------
AbstractPoint = new Type of HashTable

AbstractPoint.synonym = "point"
texMath AbstractPoint := x -> texMath coordinates x
net AbstractPoint := p -> net coordinates p

AbstractPoint == AbstractPoint := (a,b) -> areEqual(a,b) -- the default Tolerance is used

-- expected methods
notImplementedError := p -> error ("implementation of this method for the descendant of (abstract) type "|toString class p |" is expected")
coordinates = method()
coordinates AbstractPoint := notImplementedError 
status AbstractPoint := o -> notImplementedError
project = method() -- project point to the first n coordinates
project (AbstractPoint,ZZ) := (p,n) -> notImplementedError p

-- methods relying on expected methods 
matrix AbstractPoint := o -> p -> matrix {coordinates p}

norm (Thing, AbstractPoint) := (no,p) -> norm(no, coordinates p)
norm (Thing, Matrix) := (no,M) -> norm(no, flatten entries M)
norm (Thing, List) := (no,p) -> (
     if instance(no,InfiniteNumber) and no === infinity then return max(p/abs);
     assert((instance(no, ZZ) or instance(no, QQ) or instance(no, RR)) and no>0);
     (sum(p, c->abs(c)^no))^(1/no)
     )

isRealPoint = method(Options=>{Tolerance=>1e-6})
isRealPoint AbstractPoint := o -> p -> norm (coordinates p / imaginaryPart) < o.Tolerance

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
areEqual (AbstractPoint,AbstractPoint) := o -> (a,b) -> (
    a = coordinates a;
    b = coordinates b;
    if o.Projective 
    then (1 - abs sum(a,b,(x,y)->x*conjugate y))/((norm(2,a)) * (norm(2,b))) < o.Tolerance  -- projective distance is too rough in practice
    else (
	na := norm(2,a); 
	nb := norm(2,b);
	if na > 1 or nb > 1 then norm(2,(a-b)) < o.Tolerance * max(na,nb)
	else norm(2,a-b) < o.Tolerance -- in case both points are close to the origin, absolute error is looked at 
	)
    )
-*
areEqual (AbstractPoint,BasicList) := o -> (a,b) -> areEqual(coordinates a, toList b)
areEqual (BasicList,AbstractPoint) := o -> (a,b) -> areEqual(toList a, coordinates b)
*-

isGEQ = method(TypicalValue=>Boolean, Options=>{Tolerance=>1e-6})
isGEQ(AbstractPoint,AbstractPoint) := o->(t,s)-> isGEQ(coordinates t, coordinates s, o)
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
