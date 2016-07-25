-- one idea for a better point array (untested)
-- other ideas: use image of random projection as key in a hash table (this would be simpler, but how do we mainain 
-- sorted order in key list?), balanced BSTs (not as simple, not sure if hese get used in practice)

needsPackage "NumericalAlgebraicGeometry"

-- computes a random projection p:R^n->R^1
rp1d = n -> matrix(RR, {drop(flatten apply(ceiling(n/2),i-> (
	    us := apply(2, i -> random(sub(0,RR), sub(1,RR)));
	    us = {sqrt(-2* log first us), 2*pi* last us};
	    {first us * cos last us, first us * sin last us})), sub(mod(n,2),ZZ))})

-- samples from a Poisson(x) distribution (use for x<= 30)
pois = x -> (
    (L,k,p) := (exp(-x), 0, 1);
    while p > L do (
	k = k+1;
	u := random(sub(0,RR), sub(1,RR));
	p = p * u;
	);
    k-1
    )

RPAssembly = new Type of MutableHashTable

rPAssembly = method(TypicalValue => RPAssembly, Options => {MaxLoad=>10})
installMethod(rPAssembly, o -> ()-> new RPAssembly from {
	PointSets => {},
	Maps => {},
	Dimension => -1,
	MaxLoad => o.MaxLoad})

RPArray = new Type of MutableHashTable

-- ignoring MaxLoad opion now for simplicity
rPArray = method(TypicalValue => RPArray, Options => {MaxLoad=>10})
installMethod(rPArray, o -> ()-> new RPArray from {
	Points => {},
	Depths => {},
	Dimension => -1,
	MaxLoad => o.MaxLoad})
rPArray ZZ := List => o -> n -> (
    A := rPArray(o);
    A.Dimension = n;
    A
    )

RPPoint = new Type of MutableHashTable

rPPoint = method()
rPPoint Point := p -> new RPPoint from {Coordinates => transpose matrix {coordinates p}, Projections => {}, Dimension => #coordinates p}

insert (RPAssembly, RPArray) := (S,A) -> S.PointSets = append(S.PointSets, A)

-- currently assumes A is in S.PointSets 
insert (Point, RPArray, RPAssembly) := (p, A, S) -> (
    n := A.Dimension;
    l := length A.Points;
    if n != numrows p.Coordinates then error("Attempted comparison of points from different ambient spaces.");
    insertion := rPPoint p;
    insertion.Projections = {(S.Maps)#0 * insertion.Coordinates};
    (left, right, d, done, comp, collision) := (0, l, 0, false, symbol ==, false);
    while not done do ( 
        if left == right -1 then (
	    A.Points = insert(insertion, right, A.Points);
	    done = true;
	    )
	else (
	    mid := floor((left+right)/2);
	    comparisonPoint := (A.Points)#mid;
	    comp = (comparisonPoint.Projections)#d ? (insertion.Projection)#d;
	    if comp == symbol == then (
	    	if collision and not areEqual(comparisonPoint.Coordinates, insertion.Coordinates) then ( -- areEqual for real matrices?
		    done = true;
		    A.Points = insert(insertion, mid,  A.Points);
		    )
	    	else (
		    d = d+1;
		    if d +1  >= length(S.Maps) then (
		    	S.Maps = append(S.Maps, rp1d n);
		    	collision = true;
		    	d = d+1;
		    	)))
	    else if comp == symbol > then right = mid -1
	    else left = mid +1
	    );
	);
    )
