needs currentFileDirectory | "PointArray.m2"

HomotopyNode = new Type of MutableHashTable 

addNode = method()
addNode (HomotopyGraph, Point, PointArray) := (G, params, partialSols) -> append(G.Vertices, 
    new HomotopyNode from {BasePoint => params, PartialSols => partialSols}
    )

HomotopyEdge = new Type of MutableHashTable

addEdge = method()
addEdge (HomotopyGraph, HomotopyNode, HomotopyNode) := (G,a,b) -> append(G.Edges,
    new HomotopyEdge from {Node1 => a, Node2 => b, gamma1 => exp(2 * pi* ii * random RR), 
	gamma2 => exp(2 * pi* ii * random RR), 
	Correspondence12 => new MutableHashTable from {}, -- think: the map from labels of points of Node1 to those of Node2
	Correspondence21 => new MutableHashTable from {}  -- ............................................2.................1
	}
    )
addCorrespondence = method()
addCorrespondence (HomotopyEdge,ZZ,ZZ,Boolean) := (e,a,b) -> (
    e.Correspondence12#a = b;
    e.Correspondence21#b = a;
    )

HomotopyGraph = new Type of MutableHashTable

homotopyGraph = method(TypicalValue => HomotopyGraph, Options => {Family=>"IdSupport"})
installMethod(homotopyGraph, ()->new HomotopyGraph from {
	Vertices => new MutableList from {},
	Edges => new MutableList from {}
	})

-- prototype for edge tracking function
-- assumptions: 1) member function is working/optimized for PointAray objects 2) toSystem method
-- which converts parametric coefficients to a list of polynomials (inputs to track),
-- 3) positions method defined for pointset object
-- Output: 
trackEdge = method()
trackEdge (HomotopyEdge, Boolean) := (e, one'to'two) -> (
    if one'to'two then (
	(head, tail) := (e.Node1, e.Node2);
	correspondence := e.Correspondence12;
	)
    else  (
	(head, tail) := (e.Node2, e.Node1);
	correspondence := e.Correspondence21;
	);
    untrackedInds := positions(head.PartialSols, s -> not member(s, keys correspondence));
    newSols := track(e.gamma1 * toSystem head, e.gamma2 * toSystem tail, (head.PartialSols)_(untrackedInds));
    n := #tail.PartialSols;
    appendPoints(tail.PartialSols, newSols);
    scan(#untrackedInds, i->(
	    a := untrackedInds#i;
	    b := n+i;
	    addCorrespondence(if one'to'two then (e,a,b) else (e,b,a))
	    )
    )