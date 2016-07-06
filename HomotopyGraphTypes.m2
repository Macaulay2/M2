HomotopyNode = new Type of MutableHashTable 

addNode = method()
addNode (HomotopyGraph, Point, PointSet) := (G, params, partialSols) -> append(G.Vertices, 
    new HomotopyNode from {BasePoint => params, PartialSols => partialSols}
    )

HomotopyEdge = new Type of MutableHashTable

addEdge = method()
addEdge (HomotopyGraph, HomotopyNode, HomotopyNode) := (G,a,b) -> append(G.Edges,
    new HomotopyEdge from {Node1 => a, Node2 => b, gamma1 => exp(2 * pi* ii * random RR), 
	gamma2 => exp(2 * pi* ii * random RR), Correspondences => new MutableHashTable from {}}
    )

HomotopyGraph = new Type of MutableHashTable

homotopyGraph = method(TypicalValue => HomotopyGraph, Options => {Family=>"IdSupport"})
installMethod(homotopyGraph, ()->new HomotopyGraph from {
	Vertices => new MutableList from {},
	Edges => new MutableList from {}
	})

-- prototype for edge tracking function
-- assumptions: 1) member function is working/optimized for PointSet objects 2) toSystem method
-- which converts parametric coefficients to a list of polynomials (inputs to track),
-- 3) positions method defined for pointset object
-- Output: 
trackEdge = method()
trackEdge (homotopyGraph, homotopyEdge) := (G, e) -> (
    (head, tail) := (e.Node1, e.Node2);
    untrackedHeadInds := positions(head.PartialSols, s -> not member(s, keys e.Correspondences));
    scan(untrackedInds, s -> (e.Correspondences)#s = null);
    newSols := track(e.gamma1 * toSystem head, e.gamma2 * toSystem tail, (head.PartialSols)_(untrackedInds));
    tail.PartialSols = unionPointSet(tail.PartialSols, pointSet newSols);
    untrackedTailInds := apply(untrackedInds, i -> (positions(tail.PartialSols, s -> s === (head.PartialSols)_(untrackedInds)#i))#0);
    scan(untrackedHeadInds, untrackedTailInds, (i,j) -> (e.Correspondences)#i = j);
    )