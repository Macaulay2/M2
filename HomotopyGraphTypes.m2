HomotopyNode = new Type of MutableHashTable 

addNode = method()
addNode (HomotopyGraph, Point, PointSet) := (G, params, partialSols) -> append(G.Vertices, 
    new HomotopyNode from {BasePoint => params, PartialSols => partialSols}
    )

HomotopyEdge = new Type of MutableHashTable

addEdge = method()
addEdge (HomotopyGraph, HomotopyNode, HomotopyNode) := (G,a,b) -> append(G.Edges,
    new HomotopyEdge from {Node1 => a, Node2 => b, gamma1 => exp(2 * pi* ii * random RR), 
	gamma2 => exp(2 * pi* ii * random RR), Correspondences => new MutableList from {}}
    )

HomotopyGraph = new Type of MutableHashTable

homotopyGraph = method(TypicalValue => HomotopyGraph, Options => {Family => "IdSupport"}})

installMethod(homotopyGraph, ()->new HomotopyGraph from {
	Vertices => new MutableList from {},
	Edges => new MutableList from {}
	})

