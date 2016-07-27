needs (currentFileDirectory | "PointArray.m2")

HomotopyNode = new Type of MutableHashTable 
HomotopyEdge = new Type of MutableHashTable
HomotopyGraph = new Type of MutableHashTable

addNode = method()
addNode (HomotopyGraph, Point, PointArray) := (G, params, partialSols) -> (
    N := new HomotopyNode from {
        BasePoint => params,
        PartialSols => partialSols,
        Graph => G,
        System => toSystem (G, params, G.Family.PolyMap)
    };
    G.Vertices = append(G.Vertices, N);
    N
)

addEdge = method()
addEdge (HomotopyGraph, HomotopyNode, HomotopyNode) := (G,a,b) -> (
    E := new HomotopyEdge from {
            Node1 => a, 
            Node2 => b, 
	    Graph => G,
            gamma1 => exp(2 * pi* ii * random RR), 
            gamma2 => exp(2 * pi* ii * random RR), 
            Correspondence12 => new MutableHashTable from {}, -- think: the map from labels of points of Node1 to those of Node2
            Correspondence21 => new MutableHashTable from {} -- ............................................2.................1
        };
    G.Edges = prepend(E, G.Edges);
    if G.Potential =!= null then (	
    	E.Potential12 = G.Potential (E, true);
    	E.Potential21 = G.Potential (E, false);
    	);
    E
)

drop (HomotopyGraph, HomotopyEdge, ZZ) := (G,E, k) -> (
    G.Edges = drop(G.Edges, k);
    )

addCorrespondence = method()
addCorrespondence (HomotopyEdge,ZZ,ZZ) := (e,a,b) -> (
    e.Correspondence12#a = b;
    e.Correspondence21#b = a;
    )

homotopyGraph = method(TypicalValue => HomotopyGraph, Options => {Family=>"IdSupport", Potential=>null})
installMethod(homotopyGraph, o -> ()-> new HomotopyGraph from {
	Vertices => new MutableList from {},
	Edges => new MutableList from {}
	})
homotopyGraph PolySystem := o -> PF -> (
    G := homotopyGraph();
    G.Family = PF;
    G.Potential = o.Potential;
    G
    )

toSystem = method()
toSystem (HomotopyGraph, Point, Matrix) := (G, p, M) -> (
    PF := transpose M;
    nParameters := numgens coefficientRing ring PF;
    assert(nParameters == #coordinates p);
    (PR,toPR) := flattenRing ring PF;
    X := drop(gens PR, -nParameters); 
    PF = toPR PF;
    C := coefficientRing PR;
    R := C[X];
    X = vars R;
    flatten entries (map(R,PR,X|matrix p)) PF
    )

potentialLowerBound = (e,from1to2) -> (
        if from1to2 then (
	(head, tail) := (e.Node1, e.Node2);
	correspondence := e.Correspondence12;
	correspondence' := e.Correspondence21;
	)
    else  (
	(head, tail) = (e.Node2, e.Node1);
	correspondence = e.Correspondence21;
	correspondence' = e.Correspondence12;
	);
    n1 := #(keys head.PartialSols - set keys correspondence);
    n2 := #(keys tail.PartialSols - set keys correspondence');
    max(n1-n2, 0)
    ) 

selectBestEdgeAndDirection = G -> (
    m1 := maxPosition(apply(G.Edges, e -> e.Potential12));
    m2 := maxPosition(apply(G.Edges, e -> e.Potential21));
    if (G.Edges#m1).Potential12 < (G.Edges#m2).Potential21 then (G.Edges#m2, false)
    else (G.Edges#m1, true)
    )

-- prototype for edge tracking function
-- assumptions: 1) member function is working/optimized for PointAray objects 2) toSystem method
-- which converts parametric coefficients to a list of polynomials (inputs to track),
-- 3) positions method defined for pointset object
-- Output: 
trackEdge = method()
trackEdge (HomotopyEdge, Boolean) := (e, from1to2) -> (
    G := e.Graph;
    if from1to2 then (
	(head, tail) := (e.Node1, e.Node2);
	(gammaHead, gammaTail) :=  (e.gamma1, e.gamma2);
	correspondence := e.Correspondence12;
	)
    else  (
	(head, tail) = (e.Node2, e.Node1);
	(gammaHead, gammaTail) =  (e.gamma2, e.gamma1);
	correspondence = e.Correspondence21;
	);
    untrackedInds := keys head.PartialSols - set keys correspondence;
    newSols := if #untrackedInds > 0 then track(polySystem (gammaHead * head.System), polySystem(gammaTail * tail.System), (head.PartialSols)_(untrackedInds))
    else {};
    n := #tail.PartialSols;
    scan(#untrackedInds, i->(
	    s := newSols#i;
	    a := untrackedInds#i;
	    if member(s, tail.PartialSols) then b:= position(s,tail.PartialSols) 
	    else (    
		appendPoint(tail.PartialSols, s);
		b = n;
		n = n+1;
		);
	    addCorrespondence(if from1to2 then (e,a,b) else (e,b,a))
	    ));
    e.Potential12 = G.Potential (e, true);
    e.Potential21 = G.Potential (e, false);
    #untrackedInds
    )
