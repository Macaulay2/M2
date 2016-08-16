export {
    "selectBestEdgeAndDirection",
    "potentialLowerBound",
    "potentialE",
    "HomotopyGraph",
    "HomotopyEdge",
    "HomotopyNode"
    }
HomotopyNode = new Type of MutableHashTable 
HomotopyEdge = new Type of MutableHashTable
HomotopyGraph = new Type of MutableHashTable

addNode = method()
addNode (HomotopyGraph, Point, PointArray) := (G, params, partialSols) -> (
    N := new HomotopyNode from {
        BasePoint => params,
        PartialSols => partialSols,
        Graph => G,
        SpecializedSystem => toSystem (G, params, G.Family.PolyMap),
	Edges => new MutableList from {}
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
    a.Edges#(#a.Edges) = E;
    b.Edges#(#b.Edges) = E;
    G.Edges = append(G.Edges,E);
    if G.Potential =!= null then (	
    	E.Potential12 = G.Potential (E, true);
    	E.Potential21 = G.Potential (E, false);
    	);
    E
)

removeEdge = method()
removeEdge(HomotopyGraph, HomotopyEdge) := (G,e) -> (
    (N1, N2) := (G.Node1, G.Node2);
    N1.Edges = remove(N1.Edges, e);
    N2.Edges = remove(N2.Edges, e);
    G.Edges = remove(G.Edges, e);
    )

addCorrespondence = method()
addCorrespondence (HomotopyEdge,ZZ,ZZ) := (e,a,b) -> (
    if  e.Correspondence12#?a or e.Correspondence21#?b then false
    else ( 
    	e.Correspondence12#a = b;
    	e.Correspondence21#b = a;
	true
	)
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

-- convenience function for WS init
edgeInds = (G,v) -> (
    i := (positions(G.Vertices, x -> x == v))#0;
    positions(G.Vertices, x -> member(x,G.Edges)) 
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
    n1 := length head.PartialSols - length keys correspondence;
    n2 := length tail.PartialSols - length keys correspondence';
    max(n1-n2, 0)
    ) 


potentialE = (e,from1to2) -> (
    G := e.Graph;
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
    a := length head.PartialSols - length keys correspondence;
    b := length tail.PartialSols - length keys correspondence';
    d := (e.Graph).TargetSolutionCount;
    c := length keys correspondence;
--    << "# of sols to track" << a << endl;
--    << "# of sols in target w/o correspondence" << b << endl;
--    << "# of established correspondences" << c << endl;
--    << "target solution count" << d << endl;
        
    if d!=c and a!=0 then p := (d-c-b) / (d-c)
    else p=0;
    {*
    if tail === G.MasterNode then (
        << "we've hit the master node" << endl;
	p = p*G.MasterFactor;
	);
    if head === G.MasterNode then (
        << "we've hit the master node backwards" << endl;
	p = p*(1/G.MasterFactor);
	);
    *}        
    p
    ) 


selectBestEdgeAndDirection = G -> (
    p12 := apply(G.Edges, e -> e.Potential12);
    p21 := apply(G.Edges, e -> e.Potential21);
    m12 := max toList p12;
    m21 := max toList p21;
    if m12 > m21 then (
	e := positions(p12, m -> m == m12);
	(G.Edges#(e#(random length e)), true)
	)
    else (
	e = positions(p21, m -> m == m21);
	(G.Edges#(e#(random length e)), false)
	)
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
    untrackedInds := indices head.PartialSols - set keys correspondence;
    newSols := if #untrackedInds > 0 then track(polySystem (gammaHead * head.SpecializedSystem), polySystem(gammaTail * tail.SpecializedSystem), (head.PartialSols)_(untrackedInds))
    else {};
    n := length tail.PartialSols;
    scan(#untrackedInds, i->(
	    a := untrackedInds#i;
	    s := newSols#i;
	    if status s =!= Regular then (
		print "failure: a singular point"; 
		correspondence#a = null; -- record failure
	      	)
	    else ( 
	    	if member(s, tail.PartialSols) then b:= position(s,tail.PartialSols) 
	    	else (    
		    appendPoint(tail.PartialSols, s);
		    b = n;
		    n = n+1;
		    );
	    	if not addCorrespondence(if from1to2 then (e,a,b) else (e,b,a))
	    	then (
		    print "failure: correspondence conflict";
		    correspondence#a = null -- record failure 
		    )
		)
	    ));
    for e in tail.Edges do (
    	e.Potential12 = G.Potential (e, true);
    	e.Potential21 = G.Potential (e, false);
	);
    #untrackedInds
    )
