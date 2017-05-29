debug needsPackage "SLPexpressions"
debug needsPackage "NumericalAlgebraicGeometry"
export {
    "specializeSystem",
    "selectRandomEdgeAndDirection",
    "selectBestEdgeAndDirection",
    "makeRandomizedSelect",
    "potentialLowerBound",
    "makeBatchPotential",
    "potentialE",
    "HomotopyGraph",
    "HomotopyEdge",
    "HomotopyNode",
    "getTrackTime",
    "saturateEdges"}
    
HomotopyNode = new Type of MutableHashTable 
HomotopyEdge = new Type of MutableHashTable
HomotopyGraph = new Type of MutableHashTable

USEtrackHomotopy = false -- determines whether to use trackHomotopy (new engine)

addNode = method()
addNode (HomotopyGraph, Point, PointArray) := (G, params, partialSols) -> (
    N := new HomotopyNode from {
        BasePoint => params,
        PartialSols => partialSols,
        Graph => G,
        SpecializedSystem => specializeSystem (params, G.Family),
	Edges => new MutableList from {}
    };
    G.Vertices = append(G.Vertices, N);
    N
)

addEdge = method(Options=>{"random gamma"=>true})
addEdge (HomotopyGraph, HomotopyNode, HomotopyNode) := o -> (G,n1,n2) -> (
    E := new HomotopyEdge from {
            Node1 => n1, 
            Node2 => n2, 
	    Graph => G,
            gamma1 => if o#"random gamma" then exp(2 * pi* ii * random RR) else 1, 
            gamma2 => if o#"random gamma" then exp(2 * pi* ii * random RR) else 1, 
            Correspondence12 => new MutableHashTable from {}, -- think: the map from labels of points of Node1 to those of Node2
            Correspondence21 => new MutableHashTable from {}  -- ............................................2.................1
        };
    n1.Edges#(#n1.Edges) = E;
    n2.Edges#(#n2.Edges) = E;
    G.Edges = append(G.Edges,E);
    if G.Potential =!= null then (	
    	E.Potential12 = G.Potential (E, true);
    	E.Potential21 = G.Potential (E, false);
    	);
    F1 := polySystem(E.gamma1 * n1.SpecializedSystem);
    F2 := polySystem(E.gamma2 * n2.SpecializedSystem);
    if USEtrackHomotopy then (
	if o#"random gamma" then (
	    E#"homotopy12" = segmentHomotopy(F1,F2);
    	    E#"homotopy21" = segmentHomotopy(F2,F1);
	    )
	else ( -- this is a hack engaged for a more general purpose (e.g., nonlinear systems)
	    F := G.Family.PolyMap;
	    (FR, mapFR) := flattenRing ring F;
            FF := mapFR F;
	    t := symbol t;
	    Rt := CC(monoid [gens ring F, t]);
	    t = last gens Rt;
	    F12 := (map(Rt,FR,drop(gens Rt,-1) | ((1-t)*coordinates n1.BasePoint + t*coordinates n2.BasePoint))) FF;   		
	    F21 := sub(F12,t=>1-t);   		
	    XT := getVarGates Rt;
	    X := gateMatrix{drop(XT,-1)};
	    T := last XT; 
	    print "-- setting up gateHomotopy for an edge...";
	    E#"homotopy12" = gateHomotopy(gateMatrix polySystem F12, X, T, Strategy=>compress);
	    E#"homotopy21" = gateHomotopy(gateMatrix polySystem F21, X, T, Strategy=>compress);
	    )
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

specializeSystem = method()
specializeSystemInternal := (p, M, R'PR'toPR'X) -> (
    (R, PR, toPR, X) := R'PR'toPR'X; -- see below for ingredients
    flatten entries (map(R,PR,X|matrix p)) toPR transpose M
    )   
specializeSystem (Point, PolySystem) := (p, F) -> (
    if not F#?"specialization ingredients" then (
    	nParameters := numgens coefficientRing ring F;
    	assert(nParameters == #coordinates p);
    	(PR,toPR) := flattenRing ring F;
    	X := drop(gens PR, -nParameters);
	R := (coefficientRing PR)[X]; 
    	X = vars R;
    	F#"specialization ingredients" = (R,PR,toPR,X);
	);
    specializeSystemInternal(p, F#PolyMap, F#"specialization ingredients")
    )
specializeSystem (Point, Matrix) := (p, M) -> (
    nParameters := numgens coefficientRing ring M;
    assert(nParameters == #coordinates p);
    (PR,toPR) := flattenRing ring M;
    X := drop(gens PR, -nParameters); 
    R := (coefficientRing PR)[X];
    X = vars R;
    specializeSystemInternal(p,M,(R,PR,toPR,X))
    )

-- convenience function for WS init
edgeInds = (G,v) -> (
    i := (positions(G.Vertices, x -> x == v))#0;
    positions(G.Vertices, x -> member(x,G.Edges)) 
    )

-- returns (head,tail,correspondence,correspondence')
head'n'tail = (e, from1to2) -> 
    if from1to2 then (e.Node1, e.Node2, e.Correspondence12, e.Correspondence21) else
	             (e.Node2, e.Node1,	e.Correspondence21, e.Correspondence12)

potentialLowerBound = (e,from1to2) -> (
    (head,tail,correspondence,correspondence') := head'n'tail(e,from1to2);
    n1 := length head.PartialSols - length keys correspondence;
    n2 := length tail.PartialSols - length keys correspondence';
    max(n1-n2, 0)
    ) 

{*
potentialE = (e,from1to2) -> (
    G := e.Graph;
    (head,tail,correspondence,correspondence') := head'n'tail(e,from1to2);
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
    p
    ) 
*}

makeBatchPotential = method()
makeBatchPotential ZZ := batchSize -> (
    (e,from1to2) -> (
    	G := e.Graph;
    	(head,tail,correspondence,correspondence') := head'n'tail(e,from1to2);
    	c := length keys correspondence;
   	a := length head.PartialSols - c; -- known head sols without correspondence
    	b := length tail.PartialSols - c; -- known tail sols without correspondence
    	d := (e.Graph).TargetSolutionCount;
 	if d!=c and a!=0 then min(batchSize,a) * (d-c-b) / (d-c)
	else -infinity
    	)
    ) 
potentialE = makeBatchPotential 1

selectRandomEdgeAndDirection = G-> (G.Edges#(random (#G.Edges)),random 2 == 0)
selectBestEdgeAndDirection = G -> (
    p12 := toList apply(G.Edges, e -> e.Potential12);
    p21 := toList apply(G.Edges, e -> e.Potential21);
    m12 := max p12;
    m21 := max p21;
--    print (p12,p21);
    if m12 > m21 then (
	e := positions(p12, m -> m == m12);
	(G.Edges#(e#(random length e)), true)
	)
    else (
	e = positions(p21, m -> m == m21);
	(G.Edges#(e#(random length e)), false)
	)
    )
makeRandomizedSelect = method()
makeRandomizedSelect RR := p -> (
    assert (p<=1 and p>=0);
    G -> if random RR < p then selectRandomEdgeAndDirection G else selectBestEdgeAndDirection G
    )

setTrackTime = method()
setTrackTime (HomotopyGraph, Number) := (G,t) -> G#"track time" = t
 
getTrackTime = method()
getTrackTime HomotopyGraph := G -> G#"track time"

-- prototype for edge tracking function
-- assumptions: 
-- 1) member function is working/optimized for PointAray objects 
-- 2) specializeSystem method which converts parametric coefficients to a list of polynomials (inputs to track),
-- 3) positions method defined for pointset object
-- Output: 
trackEdge = method()
trackEdge (HomotopyEdge, Boolean) := (e, from1to2) -> trackEdge(e,from1to2,infinity)
trackEdge (HomotopyEdge, Boolean, Thing) := (e, from1to2, batchSize) -> (
    G := e.Graph;
    homotopy := null;
    if from1to2 then (
	(head, tail) := (e.Node1, e.Node2);
	(gammaHead, gammaTail) :=  (e.gamma1, e.gamma2);
	correspondence := e.Correspondence12;
	if e#?"homotopy12" then homotopy = e#"homotopy12";
	)
    else  (
	(head, tail) = (e.Node2, e.Node1);
	(gammaHead, gammaTail) =  (e.gamma2, e.gamma1);
	correspondence = e.Correspondence21;
	if e#?"homotopy21" then homotopy = e#"homotopy21";
	);
    untrackedInds := indices head.PartialSols - set keys correspondence;
    untrackedInds = take(untrackedInds, min(#untrackedInds, batchSize));
    startSolutions := (head.PartialSols)_(untrackedInds);
    newSols := if #untrackedInds > 0 then (
	t'sols := elapsedTiming(    	 
	    if USEtrackHomotopy then trackHomotopy(homotopy,startSolutions)  
	    else track(polySystem (gammaHead * head.SpecializedSystem), 
	    	polySystem(gammaTail * tail.SpecializedSystem), 
	    	startSolutions)
	    );
	t := first t'sols;
	sols := last t'sols;
	setTrackTime(G,getTrackTime(G)+t);
	sols
	)
    else {};
    n := length tail.PartialSols;
    scan(#untrackedInds, i->(
	    a := untrackedInds#i;
	    s := newSols#i;
	    if status s =!= Regular then (
		<< "failure: status = " << status s << endl;
		correspondence#a = null; -- record failure
	      	)
	    else ( 
	    	if member(s, tail.PartialSols) then b:= position(s,tail.PartialSols) 
	    	else (    
		    s = point {coordinates s}; -- lose the rest of info
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
    if G.Potential =!= null 
    then for e in tail.Edges do (
    	e.Potential12 = G.Potential (e, true);
    	e.Potential21 = G.Potential (e, false);
	);
    #untrackedInds
    )

saturateEdges = method()
saturateEdges HomotopyGraph := G -> (
    apply(G.Edges, e -> trackEdge(e, false));
    apply(G.Edges, e -> trackEdge(e, true));
    )


end

------------------------------------------
------------------------------------------
-- Documentation
------------------------------------------
------------------------------------------

beginDocumentation()

------------------------------------------
-- Data Types
------------------------------------------

doc ///
  Key
    Graphs
///
