debug needsPackage "SLPexpressions"
debug needsPackage "NumericalAlgebraicGeometry"
export {
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
    "saturateEdges",
    "FilterCondition",
    "FilterFailure",
    "Randomizer",
    "Equivalencer",
    "PartialSolBins",
    "FirstDirectedEdge",
    "completeGraphInit",
    "completeGraphAugment",
    "flowerGraphInit",
    "flowerGraphAugment",
    "NumSols"
    }

USEtrackHomotopy = false
    
HomotopyNode = new Type of MutableHashTable 
HomotopyEdge = new Type of MutableHashTable
HomotopyGraph = new Type of MutableHashTable

addNode = method()
addNode (HomotopyGraph, Point, PointArray) := (HG, params, partialSols) -> (
    N := new HomotopyNode from {
        BasePoint => params,
        PartialSols => partialSols,
	PartialSolBins => pointArray(HG.Equivalencer \ points partialSols),
        Graph => HG,
        Position => # HG.Vertices,
	Edges => new MutableList from {}
    };
    if (not HG.SLP)  then (
	N.SpecializedSystem = specializeSystem (params, HG.Family));
    HG.Vertices = append(HG.Vertices, N);
    N
)
addNode (HomotopyGraph, Matrix, Matrix) := (HG, p0, x0) -> addNode(point p0, pointArray {point x0})

numSols = method()
numSols HomotopyNode := N -> length N.PartialSols

addEdge = method(Options=>{"random gamma"=>true})
addEdge (HomotopyGraph, HomotopyNode, HomotopyNode) := o -> (HG,n1,n2) -> (
    E := new HomotopyEdge from {
            Node1 => n1, 
            Node2 => n2, 
	    Graph => HG,
            gamma1 => if o#"random gamma" then exp(2 * pi* ii * random RR) else 1, 
            gamma2 => if o#"random gamma" then exp(2 * pi* ii * random RR) else 1, 
            Correspondence12 => new MutableHashTable from {}, -- think: the map from labels of points of Node1 to those of Node2
            Correspondence21 => new MutableHashTable from {}  -- ............................................2.................1
        };
    n1.Edges#(#n1.Edges) = E;
    n2.Edges#(#n2.Edges) = E;
    HG.Edges = append(HG.Edges,E);
    if HG.Potential =!= null then (	
    	E.Potential12 = HG.Potential (E, true);
    	E.Potential21 = HG.Potential (E, false);
    	);
    if HG.SLP then (
	p1 := transpose matrix n1.BasePoint;
	p2 := transpose matrix n2.BasePoint;
	if (HG.Randomizer =!= null) then (
	    p1 = HG.Randomizer p1;
	    p2 = HG.Randomizer p2;
	    );
        -*

        
        p0 -> p1
        F(p, x)=0
        y(t) = (1 - * t(t-1))p0 +  (t - * t(t-1))p1
        H_01 (x, t) = F( y(t) , x)
        *-
    	E#"homotopy12" = specialize(HG.Family, 
	    ((E.gamma1)*p1)||
	    ((E.gamma2)*p2));
    	E#"homotopy21" = specialize(HG.Family, 
	    ((E.gamma2)*p2)||
	    ((E.gamma1)*p1));
		)
    else (
	    F1 := polySystem(E.gamma1 * n1.SpecializedSystem);
    	    F2 := polySystem(E.gamma2 * n2.SpecializedSystem);
	    if USEtrackHomotopy then (
		if o#"random gamma" then (
	    	    E#"homotopy12" = segmentHomotopy(F1,F2);
    	    	    E#"homotopy21" = segmentHomotopy(F2,F1);
	    	    )
		else ( -- this is a hack engaged for a more general purpose (e.g., systems which are non-linear in parameter)
	    	    F := HG.Family.PolyMap;
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
	    	    -- "-- setting up gateHomotopy for an edge...";
	    	    E#"homotopy12" = gateHomotopy(gateMatrix polySystem F12, X, T, Strategy=>compress);
	    	    E#"homotopy21" = gateHomotopy(gateMatrix polySystem F21, X, T, Strategy=>compress);
	    	    )
    		);
	    );
    	    E
)

removeEdge = method()
removeEdge(HomotopyGraph, HomotopyEdge) := (HG,e) -> (
    (N1, N2) := (HG.Node1, HG.Node2);
    N1.Edges = remove(N1.Edges, e);
    N2.Edges = remove(N2.Edges, e);
    HG.Edges = remove(HG.Edges, e);
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

homotopyGraph = method(TypicalValue => HomotopyGraph, 
    Options => {
        Potential=>null, 
        FilterCondition=>(x->false), 
        Randomizer=>(p->p), 
        Equivalencer=>(x->x),
        Verbose=>false
        }
    )
installMethod(homotopyGraph, o -> ()-> new HomotopyGraph from {
	Vertices => new MutableList from {},
	Edges => new MutableList from {}
	})
homotopyGraph System := o -> PF -> (
    HG := homotopyGraph();
    HG.SLP = instance(PF, GateSystem);
    HG.Family = if HG.SLP then parametricSegmentHomotopy PF else PF;
    HG.Potential = o.Potential;
    HG.FilterCondition = o.FilterCondition;
    HG.Randomizer = o.Randomizer;
    HG.Equivalencer = o.Equivalencer;
    HG.Verbose = o.Verbose;
    HG
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

makeBatchPotential = method()
makeBatchPotential ZZ := batchSize -> (
    (e,from1to2) -> (
    	HG := e.Graph;
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

-- _should not_ be called by a graph initializer
updateFirstDirectedEdge = HG -> (
    availableE := select(1, HG.Edges, e -> (
            (head, tail, correspondence, correspondence') := head'n'tail(e, true);
            length head.PartialSols > length keys correspondence or length tail.PartialSols > length keys correspondence'
            )
        );
    HG.FirstDirectedEdge = if (#availableE == 0) then null else (
        e := first availableE;
        (head, tail, correspondence, correspondence') := head'n'tail(e, true);
        if (length head.PartialSols > length keys correspondence) then (e, true)
        else (e, false)
        );
    )

-- edge selection routines

selectFirstEdgeAndDirection = HG -> HG.FirstDirectedEdge

selectRandomEdgeAndDirection = HG -> (HG.Edges#(random (#HG.Edges)),random 2 == 0)
        
selectBestEdgeAndDirection = HG -> (
    p12 := toList apply(HG.Edges, e -> e.Potential12);
    p21 := toList apply(HG.Edges, e -> e.Potential21);
    m12 := max p12;
    m21 := max p21;
--    print (p12,p21);
    if m12 > m21 then (
	e := positions(p12, m -> m == m12);
	(HG.Edges#(e#(random length e)), true)
	)
    else (
	e = positions(p21, m -> m == m21);
	(HG.Edges#(e#(random length e)), false)
	)
    )
makeRandomizedSelect = method()
makeRandomizedSelect RR := p -> (
    assert (p<=1 and p>=0);
    HG -> if random RR < p then selectRandomEdgeAndDirection HG else selectBestEdgeAndDirection HG
    )

setTrackTime = method()
setTrackTime (HomotopyGraph, Number) := (HG,t) -> HG#"track time" = t
 
getTrackTime = method()
getTrackTime HomotopyGraph := HG -> HG#"track time"

-- IN: HomotopyEdge (e = head--tail)
--     Boolean (from1to2) -- if true, track head->tail, else track tail->head
--     BatchSize: bound on number of points to track
-- Output: the number of _Attempted_ path-tracking tasks
-- Modifies: e and its HomotopyGraph
trackEdge = method()
trackEdge (HomotopyEdge, Boolean) := (e, from1to2) -> trackEdge(e, from1to2, infinity)
trackEdge (HomotopyEdge, Boolean, Thing) := (e, from1to2, batchSize) -> (
    HG := e.Graph;
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
	setTrackTime(HG,getTrackTime(HG)+t);
	sols
	)
    else {};
    n := length tail.PartialSols;
    scan(#untrackedInds, i->(
	    a := untrackedInds#i;
	    s := newSols#i;
	    if ((HG.FilterCondition =!= null) and 
		(HG.FilterCondition(transpose matrix tail.BasePoint, transpose matrix s))) then (
                if HG.Verbose then << "a path failed (as flagged by the FilterCondition option)" << endl;
		s.SolutionStatus = FilterFailure;
	       	correspondence#a = null; -- record failure		  
		);
	    if (status s =!= Regular) then (
		if HG.Verbose then << "a path failed: status = " << status s << endl;
		correspondence#a = null; -- record failure
		)
	    else ( 
		if member(HG.Equivalencer s, tail.PartialSolBins) then b:= position(HG.Equivalencer s,tail.PartialSolBins) 
		else (    
		    s = point {coordinates s}; -- lose the rest of info
		    appendPoint(tail.PartialSols, s);
		    appendPoint(tail.PartialSolBins, HG.Equivalencer s);
		    b = n;
		    n = n+1;
		    );
		if not addCorrespondence(if from1to2 then (e,a,b) else (e,b,a))
		then (
		    print "failure due to correspondence conflict (suggesting paths have jumped)";
		    correspondence#a = null -- record failure 
		    )
		);
	    )
	);
    if HG.Potential =!= null 
    then for e in tail.Edges do (
    	e.Potential12 = HG.Potential (e, true);
    	e.Potential21 = HG.Potential (e, false);
	);
    ret := #untrackedInds;
    if (ret > 0 and HG.Verbose) then (
        << "WARNING: no paths attempted" << endl;
        if DebuggingMode == true then error "trackEdge should not be called";
        );
    updateFirstDirectedEdge HG;
    ret
    )

-- !!
saturateEdges = method()
saturateEdges HomotopyGraph := HG -> (
    apply(HG.Edges, e -> trackEdge(e, false));
    apply(HG.Edges, e -> trackEdge(e, true));
    )

-- gives HG "complete graph" shape
completeGraphInit = (HG, p, node1, nnodes, nedges) -> (
    nextP := (p0 -> point {apply(#coordinates p0, i->exp(2*pi*ii*random RR))});
    for i from 1 to nnodes-1 do (
        addNode(HG, nextP p, pointArray {});
    );
    for i from 0 to nnodes-1 do (
        for j from i+1 to nnodes-1 do (
            apply(nedges, k -> addEdge(HG, HG.Vertices#i, HG.Vertices#j,
		    "random gamma" => (nedges>1)));
        );
    );
    HG.FirstDirectedEdge = (first HG.Edges, true);
    )

completeGraphAugment = (HG, p, node1, nStartingEdges, nNewEdges, nNewNodes) -> (
    nextP := (p0 -> point {apply(#coordinates p0, i->exp(2*pi*ii*random RR))});
    for i from 1 to nNewNodes do (
        newNode := addNode(HG, nextP p, pointArray {});
        for j from 0 to #(HG.Vertices) - 2 do (
            apply(nStartingEdges, k -> addEdge(HG, newNode, HG.Vertices#j));
            );
	);
	
	nNodes := #(HG.Vertices);
	for i from 0 to nNodes-1 do (
		for j from i+1 to nNodes-1 do (
			apply(nNewEdges, k -> addEdge(HG, HG.Vertices#i, HG.Vertices#j));
		);
	);
);

-- gives HG a "flower" shape
flowerGraphInit = (HG, p, node1, nnodes, nedges) -> (
    nextP := ((p0)->point {apply(#coordinates p0, i->exp(2*pi*ii*random RR))});
    for i from 1 to nnodes do (
        newNode := addNode(HG,nextP(p), pointArray {});
        apply(nedges, k -> addEdge(HG, node1, newNode));
	);
    HG.FirstDirectedEdge = (first HG.Edges, true);
    updateFirstDirectedEdge HG;

)

-- static flower augmentation function
flowerGraphAugment = (HG, p, node1, nStartingEdges, nNewEdges, nNewNodes) -> (
    nextP := ((p0)->point {apply(#coordinates p0, i->exp(2*pi*ii*random RR))});
    for i from 1 to nNewNodes do (
		newNode := addNode(HG,nextP(p), pointArray {});
		apply(nStartingEdges, k -> addEdge(HG, node1, newNode));
	);
	for i from 1 to #(HG.Vertices) - 1 do (
		apply(nNewEdges, k -> addEdge(HG, HG.Vertices#i, node1));
	);
)


end

restart
needsPackage "MonodromySolver"

R = CC[x,y,a]
PS = polySystem {x^2+y^2-1, x+a*y, (a^2+1)*y^2-1}
G = gateSystem(squareUp(PS,2), drop(gens R,2)) 

makeEdgeHomotopy = G -> (
    PH := parametricSegmentHomotopy G;
    c := inputGate symbol c;
    t := PH.GateHomotopy#"T";
    H := sub(PH.GateHomotopy#"H", t=> t* ( (2-4*c) *t + (4*c-1)));
    gateHomotopy(H, vars G, t, Parameters => gateMatrix{{c}} | PH.Parameters)
    )

PH = makeEdgeHomotopy G
PH.Parameters 

c0 = 0
a0 = 0; a1 = 1;
H = specialize (PH, transpose matrix{{c0,a0,a1}})
s'sols = { {{0,1}},{{0,-1}} }/point
time sols = trackHomotopy(H,s'sols)
assert areEqual(sols,{{ { -.707107, .707107}, SolutionStatus => Regular }, { {.707107, -.707107}, SolutionStatus => Regular }} / point)


        s(0) = 0
        s(0.5) = c
        s(1) = 1
        s(t) = t (at + b) 
               a+b=1
               a + 2b =4 c
               b = 4c -1
               a = 2 - 4c
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
