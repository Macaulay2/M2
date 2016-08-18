export {
    "createSeedPair",
    "computeMixedVolume",
    "monodromySolve",
    "completeGraphInit",
    "flowerGraphInit",
    "ExtraNodeCount",
    "RandomPointFunction",
    "NumberOfEdges",
    "NumberOfNodes",
    "NumberOfRepeats",
    "StoppingCriterion",
    "GraphInitFunction",
    "SelectEdgeAndDirection",
    "randomWeights"}

-- in: PF, a system of polynomials in a ring of the form CC[parameters][variables]
--     point0, (as above)
--     s0, (as above)

--needs "./random_methods.m2"

-- random complex vector via Box-Mueller transform
randomWeights = n -> matrix(CC, {apply(n,i-> (
	    us := apply(2, i -> random(sub(0,RR), sub(1,RR)));
	    us = {sqrt(-2* log first us), 2*pi* last us};
	    first us * cos last us + ii * first us * sin last us))})


completeGraphInit = (G, p, node1, nnodes, nedges) -> (
    nextP := ((p0)->point {apply(#coordinates p0, i->exp(2*pi*ii*random RR))});
    for i from 1 to nnodes-1 do (
        addNode(G,nextP(p), pointArray {});
    );
    print(peek(G));
    for i from 0 to nnodes-1 do (
        for j from i+1 to nnodes-1 do (
            apply(nedges, k -> addEdge(G, G.Vertices#i, G.Vertices#j));
        );
    );
    )

solveViaMonodromy = method(Options=>{RandomPointFunction=>null,StoppingCriterion=>((n,L)->n>3)})
solveViaMonodromy (Matrix, Point, List) := o -> (PF,point0,s0) -> (
    if #s0 < 1 then error "at least one solution expected";  
    p0 := matrix point0; -- points are row matrices
    nParameters := numgens coefficientRing ring PF;
    assert(nParameters == numcols p0);
    (PR,toPR) := flattenRing ring PF; -- ring PF = C[a][x]
    -- toPR: ring PF -> PR
    X := drop(gens PR, -nParameters); 
    PF = toPR PF;
    C := coefficientRing PR;
    R := C[X];
    X = vars R;
    nextP := if o.RandomPointFunction =!= null then o.RandomPointFunction else (
	K := ring p0;
	()->point {apply(numcols p0, i->exp(2*pi*ii*random RR))}
	); 
    sols0 := s0;
    nSols := #sols0; 
    same := 0;
    dir := temporaryFileName(); -- build a directory to store temporary data 
    makeDirectory dir;
    nPathsTracked := 0;
    << "--backup directory created: "<< toString dir << endl;
    while not o.StoppingCriterion(same,sols0) do --try 
    (
    	p1 := matrix nextP(); -- row matrix
	F0 := flatten entries (map(R,PR,X|p0)) PF;
	F1 := flatten entries (map(R,PR,X|p1)) PF;
	elapsedTime sols1 := track(F0,F1,sols0);
        nPathsTracked = nPathsTracked + #sols0;
	sols1 = select(sols1, s->status s === Regular);
	<< "  H01: " << #sols1 << endl;
    	elapsedTime sols0' := track(F1,F0,gamma=>exp(2*pi*ii*random RR),sols1);
        nPathsTracked = nPathsTracked + #sols1;
	sols0' = select(sols0', s->status s === Regular);
	<< "  H10: " << #sols0' << endl;
	elapsedTime sols0 = clusterSolutions(sols0 | sols0'); -- take the union	
    << "number of paths tracked: " << nPathsTracked << endl;
	if #sols0 == nSols then same = same + 1 else (
	    nSols = #sols0; 
	    same = 0;
	    ff := openOut (dir|"/backup-"|toString nSols|"-solutions"); 
	    ff << toExternalString sols0;
	    close ff; 
	    );  
    	<< "found " << #sols0 << " points in the fiber so far" << endl;
    	) -- else print "something went wrong"
    ;
    sols0
    )    
    
--Object based implementation where there's a base node and every new loop
--adds a new node and two new edges.
flowerStrategy = method(Options=>{RandomPointFunction=>null,StoppingCriterion=>((n,L)->n>3)})
flowerStrategy (Matrix, Point, List) := o -> (PF,point0,s0) -> (
    HG := homotopyGraph polySystem transpose PF;
    PA := pointArray s0;
    center := addNode(HG, point0, PA);
    if #s0 < 1 then error "at least one solution expected";  
    nextP := if o.RandomPointFunction =!= null then o.RandomPointFunction else (
	    ()->point {apply(#coordinates point0, i->exp(2*pi*ii*random RR))}
    ); 
    same := 0;
    nsols0 := #center.PartialSols;
    npaths := 0;
    while not o.StoppingCriterion(same,null) do (
    	
        --Create the new node
        apex := addNode(HG, nextP(), pointArray {});
    	
        --Track to the new node
        e1 := addEdge(HG, center, apex);
        npaths = npaths + trackEdge(e1, true);
        << "  H01: " << #(apex.PartialSols) << endl;
    	
	
        --Track back from the new node
        e2 := addEdge(HG, apex, center);
	npaths = npaths + trackEdge(e2, true);
	
        << "  H01: " << #(center.PartialSols) << endl;
    	
        if #center.PartialSols == nsols0 then same = same + 1 else (
            nsols0 = #center.PartialSols;
            same = 0; 
        );
        << "found " << #(center.PartialSols) << " points in the fiber so far" << endl;
    );
    (HG, npaths)
)    

twoNodes = method(Options=>{RandomPointFunction=>null,StoppingCriterion=>((n,L)->n>3), 
	SelectEdgeAndDirection => (G-> (G.Edges#(random (#G.Edges)),random 2 == 0)), 
    	TargetSolutionCount=>null, Potential=>null})
twoNodes (Matrix, Point, List, ZZ) := o -> (PF,point0,s0,nedges) -> (
    HG := homotopyGraph(polySystem transpose PF, Potential=>o.Potential);
    if o.TargetSolutionCount =!= null then (
	HG.TargetSolutionCount = o.TargetSolutionCount;
	stoppingCriterion := (n,L) -> #L >= o.TargetSolutionCount;
	)
    else stoppingCriterion = o.StoppingCriterion; 
    PA := pointArray s0;
    node1 := addNode(HG, point0, PA);
    	
    if #s0 < 1 then error "at least one solution expected";  
    nextP := if o.RandomPointFunction =!= null then o.RandomPointFunction else (
	    ()->point {apply(#coordinates point0, i->exp(2*pi*ii*random RR))}
    );
   selectEdgeAndDirection := o.SelectEdgeAndDirection;
   --Create the new node
    node2 := addNode(HG, nextP(), pointArray {});
 
    same := 0;
    --nsols0 := #node1.PartialSols;
    npaths := 0;
    E := apply(nedges, i -> addEdge(HG, node1, node2));
    while not stoppingCriterion(same,node1.PartialSols) do (
	(e, from1to2) := selectEdgeAndDirection(HG);
	{*
	<< "Correspondences are " << (keys e.Correspondence12 , e.Potential12) << " and " << (keys e.Correspondence21, e.Potential21)  << endl;
	<< "Direction is " << from1to2 << endl;
	*}
	trackedPaths := trackEdge(e, from1to2);
        npaths = npaths + trackedPaths;
        << "  node1: " << length node1.PartialSols << endl;
        << "  node2: " << length node2.PartialSols << endl;    	
        << "trackedPaths " << trackedPaths << endl; 
        if trackedPaths == 0 then same = same + 1 else same = 0; 
    );
    (HG, npaths)
)    

--Single loop strategy. Add a new edge every time. When nodecount = 2, this is
--the minimal graph version.
loopStrategy = method(Options=>{RandomPointFunction=>null,StoppingCriterion=>((n,L)->n>3)})
loopStrategy (Matrix, Point, List, ZZ) := o -> (PF,point0,s0,nodecount) -> (
    HG := homotopyGraph polySystem transpose PF;
    PA := pointArray s0;
    startNode := addNode(HG, point0, PA);
    if #s0 < 1 then error "at least one solution expected";
    if nodecount < 2 then error "at least two nodes are necessary";
    
    nextP := if o.RandomPointFunction =!= null then o.RandomPointFunction else (
	    ()->point {apply(#coordinates point0, i->exp(2*pi*ii*random RR))}
    );
    
    --Create the nodes
    for i from 1 to nodecount - 1 do (
        newNode := addNode(HG, nextP(), pointArray {});
    );
    
    npaths := 0;
    same := 0;
    nSols := #s0;
    while not o.StoppingCriterion(same,null) do (
        trackedPaths := 0;
        for i from 0 to #(HG.Vertices) - 1 do (
            node1 := HG.Vertices#i;
            node2 := HG.Vertices#((i+1)%(#HG.Vertices));
            e := addEdge(HG, node1, node2);
            trackedPaths = trackedPaths + trackEdge(e, true);
            npaths = npaths + trackedPaths;
            << "  node1: " << length(node1.PartialSols) << endl;
            << "  node2: " << length(node2.PartialSols) << endl;    	
            << "npaths " << npaths << endl; 
        );
        if #((HG.Vertices#0).PartialSols) == nSols then same = same + 1 else (
            nSols = #((HG.Vertices#0).PartialSols);
            same = 0;
        );
    )
)
--This strategy will either: 1) follow an existing loop, 2) add a new loop
--and follow it, or 3) add a new node, create and follow a loop around it.
randomFlowerStrategy = method(Options=>{RandomPointFunction=>null,StoppingCriterion=>((n,L)->n>3)})
randomFlowerStrategy (Matrix, Point, List) := o -> (PF,point0,s0) -> (
    HG := homotopyGraph polySystem transpose PF;
    PA := pointArray s0;
    startNode := addNode(HG, point0, PA);
    
    if #s0 < 1 then error "at least one solution expected";
    nextP := if o.RandomPointFunction =!= null then o.RandomPointFunction else (
	    ()->point {apply(#coordinates point0, i->exp(2*pi*ii*random RR))}
    );
    
    --We start out with a second node and a loop so all of the options are
    --possible.
    newNode := addNode(HG, nextP(), pointArray {});
    e1 := addEdge(HG, startNode, newNode);
    e2 := addEdge(HG, newNode, startNode);
    edges := new MutableList from {{e1,e2}};
    
    npaths := 0;
    same := 0;
    nSols := #s0;
    while not o.StoppingCriterion(same,null) do (
        randInt := random 3;
        --Follow an existing loop
        if randInt == 0 then (
            randEdge := edges#(random (#edges));
            (e1,e2) = (randEdge#0,randEdge#1);
        
        --Add a new loop
        ) else if randInt == 1 then (
            randNode := HG.Vertices#(random(1, #HG.Vertices - 1));
            e1 = addEdge(HG, startNode, randNode);
            e2 = addEdge(HG, randNode, startNode);
            edges = append(edges, {e1,e2});
        --Create a new node and loop
        ) else (
            newNode = addNode(HG, nextP(), pointArray {});
            e1 = addEdge(HG, startNode, newNode);
            e2 = addEdge(HG, newNode, startNode);
            edges = append(edges, {e1,e2});
        );
        
        npaths = npaths + trackEdge(e1, true);
        << "  H01: " << length(e1.Node2.PartialSols) << endl;
        npaths = npaths + trackEdge(e2, true);
        << "  H10: " << length(e1.Node1.PartialSols) << endl;    	
        << "npaths " << npaths << endl; 

        if #((HG.Vertices#0).PartialSols) == nSols then same = same + 1 else (
            nSols = #((HG.Vertices#0).PartialSols);
            same = 0;
        );
    )
)

computeMixedVolume = method()
computeMixedVolume List := polys -> mixedVolume(toRingXphc polys,StartSystem => false)

diffSolutions = method(TypicalValue=>Sequence, Options=>{Tolerance=>1e-3})
-- in:  A, B (presumably sorted)
-- out: (a,b), where a and b are lists of indices where A and B differ
diffSolutions (List,List) := o -> (A,B) -> (
     i := 0; j := 0;
     a := {}; b := {};
     while i<#A and j<#B do 
     if areEqual(A#i,B#j) then (i = i+1; j = j+1)
     else if isGEQ(A#i,B#j) then (b = append(b,j); j = j+1)
     else (a = append(a,i); i = i+1);	  
     (a|toList(i..#A-1),b|toList(j..#B-1))	      	    
     )



-- static flower
flowerGraphInit = (G, p, node1, nnodes, nedges) -> (
    nextP := ((p0)->point {apply(#coordinates p0, i->exp(2*pi*ii*random RR))});
    for i from 1 to nnodes do (
        newNode := addNode(G,nextP(p), pointArray {});
        apply(nedges, k -> addEdge(G, node1, newNode));
    );
    )
-- find the "seed" for the parametric system
createSeedPair = method(Options=>{"initial parameters" => "random"})
createSeedPair PolySystem := o -> G -> (
    R := ring G;   
    C := coefficientRing coefficientRing R; 
    init := o#"initial parameters";
    createSeedPair(G,
	if init == "random" then apply(numgens R, i->random C)
	else if init == "one" then toList(numgens R:1_C)
	else error "unknown option"
	)
    )
createSeedPair(PolySystem, List) := o -> (G, L) -> (
    SubList := apply(toList(0..numgens ring G-1), i -> (gens ring G)#i => L#i);
    C := coefficientRing ring G;
    M := sub(sub(G.PolyMap, SubList), C);
    M = flatten entries generators ideal M; 
    l := apply(M, g -> (coefficients(g, Monomials => gens C))#1);
    A := l#0;
    for i from 1 to length l - 1 do A = A | l#i;
    K := numericalKernel(transpose A, 10^(-6));
    -- K's columns are a basis for the kernel i indexes the 'most likely true positive'
    --v := K * transpose matrix {toList ((numcols K):1_CC)};  
    w := transpose randomWeights(numcols K);
    v := K * w;
    c0 := point matrix v;
    -- N := numericalIrreducibleDecomposition ideal M; -- REPLACE this with linear algebra (using numericalKernel)
    --c0 := first (first components N).Points; 
    pre0 := point{apply(SubList, i -> i#1)};
    (c0,pre0)
    )

-- main function
monodromySolve = method(Options=>{
        TargetSolutionCount => null,
        StoppingCriterion => ((n,L)->n>3),
        SelectEdgeAndDirection => selectRandomEdgeAndDirection,
        GraphInitFunction => completeGraphInit,
        Potential => null,
	NumberOfNodes => 2,
	NumberOfEdges => 3,
	NumberOfRepeats => 10})
monodromySolve (Matrix, Point, List) := o -> (PF,point0,s0) -> monodromySolve(polySystem transpose PF, point0,s0)
monodromySolve (PolySystem, Point, List) := o -> (PS,point0,s0) -> (
    HG := homotopyGraph(PS, Potential=>o.Potential);
    if o.TargetSolutionCount =!= null then (
        HG.TargetSolutionCount = o.TargetSolutionCount;
        stoppingCriterion := (n,L) -> (length L >= o.TargetSolutionCount or n>= o.NumberOfRepeats);
    )
    else stoppingCriterion = o.StoppingCriterion; 
    PA := pointArray s0;
    node1 := addNode(HG, point0, PA);
    HG.MasterNode = node1; -- not using this
    HG.MasterFactor = 1; -- ....
    
    if #s0 < 1 then error "at least one solution expected";
    
    selectEdgeAndDirection := o.SelectEdgeAndDirection;
    o.GraphInitFunction(HG, point0, node1, o.NumberOfNodes, o.NumberOfEdges);

    same := 0;
    npaths := 0;    
    lastNode := node1;
    while not stoppingCriterion(same,lastNode.PartialSols) do (
        (e, from1to2) := selectEdgeAndDirection(HG);
        {*  << "Correspondences are " << (keys e.Correspondence12 , e.Potential12);
        << " and " << (keys e.Correspondence21, e.Potential21)  << endl;
        << "Direction is " << from1to2 << endl;
	*}
	<< "-------------------------------------------------" << endl;
        trackedPaths := trackEdge(e, from1to2);
        npaths = npaths + trackedPaths;
	lastNode = if from1to2 then e.Node2 else e.Node1;
        << "  node1: " << length e.Node1.PartialSols << endl;
        << "  node2: " << length e.Node2.PartialSols << endl;    	
        << "trackedPaths " << trackedPaths << endl; 
        if trackedPaths == 0 then same = same + 1 else same = 0; 
    );
    if same == 10 then npaths = (HG.TargetSolutionCount)^2; -- some unrealistically high value for npaths, indicating failure
    (lastNode, npaths)
)

///
eRGraphInit = (G, p, node1, nnodes, prob) -> (
    nextP := ((p0)->point {apply(#coordinates p0, i->exp(2*pi*ii*random RR))});
    for i from 1 to nnodes-1 do (
        addNode(G,nextP(p), pointArray {});
    );
    for i from 0 to nnodes-1 do (
        for j from i+1 to nnodes-1 do (
            if coin(prob) == 1 then addEdge(G.Vertices#i, G.Vertices#j);
	    );
        );
    );
    )
///

-- in progress: Watts Strogartz random graph generator
-- assumes K even, might be nice to experiment with version where rewiring probability is arbitrary
///
wsInit = (G, p, nnodes, K) -> (
    nextP := ((p0)->point {apply(#coordinates p0, i->exp(2*pi*ii*random RR))});
    for i from 1 to nnodes-1 do (
	addNode(G,nextP(p), pointArray {}))
    for i from 0 to nnodes -1 do (
	vl := select(1..N, j -> (abs(i-j))%(N-1-sub(K/2,ZZ)) > 0 and (abs(i-j))%(N-1-sub(K/2,ZZ)) <= sub(K/2, ZZ));
	apply(vl, v-> apply(nedges, k -> addEdge(G, G.Vertices#i, v)))) 
    for e in G.Edges do (
	N1 := e.Node1;
	i := (positions(G.Vertices, N1))#0;
	if random(2) == 1 then (
	    remove(
	  
	)    
    )
///





