debug needsPackage "NumericalAlgebraicGeometry"
debug NAGtypes
needs(currentFileDirectory | "HomotopyGraphTypes.m2")
needs(currentFileDirectory | "random_methods.m2")
-- in: PF, a system of polynomials in a ring of the form CC[parameters][variables]
--     point0, (as above)
--     s0, (as above)

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
    TargetSolutionCount=>null, Potential=>potentialLowerBound})
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
	<< "Correspondences are " << (keys e.Correspondence12 , e.Potential12) << " and " << (keys e.Correspondence21, e.Potential21)  << endl;
	<< "Direction is " << from1to2 << endl;
	trackedPaths := trackEdge(e, from1to2);
        npaths = npaths + trackedPaths;
        << "  node1: " << #(node1.PartialSols) << endl;
        << "  node2: " << #(node2.PartialSols) << endl;    	
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
            << "  node1: " << #(node1.PartialSols) << endl;
            << "  node2: " << #(node2.PartialSols) << endl;    	
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
        << "  H01: " << #(e1.Node2.PartialSols) << endl;
        npaths = npaths + trackEdge(e2, true);
        << "  H10: " << #(e1.Node1.PartialSols) << endl;    	
        << "npaths " << npaths << endl; 

        if #((HG.Vertices#0).PartialSols) == nSols then same = same + 1 else (
            nSols = #((HG.Vertices#0).PartialSols);
            same = 0;
        );
    )
)


-- ideally, this function wouldn't be necessary. Currently there's a bug in PHCpack.m2 that
-- causes it to break when there's a variable named "e". I spent a while trying to fix it,
-- but it was difficult to fix cleanly. Instead, I chose to write a function that maps 
-- to a new ring with variables that phc will tolerate, and then computes the mixed volume.
needsPackage "PHCpack";
computeMixedVolume = method()
computeMixedVolume (List) := polys -> (
  R1 := ring polys#0;
  R1Gens := gens R1;
  numDigits := length (toString (#R1Gens));
  R2 := (coefficientRing R1) (for i in 1..#R1Gens list (
    value ("x" | demark ("",for i from 1 to numDigits-(length toString i) list "0") | toString i)
  ) );
  R2Gens := gens R2;
  generatorMapping := for i in 0..#(gens R1) - 1 list (R1Gens#i =>R2Gens#i);
  ringMap := map(R2, R1, generatorMapping);
  mixedVolume (polys/ringMap)
)    

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

{* 
The idea of this version is experimenting with making one graph that has 
some a number of nodes n = 1 + ExtraNodeCount. This function loops around
these n loops until the number of solutions stabilizes, as in the original
version. When ExtraNodeCount = 1, this is the minimal graph version.
*}
solveViaMonodromyOneLoop = method(Options=>{RandomPointFunction=>null,ExtraNodeCount=>1})
solveViaMonodromyOneLoop (Matrix, Point, List) := o -> (PF,point0,s0) -> (
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
    F0 := flatten entries (map(R,PR,X|p0)) PF;
    allowableThreads := 4;
    mvFunctionClosure := polys -> () -> computeMixedVolume polys;
    domvComputation := mvFunctionClosure F0;
    mvTask := schedule domvComputation;
    mvComputationCompleted := false;
    FList := for i in 1..o.ExtraNodeCount list(p1 := matrix nextP(); flatten entries (map(R,PR,X|p1)) PF);
    FList = {F0} | FList;
    solsList := for i in 1..o.ExtraNodeCount list({});
    solsList = {sols0} | solsList;
    solsList = new MutableList from solsList;
    previousLoopSolsList := new MutableList from for i in 0..o.ExtraNodeCount list({});
    gammaList := new MutableList;
    same := 0;
    nPathsTracked := 0;
    mv := -1;
    while true do (
        elapsedTime for i in 0..#FList - 1 do (
          ind1 := i;
          ind2 := ((i+1)%(#FList));
          F0 := FList#ind1;
          F1 := FList#ind2;
          gammaList#ind1 = exp(2*pi*ii*random RR);
          NewSols1 := track(F0,F1, gamma=>gammaList#ind1, trackbackwards=>false, solsList#ind1);
          nPathsTracked = nPathsTracked + #(solsList#ind1);
          NewSols1 = select(NewSols1, s->status s === Regular);
          previousLoopSolsList#ind2 = sortSolutions(NewSols1);

	  sols1 := clusterSolutions(solsList#ind2 | NewSols1); -- take the union
          solsList#ind2 = sortSolutions(sols1);
          << "i:" << i << ". " << #NewSols1 << " , " << #sols1 << ", " << endl;
          << "number of paths tracked: " << nPathsTracked << endl;

          if isReady mvTask then (
            mv = taskResult mvTask;
            << "Mixed volume computation completed! MV = ", << mv << endl;
            mvComputationCompleted = true;
          );

          if mvComputationCompleted then (
            if #(solsList#ind2) == mv then (
              for j in 0..ind2 do (
                startIndex := ind2 - j;
                endIndex := startIndex - 1;
                if startIndex == 0 then return nPathsTracked;

                diffs := diffSolutions(previousLoopSolsList#startIndex, solsList#startIndex);
                ptsToTrack := for diff in diffs#1 list (solsList#startIndex#diff);
                newSols := track(FList#startIndex,gammaList#endIndex*(FList#endIndex), gamma=>1,ptsToTrack);
                nPathsTracked = nPathsTracked + #ptsToTrack;
                newSols = select(newSols,s->status s === Regular);
                solsList#endIndex = sortSolutions(clusterSolutions(solsList#endIndex | newSols));
                << "number of paths tracked: " << nPathsTracked << endl;
              )
            );
          );
        );

        if mvComputationCompleted then (
          if #(solsList#0) == mv then break;
        );
    ) -- else print "something went wrong"
    ;
    solsList#0
    )

-- ?????????
-- edge potential function: counts how many new points are obtained at the target node
-- function definitely needs to be fixed!!!!
edgePotential = method()
edgePotential (HomotopyEdge, HomotopyNode) := (E, N) -> (
    A := N.PartialSols;
    trackEdge(E, to'a');
    B := N.PartialSols;
    New := scan(#B, i -> (
	    b := B#i;
	    a := A#i;
	    if member(b, A) then n := 0  
	    else (    
		n = n+1;
		);
	    ));
    << "There are" << n << "new points" 
    )


