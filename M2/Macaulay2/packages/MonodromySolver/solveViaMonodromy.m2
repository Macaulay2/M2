export {
    "createSeedPair",
    "computeMixedVolume",
    "monodromySolve",
    "completeGraphInit",
    "completeGraphAugment",
    "flowerGraphInit",
    "flowerGraphAugment",
    "dynamicFlowerSolve",
    "RandomPointFunction",
    "NumberOfEdges",
    "NumberOfNodes",
    "NumberOfRepeats",
    "StoppingCriterion",
    "GraphInitFunction",
    "SelectEdgeAndDirection",
    "BatchSize",
    "AugmentGraphFunction",
    "AugmentNumberOfRepeats",
    "AugmentEdgeCount",
    "AugmentNodeCount",
    "EdgesSaturated",
    "sparseMonodromySolve",
    "solveFamily"}

-- changes the behavior of random CC (pick uniformly in a unit disk)
old'random'Type = lookup(random,Type)
random Type := o -> R -> (
    if class R === ComplexField then (
	exp(2 * pi * random RR * ii)
	) 
    else (old'random'Type o) R
    ) 

completeGraphInit = (G, p, node1, nnodes, nedges) -> (
    nextP := ((p0)->point {apply(#coordinates p0, i->exp(2*pi*ii*random RR))});
    for i from 1 to nnodes-1 do (
        addNode(G,nextP(p), pointArray {});
    );
    for i from 0 to nnodes-1 do (
        for j from i+1 to nnodes-1 do (
            apply(nedges, k -> addEdge(G, G.Vertices#i, G.Vertices#j,
		    "random gamma" => (nedges>1)));
        );
    );
    )

completeGraphAugment = (G, p, node1, nStartingEdges, nNewEdges, nNewNodes) -> (
	nextP := ((p0)->point {apply(#coordinates p0, i->exp(2*pi*ii*random RR))});
	for i from 1 to nNewNodes do (
		newNode := addNode(G,nextP(p), pointArray {});
		for j from 0 to #(G.Vertices) - 2 do (
			apply(nStartingEdges, k -> addEdge(G, newNode, G.Vertices#j));
		);
	);
	
	nNodes := #(G.Vertices);
	for i from 0 to nNodes-1 do (
		for j from i+1 to nNodes-1 do (
			apply(nNewEdges, k -> addEdge(G, G.Vertices#i, G.Vertices#j));
		);
	);
);

-- static flower
flowerGraphInit = (G, p, node1, nnodes, nedges) -> (
	nextP := ((p0)->point {apply(#coordinates p0, i->exp(2*pi*ii*random RR))});
	for i from 1 to nnodes do (
		newNode := addNode(G,nextP(p), pointArray {});
		apply(nedges, k -> addEdge(G, node1, newNode));
	);
)

-- static flower
flowerGraphAugment = (G, p, node1, nStartingEdges, nNewEdges, nNewNodes) -> (
	nextP := ((p0)->point {apply(#coordinates p0, i->exp(2*pi*ii*random RR))});
	for i from 1 to nNewNodes do (
		newNode := addNode(G,nextP(p), pointArray {});
		apply(nStartingEdges, k -> addEdge(G, node1, newNode));
	);
	for i from 1 to #(G.Vertices) - 1 do (
		apply(nNewEdges, k -> addEdge(G, G.Vertices#i, node1));
	);
)

ms'option'list = {
	TargetSolutionCount => null,
	SelectEdgeAndDirection => selectRandomEdgeAndDirection,
	StoppingCriterion => null,
	GraphInitFunction => completeGraphInit,
	AugmentGraphFunction => null,
	AugmentNumberOfRepeats => null,
	AugmentEdgeCount=>0,
	AugmentNodeCount=>0,
	BatchSize => infinity,
	Potential => null,
	NumberOfNodes => 2,
	NumberOfEdges => 4,
	NumberOfRepeats => 10,
	"new tracking routine" => true, -- uses old "track" if false
	Verbose => false,
	EdgesSaturated => false,
	FilterCondition => null,
	Randomizer => null,
	Equivalencer => null}


dynamicFlowerSolve = method(Options=>{TargetSolutionCount=>null,RandomPointFunction=>null,StoppingCriterion=>((n,L)->n>3)})
dynamicFlowerSolve (Matrix, Point, List) := o -> (PF,point0,s0) -> (
	if #s0 < 1 then error "at least one solution expected";  
	p0 := matrix point0; -- points are row matrices
	stoppingCriterion := o.StoppingCriterion;
	if o.TargetSolutionCount =!= null then (
		stoppingCriterion = (n,L) -> (length L >= o.TargetSolutionCount);
	);
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
	while not stoppingCriterion(same,sols0) do (
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
	); -- else print "something went wrong"

	(sols0,nPathsTracked)
)

computeMixedVolume = method()
computeMixedVolume List := polys -> mixedVolume(toRingXphc polys,StartSystem => false)

-- find the "seed" for the parametric system
createSeedPair = method(Options=>{"initial parameters" => "random unit"})
createSeedPair System := o -> P -> (
    init := o#"initial parameters";
    n := numVariables P;
    x0 := if (init == "random unit") then point random(CC^1,CC^n)
      else if (init == "one") then point {for i from 1 to n list 1_CC}
      else error "unknown option";
    createSeedPair(P, x0)
    )
-*
createSeedPair(PolySystem, List) := o -> (G, L) -> (
    SubList := apply(toList(0..numgens ring G-1), i -> (gens ring G)#i => L#i);
    subZeros := apply(gens coefficientRing ring G, g -> g => 0);
    C := coefficientRing ring G;
    M := sub(sub(G.PolyMap, SubList), C);
    M = flatten entries generators ideal M; 
    b := sub(matrix {apply(M, m-> sub(m, subZeros))}, coefficientRing coefficientRing ring G);
    l := apply(M, g -> (coefficients(g, Monomials => gens C))#1);
    A := l#0;
    for i from 1 to length l - 1 do A = A | l#i;
    A = sub(A, coefficientRing coefficientRing ring G);
    K := numericalKernel(transpose A, 1e-6);
    offset := solve(transpose A,transpose b,ClosestFit=>true);
    -- K's columns are a basis for the kernel i indexes the 'most likely true positive'
    --v := K * transpose matrix {toList ((numcols K):1_CC)};  
    w := transpose randomWeights(numcols K);
    v := K * w - offset;
    c0 := point matrix v;
    -- N := numericalIrreducibleDecomposition ideal M; -- REPLACE this with linear algebra (using numericalKernel)
    --c0 := first (first components N).Points; 
    pre0 := point{apply(SubList, i -> i#1)};
    G0 := specializeSystem(c0,G);
    pre0' := first refine(G0,{pre0});
    (c0,pre0')
    )
*-
createSeedPair (System, Point) := o -> (P, x0) -> (
    G := if instance(P, GateSystem) then P else gateSystem P.PolyMap;
    n := numVariables G;
    m := numParameters G;
    N := numFunctions G;
    I := id_(CC^m);
    A := random(CC^0,CC^N);
    scan(m, i -> A = A || evaluate(G, point I_{i}, x0));
    b := evaluate(G, point matrix 0_(CC^m), x0);
    K := numericalKernel(transpose A, 1e-5) ;
    offset := solve(transpose A,transpose b,ClosestFit=>true);
    p0 := point(K* random(CC^(numcols K), CC^1) - offset);
    (p0, x0)
    )


TEST ///
--- seeding bug discovered by Courtney Gibbons
setRandomSeed 2 
T = CC[a_1..a_6][x_1,x_2,lambda]
f_1 = a_1*x_1+a_2*x_2 - x_1*lambda 
f_2 = a_3*x_1+a_4*x_2 - x_2*lambda
f_3 = a_5*x_1+a_6*x_2 + 1
H = {f_1,f_2,f_3}
assert(status (last createSeedPair polySystem H) =!= RefinementFailure)
///

staticMonodromySolve = method(Options=>{
	TargetSolutionCount => null,
	StoppingCriterion => null,
	SelectEdgeAndDirection => null,
	GraphInitFunction => null,
	BatchSize => null,
	Potential => null,
	NumberOfNodes => null,
	NumberOfEdges => null,
	NumberOfRepeats => null,
	"new tracking routine" => true, -- uses old "track" if false
	Verbose => false,
	EdgesSaturated => false,
	FilterCondition => null,
	Randomizer => null,
	Equivalencer => null})
staticMonodromySolve (System, Point, List) := o -> (PS,point0,s0) -> (
    	isGS := instance(PS,GateSystem);
        if (isGS and not o#"new tracking routine") then error "GateSystem requires new tracking routine";
	USEtrackHomotopy = isGS or (getDefault Software === M2engine and o#"new tracking routine");
	mutableOptions := new MutableHashTable from o;
	if mutableOptions.TargetSolutionCount =!= null then
		mutableOptions.StoppingCriterion = (n,L) -> (length L >= mutableOptions.TargetSolutionCount or n >= mutableOptions.NumberOfRepeats);

	if mutableOptions.StoppingCriterion === null then 
		mutableOptions.StoppingCriterion = (n,L) -> n >= mutableOptions.NumberOfRepeats;
	-- 
	equivalencer := if (o.Equivalencer =!= null) then o.Equivalencer else x -> x;
	randomizer := if (o.Randomizer =!= null) then o.Randomizer else x -> x;
	filterCondition := if (o.FilterCondition =!= null) then o.FilterCondition else x -> false;
	assert all({equivalencer,randomizer,filterCondition},f->instance(f,Function));
	HG := homotopyGraph(PS, Potential=>o.Potential, Randomizer => randomizer, FilterCondition => filterCondition, Equivalencer => equivalencer);
	if o.TargetSolutionCount =!= null then (
		HG.TargetSolutionCount = o.TargetSolutionCount;
	);
	PA := pointArray s0;
	node1 := addNode(HG, point0, PA);
	setTrackTime(HG,0);

	if #s0 < 1 then error "at least one solution expected";

	o.GraphInitFunction(HG, point0, node1, o.NumberOfNodes, o.NumberOfEdges);
	--Needs to return HG for use by dynamicMonodromySolve
	coreMonodromySolve(HG,node1, new OptionTable from (new HashTable from mutableOptions))
)

TEST ///
setRandomSeed 0
R = CC[a,b,c,d,e,f,g,h][A,B,C];
polys = polySystem {
	a*A+b*B+c*C,
	d*A*B+e*B*C+f*C*A,
	g*A*B*C-h*1};
(p0,x0) = createSeedPair polys;
count = 6;

--The first set of tests may not find all solutions, as there is no
--target root count.

(V,npaths) = monodromySolve polys;
assert( length V.PartialSols == count );

(V,npaths) = monodromySolve(polys,p0,{x0},NumberOfNodes=>3);
assert( length V.PartialSols == count );

setRandomSeed 0
--NumberOfNodes, NumberOfEdges, NumberOfRepeats
(V,npaths) = monodromySolve(polys,p0,{x0},
	NumberOfNodes=>2,
	NumberOfEdges=>5,
	NumberOfRepeats=>11);
assert( length V.PartialSols == count );

--Two options for SelectEdgeAndDirection. If SelectBestEdgeAndDirection, then
--must also provide a Potential function.
(V,npaths) = monodromySolve(polys,p0,{x0},
    	    	NumberOfNodes=>3,
		NumberOfEdges=>5,
		SelectEdgeAndDirection=>selectRandomEdgeAndDirection);
assert( length V.PartialSols == count );

(V,npaths) = monodromySolve(polys,p0,{x0},
	NumberOfEdges=>5,	
	SelectEdgeAndDirection=>selectBestEdgeAndDirection,
	Potential=>potentialLowerBound);
assert( length V.PartialSols == count );

--Two different GraphInitFunctions. Also, BatchSize can be set,
--which will change the number of paths tracked simultaneously.
(V,npaths) = monodromySolve(polys,p0,{x0},
	GraphInitFunction=>flowerGraphInit,
	NumberOfEdges=>5);
assert( length V.PartialSols == count );

setRandomSeed 0
(V,npaths) = monodromySolve(polys,p0,{x0},
	NumberOfEdges=>5,
	GraphInitFunction=>completeGraphInit,
	BatchSize=>1);
assert( length V.PartialSols == count );

--NumberOfEdges=>1 (no randomization)
(V,npaths) = monodromySolve(polys,p0,{x0},
	NumberOfNodes=>10,
	NumberOfEdges=>1);
assert( length V.PartialSols == count );

--The next two tests test booleans: "new tracking routine" (defaults to true)
--and Verbose (defaults to false). We test that both the defaults work
--and that non-default values work.
(V,npaths) = monodromySolve(polys,p0,{x0},
		NumberOfEdges=>4,
		NumberOfNodes=>3,
		"new tracking routine"=>false,
		Verbose=>false);
assert( length V.PartialSols == count );

--The next three tests use strict equality, as they ought to always succeed.
--Can provide TargetSolutionCount
(V,npaths) = monodromySolve(polys,p0,{x0},
	SelectEdgeAndDirection=>selectBestEdgeAndDirection,
	Potential=>potentialE,
	NumberOfNodes=>3,
	NumberOfEdges=>3,
	TargetSolutionCount=>count);
assert( length V.PartialSols == count );

--Set dynamic options. Need to provide an AugmentGraphFunction and
--the AugmentEdgeCount and/or AugmentNodeCount should be greater than 0 if
--any augmenting is going to happen. AugmentNumberOfRepeats can be used to
--keep it from running indefinitely.
(V,npaths) = monodromySolve(polys,p0,{x0},
	SelectEdgeAndDirection=>selectBestEdgeAndDirection,
	Potential=>potentialE,
	GraphInitFunction=>completeGraphInit,
	AugmentGraphFunction=>completeGraphAugment,
	AugmentNodeCount=>1,
	AugmentNumberOfRepeats=>10);
assert( length V.PartialSols == count );
(V,npaths) = monodromySolve(polys,p0,{x0},
	SelectEdgeAndDirection=>selectBestEdgeAndDirection,
	Potential=>potentialE,
	GraphInitFunction=>flowerGraphInit,
	AugmentGraphFunction=>flowerGraphAugment,
	AugmentEdgeCount=>1,
	AugmentNumberOfRepeats=>10);
assert( length V.PartialSols == count );

-- test for sparseSolver which sometimes fails: many repeats is there to reduce failure probability, but might slow tests down
S = CC[x,y]
P = polySystem {(x-ii)^2+y^2-1, x+1-y^2}
sols = sparseMonodromySolve(P, NumberOfEdges=>10, NumberOfRepeats=>20)
assert (#sols == 4) 
assert all(sols,s->norm evaluate(P,s) < 0.0001)
///

monodromySolve = method(Options=>ms'option'list)
monodromySolve System := o -> PS -> (
    (p0,x0) := createSeedPair PS;
    monodromySolve(PS,p0,{x0},o)
    )
monodromySolve (System, Point, List) := o -> (PS,point0,s0) -> (
	if o.AugmentGraphFunction =!= null then
		result := dynamicMonodromySolve(PS,point0,s0, o)
	else
		result = staticMonodromySolve(PS, point0, s0, trimDynamicOptions(o));
	result
);
    

trimDynamicOptions = method()
trimDynamicOptions OptionTable := opt -> trimDynamicOptions(new MutableHashTable from opt)
trimDynamicOptions MutableHashTable := MutableOptions -> (
	--if we ever add more dynamic-only options, add them into dynamicOptions.
	dynamicOptions := (AugmentGraphFunction, 
		AugmentNumberOfRepeats,
		AugmentEdgeCount, 
		AugmentNodeCount);
	for opt in dynamicOptions do (remove(MutableOptions,opt));
	
	new OptionTable from (new HashTable from MutableOptions)
)

dynamicMonodromySolve = method(Options=>{
	TargetSolutionCount => null,
	SelectEdgeAndDirection => null,
	StoppingCriterion => null,
	GraphInitFunction => null,
	AugmentGraphFunction => null,
	AugmentNumberOfRepeats => null,
	AugmentEdgeCount=>0,
	AugmentNodeCount=>0,
	BatchSize => null,
	Potential => null,
	NumberOfNodes => null,
	NumberOfEdges => null,
	NumberOfRepeats => null,
	"new tracking routine" => true, -- uses old "track" if false
	Verbose => false,
	EdgesSaturated => false,
	FilterCondition => null,
	Randomizer => null,
	Equivalencer => null})
dynamicMonodromySolve (MutableHashTable, Point, List) := o -> (PS,point0,s0) -> (
	mutableOptions := new MutableHashTable from o;
	if mutableOptions.TargetSolutionCount === null then 
		mutableOptions.TargetSolutionCount = computeMixedVolume specializeSystem (point0,PS);
	mutableOptions.StoppingCriterion = (n,L) -> (length L >= mutableOptions.TargetSolutionCount or n >= mutableOptions.NumberOfRepeats);
	staticOptions := trimDynamicOptions(mutableOptions);
	(node1,npaths) := staticMonodromySolve(PS,point0,s0,staticOptions);
	HG := node1.Graph;
	success := class(npaths) === ZZ;
	edgeCount := o.NumberOfEdges;
	iterations := 0;
	augmentNumberOfRepeats := 1000000; -- Arbirtary large number
	if (o.AugmentNumberOfRepeats =!= null) then
		augmentNumberOfRepeats = o.AugmentNumberOfRepeats;

	while not (success) and (augmentNumberOfRepeats > iterations) do (
		o.AugmentGraphFunction(
			HG, point0, HG.Vertices#0, edgeCount, o.AugmentEdgeCount, o.AugmentNodeCount);
		edgeCount = edgeCount + o.AugmentEdgeCount;
		(node1,npaths) = coreMonodromySolve(HG, node1, staticOptions);
		success = class(npaths) === ZZ;
		iterations = iterations + 1;
	);
	(node1,npaths)
)

-- main function
coreMonodromySolve = method(Options=>{
	TargetSolutionCount => null,
	StoppingCriterion => null,
	SelectEdgeAndDirection => null,
	GraphInitFunction => null,
	BatchSize => null,
	Potential => null,
	NumberOfNodes => null,
	NumberOfEdges => null,
	NumberOfRepeats => 10,
	"new tracking routine" => true, -- uses old "track" if false
	Verbose => false,
	EdgesSaturated => false,
	FilterCondition => null,
	Randomizer => null,
	Equivalencer => null})
coreMonodromySolve HomotopyGraph := o -> HG -> coreMonodromySolve(HG, first HG#"Vertices", o)
coreMonodromySolve (HomotopyGraph, HomotopyNode) := o -> (HG,node1) -> (
	selectEdgeAndDirection := o.SelectEdgeAndDirection;
	same := 0;
	npaths := 0;    
	lastNode := node1;
	while not o.StoppingCriterion(same,lastNode.PartialSols) do (
		(e, from1to2) := selectEdgeAndDirection(HG);
		if o.Verbose then (
			-- << "Correspondences are " << keys e.Correspondence12;
			if e.?Potential12 then << " (potential12 = " << e.Potential12 << ")";
			-- << " and " << keys e.Correspondence21;
			if e.?Potential21 then << " (potential21 = " << e.Potential21 << ")";
			<< endl << "Direction is " << from1to2 << endl;
			<< "-------------------------------------------------" << endl;
		);
		lastNode = if from1to2 then e.Node2 else e.Node1;
		nKnownPoints := length lastNode.PartialSols;
		trackedPaths := trackEdge(e, from1to2, o.BatchSize);
		npaths = npaths + trackedPaths;
		if o.Verbose then (
			<< "  node1: " << length e.Node1.PartialSols << endl;
			<< "  node2: " << length e.Node2.PartialSols << endl;    	
			<< "trackedPaths " << trackedPaths << endl; 
		);
		if length lastNode.PartialSols == nKnownPoints 
		then same = same + 1 else same = 0;
	);
	if o.TargetSolutionCount =!= null and o.TargetSolutionCount != length lastNode.PartialSols 
	then npaths = "failed";
	if o.EdgesSaturated then saturateEdges HG;
	(lastNode, npaths)
)

-- 
sparseMonodromySolve = method(Options=>{
	TargetSolutionCount => null,
	SelectEdgeAndDirection => selectRandomEdgeAndDirection,
	StoppingCriterion => null,
	GraphInitFunction => completeGraphInit,
	AugmentGraphFunction => null,
	AugmentNumberOfRepeats => null,
	AugmentEdgeCount=>0,
	AugmentNodeCount=>0,
	BatchSize => infinity,
	Potential => null,
	NumberOfNodes => 2,
	NumberOfEdges => 4,
	NumberOfRepeats => 10,
	"new tracking routine" => true, -- uses old "track" if false
	Verbose => false,
	EdgesSaturated => false})
sparseMonodromySolve PolySystem := o ->  PS -> (
    polys := flatten entries PS.PolyMap;
    ind := flatten apply(#polys,i-> -- indices for parameters
	apply(exponents polys#i, t->(i,t))
	);
    R := PS.PolyMap.ring;
    W := symbol W;
    AR := CC[apply(ind,i->W_i)][gens R];
    polysP := transpose matrix{for i to #polys-1 list -- system with parameteric coefficients and same support 
      sum(exponents polys#i, t->W_(i,t)*AR_(t))};
  P := if o#"new tracking routine" then gateSystem polysP else polySystem polysP;
  targetParam := point sub(fold(polys/coefficients/last,(a,b)->a||b),CC);
  solveFamily(P, targetParam, o)
)

-- IN: parametric Polysystem OUT: a sequence (random system, its solutions)
solveFamily = method(Options=>{
	TargetSolutionCount => null,
	SelectEdgeAndDirection => selectRandomEdgeAndDirection,
	StoppingCriterion => null,
	GraphInitFunction => completeGraphInit,
	AugmentGraphFunction => null,
	AugmentNumberOfRepeats => 2,
	AugmentEdgeCount=>3,
	AugmentNodeCount=>1,
	BatchSize => infinity,
	Potential => null,
	NumberOfNodes => 3,
	NumberOfEdges => 4,
	NumberOfRepeats => 10,
	"new tracking routine" => true, -- uses old "track" if false
	Verbose => false,
	EdgesSaturated => false})
solveFamily System := o -> P -> (
    (p0, x0) := createSeedPair P;
    (p0, solveFamily(P, p0, o))
    )
solveFamily (System, Point) := o -> (P, p1) -> (
    mutableOptions := new MutableHashTable from o;
    if mutableOptions.TargetSolutionCount =!= null then
      mutableOptions.StoppingCriterion = (n,L) -> (length L >= mutableOptions.TargetSolutionCount or n >= mutableOptions.NumberOfRepeats);
    if mutableOptions.StoppingCriterion === null then 
      mutableOptions.StoppingCriterion = (n,L) -> n >= mutableOptions.NumberOfRepeats;
    (p0, x0) := createSeedPair P;
    (V, npaths) := monodromySolve(P,p0,{x0}, new OptionTable from (new HashTable from mutableOptions));
    G := V.Graph;
    start := transpose matrix V.BasePoint;
    targ := transpose matrix p1;
    H := if G.SLP then G.Family else parametricSegmentHomotopy gateSystem G.Family.PolyMap;
    if o#"new tracking routine" then (
	H01 := specialize(H, start||targ);
    	trackHomotopy(H01,points V.PartialSols)
	) else (
	P0 := specializeSystem(start, H);
	P1 := specializeSystem(targ, H);
	track(P0, P1, points V.PartialSols)
	)
    )

end