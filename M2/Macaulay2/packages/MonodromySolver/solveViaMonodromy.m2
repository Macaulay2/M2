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
    "randomWeights",
    "EdgesSaturated",
    "sparseMonodromySolve",
    "solveFamily"}

-- change the behavior of random CC (pick uniformly in a unit disk)
old'random'Type = lookup(random,Type)
random Type := o -> R -> (
    if class R === ComplexField then (
	exp(2 * pi * random RR * ii)
	) 
    else (old'random'Type o) R
    ) 

-- random complex vector
randomWeights = method()
randomWeights ZZ := n -> matrix(CC, {apply(n,i->random CC)})

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
createSeedPair PolySystem := o -> G -> (
	R := ring G;   
	C := coefficientRing coefficientRing R; 
	init := o#"initial parameters";
	createSeedPair(G,
		if init == "random unit" then apply(numgens R, i->(x:=random C; x/abs x))
		else if init == "random" then apply(numgens R, i->random C)
		else if init == "one" then toList(numgens R:1_C)
		else error "unknown option"
	)
)
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
    v := K * w + offset;
    c0 := point matrix v;
    -- N := numericalIrreducibleDecomposition ideal M; -- REPLACE this with linear algebra (using numericalKernel)
    --c0 := first (first components N).Points; 
    pre0 := point{apply(SubList, i -> i#1)};
    G0 := specializeSystem(c0,G);
    pre0' := first refine(G0,{pre0});
    (c0,pre0')
    )

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
	EdgesSaturated => false})
staticMonodromySolve (PolySystem, Point, List) := o -> (PS,point0,s0) -> (
	USEtrackHomotopy = (getDefault Software === M2engine and o#"new tracking routine");
	mutableOptions := new MutableHashTable from o;
	if mutableOptions.TargetSolutionCount =!= null then
		mutableOptions.StoppingCriterion = (n,L) -> (length L >= mutableOptions.TargetSolutionCount or n >= mutableOptions.NumberOfRepeats);

	if mutableOptions.StoppingCriterion === null then 
		mutableOptions.StoppingCriterion = (n,L) -> n >= mutableOptions.NumberOfRepeats;
		
	HG := homotopyGraph(PS, Potential=>o.Potential);
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

-- Can provide no options
(V,npaths) = monodromySolve(polys,p0,{x0});
assert( length V.PartialSols == count );

setRandomSeed 0
--NumberOfNodes, NumberOfEdges, NumberOfRepeats
(V,npaths) = monodromySolve(polys,p0,{x0},
	NumberOfNodes=>2,
	NumberOfEdges=>4,
	NumberOfRepeats=>11);
assert( length V.PartialSols == count );

--Two options for SelectEdgeAndDirection. If SelectBestEdgeAndDirection, then
--must also provide a Potential function.
(V,npaths) = monodromySolve(polys,p0,{x0},
	SelectEdgeAndDirection=>selectRandomEdgeAndDirection);
	assert( length V.PartialSols == count );

(V,npaths) = monodromySolve(polys,p0,{x0},
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

-- test for sparseSolver which sometimes fails: 3 iterations is to reduce failure probability, but might slow tests down
S = QQ[x,y]
P = polySystem {x+y, 2*x+1-2*y^2}
sols = sparseMonodromySolve P
assert (#sols == 2) 
assert all(sols,s->norm evaluate(P,s) < 0.0001)
///

monodromySolve = method(Options=>{
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
monodromySolve PolySystem := o -> PS -> (
    (p0,x0) := createSeedPair PS;
    monodromySolve(PS,p0,{x0},o)
    )
monodromySolve (PolySystem, Point, List) := o -> (PS,point0,s0) -> (
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
	EdgesSaturated => false})
dynamicMonodromySolve (PolySystem, Point, List) := o -> (PS,point0,s0) -> (
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
	EdgesSaturated => false})
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
{*    mutableOptions := new MutableHashTable from o;
    if mutableOptions.TargetSolutionCount =!= null then
        mutableOptions.StoppingCriterion = (n,L) -> (length L >= mutableOptions.TargetSolutionCount or n >= mutableOptions.NumberOfRepeats);
    if mutableOptions.StoppingCriterion === null then 
        mutableOptions.StoppingCriterion = (n,L) -> n >= mutableOptions.NumberOfRepeats;*}
    polys := flatten entries PS.PolyMap;
    ind := flatten apply(#polys,i-> -- indices for parameters
	apply(exponents polys#i, t->(i,t))
	);
    R := PS.PolyMap.ring;
    W := symbol W;
    AR := CC[apply(ind,i->W_i)][gens R];
    polysP := for i to #polys-1 list -- system with parameteric coefficients and same support 
    sum(exponents polys#i, t->W_(i,t)*AR_(t));
    genericPS := polySystem transpose matrix {polysP};
    (sys,sols):=solveFamily(genericPS,o);--new OptionTable from (new HashTable from mutableOptions));
    track(polySystem sys,PS,sols)
)

-- IN: parametric Polysystem OUT: a sequence (random system, its solutions)
solveFamily = method(Options=>{
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
solveFamily PolySystem := o -> PS -> (
    (point0,s0) := createSeedPair PS;
    solveFamily(PS, point0, {s0},o)
    )
solveFamily (PolySystem, Point, List) := o -> (PS,point0,s0) -> (
    mutableOptions := new MutableHashTable from o;
    if mutableOptions.TargetSolutionCount =!= null then
        mutableOptions.StoppingCriterion = (n,L) -> (length L >= mutableOptions.TargetSolutionCount or n >= mutableOptions.NumberOfRepeats);
    if mutableOptions.StoppingCriterion === null then 
        mutableOptions.StoppingCriterion = (n,L) -> n >= mutableOptions.NumberOfRepeats;
    N := first monodromySolve(PS,point0,s0,new OptionTable from (new HashTable from mutableOptions));
    (N.SpecializedSystem, points N.PartialSols)
    )


end

------------------------------------------
------------------------------------------
-- Documentation
------------------------------------------
------------------------------------------

beginDocumentation()

doc ///
  Key
      monodromySolve
      (monodromySolve, Matrix, Point, List)
      (monodromySolve, PolySystem, Point, List)
  Headline
      Finds solutions for a given polynomial system (generic relative to a given family) given some seed solution.
  Usage
      (N,m) = monodromySolve(SP, c0, {pre0})
      (N,m) = monodromySolve(polys, c0, {pre0})
  Inputs
      SP:Matrix
      polys:PolySystem
      c0:Point
      p0:Point
  Outputs
      N:HomotopyNode
      m:ZZ
  Description
      Text      
          There are a lot of options. Where should we describe these?
///
