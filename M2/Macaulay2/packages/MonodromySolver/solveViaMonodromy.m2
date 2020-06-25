export {
    "createSeedPair",
    "computeMixedVolume",
    "monodromySolve",
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

-*
Organization:
--0) global variables, overrides, & miscellaneous functions
--1) "seeding": finding a good initial point
--2) "solving", wrapper methods: sparseMonodromySolve, solveFamily, monodromySolve
--3) "solving", core methods: (core/static/dynamic)MonodromySolve
*- 

-*
TODO:
resolve mutableOptions, MutableOptions, dynamicOptions, etc
eliminate logic that distinguishes GateSystems and PolySystems
make sure "new tracking routine" => false actually works!
complete rewrite of solveFamily
eliminate staticMonodromySolve
move tests
change solveFamily documentation
*-

--0) global variables, overrides, & uncategorized service functions
 
-- Option table that gives defaults for exported functions
MonodromyOptions = {
	TargetSolutionCount => null,
	SelectEdgeAndDirection => selectFirstEdgeAndDirection,
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
	"new tracking routine" => true, -- uses "track" if false, "trackHomotopy" if true
	Verbose => false,
	EdgesSaturated => false,
	FilterCondition => (x -> false), -- experimental: if (o.FilterCondition x == true), don't record a correspondence
	Randomizer => (p -> p), -- experimental: want p and o.Randomizer p to have the same solutions
	Equivalencer => (x -> x) -- experimental: when are two solutions equal?
        }

-- overrides the behavior of random CC (pick uniformly in a unit disk)
old'random'Type = lookup(random,Type)
random Type := o -> R -> (
    if class R === ComplexField then (
	exp(2 * pi * random RR * ii)
	) 
    else (old'random'Type o) R
    ) 

-- compute MV for parameter-less system via PHCPACK (toRingXphc currently assumes coeffField is Complex)
computeMixedVolume = method()
computeMixedVolume List := polys -> mixedVolume(toRingXphc polys,StartSystem => false)
computeMixedVolume PolySystem := PS -> mixedVolume equations PS
computeMixedVolume GateSystem := GS -> (
    assert numParameters GS == 0;
    monR := monoid [Variables => numVariables GS];
    R := CC monR;
    computeMixedVolume evaluate(GS, vars R)
    )

-- in: options for an exported method (or dynamicMonodromySolve)
-- out: options that get used for staticMonodromySolve
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

-- assumes: stopping criterion depends solely on n = # repeats, HG = homotopy graph
-- modifies: mutableOptions (no return value)
setStoppingCriterion = (n , HG, mutableOptions) -> (
    (targ, lim) := (mutableOptions.TargetSolutionCount, mutableOptions.NumberOfRepeats);
    -- basicTest: how to stop without a TargetSolutionCount
    -- note: the state of mutableOptions doesn't change once we are doing monodromy
    basicTest := ( (n, HG) -> (
        -- too many iterations without progress?
        n >= mutableOptions.NumberOfRepeats or 
        -- are all edges "spent"?
        instance(HG.FirstDirectedEdge, Nothing)
        )
    );
    mutableOptions.StoppingCriterion = if instance(targ, Nothing) then basicTest else (n, HG) -> (
        basicTest(n, HG) or any(HG.Vertices, v -> numSols v >= mutableOptions.TargetSolutionCount)
        );
    )            


-- "naive" implementation of monodromy, independent of core methods
-- makes many assumptions, not maintained
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


--1) "seeding": finding a good initial point


-- find the "seed" for the parametric system
-- assumes: systems is affine linear in parameters
createSeedPair = method(Options=>{"initial parameters" => "random unit"})
createSeedPair System := o -> P -> (
    init := o#"initial parameters";
    n := numVariables P;
    x0 := if (init == "random unit") then point random(CC^1,CC^n)
      else if (init == "one") then point {for i from 1 to n list 1_CC}
      else error "unknown option";
    createSeedPair(P, x0)
    )
createSeedPair (System, Point) := o -> (P, x0) -> (
    G := if instance(P, GateSystem) then P else gateSystem P.PolyMap;
    n := numVariables G;
    m := numParameters G;
    N := numFunctions G;
    -*
    for fixed x0: p --> P(p,x0)=Ap+b for A in CC^(Nxn), b in CC^N
    *-
    I := id_(CC^m);
    A := random(CC^N,CC^0);
    b := transpose evaluate(G, point matrix 0_(CC^m), x0);
    scan(m, i -> A = A | transpose evaluate(G, point I_{i}, x0) - b);
--    xp := A * (transpose matrix x0 - offset);
    xp := solve(A,-b);
    K := numericalKernel(A, 1e-5);
    xh := K * random(CC^(numcols K), CC^1);
    p0 := point(xh + xp);
    if not areEqual(0, norm evaluate(G, p0, x0)) then error "linear seeding failed";
    (p0, x0)
    )

--2) "solving", wrapper methods: sparseMonodromySolve, solveFamily, monodromySolve

sparseMonodromySolve = method(Options=>MonodromyOptions)
sparseMonodromySolve PolySystem := o ->  PS -> (
    polys := flatten entries PS.PolyMap;
    ind := flatten apply(#polys,i-> -- indices for parameters
	apply(exponents polys#i, t->(i,t))
	);
    R := PS.PolyMap.ring;
    if numgens coefficientRing R > 0 then error "expected parameter-less system";
    W := symbol W;
    AR := CC[apply(ind,i->W_i)][gens R];
    polysP := transpose matrix{for i to #polys-1 list -- system with parameteric coefficients and same support 
        sum(exponents polys#i, t->W_(i,t)*AR_(t))};
    P := if o#"new tracking routine" then gateSystem polysP else polySystem polysP;
    targetParam := point sub(fold(polys/coefficients/last,(a,b)->a||b),CC);
    solveFamily(targetParam, P, o)
)

--  IN: parametric Polysystem 
-- OUT: a sequence (parameters of a random system, pointarray of solutions)
solveFamily = method(Options=>MonodromyOptions)
solveFamily System := o -> P -> (
    (V0, npaths) := monodromySolve(P, o);
    G := V0.Graph;
    -- get a "good node"
    V1 := if instance(o.TargetSolutionCount, Nothing) then V0 else (
        targV := select(1, G.Vertices, v -> numSols v == o.TargetSolutionCount);
        existsGoodNode := (#targV > 0);
        if (o.Verbose and not existsGoodNode) then << "WARNING: no node attained TargetSolutionCount" << endl;
        if existsGoodNode then first targV else V0
        );
    (V1.BasePoint, V1.PartialSols)
    )
solveFamily (Point, System) := o -> (p1, P) -> (
    (p0, sols0) := solveFamily(P, o);
    GS := if instance(P, GateSystem) then P else (
        R := ring P;
        Pflat := sub(P, first flattenRing R);
        gateSystem(Pflat, drop(gens ring Pflat, numgens R))
        );
    specMat := matrix p0 | matrix p1;
    H01 := specialize(parametricSegmentHomotopy GS, transpose specMat); -- this is annoying to have to do
    (p1, pointArray trackHomotopy(H01, points sols0))
    )

monodromySolve = method(Options=>MonodromyOptions)
monodromySolve System := o -> PS -> (
    (p0, x0) := createSeedPair PS;
    monodromySolve(PS, p0, {x0}, o)
    )
monodromySolve (System, Point, List) := o -> (PS,point0,s0) -> (
	if o.AugmentGraphFunction =!= null then
		result := dynamicMonodromySolve(PS, point0, s0, o)
	else
		result = staticMonodromySolve(PS, point0, s0, trimDynamicOptions o);
	result
);
    
--3) "solving", core methods: (static/dynamic/core)MonodromySolve

dynamicMonodromySolve = method(Options=>MonodromyOptions)
dynamicMonodromySolve (MutableHashTable, Point, List) := o -> (PS, point0, s0) -> (
    augmentNumberOfRepeats := o.AugmentNumberOfRepeats;
    if not instance(augmentNumberOfRepeats, ZZ) then error "dynamic solver triggered: need to properly set AugmentNumberOfRepeats";
    mutableOptions := new MutableHashTable from o;  
    staticOptions := trimDynamicOptions o;      
    (node1, npaths) := staticMonodromySolve(PS, point0, s0, staticOptions);
    HG := node1.Graph;
    if instance(o.StoppingCriterion, Nothing) then setStoppingCriterion(o.NumberOfRepeats, HG, mutableOptions);        
    success := instance(npaths, ZZ);
    edgeCount := o.NumberOfEdges;
    iterations := 0;
    while not success and augmentNumberOfRepeats > iterations do (
        o.AugmentGraphFunction(HG, point0, HG.Vertices#0, edgeCount, o.AugmentEdgeCount, o.AugmentNodeCount);
        edgeCount = edgeCount + o.AugmentEdgeCount;
        (node1,npaths) = coreMonodromySolve(HG, node1, staticOptions);
        success = instance(npaths, ZZ);
        iterations = iterations + 1;
	);
    (node1,npaths)
)


staticMonodromySolve = method(Options=>MonodromyOptions)
staticMonodromySolve (System, Point, List) := o -> (PS,point0,s0) -> (
    -- general error handling for MonodromyOptions can be done here
    isGS := instance(PS, GateSystem);
    if isGS and not o#"new tracking routine" then error "GateSystem requires new tracking routine";
    noPotential := instance(o.Potential, Nothing);
    if noPotential and toString o.SelectEdgeAndDirection == "selectBestEdgeAndDirection" then error "selectBestEdgeAndDirection requires a potential";
    if  not noPotential and toString o.Potential == "potentialE" and instance(o.TargetSolutionCount, Nothing) then error "potentialE requires target solution count";    
    -- !! global assignment !!
    USEtrackHomotopy = isGS or (getDefault Software === M2engine and o#"new tracking routine");
    -- certain options must be passed to homotopyGraph
    graphConstructorOptionTable := new OptionTable from {
        Potential => o.Potential, 
        Randomizer => o.Randomizer, 
        FilterCondition => o.FilterCondition, 
        Equivalencer => o.Equivalencer
        };
    HG := homotopyGraph(PS, graphConstructorOptionTable);
    mutableOptions := new MutableHashTable from o;
    if instance(o.StoppingCriterion, Nothing) then setStoppingCriterion(o.NumberOfRepeats, HG, mutableOptions);
    if o.TargetSolutionCount =!= null then HG.TargetSolutionCount = o.TargetSolutionCount;
    PA := pointArray s0;
    node1 := addNode(HG, point0, PA);
    setTrackTime(HG, 0);    
    if #s0 < 1 then error "at least one solution expected";    
    o.GraphInitFunction(HG, point0, node1, o.NumberOfNodes, o.NumberOfEdges);
    --Needs to return HG for use by dynamicMonodromySolve
    coreMonodromySolve(HG, node1, new OptionTable from (new HashTable from mutableOptions))
)

-- core function called by static and dynamic variants
-- assumes: stopping criterion is set
coreMonodromySolve = method(Options=>MonodromyOptions)
coreMonodromySolve HomotopyGraph := o -> HG -> coreMonodromySolve(HG, first HG#"Vertices", o)
coreMonodromySolve (HomotopyGraph, HomotopyNode) := o -> (HG, node1) -> (
    selectEdgeAndDirection := o.SelectEdgeAndDirection;
    same := 0;
    npaths := 0;    
    lastNode := node1;
    e1 := selectFirstEdgeAndDirection HG;
    while not o.StoppingCriterion(same, HG) do (
        (e, from1to2) := selectEdgeAndDirection HG;
        if o.Verbose then (
            if e.?Potential12 then << " (potential12 = " << e.Potential12 << ")";
            if e.?Potential21 then << " (potential21 = " << e.Potential21 << ")";
            << "-------------------------------------------------" << endl;
            );
        lastNode = if from1to2 then e.Node2 else e.Node1;
        nKnownPoints := length lastNode.PartialSols;
        trackedPaths := trackEdge(e, from1to2, o.BatchSize);
        npaths = npaths + trackedPaths;
        (startNode, targetNode) := if from1to2 then (e.Node1, e.Node2) else (e.Node2, e.Node1);
        if o.Verbose then (
            << " start node " << startNode.Position << " : " << length startNode.PartialSols << " known sols " << endl;
            << " targ. node " << targetNode.Position << " : " << length targetNode.PartialSols << " known sols " << endl;
            << " trackedPaths : " << trackedPaths << endl; 
            );
        if length lastNode.PartialSols == nKnownPoints 
        then same = same + 1 else same = 0;
        e1 = selectFirstEdgeAndDirection HG;
	);
    if o.TargetSolutionCount =!= null and o.TargetSolutionCount != length lastNode.PartialSols 
    then npaths = "failed";
    if o.EdgesSaturated then saturateEdges HG; -- !!
    (lastNode, npaths)
)



end
