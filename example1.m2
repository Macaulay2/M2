needsPackage "MonodromySolver"
n=5; count=70;
needs (currentFileDirectory|"cyclic.m2")
plugin'c0 = map(R,AR,vars R | matrix c0) -- the actual polynomial system we solve
apply(polysP,p->plugin'c0 p) 
stop = (n,L)-> #L >= count
getDefault Software
{*
setDefault(Software=>PHCPACK)
*}

coin = prob -> if random RR <= prob then return 1 else return 0

end
-- make sure /code/MonodromySolve is in your path
restart
load "example1.m2"

setRandomSeed 0
-- bare bones
elapsedTime sols' = graphStrategy(SP,c0,{pre0})

-- default potential
elapsedTime sols' = graphStrategy(SP,c0,{pre0}, SelectEdgeAndDirection => selectBestEdgeAndDirection,
     NumberOfNodes=>4, NumberOfEdges=>1)

-- 
elapsedTime sols' = graphStrategy(SP,c0,{pre0}, SelectEdgeAndDirection => selectBestEdgeAndDirection,
     TargetSolutionCount=>count, 
     Potential=>potentialE, 
     GraphInitFunction=>completeGraphInit,
     NumberOfNodes=>4, NumberOfEdges=>1)

-- obsolete: twoNodes = completeGraphInit with NumberOfNodes => 2
nedges = 5
setRandomSeed 0
elapsedTime sols = twoNodes(SP,c0,{pre0},nedges,StoppingCriterion=>stop)


-----------------------------------------------------------------------------------
-- plotting
setRandomSeed 0
x = {}
for i from 0 to 100  do (
    setRandomSeed i;
    x = append(x,(graphStrategy(SP,c0,{pre0}, SelectEdgeAndDirection => selectBestEdgeAndDirection,
     TargetSolutionCount=>70, Potential=>potentialE, 
     GraphInitFunction=>(G,p,n1)->twoNodeGraphInit(G,p,n1,5)))#1))
"firsttwonodesexperiment" << concatenate between("\n", apply(x, n -> toExternalString n)) << close

-- to generate plot in R: hist(as.numeric(scan("firsttwonodesexperiment")))

elapsedTime sols' = twoNodes(SP,c0,{pre0},nedges, SelectEdgeAndDirection => selectBestEdgeAndDirection, TargetSolutionCount=>70)
setRandomSeed 0
elapsedTime sols' = twoNodes(SP,c0,{pre0},nedges, SelectEdgeAndDirection => selectBestEdgeAndDirection, TargetSolutionCount=>70, Potential=>potentialE)
setRandomSeed 0
elapsedTime sols' = graphStrategy(SP,c0,{pre0}, SelectEdgeAndDirection => selectBestEdgeAndDirection,
     TargetSolutionCount=>70, Potential=>potentialE, GraphInitFunction=>(G,p,n1)->twoNodeGraphInit(G,p,n1,3))
setRandomSeed 0
elapsedTime sols' = graphStrategy(SP,c0,{pre0}, SelectEdgeAndDirection => selectBestEdgeAndDirection,
     TargetSolutionCount=>70, Potential=>potentialE, GraphInitFunction=>(G,p,n1)->completeGraphInit(G,p,n1,1,6))

setRandomSeed 0
N = 3
E = 4
elapsedTime sols' = graphStrategy(SP,c0,{pre0}, SelectEdgeAndDirection => selectBestEdgeAndDirection, TargetSolutionCount=>70, Potential=>potentialE, GraphInitFunction=>completeGraphInit, nnodes => N, nedges => E)
sols'#1

trackedlist = apply(pairs(0..15), (graphStrategy(SP,c0,{pre0}, SelectEdgeAndDirection => selectBestEdgeAndDirection, 
	TargetSolutionCount=>70, Potential=>potentialE, GraphInitFunction=>completeGraphInit, NumberOfNodes => N, NumberOfEdges => E))#1)

apply(0..3,0..4, (i, j) -> i*j)
0..3
G = first sols
E = G.Edges
for e in E do (
    print peek e.Correspondence12;
    print peek e.Correspondence21;
    )

-- other strategies
elapsedTime sols = randomFlowerStrategy(SP,c0,{pre0},StoppingCriterion=>stop);
elapsedTime sols = solveViaMonodromy(SP,c0,{pre0},StoppingCriterion=>stop)
elapsedTime sols = flowerStrategy(SP,c0,{pre0},StoppingCriterion=>stop);
sols

-- currently not working?
elapsedTime sols = loopStrategy(SP,c0,{pre0},3,StoppingCriterion=>stop);





