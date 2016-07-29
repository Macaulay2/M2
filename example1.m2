restart
needs "code/solveViaMonodromy.m2"
needs "examples/cyclic.m2"
plugin'c0 = map(R,AR,vars R | matrix c0) -- the actual polynomial system we solve
apply(polysP,p->plugin'c0 p) 
stop = (n,L)-> #L >= 70
getDefault Software
{*
setDefault(Software=>PHCPACK)
*}

twoNodeGraphInit = (G, p, node1) -> (
    nedges := 10;
    nextP := ((p0)->point {apply(#coordinates p0, i->exp(2*pi*ii*random RR))});
    node2 := addNode(G, nextP(p), pointArray {});
    apply(nedges, i -> addEdge(G, node1, node2));
    )

completeGraphInit = (G, p, node1) -> (
    nedges := 10;
    nnodes := 5;
    nextP := ((p0)->point {apply(#coordinates p0, i->exp(2*pi*ii*random RR))});
    for i from 1 to nnodes do (
        addNode(G,nextP(p), pointArray {});
    );
    print(peek(G));
    for i from 0 to nnodes do (
        for j from i+1 to nnodes do (
            apply(nedges, k -> addEdge(G, G.Vertices#i, G.Vertices#j));
        );
    );
    )

flowerGraphInit = (G, p, node1) -> (
    nedges := 10;
    nnodes := 3;
    nextP := ((p0)->point {apply(#coordinates p0, i->exp(2*pi*ii*random RR))});
    for i from 1 to nnodes do (
        newNode := addNode(G,nextP(p), pointArray {});
        apply(nedges, k -> addEdge(G, node1, newNode));
    );
    )

-- two vertex

nedges = 10
setRandomSeed 0
elapsedTime sols = twoNodes(SP,c0,{pre0},nedges,StoppingCriterion=>stop)
setRandomSeed 0
elapsedTime sols' = twoNodes(SP,c0,{pre0},nedges, SelectEdgeAndDirection => selectBestEdgeAndDirection, TargetSolutionCount=>70)
setRandomSeed 0
elapsedTime sols' = twoNodes(SP,c0,{pre0},nedges, SelectEdgeAndDirection => selectBestEdgeAndDirection, TargetSolutionCount=>70, Potential=>potentialAsymptotic)

--When I try running graphStrategy with potentialAsymptotic and the completeGraphInit, I get div 0 errors from potentialAsymptotic
elapsedTime sols' = graphStrategy(SP,c0,{pre0}, SelectEdgeAndDirection => selectBestEdgeAndDirection, TargetSolutionCount=>70, Potential=>potentialLowerBound, GraphInitFunction=>flowerGraphInit)


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





