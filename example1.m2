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

-- two vertex

nedges = 10
setRandomSeed 0
elapsedTime sols = twoNodes(SP,c0,{pre0},nedges,StoppingCriterion=>stop)
setRandomSeed 0
elapsedTime sols' = twoNodes(SP,c0,{pre0},nedges, SelectEdgeAndDirection => selectBestEdgeAndDirection, TargetSolutionCount=>70)
setRandomSeed 0
elapsedTime sols' = twoNodes(SP,c0,{pre0},nedges, SelectEdgeAndDirection => selectBestEdgeAndDirection, TargetSolutionCount=>70, Potential=>potentialAsymptotic)


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





