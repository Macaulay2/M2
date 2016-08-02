restart
load (currentFileDirectory|"../code/solveViaMonodromy.m2")
needsPackage "ReactionNetworks"

FF = CC

stop = (n,L)-> #L >= 6

CRN = reactionNetwork "A <--> 2B, A+C<-->D, B+E-->A+C, A+C-->D, D-->B+E"
R = createRing(CRN, FF)
CEforms = matrix{conservationEquations(CRN,FF)}
CE =sub(CEforms, apply(gens ring CEforms, x -> x => 1)) - CEforms
SSE = matrix {steadyStateEquations CRN}	       	   
T = transpose(CE|SSE)
rM = sub(random(FF^5, FF^7),R)
G = polySystem(rM * T)
setUpPolysparse = G -> (
    C := coefficientRing ring G;
    M := sub(sub(G.PolyMap, apply(gens ring G, x -> x => 1)), C);
    N := numericalIrreducibleDecomposition ideal M;
    c0 := first (first components N).Points; 
    pre0 := point{toList(numgens ring G : 1_CC)};
    (c0,pre0)
    )


-- simplify code -- too much stuff!
-- more examples!



W = wnt()
R' = createRing(W, FF)
CEforms' = matrix{conservationEquations(W,FF)}
randomList = apply(gens R', x -> x => random CC)
CE' =sub(CEforms', randomList) - CEforms'
-- subsituting random complex number gives error 
SSE' = matrix {steadyStateEquations W}
T' = transpose(CE'|SSE')
rM' = sub(random(FF^19, FF^24),R')    
G' = polySystem(rM' * T')

-- phosphorilation 

F = twoSiteModificationF()
R'' = createRing(F, FF)
CEforms'' = matrix{conservationEquations(F,FF)}
CE'' =sub(CEforms'', apply(gens ring CEforms'', x -> x => 1)) - CEforms''
SSE'' = matrix {steadyStateEquations F}	       	   
T'' = transpose(CE''|SSE'')
rM'' = sub(random(FF^10, FF^14),R'')
G'' = polySystem(rM'' * T'')

-- using random points
setUpPolysparse' = G' -> (
    C' := coefficientRing ring G';
    M' := sub(sub(G'.PolyMap, randomList), C');
    N' := numericalIrreducibleDecomposition ideal M';  -- N'=null, so c0' cannot be comput
    c0' := first (first components N').Points; 
    pre0' := point{apply(randomList, i -> i#1)};
    (c0',pre0')
    )

end ---------------------------------
restart
load "example-CRN.m2"
setRandomSeed 0
(c0,pre0) = setUpPolysparse G
elapsedTime sols = twoNodes(transpose G.PolyMap,c0,{pre0},5)


(c0',pre0') = setUpPolysparse' G'
matrix c0'
matrix pre0'
elapsedTime sols' = twoNodes(transpose G'.PolyMap,c0',{pre0'},20)


(c0'', pre0'') = setUpPolysparse G''
elapsedTime sols'' = twoNodes(transpose G''.PolyMap,c0'',{pre0''},5)
elapsedTime sols'' = graphStrategy(transpose G''.PolyMap,c0'',{pre0''}, 
    SelectEdgeAndDirection => selectBestEdgeAndDirection, 
    TargetSolutionCount=>6, 
    Potential=>potentialAsymptotic, 
    GraphInitFunction=>flowerGraphInit)
-- some other examples? simpler than wnt but with more solutions?







