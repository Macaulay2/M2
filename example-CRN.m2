restart
load (currentFileDirectory|"../code/solveViaMonodromy.m2")
needsPackage "ReactionNetworks"

FF = CC

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

W = wnt()
Rw = createRing(W, FF)
CEformsW = matrix{conservationEquations(W,FF)}
CEw =sub(CEformsW, apply(gens ring CEformsW, x -> x => 1)) - CEformsW
SSEw = matrix {steadyStateEquations W}
Tw = transpose(CEw|SSEw)
rMw = sub(random(FF^19, FF^24),Rw)
Gw = polySystem(rMw * Tw)


end ---------------------------------
restart
load "example-CRN.m2"
setRandomSeed 0
(c0,pre0) = setUpPolysparse G
elapsedTime sols = twoNodes(transpose G.PolyMap,c0,{pre0},5)


-- try WNT ???  
-- encounters singular points, does not recognize StoppingCriterion

(c0w,pre0w) = setUpPolysparse Gw
elapsedTime solsW = twoNodes(transpose Gw.PolyMap,c0w,{pre0w},10)
elapsedTime solsW = twoNodes(transpose Gw.PolyMap,c0w,{pre0w},5,
    SelectEdgeAndDirection => selectBestEdgeAndDirection, 
    TargetSolutionCount=>9)
elapsedTime solsW = twoNodes(transpose Gw.PolyMap,c0w,{pre0w},5,
    SelectEdgeAndDirection => selectBestEdgeAndDirection, 
    TargetSolutionCount=>9, 
    Potential=>potentialAsymptotic)
elapsedTime solsW = flowerStrategy(transpose Gw.PolyMap,c0w,{pre0w},
    StoppingCriterion=>stop)
elapsedTime solsW = loopStrategy(transpose Gw.PolyMap,c0w,{pre0w},3,
    StoppingCriterion=>stop)


-- some other examples?







