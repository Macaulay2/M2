restart
load (currentFileDirectory|"../code/solveViaMonodromy.m2")
needsPackage "ReactionNetworks"

FF = CC

CRN = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, A+C --> D"
R = createRing(CRN, FF)
CEforms = matrix{conservationEquations(CRN,FF)}
CE =sub(CEforms, apply(gens ring CEforms, x -> x => 1)) - CEforms
SSE = matrix {steadyStateEquations CRN}
T = transpose(CE|SSE)
rM = sub(random(FF^5, FF^7),R)
G = polySystem(rM * T)
end ---------------------------------
restart
load "example-CRN.m2"
setUpPolysparse = G -> (
    C := coefficientRing ring G;
    M := sub(sub(G.PolyMap, apply(gens ring G, x -> x => 1)), C);
    N := numericalIrreducibleDecomposition ideal M;
    c0 := first (first components N).Points; 
    pre0 := point{toList(numgens ring G : 1_CC)};
    (c0,pre0)
    )
setRandomSeed 0
(c0,pre0) = setUpPolysparse G
elapsedTime sols = twoNodes(transpose G.PolyMap,c0,{pre0},5)









