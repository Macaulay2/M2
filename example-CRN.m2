restart
load (currentFileDirectory|"../code/solveViaMonodromy.m2")
needsPackage "ReactionNetworks"

FF = CC

stop = (n,L)-> #L >= 9

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


-- service function that displays numbering of complexes

-- display network with correspondence of reaction and complexes



W = wnt()
R' = createRing(W, FF)
CEforms' = matrix{conservationEquations(W,FF)}
CE' =sub(CEforms', apply(gens ring CEforms', x -> x => random CC)) - CEforms'
-- subsituting random complex number gives error 
SSE' = matrix {steadyStateEquations W}
T' = transpose(CE'|SSE')
rM' = sub(random(FF^19, FF^24),R')    
G' = polySystem(rM' * T')


randomList = apply(gens ring G', x -> x => random CC)

setUpPolysparse' = G' -> (
    C' := coefficientRing ring G';
    M' := sub(sub(G'.PolyMap, randomList), C');
    N' := numericalIrreducibleDecomposition ideal M';  -- N'=null, so c0' cannot be comput
    c0' := first (first components N').Points; 
    pre0' := point{toList(numgens ring G' : 1_CC)};
    (c0',pre0')
    )

end ---------------------------------
restart
load "example-CRN.m2"
setRandomSeed 0
(c0,pre0) = setUpPolysparse G
elapsedTime sols = twoNodes(transpose G.PolyMap,c0,{pre0},5)


-- try WNT ???  
-- encounters singular points
-- cannot get random value substitution to work. 

(c0',pre0') = setUpPolysparse' G'

elapsedTime sols' = twoNodes(transpose G'.PolyMap,c0',{pre0'},20)


-- some other examples? simpler than wnt but with more solutions?







