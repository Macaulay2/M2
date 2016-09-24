restart
needsPackage "MonodromySolver"
needsPackage "ReactionNetworks"

FF = CC

-- creates a polynomial system from a chemical reaction network
createPolySystem = method()
createPolySystem (ReactionNetwork, InexactFieldFamily):= (Rn, FF) -> (
    S := createRing(Rn, FF);
    createPolySystem(Rn,FF,toList(numgens S : 1_FF))
    )
createPolySystem (ReactionNetwork, InexactFieldFamily, List) := (Rn, FF, L) -> (
    S := createRing(Rn, FF);
    CEforms := matrix{conservationEquations(Rn,FF)};
    SubList := apply(toList(0..numgens S-1), i -> (gens S)#i => L#i);
    CE := sub(CEforms, SubList) - CEforms;    
    SSE := matrix {steadyStateEquations Rn};	       	   
    T := transpose(CE|SSE);
    rM := sub(random(FF^(numgens S), FF^(numrows T)), S);
    polySystem(rM * T)
    )

TEST ///
L = toList (#vars R:random CC)
createPolySystem(CRN, FF, L)
///

-- example from Elizabeth's talk
CRN = reactionNetwork "A <--> 2B, A+C<-->D, B+E-->A+C, D-->B+E"
G = createPolySystem(CRN, FF)

-- example of a motif
F = twoSiteModificationF()
G' = createPolySystem(F, FF)

-- example of a motif
C = clusterModelCellDeath()
GC = createPolySystem(C, FF)

-- random example to test number of solutions
Q = reactionNetwork "A <--> 2B, A+3C<-->D, B+4E-->A+3C, A+3C-->D, D-->B+4E"
GQ = createPolySystem(Q, FF)


end ---------------------------------
restart
load "example-CRN.m2"

setRandomSeed 0
-- system for example from Elizabeth's talk
(p0, x0) = createSeedPair(G,"initial parameters" => "one")  -- random doesn't work
elapsedTime sols = monodromySolve(G,p0,{x0}, NumberOfEdges => 5)

-- system for motif twoSiteModificationF
(p0, x0) = createSeedPair(G',"initial parameters" => "one")
elapsedTime sols = monodromySolve(G',p0,{x0},NumberOfEdges => 3)

-- system for wnt signaling pathway
W = wnt()
setRandomSeed 0
L = apply(numgens createRing(W,FF), i->random FF)
F = createPolySystem(W, FF, L)
(p0, x0) = createSeedPair(F,L)
elapsedTime sols = monodromySolve(F,p0,{x0},
    GraphInitFunction=>completeGraphInit,
    NumberOfNodes=>3,
    NumberOfEdges=>3,
    TargetSolutionCount => 9,
    "new tracking routine"=>false,
    Verbose=>true)
-- wnt via Bertini
specPolys = specializeSystem (p0,F);
R = CC[x_1..x_(numgens ring first specPolys)]
toR = map(R,ring first specPolys,vars R)
elapsedTime sols = solveSystem(specPolys/toR);
elapsedTime sols = solveSystem(specPolys/toR, Software=>BERTINI);

-- system for random example
(p0, x0) = createSeedPair(GQ, "initial parameters" => "one")
elapsedTime sols = monodromySolve(GQ,p0,{x0}, NumberOfEdges => 1, NumberOfNodes => 5)










