restart
load (currentFileDirectory|"../code/solveViaMonodromy.m2")
needsPackage "ReactionNetworks"

FF = CC

stop = (n,L)-> #L >= 6

createPolySystem = method()
createPolySystem (ReactionNetwork, Ring) := (Rn, FF) -> (
    S := createRing(Rn, FF);
    CEforms := matrix{conservationEquations(Rn,FF)};
    CE := sub(CEforms, apply(gens S, x -> x => 1)) - CEforms;
    SSE := matrix {steadyStateEquations Rn};	       	   
    T := transpose(CE|SSE);
    rM := sub(random(FF^(numgens S), FF^(numrows T)), S);
    G := polySystem(rM * T)
    )
createPolySystem (ReactionNetwork, InexactFieldFamily):= (Rn, FF) -> (
    S := createRing(Rn, FF);
    CEforms := matrix{conservationEquations(Rn,FF)};
    CE := sub(CEforms, apply(gens S, x -> x => 1)) - CEforms;
    SSE := matrix {steadyStateEquations Rn};	       	   
    T := transpose(CE|SSE);
    rM := sub(random(FF^(numgens S), FF^(numrows T)), S);
    G := polySystem(rM * T)
    )
createPolySystem (ReactionNetwork, InexactFieldFamily, List) := (Rn, FF, L) -> (
    S := createRing(Rn, FF);
    CEforms := matrix{conservationEquations(Rn,FF)};
    SubList := apply(toList(0..numgens S-1), i -> (gens S)#i => L#i);
    CE := sub(CEforms, SubList) - CEforms;    
    SSE := matrix {steadyStateEquations Rn};	       	   
    T := transpose(CE|SSE);
    rM := sub(random(FF^(numgens S), FF^(numrows T)), S);
    G := polySystem(rM * T)
    )

TEST ///
L = toList (#vars R:random CC)
createPolySystem(CRN, FF, L)
///

setUpPolysparse = method()
setUpPolysparse PolySystem := G -> (
    C := coefficientRing ring G;
    M := sub(sub(G.PolyMap, apply(gens ring G, x -> x => 1)), C);
    N := numericalIrreducibleDecomposition ideal M;
    c0 := first (first components N).Points; 
    pre0 := point{toList(numgens ring G : 1_CC)};
    (c0,pre0)
    )
setUpPolysparse(PolySystem, List) := (G, L) -> (
    SubList := apply(toList(0..numgens ring G-1), i -> (gens ring G)#i => L#i);
    C := coefficientRing ring G;
    M := sub(sub(G.PolyMap, SubList), C);
    N := numericalIrreducibleDecomposition ideal M; 
    c0 := first (first components N).Points; 
    pre0 := point{apply(SubList, i -> i#1)};
    (c0,pre0)
    )

CRN = reactionNetwork "A <--> 2B, A+C<-->D, B+E-->A+C, A+C-->D, D-->B+E"
G = createPolySystem(CRN, FF)


F = twoSiteModificationF()
G' = createPolySystem(F, FF)

C = clusterModelCellDeath()
GC = createPolySystem(C, FF)


-- example below is work in progress
A = oneSiteModificationA()
A.Species
A' = sub(oneSiteModificationA(), {"S_0" => "S_1", "S_1" => "S_2"})
A'' = sub(oneSiteModificationA(), {"S_0" => "S_2", "S_1" => "S_3"})
A'''=sub(oneSiteModificationA(), {"S_0" => "S_3", "S_1" => "S_4"})
B = sub(oneSiteModificationA(), {"S_0" => "S_4", "S_1" => "S_5"})
C'' = glue(A,A')
C' = glue(C'',A'')
D = glue (C',A''')
C = glue(D,B)
R1 = createRing(C, FF)
CEforms1 = matrix{conservationEquations(C,FF)}
CE1 =sub(CEforms1, apply(gens ring CEforms1, x -> x => 1)) - CEforms1
SSE1 = matrix {steadyStateEquations C}	       	   
T1 = transpose(CE1|SSE1)
rM1 = sub(random(FF^10, FF^13),R1)
G1 = polySystem(rM1 * T1)



#C.Species



end ---------------------------------
restart
load "example-CRN.m2"
setRandomSeed 0
(c0, pre0) = setUpPolysparse G
elapsedTime sols = twoNodes(transpose G.PolyMap,c0,{pre0},5)

(c0', pre0') = setUpPolysparse G'
elapsedTime sols' = twoNodes(transpose G'.PolyMap,c0',{pre0'},5)

W = wnt()
R = createRing(W, FF)
L = apply(toList(1..numgens R), i -> random CC)
F = createPolySystem(W, FF, L)
(c0, pre0) = setUpPolysparse(F, L)
elapsedTime sols = twoNodes(transpose F.PolyMap,c0,{pre0},5)


(c0, pre0) = setUpPolysparse GC
elapsedTime sols = twoNodes(transpose GC.PolyMap,c0,{pre0},5)




-- some other examples? simpler than wnt but with more solutions?


(c01,pre01) = setUpPolysparse G1
elapsedTime sols = twoNodes(transpose G1.PolyMap,c01,{pre01},5)


