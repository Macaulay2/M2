needsPackage "MonodromySolver"
needsPackage "ReactionNetworks"

FF = CC

-- creates a polynomial system from a chemical reaction network
createPolySystem = method()
createPolySystem (ReactionNetwork, InexactFieldFamily):= (Rn, FF) -> (
    S := createRing(Rn, FF);
    createPolySystem(Rn,FF,toList(length Rn.ConcentrationRates : 1_FF))
    )
createPolySystem'overdetermined = (Rn, FF, L) -> (
    S := createRing(Rn, FF);
    CEforms := matrix{conservationEquations(Rn,FF)};
    SubList := toList(apply(0..length Rn.ConcentrationRates-1, i -> 
	    value(Rn.ConcentrationRates#i) => L#i));
    CE := sub(CEforms, SubList) - CEforms;    
    SSE := steadyStateEquations Rn;
    R := CC[Rn.ReactionRates][Rn.ConcentrationRates];	       	   
    M := sub((transpose CE || SSE), R);
    polySystem M
    )
createPolySystem (ReactionNetwork, InexactFieldFamily, List) := (Rn, FF, L) -> (
    squareUp createPolySystem'overdetermined(Rn,FF,L)
    )

-- n copies of oneSiteModificationA
multipleModificationA = n -> (
    A := oneSiteModificationA();
    for i from 2 to n do 
    A = glue(A, sub(oneSiteModificationA(), {"S_0" => "2S_"|(i-1), "S_1" => "S_"|i}));
    A
    )
-- experiment 
X = reactionNetwork "A+B <--> C, X <--> 2A+D, 2A+D <--> Y, D <--> C+W, B+D <--> Z"
{*
X' = reactionNetwork ({"A+B <--> C", "X <--> 2A+D", "2A+D <--> Y", 
    "D <--> C+W", "B+D <--> Z", "0 <--> A", "0 <--> B", "0 <--> C", 
    "0 <--> D", "0 <--> W", "0 <--> X", "0 <--> Y", "0 <--> Z"}, NullSymbol => "0")
X'S = createPolySystem(X', FF) --not working
*}

end
restart
load "example-large-CRN.m2"

setRandomSeed 0
-- system for 5 copies of oneSiteModificationA
n = 7
W = X
W = multipleModificationA n
H = createPolySystem(W, FF)
(p0, x0) = createSeedPair(H,"initial parameters" => "one")
elapsedTime (V,npaths) = monodromySolve(H,p0,{x0},NumberOfEdges => 5)
length V.PartialSols

L = apply(numgens createRing(W,FF), i->random FF)
specPolys = specializeSystem (p0,createPolySystem'overdetermined(W,FF,L));
R = CC[x_1..x_(numgens ring first specPolys)]
toR = map(R,ring first specPolys,vars R)
elapsedTime NV = numericalIrreducibleDecomposition(ideal (specPolys/toR),Software=>BERTINI)
length components NV

