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
    if conservationEquations Rn == {} 
    then (
	SubList := toList(apply(0..length Rn.ConcentrationRates-1, i -> 
	    value(Rn.ConcentrationRates#i) => L#i));
    	SSE := steadyStateEquations Rn;
	R := CC[Rn.ReactionRates][Rn.ConcentrationRates];	       	   
    	M := sub(SSE, R);
    	polySystem M
	) 
    else (
    CEforms := matrix{conservationEquations(Rn,FF)};
    SubList = toList(apply(0..length Rn.ConcentrationRates-1, i -> 
	    value(Rn.ConcentrationRates#i) => L#i));
    CE := sub(CEforms, SubList) - CEforms;    
    SSE = steadyStateEquations Rn;
    R = CC[Rn.ReactionRates][Rn.ConcentrationRates];	       	   
    M = sub((transpose CE || SSE), R);
    polySystem M
    ))

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

N = reactionNetwork ({"A+R-->RA", "A+RD-->RDA", "A+RT-->RTA", "E+R-->RE",
	"E+RD-->RDE", "E+RT-->RTE", "R-->RD", "R-->RT", "RA-->A+R", "RD-->R",
	"RDA-->A+RD", "RDE-->E+RD", "RDE-->RE", "RE-->E+R", "RE-->RTE", "RE-->RDE",
	"RT-->R", "RT-->RD", "RTA-->A+RT", "RTA-->RDA", "RTE-->E+RT", "RTE-->RDE", "RTE-->RE"
	})
--R = createRing(N,QQ)


D1 = reactionNetwork ({"X_0+X_1 <--> X_3", "X_0+X_3 <--> X_4", "X_2+X_3 <--> X_5", 
	"X_5 <--> X_4+X_1", "X_5 <--> X_6", "X_7+X_8 <--> X_6", "X_6 <--> X_9+Y_0",
	"X_9 <--> X_0+X_8", "X_7 <--> X_0+Y_0", "X_4+Y_0 <--> Y_1", "Y_1 <--> X_7+X_3",
	"X_2+X_8 <--> Y_2", "Y_2 <--> X_9+X_1"
	})
D2 = glue(D1, sub(D1, {"X_0"=>"Z_0"}))

R = createRing(D2,QQ)
F = join(subRandomInitVals D2, subRandomReactionRates D2)
I = ideal F
S = QQ[D2.ConcentrationRates]
J = sub(I, S)
dim J
degree J

GC = reactionNetwork ({"G+ATP-->G6P+ADP+H", "G6P<-->F6P", "F6P+ATP<-->FBP+ADP+H", 
	"FBP<-->DHAP", "FEP<-->GAP", "DHAP<-->GAP", "GAP+NAD+P<-->BPG+NADH+H",
	"BPG+ADP<-->TrPG+ATP", "TrPG<-->TwPG", "TwPG<-->PEP+HOH",
	"PEP+ADP+H-->Pyr+ATP", "G+2ADP+2P+2NAD-->2Pyr+2ATP+2NADH+2HOH+2H"}
	) 
R = createRing(GC,QQ)
F = join(subRandomInitVals GC, subRandomReactionRates GC)
I = ideal F
S = QQ[GC.ConcentrationRates]
J = sub(I, S)
dim J
degree J

K4 = reactionNetwork({"3Y+Z --> 2Y+2Z", "3Y+Z --> 4Z", "3Y+Z --> Y+3Z", 
	"2Y+2Z --> Y+3Z", "2Y+2Z --> 4Z", "Y+3Z --> 4Z"})

OR = reactionNetwork({"0-->Ff", "Dd-->0", "Dd-->Df", "Dd-->Mf", "Dd-->Mb",
	"Df-->0", "Df-->Dd", "Df-->P", "Ff-->0", "Ff-->Dd", "Ff-->P", "Mf-->0",
	"Mf-->Dd", "Mf-->Df", "Mb-->0", "Mb-->Df", "Mb-->Mf", "P-->0", "P-->Dd"},
	NullSymbol=>"0"
    )

MAPK = reactionNetwork({"X1+X2<-->X3", "X4+X5+X3-->X4+X6", "X4+X3-->X4+X7", "X6-->X5+X3",
	"X6+X8-->X6+X9", "X7+X8-->X7+X9", "X9-->X8", "X10+X9-->X10+X8",
	"X11+X12-->X13+X12", "X13+X9-->X14+X9", "X14+X9+X15-->X16+X9+X15",
	"X17+X16-->X17+X14", "X14-->X11", "X18-->X14+X9", "X16+X19-->X16+X20",
	"X15+X19-->X15+X21", "X21+X16-->X20+X16", "X20+X22-->X20+X23", "X23+X19-->X23+X24",
	"X24+X16-->X20+X16", "X23+X6-->X23+X25+X2", "X23+X7-->X23+X26+X2",
	"X25-->X5+X1", "X26<-->X1", "X23+X27-->X28", "X28+X29-->X28+X29+X30+X31",
	"X23+X31-->X23+X32", "X28+X33-->X28+X33+X34+X35", "X35+X23-->X35+X22", 
	"X23+X36-->X37", "X37+X27-->X38", "X38+X29-->X38+X31+X29+X30", "X37+X31-->X37+X32",
	"X38+X33-->X38+X35+X33+X34", "X35+X37-->X35+X22+X36", "X28-->X23+X27", 
	"X37-->X23+X36", "X38-->X37+X27"},
	NullSymbol=>"0"
    );
R = createRing(MAPK,QQ);
F = join(subRandomInitVals MAPK, subRandomReactionRates MAPK);
I = ideal F;
S = QQ[MAPK.ConcentrationRates];
J = sub(I, S);
dim J
degree J

--e1 is bad
e1 = reactionNetwork({"E_1+S_1<-->E_1S_1", "S_2+E_1<-->E_1S_1S_2",
	"E_1S_1S_2<-->P_1+E_1", "S_2+E_2<-->E_2S_2", "E_2S_2-->2S_1+E_2",
	"S_3+E_2S_2<-->E_2S_2S_3",
	"E_2S_2S_3<-->P_2+E_2", "S_3+E_3<-->E_3S_3", "S_4+E_3S_3<-->E_3S_3S_4",
	"E_3S_3S_4<-->P_3+E_3", "S_4+E_4<-->E_4S_4",
	"E_4S_4-->2S_1+E_4"
	})
R = createRing(e1,QQ);
F = join(subRandomInitVals e1, subRandomReactionRates e1)
I = ideal F
S = QQ[e1.ConcentrationRates]
J = sub(I, S)
dim J
degree J

E7 = reactionNetwork({"A<-->2A", "A+B<-->C", "C<-->B", "B<-->2B", "B+C<-->D", "D<-->C",
	"C<-->2C", "C+D<-->F", "F<-->D", "D<-->2D", "D+F<-->G", "G<-->F",
	"F<-->2F", "F+G<-->H", "H<-->G", "G<-->2G", "G+H<-->K", "K<-->H"
	})

end-----------------------------------


restart
load "example-large-CRN.m2"

setRandomSeed 0
-- system for n copies of oneSiteModificationA
n = 7
An = multipleModificationA n
-- testing different networks
E7
H = createPolySystem(e1, FF)
(p0, x0) = createSeedPair(H, "initial parameters" => "one")
elapsedTime (V,npaths) = monodromySolve(H,p0,{x0},NumberOfNodes => 4, NumberOfEdges => 1)
length V.PartialSols

L = apply(numgens createRing(e1,FF), i->random FF)
specPolys = specializeSystem (p0,createPolySystem'overdetermined(e1,FF,L));
R = CC[x_1..x_(numgens ring first specPolys)]
toR = map(R,ring first specPolys,vars R)
elapsedTime NV = numericalIrreducibleDecomposition(ideal (specPolys/toR),Software=>BERTINI)
length components NV

