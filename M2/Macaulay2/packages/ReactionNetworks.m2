-- -*- coding: utf-8 -*-
newPackage(
	"ReactionNetworks",
    	Version => "1.0", 
    	Date => "June, 2016",
    	Authors => {
	     {Name => "Jane Doe", Email => "doe@math.uiuc.edu"}
	     },
    	HomePage => "http://www.math.uiuc.edu/~doe/",
    	Headline => "Reaction networks",
	PackageImports => {"Graphs"},
	AuxiliaryFiles => true, -- set to true if package comes with auxiliary files
    	DebuggingMode => true		 -- set to true only during development
    	)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export {"reactionNetwork", "ReactionNetwork", "Species", "Complexes", "ReactionGraph",
    "stoichiometricSubspace",
    "steadyStateEquations",
    "parameterRing",
    "glueNetworks"
    }
exportMutable {}

removeWhitespace = s -> s = replace(" ", "", s)

stripCoef = (s) -> (
    i := 0;
    while regex("[0-9]", s#i) =!= null do i = i+1;
    substring(i,length(s),s)
    )

specProportion = (s) -> (
    i := 0;
    while regex("[0-9]", s#i) =!= null do i = i+1;
    value substring(0,i,s)    
    )

ReactionNetwork = new Type of MutableHashTable

reactionNetwork = method()
reactionNetwork String := str -> reactionNetwork separateRegexp(",", str)
reactionNetwork List := rs -> (
    Rn := new ReactionNetwork from {Species => {}, Complexes => {}, ReactionGraph => digraph {}};
    scan(rs, r -> addReaction(r,Rn));
    Rn
    )

addSpecies = method()
addSpecies(String, ReactionNetwork) := (s,Rn) -> 
    if not member(s,Rn.Species) then (
	Rn.Species = Rn.Species | {s};
	Rn.Complexes = apply(Rn.Complexes, c -> c | matrix{{0}})
	)
    

addComplex = method()
addComplex(String, ReactionNetwork) := (c,Rn) -> (
    species := apply(delete("", separateRegexp("[^(((A-Z)|(a-z))_?(0-9)*)]", c)), stripCoef);
    for specie in species do addSpecies(specie, Rn);
    v := mutableMatrix(ZZ,1,#Rn.Species);    	
    apply(separateRegexp("\\+", c), t -> (
	    s:=stripCoef(t);
	    a:=specProportion(t);
	    if a === null then a = 1;
	    i:=position(Rn.Species, s' -> s' == s);
	    v_(0,i) = v_(0,i) + a;
	    ));
    v = matrix v;
    if member(v,Rn.Complexes) then position(Rn.Complexes, v' -> v' == v) 
    else (
	Rn.Complexes = Rn.Complexes | {v};	
        #Rn.Complexes - 1
	)
    )

addReaction = method()
addReaction(String, ReactionNetwork) := (r,Rn) -> (
    r = removeWhitespace r;
    complexes := apply(separateRegexp("(-->)|(<--)|(<-->)|,", r), removeWhitespace);
    if #complexes != 2 then error "Expected two complexes.";
    i := addComplex(first complexes, Rn);
    j := addComplex(last complexes, Rn);
    Rn.ReactionGraph = addVertices(Rn.ReactionGraph, {i,j});
    delim := concatenate separateRegexp(///[A-Z]|[a-z]|[0-9]|,|_|\+| ///, r);
    if delim == "-->" then Rn.ReactionGraph = addEdges'(Rn.ReactionGraph, {{i,j}})
    else if delim == "<--" then Rn.ReactionGraph = addEdges'(Rn.ReactionGraph, {{j,i}})
    else if delim == "<-->" then Rn.ReactionGraph = addEdges'(Rn.ReactionGraph, {{i,j},{j,i}})
    else error "String not in expected format";
    )
 


netComplex = (r,c) -> (
    C := flatten entries r.Complexes#c;
    l := apply(#r.Species, i -> if C#i == 0 then "" 
	else (if C#i ==1 then "" else toString C#i) | r.Species#i);
    l = delete("", l);
    l = between("+", l);
    concatenate l
    )


merge (ReactionNetwork,ReactionNetwork):= (N1, N2) -> (
    N := copy N1;
    apply(networkToHRF N2, r -> addReaction(r,N));
    N
    );
merge (List, ReactionNetwork) := (L, N) -> glueNetworks(reactionNetwork L, N)
merge (ReactionNetwork, List) := (N, L) -> glueNetworks(N, reactionNetwork L)


TEST ///
restart
needsPackage "ReactionNetworks"
NM = reactionNetwork "A <-- 2B, A + C <-- D, B + E --> A + C"
NN = reactionNetwork "A --> 2B, A + C --> D, D --> B + E"
glueNetworks(NM,NN)
NM
NN
glueNetworks({"S --> T"}, NM)
glueNetworks(NN, {"S --> T"})
glueNetworks({"S --> T"}, {"P <--> Q"})  
glueNetworks({"S --> T"}, {"S --> T"})  
NN
///



-- L is a list of options of the form "A" => "B"
sub(ReactionNetwork, List) := (N, L) -> (
    T := new HashTable from L;
    N.Species = for s in N.Species list (if T#?s then T#s else s);
    N
    )


TEST ///
restart
needsPackage "ReactionNetworks"
NM = reactionNetwork "A <-- 2B, A + C <-- D, B + E --> A + C"
sub(NM, {"A" => "Y"})

N = oneSiteModificationA()
sub(N, {"S_0" => "A"})
///



networkToHRF = N -> apply(edges N.ReactionGraph, e -> netComplex(N, first e) | "-->" | 
    netComplex(N, last e))

net ReactionNetwork := N -> stack networkToHRF N 

stoichiometricSubspace = method()
stoichiometricSubspace ReactionNetwork := N -> (
    C := N.Complexes;
    reactions := apply(edges N.ReactionGraph, e -> C#(last e) - C#(first e));
    M:=reactions#0;
    for i from 1 to #reactions - 1 do M=M||reactions#i;
    ker M
    )


TEST ///
CRN = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B + E"
stoichiometricSubspace CRN
///

concentration = (species,N,R) -> R_(position(N.Species, s->s==species))
    
termInp = (a,inp,out,N,R) -> if member(a,inp/first) then (     
    p := position(inp/first,x->x==a);
    - last inp#p * product(inp,b->(concentration(first b,N,R))^(last b)) 
    ) else 0
termOut = (a,inp,out,N,R) -> if member(a,out/first) then (     
    p := position(out/first,x->x==a);
    last out#p * product(inp,b->(concentration(first b,N,R))^(last b)) 
    ) else 0

parameterRing = (N,FF) -> (
    kk := symbol kk; 
    rates := apply(edges N.ReactionGraph, e->kk_e);
    FF[rates]
    )

steadyStateEquations = method()
steadyStateEquations ReactionNetwork := N -> steadyStateEquations(N,QQ)
steadyStateEquations (ReactionNetwork,Ring) := (N,FF) -> (
    -- K is the parameter ring
    kk := symbol kk; 
    rates := apply(edges N.ReactionGraph, e->kk_e);
    K := FF[rates];
    kk = gens K;
    -- C is a list of pairs (species, input_rate)
    C := apply(N.Species,a->(a,0));
    -- R is a list of reaction equations, formatted ({(specie, coefficient), ... } => {(specie, coefficient), ...}, fwdrate, bckwd rate)
    R := apply(edges N.ReactionGraph, e->(
	    (i,j) := toSequence e;
	    (
		apply(N.Species, flatten entries N.Complexes#i, (s,c)->(s,c)) =>
	    	apply(N.Species, flatten entries N.Complexes#j, (s,c)->(s,c))
		,
		kk#(position(edges N.ReactionGraph,e'->e'==e))
		,
		0
		)  
	    ));
    cc := symbol cc;
    RING := K[apply(C,i->cc_(first i))];
    cc = gens RING;
    F := for i in C list (
	(a,af) := i;
	sum(R,reaction->(
		(inp'out,k1,k2) := reaction;
		r1 := first inp'out;
		r2 := last inp'out;
		k1 * (termInp(a,r1,r2,N,RING) + termOut(a,r1,r2,N,RING)) +
		k2 * (termInp(a,r2,r1,N,RING) + termOut(a,r2,r1,N,RING))
		))  
	)
    )

TEST ///
restart
needsPackage "ReactionNetworks"
CRN = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B + E"
parameterRing(CRN, QQ)
F = steadyStateEquations CRN
///

load "ReactionNetworks/motifs-Kisun.m2"
load "ReactionNetworks/motifs-Cvetelina.m2"

-- end

beginDocumentation()
scan({
    -- "CrosslinkingModelCellDeath.m2",
    -- "OnesiteModificationA.m2",
    -- "OnesiteModificationB.m2",
    -- "OnesiteModificationC.m2",
    -- "OnesiteModificationD.m2",
    -- "TwolayerCascadeL.m2",
    -- "TwositeModificationE.m2",
    -- "TwositeModificationF.m2"
    },
    motif -> load("./ReactionNetworks/"|motif) 
    )
end

-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.

restart
uninstallPackage "ReactionNetworks"
installPackage "ReactionNetworks"
installPackage("ReactionNetworks", RemakeAllDocumentation=>true)
check "ReactionNetworks"
peek ReactionNetworks
help "OnesiteModificationA"
viewHelp "OnesiteModificationA"
examples "OnesiteModificationA"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=PackageTemplate pre-install"
-- End:
